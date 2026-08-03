# ═══════════════════════════════════════════════════════════════════════
# R/mod_export.R
# ═══════════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("summary_strip")),
    div(
      class = "export-main-layout",
      card(
        class = "export-channel-panel",
        card_header(div(class = "card-header-inner",
                        icon("circle-check", class = "icon-blue-sm"),
                        "Channel Status")),
        uiOutput(ns("channel_status"))
      ),
      div(
        class = "export-right-stack",
        uiOutput(ns("scwa_workaround")),
        card(
          class = "export-package-panel",
          card_header(div(
            class = "card-header-between",
            div(class = "card-header-inner",
                icon("file-zipper", class = "icon-blue-sm"),
                "Export Package"),
            uiOutput(ns("readiness_badge"))
          )),
          uiOutput(ns("export_issues")),
          div(class = "export-contents-pad", uiOutput(ns("export_contents"))),
          uiOutput(ns("roi_coverage")),
          hr(class = "hr-export"),
          uiOutput(ns("download_section"))
        )
      )
    )
  )
}

mod_export_server <- function(id, results, data, config, channels,
                              clean_results = reactive(list()),
                              process_qa = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    export_file_names <- list(
      analytical_csv = "Analytical Splits Extended.csv",
      analytical_rdata = "Analytical Splits Extended.RData",
      side_mapping = "Side Model Mapping.csv",
      seed_indices = "Seed For Indices.csv",
      split_composition = "Split Composition.csv",
      channel_config = "Channel Config.csv"
    )
    scwa_flags <- reactiveVal(setNames(logical(0), character(0)))
    scwa_channel_cache <- reactiveValues(keys = character(0), values = list())

    profile_export <- function(label, expr) {
      if (isTRUE(getOption("pso.export.profile", FALSE)) ||
          isTRUE(getOption("pso.profile", FALSE))) {
        elapsed <- system.time(out <- force(expr))
        message(sprintf("[mod_export] %s built in %.3fs", label, elapsed[["elapsed"]]))
        out
      } else {
        force(expr)
      }
    }

    scwa_cache_get <- function(key) {
      keys <- scwa_channel_cache$keys %||% character(0)
      idx <- match(key, keys)
      if (is.na(idx)) return(NULL)
      list(hit = TRUE, value = (scwa_channel_cache$values %||% list())[[idx]])
    }

    scwa_cache_set <- function(key, value, max_items = 12L) {
      keys <- scwa_channel_cache$keys %||% character(0)
      values <- scwa_channel_cache$values %||% list()
      idx <- match(key, keys)
      if (!is.na(idx)) {
        keys <- keys[-idx]
        values <- values[-idx]
      }
      keys <- c(key, keys)
      values <- c(list(value), values)
      if (length(keys) > max_items) {
        keys <- keys[seq_len(max_items)]
        values <- values[seq_len(max_items)]
      }
      scwa_channel_cache$keys <- keys
      scwa_channel_cache$values <- values
      invisible(value)
    }

    normalize_export_split <- function(x) {
      trimws(as.character(x))
    }

    strip_export_time <- function(x) {
      stringr::str_remove(
        as.character(x),
        "(_Before .*|_Before_.*|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$"
      )
    }

    split_key_variants <- function(x) {
      x <- normalize_export_split(x)
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return(character(0))
      squish <- function(v) tolower(stringr::str_squish(as.character(v)))
      unique(c(x, strip_export_time(x), squish(x), squish(strip_export_time(x))))
    }

    split_export_signature <- function(x, cfg = list()) {
      base <- strip_export_time(x)
      suffix <- tolower(trimws(stringr::str_remove(as.character(x), stringr::fixed(base))))
      vars <- unique(normalize_export_split(cfg$varname_include %||% character(0)))
      vars <- vars[!is.na(vars) & nzchar(vars)]
      vars <- vars[order(nchar(vars), decreasing = TRUE)]

      matched_var <- ""
      remainder <- base
      for (vn in vars) {
        vn_l <- tolower(vn)
        base_l <- tolower(base)
        if (identical(base_l, vn_l)) {
          matched_var <- vn
          remainder <- ""
          break
        }
        prefix <- paste0(vn, "_")
        if (startsWith(base_l, tolower(prefix))) {
          matched_var <- vn
          remainder <- substring(base, nchar(prefix) + 1L)
          break
        }
      }

      if (!nzchar(matched_var)) {
        pieces <- strsplit(base, "_", fixed = TRUE)[[1]]
        matched_var <- pieces[1] %||% ""
        remainder <- if (length(pieces) > 1) paste(pieces[-1], collapse = "_") else ""
      }

      parts <- strsplit(remainder, "_", fixed = TRUE)[[1]]
      parts <- normalize_export_split(parts)
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(
        tolower(normalize_export_split(matched_var)),
        suffix,
        paste(sort(tolower(parts)), collapse = "|"),
        sep = "||"
      )
    }

    summarize_split_diagnostics <- function(df, nm, cfg = list()) {
      if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric(),
          total_spend = numeric()
        ))
      }

      out <- as.data.frame(df)
      out$VariableSplit <- normalize_export_split(out$VariableSplit)
      out <- out[!is.na(out$VariableSplit) & nzchar(out$VariableSplit), , drop = FALSE]
      if (!nrow(out)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric(),
          total_spend = numeric()
        ))
      }

      if (!"MainModelVariableName" %in% names(out)) {
        if ("model_var" %in% names(out)) {
          out$MainModelVariableName <- out$model_var
        } else {
          out$MainModelVariableName <- cfg$model_variable %||% cfg$channel_name %||% nm
        }
      }
      out$MainModelVariableName <- normalize_export_split(out$MainModelVariableName)
      out$MainModelVariableName[!nzchar(out$MainModelVariableName)] <-
        cfg$model_variable %||% cfg$channel_name %||% nm

      if (!"total_activity" %in% names(out)) out$total_activity <- NA_real_
      if (!"total_spend" %in% names(out)) out$total_spend <- NA_real_
      out$total_activity <- suppressWarnings(as.numeric(out$total_activity))
      out$total_spend <- suppressWarnings(as.numeric(out$total_spend))

      out %>%
        dplyr::group_by(.data$VariableSplit, .data$MainModelVariableName) %>%
        dplyr::summarise(
          total_activity = if (all(is.na(.data$total_activity))) NA_real_
          else sum(.data$total_activity, na.rm = TRUE),
          total_spend = if (all(is.na(.data$total_spend))) NA_real_
          else sum(.data$total_spend, na.rm = TRUE),
          .groups = "drop"
        )
    }

    activity_splits_from_rag <- function(res, cfg = list(), nm = "") {
      empty <- tibble::tibble(
        VariableSplit = character(),
        MainModelVariableName = character(),
        total_activity = numeric()
      )
      if (is.null(res) || is.null(res$rag)) return(empty)

      rag_df <- as.data.frame(res$rag)
      if (!nrow(rag_df)) return(empty)

      cross_cols <- res$cross_cols %||% cfg$cross_cols %||% character(0)
      id_cols <- intersect(c(cross_cols, "Period"), names(rag_df))
      num_cols <- setdiff(names(rag_df)[vapply(rag_df, is.numeric, logical(1))], id_cols)
      if (!length(num_cols)) return(empty)

      spend_kw <- cfg$spend_keyword %||% "Spend"
      if (nzchar(spend_kw)) {
        num_cols <- num_cols[!grepl(spend_kw, num_cols, ignore.case = TRUE)]
      }
      num_cols <- num_cols[!grepl("^ModelTotal$|^TotalCheck$|^Total$", num_cols, ignore.case = TRUE)]
      if (!length(num_cols)) return(empty)

      act_kw <- cfg$activity_keyword %||% ""
      act_cols <- if (nzchar(act_kw)) {
        grep(act_kw, num_cols, ignore.case = TRUE, value = TRUE)
      } else {
        character(0)
      }

      diag_cols <- character(0)
      if (!is.null(res$act_diagnoses) &&
          "VariableSplit" %in% names(res$act_diagnoses)) {
        diag_cols <- normalize_export_split(unique(res$act_diagnoses$VariableSplit))
        diag_cols <- intersect(num_cols, diag_cols)
      }
      act_cols <- unique(c(act_cols, diag_cols))
      if (!length(act_cols)) return(empty)

      active_cols <- act_cols[vapply(act_cols, function(col) {
        vals <- suppressWarnings(as.numeric(rag_df[[col]]))
        any(!is.na(vals) & vals != 0)
      }, logical(1))]
      if (!length(active_cols)) return(empty)

      mmv <- cfg$model_variable %||% cfg$channel_name %||% nm
      tibble::tibble(
        VariableSplit = active_cols,
        MainModelVariableName = mmv,
        total_activity = vapply(active_cols, function(col) {
          sum(suppressWarnings(as.numeric(rag_df[[col]])), na.rm = TRUE)
        }, numeric(1))
      ) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    spend_splits_from_rag <- function(res, cfg = list(), nm = "") {
      empty <- tibble::tibble(
        VariableSplit = character(),
        MainModelVariableName = character(),
        total_spend = numeric()
      )
      if (is.null(res) || is.null(res$rag)) return(empty)

      rag_df <- as.data.frame(res$rag)
      if (!nrow(rag_df)) return(empty)

      cross_cols <- res$cross_cols %||% cfg$cross_cols %||% character(0)
      id_cols <- intersect(c(cross_cols, "Period"), names(rag_df))
      num_cols <- setdiff(names(rag_df)[vapply(rag_df, is.numeric, logical(1))], id_cols)
      num_cols <- num_cols[!grepl("^ModelTotal$|^TotalCheck$|^Total$", num_cols, ignore.case = TRUE)]
      if (!length(num_cols)) return(empty)

      spend_kw <- cfg$spend_keyword %||% "Spend"
      spend_cols <- if (nzchar(spend_kw)) {
        grep(spend_kw, num_cols, ignore.case = TRUE, value = TRUE)
      } else {
        character(0)
      }

      diag_cols <- character(0)
      if (!is.null(res$cost_diagnoses) &&
          "VariableSplit" %in% names(res$cost_diagnoses)) {
        diag_cols <- normalize_export_split(unique(res$cost_diagnoses$VariableSplit))
        diag_cols <- intersect(num_cols, diag_cols)
      }
      spend_cols <- unique(c(spend_cols, diag_cols))
      if (!length(spend_cols)) return(empty)

      active_cols <- spend_cols[vapply(spend_cols, function(col) {
        vals <- suppressWarnings(as.numeric(rag_df[[col]]))
        any(!is.na(vals) & vals != 0)
      }, logical(1))]
      if (!length(active_cols)) return(empty)

      mmv <- cfg$model_variable %||% cfg$channel_name %||% nm
      tibble::tibble(
        VariableSplit = active_cols,
        MainModelVariableName = mmv,
        total_spend = vapply(active_cols, function(col) {
          sum(suppressWarnings(as.numeric(rag_df[[col]])), na.rm = TRUE)
        }, numeric(1))
      ) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    export_metric_totals_from_rae <- function(d, cfg = list(), gcfg = list(), nm = "") {
      empty <- list(
        activity = tibble::tibble(VariableSplit = character(), total_activity = numeric()),
        spend = tibble::tibble(VariableSplit = character(), total_spend = numeric()),
        seed = tibble::tibble(
          VariableSplit = character(), Geography = character(),
          total_activity = numeric(), total_spend = numeric()
        )
      )
      if (is.null(d) || is.null(d$all_rags) || !"Period" %in% names(d$all_rags) ||
          !"VariableName" %in% names(d$all_rags)) {
        return(empty)
      }

      source_data <- as.data.frame(d$all_rags)
      source_data$Period <- if (inherits(source_data$Period, "Date")) {
        source_data$Period
      } else {
        parse_period_robust(source_data$Period)
      }
      source_data <- source_data[!is.na(source_data$Period), , drop = FALSE]
      if (!nrow(source_data)) return(empty)

      min_p <- tryCatch(as.Date(cfg$min_period), error = \(e) as.Date(NA))
      max_p <- tryCatch(as.Date(cfg$max_period), error = \(e) as.Date(NA))
      if (!is.na(min_p)) source_data <- source_data[source_data$Period >= min_p, , drop = FALSE]
      if (!is.na(max_p)) source_data <- source_data[source_data$Period <= max_p, , drop = FALSE]

      if (!is.null(d$dates_df) && "Period" %in% names(d$dates_df) && nrow(d$dates_df) > 0) {
        date_spine <- d$dates_df$Period
        date_spine <- if (inherits(date_spine, "Date")) date_spine else parse_period_robust(date_spine)
        date_spine <- date_spine[!is.na(date_spine)]
        if (length(date_spine)) {
          source_data <- source_data[
            source_data$Period >= min(date_spine) &
              source_data$Period <= max(date_spine), , drop = FALSE
          ]
        }
      }
      if (!nrow(source_data)) return(empty)

      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (length(vi) > 0) {
        vi <- expand_varname_include_with_spend(
          unique(source_data$VariableName),
          vi,
          cfg$spend_keyword %||% NULL
        )
        vi <- unique(trimws(as.character(vi)))
        vi <- vi[!is.na(vi) & nzchar(vi)]
      }
      if (length(vi) > 0) {
        vn <- trimws(as.character(source_data$VariableName))
        match_mode <- cfg$varname_match_mode %||%
          if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
        keep <- if (identical(match_mode, "exact")) {
          tolower(vn) %in% tolower(vi)
        } else {
          pattern <- paste(
            paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")),
            collapse = "|"
          )
          grepl(pattern, vn, ignore.case = TRUE, perl = TRUE)
        }
        source_data <- source_data[keep %in% TRUE, , drop = FALSE]
      }
      if (!nrow(source_data)) return(empty)

      filter_regex <- function(df, col, pats) {
        if (!col %in% names(df)) return(df)
        for (p in pats %||% character(0)) {
          if (nchar(p %||% "") > 0)
            df <- df[!grepl(p, df[[col]], ignore.case = TRUE), , drop = FALSE]
        }
        df
      }

      has_geo_overrides <- length(cfg$segment_overrides %||% list()) > 0 &&
        any(vapply(cfg$segment_overrides %||% list(), function(o) {
          length(o$geography_exclude %||% character(0)) > 0
        }, logical(1)))

      source_data <- filter_regex(source_data, "VariableName", cfg$varname_exclude)
      if (!has_geo_overrides)
        source_data <- filter_regex(source_data, "Geography", cfg$geography_exclude)
      source_data <- filter_regex(source_data, "Campaign", cfg$campaign_exclude)
      source_data <- filter_regex(source_data, "Outlet", cfg$outlet_exclude)
      source_data <- filter_regex(source_data, "Creative", cfg$creative_exclude)

      if (!is.null(d$schema_metadata) &&
          !is.null(d$schema_metadata$name_lookup) &&
          length(d$schema_metadata$useful_long) > 0 &&
          length(cfg$analytical_varkeys %||% character(0)) > 0) {
        nl <- d$schema_metadata$name_lookup
        for (dim in d$schema_metadata$useful_long) {
          if (!dim %in% names(source_data)) next
          dim_vals <- get_useful_long_values(cfg$analytical_varkeys, nl, dim)
          if (length(dim_vals) > 0)
            source_data <- source_data[source_data[[dim]] %in% dim_vals, , drop = FALSE]
        }
      }

      if (has_geo_overrides) {
        seg_ovr <- Filter(\(o) isTRUE(o$seg == 1L), cfg$segment_overrides %||% list())
        geo_exc <- if (length(seg_ovr) > 0)
          seg_ovr[[1]]$geography_exclude %||% character(0)
        else
          cfg$geography_exclude %||% character(0)
        source_data <- filter_regex(source_data, "Geography", geo_exc)
      }
      if (!nrow(source_data)) return(empty)

      start_d <- tryCatch(as.Date(gcfg$start_report_date), error = \(e) as.Date(NA))
      end_d <- tryCatch(as.Date(gcfg$end_report_date), error = \(e) as.Date(NA))
      update_label <- gcfg$update_label %||% "Focus"
      if (!is.na(end_d)) source_data <- source_data[source_data$Period <= end_d, , drop = FALSE]
      if (!nrow(source_data)) return(empty)

      source_data$VariableValue <- suppressWarnings(as.numeric(as.character(source_data$VariableValue)))
      source_data$VariableValue[is.na(source_data$VariableValue)] <- 0

      source_data <- apply_dimension_breaks(
        source_data,
        cfg$dimension_breaks %||% list(),
        channel_name = cfg$channel_name %||% nm
      )
      source_data <- apply_dimension_aliases(
        source_data,
        cfg$dimension_aliases %||% list()
      )
      split_cols_technical <- unique(c("VariableName", cfg$split_columns %||% character(0)))
      source_data$SplitName <- build_split_name_from_columns(source_data, split_cols_technical)

      period_tag <- rep(NA_character_, nrow(source_data))
      if (!is.na(start_d))
        period_tag[source_data$Period < start_d] <- "nonfocus"
      if (!is.na(start_d) && !is.na(end_d))
        period_tag[source_data$Period >= start_d & source_data$Period <= end_d] <- "focus"
      if (is.na(start_d) && !is.na(end_d))
        period_tag[source_data$Period <= end_d] <- "focus"
      keep_period <- !is.na(period_tag)
      source_data <- source_data[keep_period, , drop = FALSE]
      period_tag <- period_tag[keep_period]
      if (!nrow(source_data)) return(empty)

      nf_sfx <- {
        tbr <- cfg$time_break_label %||% ""
        if (nzchar(tbr)) paste0("Before ", update_label, "|", tbr)
        else paste0("Before ", update_label)
      }
      source_data$VariableSplit <- ifelse(
        period_tag == "focus",
        paste0(source_data$SplitName, "_", update_label),
        paste0(source_data$SplitName, "_", nf_sfx)
      )

      roi_file_for_keys <- clean_roi_columns(d$channels_rois)
      roi_seed_key_cols <- roi_rae_key_columns(roi_file_for_keys, source_data)
      roi_seed_key_cols <- setdiff(roi_seed_key_cols, "Geography")

      cross_cols <- gcfg$cross_cols %||% "Geography"
      cross_id <- intersect(c(cross_cols, "Period"), names(source_data))
      if (!"Period" %in% cross_id) cross_id <- c(cross_id, "Period")
      geo_col <- cross_cols[1]

      dt <- data.table::as.data.table(source_data)
      by_cols <- unique(c(cross_id, "VariableSplit"))
      agg <- dt[, .(Value = sum(VariableValue, na.rm = TRUE)), by = by_cols]
      if (!nrow(agg)) return(empty)

      is_local_by_split <- agg[, {
        local_period <- vapply(split(Value, Period), function(x) {
          nz <- x[!is.na(x) & x > 0]
          length(nz) >= 2 && diff(range(nz)) / max(nz) > 0.001
        }, logical(1))
        .(is_local = any(local_period))
      }, by = VariableSplit]

      period_totals <- merge(agg, is_local_by_split, by = "VariableSplit", all.x = TRUE)
      period_totals <- period_totals[, .(
        PeriodTotal = if (isTRUE(is_local[1])) {
          sum(Value, na.rm = TRUE)
        } else {
          pos <- Value[!is.na(Value) & Value > 0]
          if (length(pos)) max(pos) else 0
        },
        is_local = isTRUE(is_local[1])
      ), by = .(VariableSplit, Period)]

      totals <- period_totals[, .(
        total = sum(PeriodTotal, na.rm = TRUE),
        is_local = isTRUE(is_local[1])
      ), by = VariableSplit]

      act_kw <- cfg$activity_keyword %||% ""
      spend_kw <- cfg$spend_keyword %||% ""
      act_rows <- if (nzchar(act_kw)) {
        totals[grepl(act_kw, VariableSplit, ignore.case = TRUE)]
      } else totals[0]
      spend_rows <- if (nzchar(spend_kw)) {
        totals[grepl(spend_kw, VariableSplit, ignore.case = TRUE)]
      } else totals[0]

      seed_activity <- if (nrow(act_rows)) {
        tibble::tibble(
          VariableSplit = act_rows$VariableSplit,
          total_activity = act_rows$total,
          is_local = act_rows$is_local
        )
      } else tibble::tibble(VariableSplit = character(), total_activity = numeric(), is_local = logical())

      activity <- seed_activity %>%
        dplyr::select(VariableSplit, total_activity)

      spend <- if (nrow(spend_rows)) {
        tibble::tibble(
          VariableSplit = spend_rows$VariableSplit,
          total_spend = spend_rows$total,
          is_local = spend_rows$is_local
        )
      } else tibble::tibble(VariableSplit = character(), total_spend = numeric())

      if (identical(normalize_model_metric(cfg$model_metric %||% "activity"), "spend")) {
        seed <- spend %>%
          dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
          dplyr::mutate(
            key = stringr::str_remove_all(.data$VariableSplit, stringr::regex(spend_kw, ignore_case = TRUE))
          )
        if (nrow(seed)) {
          activity_focus <- activity %>%
            dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
            dplyr::mutate(
              key = stringr::str_remove_all(.data$VariableSplit, stringr::regex(act_kw, ignore_case = TRUE))
            ) %>%
            dplyr::select(key, total_activity)
          seed <- seed %>%
            dplyr::left_join(activity_focus, by = "key") %>%
            dplyr::select(-key)
        } else {
          seed$total_activity <- numeric(0)
        }

        if (geo_col %in% names(agg) && nrow(seed)) {
          local_splits <- seed$VariableSplit[seed$is_local %in% TRUE]
          geo_totals <- agg[VariableSplit %in% local_splits, .(
            total_spend = sum(Value, na.rm = TRUE)
          ), by = c("VariableSplit", geo_col)]
          if (nrow(geo_totals)) {
            setnames(geo_totals, geo_col, "Geography")
            activity_geo <- if (nzchar(act_kw) && nrow(act_rows)) {
              act_focus_names <- activity$VariableSplit[
                !grepl("_Before(\\s+|_)", activity$VariableSplit, ignore.case = TRUE)
              ]
              act_geo <- agg[VariableSplit %in% act_focus_names, .(
                total_activity = sum(Value, na.rm = TRUE)
              ), by = c("VariableSplit", geo_col)]
              if (nrow(act_geo)) {
                setnames(act_geo, geo_col, "Geography")
                as.data.frame(act_geo) %>%
                  dplyr::mutate(
                    key = stringr::str_remove_all(
                      .data$VariableSplit,
                      stringr::regex(act_kw, ignore_case = TRUE)
                    )
                  ) %>%
                  dplyr::select(key, Geography, total_activity)
              } else {
                tibble::tibble(key = character(), Geography = character(), total_activity = numeric())
              }
            } else {
              tibble::tibble(key = character(), Geography = character(), total_activity = numeric())
            }
            seed_local <- as.data.frame(geo_totals) %>%
              dplyr::mutate(
                key = stringr::str_remove_all(
                  .data$VariableSplit,
                  stringr::regex(spend_kw, ignore_case = TRUE)
                )
              ) %>%
              dplyr::left_join(activity_geo, by = c("key", "Geography")) %>%
              dplyr::select(-key)
            seed_national <- seed %>%
              dplyr::filter(!.data$VariableSplit %in% local_splits) %>%
              dplyr::mutate(Geography = NA_character_) %>%
              dplyr::select(VariableSplit, Geography, total_activity, total_spend)
            seed <- dplyr::bind_rows(seed_local, seed_national)
          } else {
            seed <- seed %>% dplyr::mutate(Geography = NA_character_)
          }
        } else if (nrow(seed)) {
          seed <- seed %>% dplyr::mutate(Geography = NA_character_)
        }

        if (!"Geography" %in% names(seed)) seed$Geography <- NA_character_
        if (!"total_activity" %in% names(seed)) seed$total_activity <- NA_real_
        if (nrow(seed)) {
          spend_meta_source <- source_data[
            period_tag == "focus" &
              grepl(spend_kw, source_data$VariableSplit, ignore.case = TRUE),
            , drop = FALSE
          ]
          if (nrow(spend_meta_source)) {
            spend_meta_source$`Sourced VariableName` <- spend_meta_source$VariableName
            meta_cols <- unique(c("Sourced VariableName", roi_seed_key_cols))
            meta_cols <- intersect(meta_cols, names(spend_meta_source))
            if (length(meta_cols)) {
              meta_dt <- data.table::as.data.table(spend_meta_source[, c("VariableSplit", meta_cols), drop = FALSE])
              split_meta <- meta_dt[, lapply(.SD, function(x) {
                vals <- unique(trimws(as.character(x)))
                vals <- vals[!is.na(vals) & nzchar(vals)]
                if (length(vals) == 1L) vals[1] else NA_character_
              }), by = VariableSplit, .SDcols = meta_cols]
              seed <- seed %>%
                dplyr::left_join(as.data.frame(split_meta), by = "VariableSplit")
            }
          }
        }
        seed <- seed %>%
          dplyr::select(VariableSplit, Geography, total_activity, total_spend,
                        dplyr::any_of(c("Sourced VariableName", roi_seed_key_cols)))
        return(list(activity = activity, spend = spend %>% dplyr::select(VariableSplit, total_spend),
                    seed = seed))
      }

      seed <- seed_activity %>%
        dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
        dplyr::mutate(
          key = stringr::str_remove_all(.data$VariableSplit, stringr::regex(act_kw, ignore_case = TRUE))
        )
      if (nrow(seed)) {
        spend_focus <- spend %>%
          dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
          dplyr::mutate(
            key = stringr::str_remove_all(.data$VariableSplit, stringr::regex(spend_kw, ignore_case = TRUE))
          ) %>%
          dplyr::select(key, total_spend)
        seed <- seed %>%
          dplyr::left_join(spend_focus, by = "key") %>%
          dplyr::select(-key)
      } else {
        seed$total_spend <- numeric(0)
      }

      if (geo_col %in% names(agg) && nrow(seed)) {
        local_splits <- seed$VariableSplit[seed$is_local %in% TRUE]
        geo_totals <- agg[VariableSplit %in% local_splits, .(
          total_activity = sum(Value, na.rm = TRUE)
        ), by = c("VariableSplit", geo_col)]
        if (nrow(geo_totals)) {
          setnames(geo_totals, geo_col, "Geography")
          geo_spend <- if (nzchar(spend_kw) && nrow(spend_rows)) {
            spend_focus_names <- spend$VariableSplit[
              !grepl("_Before(\\s+|_)", spend$VariableSplit, ignore.case = TRUE)
            ]
            spend_geo <- agg[VariableSplit %in% spend_focus_names, .(
              total_spend = sum(Value, na.rm = TRUE)
            ), by = c("VariableSplit", geo_col)]
            if (nrow(spend_geo)) {
              setnames(spend_geo, geo_col, "Geography")
              as.data.frame(spend_geo) %>%
                dplyr::mutate(
                  key = stringr::str_remove_all(
                    .data$VariableSplit,
                    stringr::regex(spend_kw, ignore_case = TRUE)
                  )
                ) %>%
                dplyr::select(key, Geography, total_spend)
            } else {
              tibble::tibble(key = character(), Geography = character(), total_spend = numeric())
            }
          } else {
            tibble::tibble(key = character(), Geography = character(), total_spend = numeric())
          }
          seed_local <- as.data.frame(geo_totals) %>%
            dplyr::mutate(
              key = stringr::str_remove_all(
                .data$VariableSplit,
                stringr::regex(act_kw, ignore_case = TRUE)
              )
            ) %>%
            dplyr::left_join(geo_spend, by = c("key", "Geography")) %>%
            dplyr::select(-key)
          seed_national <- seed %>%
            dplyr::filter(!.data$VariableSplit %in% local_splits) %>%
            dplyr::mutate(Geography = NA_character_) %>%
            dplyr::select(VariableSplit, Geography, total_activity, total_spend)
          seed <- dplyr::bind_rows(seed_local, seed_national)
        } else {
          seed <- seed %>% dplyr::mutate(Geography = NA_character_)
        }
      } else if (nrow(seed)) {
        seed <- seed %>% dplyr::mutate(Geography = NA_character_)
      }

      if (!"Geography" %in% names(seed))
        seed$Geography <- NA_character_
      if (!"total_spend" %in% names(seed))
        seed$total_spend <- NA_real_

      if (nrow(seed)) {
        activity_meta_source <- source_data[
          period_tag == "focus" &
            grepl(act_kw, source_data$VariableSplit, ignore.case = TRUE),
          , drop = FALSE
        ]
        if (nrow(activity_meta_source)) {
          activity_meta_source$`Sourced VariableName` <- activity_meta_source$VariableName
          meta_cols <- unique(c("Sourced VariableName", roi_seed_key_cols))
          meta_cols <- intersect(meta_cols, names(activity_meta_source))
          if (length(meta_cols)) {
            meta_dt <- data.table::as.data.table(activity_meta_source[, c("VariableSplit", meta_cols), drop = FALSE])
            split_meta <- meta_dt[, lapply(.SD, function(x) {
              vals <- unique(trimws(as.character(x)))
              vals <- vals[!is.na(vals) & nzchar(vals)]
              if (length(vals) == 1L) vals[1] else NA_character_
            }), by = VariableSplit, .SDcols = meta_cols]
            seed <- seed %>%
              dplyr::left_join(as.data.frame(split_meta), by = "VariableSplit")
          }
        }
      }

      seed <- seed %>%
        dplyr::select(VariableSplit, Geography, total_activity, total_spend,
                      dplyr::any_of(c("Sourced VariableName", roi_seed_key_cols)))

      list(activity = activity,
           spend = spend %>% dplyr::select(VariableSplit, total_spend),
           seed = seed)
    }

    final_activity_splits <- function(res, cfg = list(), nm = "") {
      if (is.null(res)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric()
        ))
      }

      if (identical(normalize_model_metric(res$model_metric %||% cfg$model_metric %||% "activity"), "spend")) {
        spend_final <- final_spend_splits(res, cfg, nm)
        if (!nrow(spend_final)) {
          return(tibble::tibble(
            VariableSplit = character(),
            MainModelVariableName = character(),
            total_activity = numeric()
          ))
        }
        return(spend_final %>%
                 dplyr::mutate(total_activity = NA_real_) %>%
                 dplyr::select(dplyr::all_of(c("VariableSplit", "MainModelVariableName",
                                                "total_activity")),
                               dplyr::everything()))
      }

      final <- activity_splits_from_rag(res, cfg, nm)
      if (!nrow(final)) return(final)

      diag_meta <- summarize_split_diagnostics(res$act_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_activity.diag = total_activity) %>%
        dplyr::distinct(VariableSplit, .keep_all = TRUE)
      if (nrow(diag_meta) > 0) {
        final <- final %>%
          dplyr::left_join(diag_meta, by = "VariableSplit", suffix = c("", ".diag")) %>%
          dplyr::mutate(
            MainModelVariableName = dplyr::coalesce(
              .data$MainModelVariableName.diag,
              .data$MainModelVariableName
            ),
            total_activity = dplyr::coalesce(.data$total_activity, .data$total_activity.diag)
          ) %>%
          dplyr::select(-dplyr::any_of(c("MainModelVariableName.diag", "total_activity.diag")))
      }

      if (!is.null(res$side_mapping) &&
          nrow(res$side_mapping) > 0 &&
          "VariableSplit" %in% names(res$side_mapping)) {
        sm <- as.data.frame(res$side_mapping)
        sm$VariableSplit <- normalize_export_split(sm$VariableSplit)
        sm <- sm[sm$VariableSplit %in% final$VariableSplit, , drop = FALSE]
        if ("MainModelVariableName" %in% names(sm)) {
          sm$MainModelVariableName <- normalize_export_split(sm$MainModelVariableName)
          final <- final %>%
            dplyr::left_join(
              sm %>%
                dplyr::select(dplyr::any_of(c("VariableSplit", "MainModelVariableName"))) %>%
                dplyr::filter(nzchar(.data$MainModelVariableName)) %>%
                dplyr::distinct(VariableSplit, .keep_all = TRUE),
              by = "VariableSplit",
              suffix = c("", ".side")
            ) %>%
            dplyr::mutate(
              MainModelVariableName = dplyr::coalesce(
                .data$MainModelVariableName.side,
                .data$MainModelVariableName
              )
            ) %>%
            dplyr::select(-dplyr::any_of("MainModelVariableName.side"))
        }
      }

      final %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    pre_merge_activity_splits <- function(clean, cfg = list(), nm = "") {
      if (identical(normalize_model_metric(clean$model_metric %||% cfg$model_metric %||% "activity"), "spend")) {
        spend_pre <- pre_merge_spend_splits(clean, cfg, nm)
        return(spend_pre %>%
                 dplyr::mutate(total_activity = NA_real_) %>%
                 dplyr::select(dplyr::all_of(c("VariableSplit", "MainModelVariableName",
                                                "total_activity")),
                               dplyr::everything()))
      }
      pre <- activity_splits_from_rag(clean, cfg, nm)
      if (nrow(pre)) return(pre)
      summarize_split_diagnostics(clean$act_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_activity)
    }

    final_spend_splits <- function(res, cfg = list(), nm = "") {
      final <- spend_splits_from_rag(res, cfg, nm)
      if (!nrow(final)) {
        return(
          summarize_split_diagnostics(res$cost_diagnoses, nm, cfg) %>%
            dplyr::select(VariableSplit, MainModelVariableName, total_spend) %>%
            dplyr::filter(!is.na(.data$total_spend) & .data$total_spend > 0) %>%
            dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
        )
      }

      diag_meta <- summarize_split_diagnostics(res$cost_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_spend.diag = total_spend) %>%
        dplyr::distinct(VariableSplit, .keep_all = TRUE)
      if (nrow(diag_meta) > 0) {
        final <- final %>%
          dplyr::left_join(diag_meta, by = "VariableSplit", suffix = c("", ".diag")) %>%
          dplyr::mutate(
            MainModelVariableName = dplyr::coalesce(
              .data$MainModelVariableName.diag,
              .data$MainModelVariableName
            ),
            total_spend = dplyr::coalesce(.data$total_spend, .data$total_spend.diag)
          ) %>%
          dplyr::select(-dplyr::any_of(c("MainModelVariableName.diag", "total_spend.diag")))
      }

      final %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    pre_merge_spend_splits <- function(clean, cfg = list(), nm = "") {
      pre <- spend_splits_from_rag(clean, cfg, nm)
      if (nrow(pre)) return(pre)
      summarize_split_diagnostics(clean$cost_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_spend) %>%
        dplyr::filter(!is.na(.data$total_spend) & .data$total_spend > 0) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    extract_export_merges <- function(cfg) {
      first_non_empty <- function(x) {
        x <- unlist(x, use.names = FALSE)
        x <- normalize_export_split(x)
        x <- x[!is.na(x) & nzchar(x)]
        if (length(x)) x[1] else NA_character_
      }

      merges <- cfg$saved_merges %||% list()
      if (is.data.frame(merges)) merges <- split(merges, seq_len(nrow(merges)))
      rows <- lapply(merges, function(m) {
        if (is.null(m) || !is.list(m)) return(NULL)
        active <- isTRUE(m$active) || isTRUE(m$enabled) || isTRUE(m$checked)
        if (!active) return(NULL)
        merge_metric <- normalize_model_metric(m$metric %||% cfg$model_metric %||% "activity")
        channel_metric <- normalize_model_metric(cfg$model_metric %||% "activity")
        if (!is.null(m$metric) && !identical(merge_metric, channel_metric)) return(NULL)
        merged_name <- first_non_empty(c(
          m$new_name, m$name, m$merged_split, m$MergeName, m$merge_name
        ))
        if (is.na(merged_name) || !nzchar(merged_name)) return(NULL)
        comps <- m$merged %||%
          m$merged_splits %||%
          m$components %||%
          m$component_splits %||%
          m$merged_items
        comps <- normalize_export_split(unlist(comps, use.names = FALSE))
        comps <- unique(comps[!is.na(comps) & nzchar(comps)])
        if (!length(comps)) return(NULL)
        list(MergedSplitName = merged_name, Components = comps)
      })
      Filter(Negate(is.null), rows)
    }

    resolve_split_name <- function(name, candidates, cfg = list()) {
      name <- normalize_export_split(name)
      candidates <- normalize_export_split(candidates)
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (!length(candidates) || is.na(name) || !nzchar(name)) return(NA_character_)
      exact <- candidates[candidates == name]
      if (length(exact)) return(exact[1])
      keys <- split_key_variants(name)
      for (key in keys) {
        hit <- candidates[vapply(candidates, function(candidate) {
          key %in% split_key_variants(candidate)
        }, logical(1))]
        hit <- hit[!is.na(hit) & nzchar(hit)]
        if (length(hit) == 1L) return(hit[1])
      }
      name_sig <- split_export_signature(name, cfg)
      candidate_sigs <- vapply(candidates, split_export_signature, character(1), cfg = cfg)
      sig_hits <- candidates[candidate_sigs == name_sig]
      sig_hits <- sig_hits[!is.na(sig_hits) & nzchar(sig_hits)]
      if (length(sig_hits) == 1L) return(sig_hits[1])
      NA_character_
    }

    resolve_export_merge_map <- function(cfg, final_splits, component_splits) {
      merges <- extract_export_merges(cfg)
      final_names <- final_splits$VariableSplit %||% character(0)
      component_names <- component_splits$VariableSplit %||% character(0)
      issues <- character(0)
      empty_map <- tibble::tibble(
        MergedSplitName = character(),
        ComponentSplit = character()
      )

      rows <- lapply(merges, function(m) {
        resolved_merge <- resolve_split_name(m$MergedSplitName, final_names, cfg)
        if (is.na(resolved_merge) || !nzchar(resolved_merge)) {
          issues <<- c(issues, paste0("Missing merged split: ", m$MergedSplitName))
          return(NULL)
        }
        comps <- vapply(m$Components, resolve_split_name, character(1),
                        candidates = component_names, cfg = cfg)
        missing <- m$Components[is.na(comps) | !nzchar(comps)]
        if (length(missing)) {
          issues <<- c(issues, paste0(
            resolved_merge, " missing component(s): ",
            paste(utils::head(missing, 3), collapse = " | "),
            if (length(missing) > 3) paste0(" +", length(missing) - 3, " more") else ""
          ))
        }
        comps <- unique(comps[!is.na(comps) & nzchar(comps)])
        if (!length(comps)) return(NULL)
        tibble::tibble(MergedSplitName = resolved_merge, ComponentSplit = comps)
      })

      list(
        map = if (length(Filter(Negate(is.null), rows))) {
          dplyr::bind_rows(Filter(Negate(is.null), rows))
        } else {
          empty_map
        },
        issues = unique(issues)
      )
    }

    build_canonical_export_totals <- function(rae_totals, merge_resolved,
                                              model_metric = "activity") {
      empty_component <- tibble::tibble(
        VariableSplit = character(),
        Component_Activity = numeric(),
        Component_Spend = numeric()
      )
      empty_final <- tibble::tibble(
        VariableSplit = character(),
        Activity = numeric(),
        Spend = numeric()
      )
      empty_seed <- tibble::tibble(
        VariableSplit = character(),
        Geography = character(),
        total_activity = numeric(),
        total_spend = numeric()
      )
      empty <- list(
        component_focus_totals = empty_component,
        final_focus_totals = empty_final,
        seed_focus_totals = empty_seed,
        merge_map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
      )

      component_seed <- rae_totals$seed %||% NULL
      if (is.null(component_seed) || !nrow(component_seed) ||
          !"VariableSplit" %in% names(component_seed)) {
        return(empty)
      }
      component_seed <- as.data.frame(component_seed)
      if (!"Geography" %in% names(component_seed)) component_seed$Geography <- NA_character_
      if (!"total_activity" %in% names(component_seed)) component_seed$total_activity <- 0
      if (!"total_spend" %in% names(component_seed)) component_seed$total_spend <- 0
      component_seed$VariableSplit <- normalize_export_split(component_seed$VariableSplit)
      component_seed$Geography <- as.character(component_seed$Geography)
      component_seed$total_activity <- suppressWarnings(as.numeric(component_seed$total_activity))
      component_seed$total_spend <- suppressWarnings(as.numeric(component_seed$total_spend))
      component_seed$total_activity[is.na(component_seed$total_activity)] <- 0
      component_seed$total_spend[is.na(component_seed$total_spend)] <- 0
      seed_meta_cols <- setdiff(
        names(component_seed),
        c("VariableSplit", "Geography", "total_activity", "total_spend")
      )
      stable_seed_meta <- function(x) {
        vals <- unique(trimws(as.character(x)))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals) == 1L) vals[[1]] else NA_character_
      }
      component_seed <- component_seed[
        !is.na(component_seed$VariableSplit) &
          nzchar(component_seed$VariableSplit) &
          !grepl("_Before(\\s+|_)", component_seed$VariableSplit, ignore.case = TRUE),
        , drop = FALSE
      ]
      if (!nrow(component_seed)) return(empty)

      component_focus <- component_seed %>%
        dplyr::group_by(.data$VariableSplit) %>%
        dplyr::summarise(
          Component_Activity = sum(.data$total_activity, na.rm = TRUE),
          Component_Spend = sum(.data$total_spend, na.rm = TRUE),
          .groups = "drop"
        )

      merge_map <- merge_resolved$map %||%
        tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
      if (!is.null(merge_map) && nrow(merge_map)) {
        merge_map <- as.data.frame(merge_map)
        merge_map$MergedSplitName <- normalize_export_split(merge_map$MergedSplitName)
        merge_map$ComponentSplit <- normalize_export_split(merge_map$ComponentSplit)
        merge_map <- merge_map[
          !is.na(merge_map$MergedSplitName) &
            nzchar(merge_map$MergedSplitName) &
            !grepl("_Before(\\s+|_)", merge_map$MergedSplitName, ignore.case = TRUE) &
            merge_map$ComponentSplit %in% component_seed$VariableSplit,
          c("MergedSplitName", "ComponentSplit"),
          drop = FALSE
        ]
        merge_map <- dplyr::distinct(tibble::as_tibble(merge_map),
                                     .data$ComponentSplit, .keep_all = TRUE)
      } else {
        merge_map <- tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
      }

      final_seed <- component_seed %>%
        dplyr::left_join(merge_map, by = c("VariableSplit" = "ComponentSplit")) %>%
        dplyr::mutate(
          VariableSplit = dplyr::coalesce(.data$MergedSplitName, .data$VariableSplit)
        ) %>%
        dplyr::select(-dplyr::any_of("MergedSplitName")) %>%
        dplyr::group_by(.data$VariableSplit, .data$Geography) %>%
        dplyr::summarise(
          total_activity = sum(.data$total_activity, na.rm = TRUE),
          total_spend = sum(.data$total_spend, na.rm = TRUE),
          dplyr::across(dplyr::all_of(seed_meta_cols), stable_seed_meta),
          .groups = "drop"
        ) %>%
        dplyr::filter(
          if (identical(normalize_model_metric(model_metric), "spend"))
            .data$total_spend > 0 else .data$total_activity > 0
        )

      final_focus <- final_seed %>%
        dplyr::group_by(.data$VariableSplit) %>%
        dplyr::summarise(
          Activity = sum(.data$total_activity, na.rm = TRUE),
          Spend = sum(.data$total_spend, na.rm = TRUE),
          .groups = "drop"
        )

      list(
        component_focus_totals = component_focus,
        final_focus_totals = final_focus,
        seed_focus_totals = final_seed,
        merge_map = merge_map
      )
    }

    empty_export_metric_totals <- function() {
      list(
        activity = tibble::tibble(VariableSplit = character(), total_activity = numeric()),
        spend = tibble::tibble(VariableSplit = character(), total_spend = numeric()),
        seed = tibble::tibble(
          VariableSplit = character(), Geography = character(),
          total_activity = numeric(), total_spend = numeric()
        )
      )
    }

    ensure_channel_export_payload <- function(item, d, gcfg) {
      if (is.null(item) || !is.list(item)) return(item)
      if (!is.null(item$rae_totals) && !is.null(item$canonical_totals) &&
          !is.null(item$merge_resolved)) return(item)

      has_processed_splits <- !is.null(item$res) &&
        (nrow(item$final %||% tibble::tibble()) > 0 ||
           nrow(item$pre_act %||% tibble::tibble()) > 0)
      item$merge_resolved <- if (isTRUE(has_processed_splits) &&
                                 length(extract_export_merges(item$cfg %||% list()))) {
        resolve_export_merge_map(
          item$cfg %||% list(),
          item$final %||% tibble::tibble(),
          item$pre_act %||% tibble::tibble()
        )
      } else {
        list(
          map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character()),
          issues = character(0)
        )
      }

      cfg_effective <- item$cfg %||% list()
      cfg_effective$model_metric <- item$res$model_metric %||% cfg_effective$model_metric %||% "activity"

      item$rae_totals <- if (isTRUE(has_processed_splits)) {
        export_metric_totals_from_rae(d, cfg_effective, gcfg %||% list(), item$name %||% "")
      } else {
        empty_export_metric_totals()
      }
      item$canonical_totals <- build_canonical_export_totals(
        item$rae_totals,
        item$merge_resolved %||% list(
          map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character()),
          issues = character(0)
        ),
        model_metric = item$res$model_metric %||% item$cfg$model_metric %||% "activity"
      )
      item
    }

    ensure_export_payload <- function(snapshot) {
      if (is.null(snapshot) || is.null(snapshot$export_data)) return(snapshot)
      snapshot$export_data <- lapply(
        snapshot$export_data,
        ensure_channel_export_payload,
        d = snapshot$data,
        gcfg = snapshot$config
      )
      snapshot
    }

    build_channel_summary_item <- function(nm, res, cfg_ch, final_splits, d, gcfg) {
      if (is.null(res)) {
        return(list(name = nm, processed = FALSE, has_warning = FALSE,
                    tc_mismatch = FALSE, n_splits = 0L))
      }

      n_splits_total <- nrow(final_splits)

      if (n_splits_total == 0L) {
        return(list(name = nm, processed = TRUE, has_warning = TRUE,
                    tc_mismatch = FALSE, n_splits = 0L))
      }

      an_periods_all <- if (!is.null(d$analytical) &&
                            "Period" %in% names(d$analytical))
        sort(unique(d$analytical$Period)) else NULL
      an_min_all <- if (length(an_periods_all)) min(an_periods_all) else NULL
      an_max_all <- if (length(an_periods_all)) max(an_periods_all) else NULL
      has_analytical <- !is.null(d$analytical) &&
        !is.null(gcfg$cross_cols) && length(an_periods_all) > 0

      tc_mismatch <- tryCatch({
          if (!has_analytical) return(FALSE)
          model_var <- cfg_ch$model_variable %||% ""
          if (!nzchar(model_var) || !model_var %in% names(d$analytical)) return(FALSE)

          cross_cols  <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
          cross_id    <- c(cross_cols, "Period")
          geo_col     <- cross_cols[1]
          cfg_min     <- tryCatch(as.Date(cfg_ch$min_period), error = \(e) NA)
          cfg_max     <- tryCatch(as.Date(cfg_ch$max_period), error = \(e) NA)
          scope_min_p <- if (!is.na(cfg_min)) max(an_min_all, cfg_min) else an_min_all
          scope_max_p <- if (!is.na(cfg_max)) min(an_max_all, cfg_max) else an_max_all
          if (is.na(scope_min_p) || is.na(scope_max_p) || scope_min_p > scope_max_p) {
            scope_min_p <- an_min_all; scope_max_p <- an_max_all
          }

          rag_df      <- as.data.frame(res$rag)
          rag_periods <- sort(unique(rag_df$Period))
          an_scoped   <- an_periods_all[an_periods_all >= scope_min_p &
                                          an_periods_all <= scope_max_p]
          if (!length(an_scoped) || !length(rag_periods)) return(FALSE)

          rag_scope_periods <- {
            in_scope <- rag_periods[rag_periods >= scope_min_p & rag_periods <= scope_max_p]
            if (length(in_scope) > 0) in_scope else rag_periods
          }
          if (!length(rag_scope_periods)) return(FALSE)

          matched_idx        <- vapply(an_scoped, function(p)
            which.min(abs(as.numeric(rag_scope_periods) - as.numeric(p))), integer(1))
          period_map <- tibble::tibble(
            an_period = an_scoped,
            rag_period = rag_scope_periods[matched_idx]
          )

          model_at_an <- build_model_total(d$analytical, cross_id, c(model_var), character(0)) %>%
            dplyr::filter(Period >= scope_min_p, Period <= scope_max_p)

          normalize_geo <- function(x)
            trimws(gsub("\\s+", " ", tolower(gsub("[,.]", " ", as.character(x)))))

          rag_geos <- if (geo_col %in% names(rag_df)) unique(rag_df[[geo_col]]) else character(0)
          an_geos  <- if (geo_col %in% names(model_at_an)) unique(model_at_an[[geo_col]]) else character(0)
          geo_map <- if (length(an_geos) > 0 && length(rag_geos) > 0) {
            tibble::tibble(an_geo = an_geos, norm = normalize_geo(an_geos)) %>%
              dplyr::left_join(tibble::tibble(rag_geo = rag_geos, norm = normalize_geo(rag_geos)),
                               by = "norm") %>%
              dplyr::mutate(rag_geo = dplyr::if_else(is.na(rag_geo), an_geo, rag_geo)) %>%
              dplyr::select(an_geo, rag_geo)
          } else {
            tibble::tibble(an_geo = character(0), rag_geo = character(0))
          }

          model_at_an <- if (geo_col %in% names(model_at_an) && nrow(geo_map) > 0) {
            model_at_an %>%
              dplyr::rename(an_geo = !!rlang::sym(geo_col)) %>%
              dplyr::left_join(geo_map, by = "an_geo") %>%
              dplyr::mutate(!!geo_col := dplyr::if_else(!is.na(rag_geo), rag_geo, an_geo)) %>%
              dplyr::select(-an_geo, -rag_geo)
          } else {
            model_at_an
          }

          apply_geo_filters <- function(df, col) {
            if (!col %in% names(df)) return(df)
            for (p in cfg_ch$geography_exclude %||% character(0)) {
              if (nchar(p %||% "") > 0)
                df <- df[!grepl(p, df[[col]], ignore.case = TRUE), ]
            }
            for (so in cfg_ch$segment_overrides %||% list()) {
              geo_exc <- so$geography_exclude %||% character(0)
              if (!length(geo_exc)) next
              for (p in geo_exc) {
                if (nchar(p %||% "") > 0)
                  df <- df[!grepl(p, df[[col]], ignore.case = TRUE), ]
              }
            }
            df
          }

          rag_scope <- rag_df %>%
            dplyr::filter(Period >= scope_min_p, Period <= scope_max_p)
          rag_scope <- apply_geo_filters(rag_scope, geo_col)
          model_at_an <- apply_geo_filters(model_at_an, geo_col)

          id_in_rag    <- intersect(cross_id, names(rag_scope))
          model_metric <- normalize_model_metric(res$model_metric %||% cfg_ch$model_metric %||% "activity")
          spend_kw     <- cfg_ch$spend_keyword %||% "Spend"
          activity_kw  <- cfg_ch$activity_keyword %||% "Activity"
          all_num      <- setdiff(names(rag_scope)[vapply(rag_scope, is.numeric, logical(1))], id_in_rag)
          split_cols <- if (identical(model_metric, "spend")) {
            cols <- all_num[grepl(spend_kw, all_num, ignore.case = TRUE)]
            if (!length(cols) && !is.null(res$cost_diagnoses) &&
                "VariableSplit" %in% names(res$cost_diagnoses)) {
              cols <- intersect(normalize_export_split(unique(res$cost_diagnoses$VariableSplit)), all_num)
            }
            cols
          } else {
            cols <- all_num[!grepl(spend_kw, all_num, ignore.case = TRUE)]
            if (!length(cols) && nzchar(activity_kw)) {
              cols <- all_num[grepl(activity_kw, all_num, ignore.case = TRUE)]
            }
            if (!length(cols) && !is.null(res$act_diagnoses) &&
                "VariableSplit" %in% names(res$act_diagnoses)) {
              cols <- intersect(normalize_export_split(unique(res$act_diagnoses$VariableSplit)), all_num)
            }
            cols
          }
          if (!length(split_cols)) return(FALSE)

          rag_scope$row_splits <- rowSums(rag_scope[, split_cols, drop = FALSE], na.rm = TRUE)
          group_cols <- intersect(cross_id, names(rag_scope))
          if (!length(group_cols)) return(FALSE)

          splits_side <- rag_scope %>%
            dplyr::select(dplyr::any_of(c(group_cols, "row_splits"))) %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
            dplyr::summarise(SplitsTotal = sum(row_splits, na.rm = TRUE), .groups = "drop")

          model_side <- model_at_an %>%
            dplyr::rename(an_period = Period) %>%
            dplyr::left_join(period_map, by = "an_period", relationship = "many-to-one") %>%
            dplyr::mutate(Period = dplyr::if_else(!is.na(rag_period), rag_period, an_period)) %>%
            dplyr::select(dplyr::any_of(c(cross_cols, "Period", "ModelTotal"))) %>%
            dplyr::filter(ModelTotal > 0)

          join_cols <- intersect(cross_id, intersect(names(model_side), names(splits_side)))
          if (!length(join_cols)) return(FALSE)

          model_side <- model_side %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
            dplyr::summarise(ModelTotal = sum(ModelTotal, na.rm = TRUE), .groups = "drop") %>%
            dplyr::filter(ModelTotal > 0)

          check_df <- model_side %>%
            dplyr::left_join(splits_side, by = join_cols) %>%
            dplyr::mutate(
              SplitsTotal = tidyr::replace_na(SplitsTotal, 0),
              Diff = ModelTotal - SplitsTotal
            ) %>%
            dplyr::filter(ModelTotal > 0)

          any(abs(check_df$Diff) >= 0.01, na.rm = TRUE)
      }, error = function(e) FALSE)

      list(name = nm, processed = TRUE,
           has_warning = tc_mismatch, tc_mismatch = tc_mismatch,
           n_splits = n_splits_total)
    }

    build_file_dims <- function(export_data, d, ch_list) {
      total_splits <- sum(vapply(export_data, function(x) {
        if (!is.list(x)) return(0L)
        nrow(x$final %||% tibble::tibble())
      }, integer(1)))
      total_composition_rows <- sum(vapply(export_data, function(x) {
        if (!is.list(x)) return(0L)
        merges <- extract_export_merges(x$cfg %||% list())
        sum(vapply(merges, function(m) length(m$Components %||% character(0)), integer(1)))
      }, integer(1)))

      in_vars <- if (!is.null(d$details) &&
                     all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          dplyr::filter(!stringr::str_detect(
            stringr::str_to_lower(trimws(Type)), "none")) %>%
          dplyr::pull(VariableName) %>% unique()
      } else {
        mv <- unique(vapply(ch_list, \(c) c$model_variable %||% "", character(1)))
        mv[nzchar(mv)]
      }

      id_an   <- length(intersect(c("Geography", "Product", "Period", "BP_Year"),
                                  names(d$analytical %||% list())))
      an_vars <- if (!is.null(d$analytical))
        length(intersect(in_vars, names(d$analytical))) else 0L
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical) else 0L
      an_cols <- id_an + an_vars + total_splits
      n_ch    <- length(ch_list)

      nonfocus_n <- if (!is.null(d$side_mapping_nonfocus))
        nrow(d$side_mapping_nonfocus) else 0L
      rois_for_dims <- clean_roi_columns(d$channels_rois)
      seed_has_geo <- !is.null(rois_for_dims) && "Geography" %in% names(rois_for_dims)
      seed_roi_cols <- if (!is.null(rois_for_dims)) {
        setdiff(names(rois_for_dims)[sapply(rois_for_dims, is.numeric)],
                c("MainModelVariableName", "Channel"))
      } else character(0)
      seed_cols <- 6L + as.integer(seed_has_geo) + length(seed_roi_cols)

      list(
        analytical  = if (an_rows > 0) list(rows = an_rows,
                                            cols = an_cols + nonfocus_n) else NULL,
        side_map    = if (total_splits > 0) list(rows = total_splits + nonfocus_n,
                                                 cols = 6L) else NULL,
        activity    = if (total_splits > 0) list(rows = total_splits, cols = seed_cols) else NULL,
        composition = if (total_composition_rows > 0) {
          list(rows = total_composition_rows, cols = 10L)
        } else NULL,
        config      = if (n_ch > 0) list(rows = n_ch, cols = NULL) else NULL
      )
    }

    export_snapshot <- reactive({
      profile_export("snapshot", {
        res_list <- results()
        clean_list <- clean_results()
        ch_list <- channels()
        d <- data()
        gcfg <- config()

        export_data <- lapply(names(ch_list), function(nm) {
          res <- res_list[[nm]]
          clean <- clean_list[[nm]] %||% list()
          cfg <- ch_list[[nm]] %||% list()
          final <- final_activity_splits(res, cfg, nm)
          pre_act <- pre_merge_activity_splits(clean, cfg, nm)
          if (!nrow(pre_act)) pre_act <- final
          final_cost <- final_spend_splits(res, cfg, nm)
          pre_cost <- pre_merge_spend_splits(clean, cfg, nm)
          list(
            name = nm,
            res = res,
            clean = clean,
            cfg = cfg,
            final = final,
            pre_act = pre_act,
            final_cost = final_cost,
            pre_cost = pre_cost,
            merge_resolved = NULL,
            rae_totals = NULL,
            canonical_totals = NULL
          )
        })
        names(export_data) <- names(ch_list)

        channel_items <- lapply(names(ch_list), function(nm) {
          build_channel_summary_item(
            nm = nm,
            res = res_list[[nm]],
            cfg_ch = ch_list[[nm]] %||% list(),
            final_splits = export_data[[nm]]$final,
            d = d,
            gcfg = gcfg
          )
        })

        list(
          data = d,
          config = gcfg,
          channels = ch_list,
          results = res_list,
          clean_results = clean_list,
          export_data = export_data,
          channel_summary = channel_items,
          merge_issues = list(count = 0L, names = character(0)),
          file_dims = build_file_dims(export_data, d, ch_list)
        )
      })
    })

    export_heavy_payload <- reactive({
      profile_export("heavy payload", {
        ensure_export_payload(export_snapshot())
      })
    })

    observeEvent(export_snapshot(), {
      scwa_channel_cache$keys <- character(0)
      scwa_channel_cache$values <- list()
    }, ignoreInit = TRUE)

    export_merge_issue_summary <- reactive({
      export_snapshot()$merge_issues
    })

    normalize_channel_summary <- function(summ) {
      if (is.null(summ) || !length(summ)) return(list())
      summ <- Filter(function(x) is.list(x) && !is.null(x$name), summ)
      lapply(summ, function(x) {
        n_splits <- suppressWarnings(as.integer(x$n_splits %||% 0L))[1]
        if (is.na(n_splits)) n_splits <- 0L
        list(
          name = as.character(x$name %||% "")[1],
          processed = isTRUE(x$processed),
          has_warning = isTRUE(x$has_warning),
          tc_mismatch = isTRUE(x$tc_mismatch),
          n_splits = n_splits
        )
      })
    }

    safe_process_qa <- function() {
      proc <- tryCatch(process_qa(), error = function(e) list())
      if (!is.list(proc)) proc <- list()
      proc
    }

    channel_summary <- reactive({
      normalize_channel_summary(export_snapshot()$channel_summary)
    })

    n_ok <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && !isTRUE(x$has_warning), logical(1)))
    })
    n_warnings <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && isTRUE(x$has_warning), logical(1)))
    })
    n_critical <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) !isTRUE(x$processed), logical(1)))
    })
    n_splits <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) as.integer(x$n_splits %||% 0L), integer(1)))
    })
    split_period_counts <- reactive({
      snap <- export_snapshot()
      split_lists <- lapply(snap$export_data, function(x) {
        if (!is.list(x) || is.null(x$final)) return(character(0))
        splits <- x$final$VariableSplit %||% character(0)
        splits[!is.na(splits) & nzchar(splits)]
      })
      nonfocus_counts <- vapply(split_lists, function(splits) {
        sum(grepl("_Before(\\s+|_)", splits, ignore.case = TRUE))
      }, integer(1))
      total_counts <- vapply(split_lists, length, integer(1))
      list(
        focus = sum(total_counts - nonfocus_counts),
        nonfocus = sum(nonfocus_counts),
        total = sum(total_counts)
      )
    })

    is_nonfocus_split <- function(x) {
      grepl("_Before(\\s+|_)", as.character(x), ignore.case = TRUE)
    }

    focus_splits_for_export_item <- function(item) {
      if (!is.list(item) || is.null(item$final)) return(character(0))
      splits <- item$final$VariableSplit %||% character(0)
      splits <- splits[!is.na(splits) & nzchar(trimws(splits))]
      unique(splits[!is_nonfocus_split(splits)])
    }

    focus_geos_for_export_item <- function(item, gcfg) {
      if (!is.list(item)) return(character(0))
      focus_splits <- focus_splits_for_export_item(item)
      r <- item$res
      if (is.null(r) || is.null(r$rag) || !length(focus_splits)) return(character(0))
      rag <- as.data.frame(r$rag)
      geo_col <- (r$cross_cols %||% gcfg$cross_cols %||% "Geography")[1]
      if (!geo_col %in% names(rag)) return(character(0))
      focus_cols <- intersect(focus_splits, names(rag))
      if (!length(focus_cols)) return(character(0))
      has_focus <- rowSums(rag[, focus_cols, drop = FALSE], na.rm = TRUE) > 0
      sort(unique(trimws(as.character(rag[[geo_col]][has_focus]))))
    }

    clean_roi_columns <- function(df) {
      clean_data_columns(df)
    }

    roi_value_columns <- function(roi_df) {
      if (is.null(roi_df) || !nrow(roi_df)) return(character(0))
      roi_named_cols <- names(roi_df)[
        stringr::str_detect(names(roi_df), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE))
      ]
      if (length(roi_named_cols)) return(unique(roi_named_cols))
      roi_meta_cols <- c("MainModelVariableName", "Channel", "Geography",
                         "Sourced VariableName", "VariableSplit", "SplitOrder")
      setdiff(names(roi_df)[vapply(roi_df, is.numeric, logical(1))], roi_meta_cols)
    }

    roi_rae_key_columns <- function(roi_df, rae_df) {
      if (is.null(roi_df) || is.null(rae_df)) return(character(0))
      roi_cols <- roi_value_columns(roi_df)
      reserved <- c("MainModelVariableName", "Channel", "VariableSplit", "SplitOrder",
                    "Period", "VariableName", "VariableValue", roi_cols)
      keys <- intersect(names(roi_df), names(rae_df))
      setdiff(keys, reserved)
    }

    normalize_roi_text <- function(x) {
      x <- trimws(as.character(x %||% ""))
      x <- stringr::str_squish(x)
      tolower(x)
    }

    normalize_roi_mv <- function(x) {
      normalize_roi_text(stringr::str_remove(
        as.character(x %||% ""),
        stringr::regex("(_Total)+$", ignore_case = TRUE)
      ))
    }

    export_channel_label <- function(nm, cfg = list(), d = NULL) {
      rois <- if (!is.null(d)) clean_roi_columns(d$channels_rois) else NULL
      if (!is.null(rois) &&
          all(c("MainModelVariableName", "Channel") %in% names(rois))) {
        mv <- cfg$model_variable %||% nm
        rows_roi <- rois[trimws(as.character(rois$MainModelVariableName)) ==
                           trimws(as.character(mv)), "Channel", drop = TRUE]
        rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(as.character(rows_roi)))]
        if (length(rows_roi)) return(trimws(as.character(rows_roi[1])))
      }
      cfg$media_channel %||% cfg$channel_name %||% nm
    }

    compact_names <- function(x, max_n = 4) {
      x <- unique(x[!is.na(x) & nzchar(trimws(x))])
      if (!length(x)) return("")
      out <- paste(head(x, max_n), collapse = ", ")
      if (length(x) > max_n) out <- paste0(out, " +", length(x) - max_n, " more")
      out
    }

    scwa_key <- function(channel, main_model_variable, merged_split) {
      paste(
        normalize_export_split(channel),
        normalize_export_split(main_model_variable),
        normalize_export_split(merged_split),
        sep = " || "
      )
    }

    scwa_is_checked <- function(channel, main_model_variable, merged_split) {
      flags <- scwa_flags()
      key <- scwa_key(channel, main_model_variable, merged_split)
      isTRUE(unname(flags[[key]]))
    }

    observeEvent(input$scwa_toggle, {
      event <- input$scwa_toggle
      key <- normalize_export_split(event$key %||% "")
      if (!nzchar(key)) return()
      flags <- scwa_flags()
      flags[[key]] <- isTRUE(event$value)
      scwa_flags(flags)
    }, ignoreInit = TRUE)

    roi_issue_summary <- reactive({
      snap <- export_snapshot()
      roi_df <- clean_roi_columns(snap$data$channels_rois)
      export_data <- snap$export_data
      if (is.null(roi_df) || !length(export_data)) {
        return(list(count = 0L, names = character(0)))
      }
      if (!"MainModelVariableName" %in% names(roi_df)) {
        return(list(count = 1L, names = "ROI metadata"))
      }

      normalize_mv <- function(x) {
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      }
      has_geo_col <- "Geography" %in% names(roi_df)
      roi_norm <- roi_df %>%
        dplyr::mutate(
          mv_norm = normalize_mv(MainModelVariableName),
          geo_val = if (has_geo_col) trimws(as.character(Geography %||% "")) else ""
        )

      all_mvs <- dplyr::bind_rows(lapply(names(export_data), function(nm) {
        if (!length(focus_splits_for_export_item(export_data[[nm]]))) return(NULL)
        cfg <- snap$channels[[nm]]
        mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble::tibble(Channel = nm, mv_norm = normalize_mv(mv))
      }))
      if (!nrow(all_mvs)) return(list(count = 0L, names = character(0)))

      missing <- character(0)
      if (has_geo_col) {
        for (i in seq_len(nrow(all_mvs))) {
          nm <- all_mvs$Channel[i]
          ch_roi <- roi_norm[roi_norm$mv_norm == all_mvs$mv_norm[i], , drop = FALSE]
          if (!nrow(ch_roi)) {
            missing <- c(missing, nm)
            next
          }
          geo_entries <- ch_roi[nzchar(ch_roi$geo_val), , drop = FALSE]
          nat_entry <- ch_roi[!nzchar(ch_roi$geo_val), , drop = FALSE]
          if (!nrow(geo_entries) && nrow(nat_entry) > 0) next

          gcfg <- snap$config
          rag_geos <- focus_geos_for_export_item(export_data[[nm]], gcfg)
          if (length(setdiff(rag_geos, trimws(geo_entries$geo_val))) > 0) {
            missing <- c(missing, nm)
          }
        }
      } else {
        roi_norms <- unique(roi_norm$mv_norm)
        missing <- all_mvs$Channel[!all_mvs$mv_norm %in% roi_norms]
      }

      list(count = length(unique(missing)), names = unique(missing))
    })

    export_issue_summary <- reactive({
      snap <- export_snapshot()
      summ <- channel_summary()
      proc <- safe_process_qa()
      failed_names <- proc$failed_names %||% character(0)
      stale_names <- proc$stale_names %||% character(0)
      pending_names <- vapply(summ, \(x) if (!isTRUE(x$processed)) x$name else NA_character_,
                              character(1), USE.NAMES = FALSE)
      pending_names <- pending_names[!is.na(pending_names)]
      tc_names <- vapply(summ, \(x) {
        if (isTRUE(x$processed) && isTRUE(x$tc_mismatch)) x$name else NA_character_
      }, character(1), USE.NAMES = FALSE)
      tc_names <- tc_names[!is.na(tc_names)]
      zero_names <- vapply(summ, \(x) {
        if (isTRUE(x$processed) && isTRUE(x$has_warning) &&
            !isTRUE(x$tc_mismatch) && as.integer(x$n_splits %||% 0L) == 0L) {
          x$name
        } else NA_character_
      }, character(1), USE.NAMES = FALSE)
      zero_names <- zero_names[!is.na(zero_names)]
      merge_issues <- list(
        count = proc$merge_review %||% 0L,
        names = proc$merge_review_names %||% character(0)
      )
      rois_missing <- is.null(snap$data$channels_rois)
      roi_issues <- roi_issue_summary()

      issues <- Filter(Negate(is.null), list(
        if (!length(summ)) list(
          severity = "warn", icon = "layer-group",
          title = "No channels configured",
          detail = "Configure channels before building the export package.",
          names = character(0)
        ),
        if (length(failed_names)) list(
          severity = "error", icon = "circle-xmark",
          title = paste0(length(failed_names), " failed channel",
                         if (length(failed_names) != 1) "s" else ""),
          detail = "Reprocess failed channels before exporting.",
          names = failed_names
        ),
        if (length(stale_names)) list(
          severity = "error", icon = "rotate",
          title = paste0(length(stale_names), " changed channel",
                         if (length(stale_names) != 1) "s need" else " needs",
                         " reprocess"),
          detail = "These results are obsolete after configuration changes.",
          names = stale_names
        ),
        if (length(pending_names)) list(
          severity = "warn", icon = "hourglass-half",
          title = paste0(length(pending_names), " pending channel",
                         if (length(pending_names) != 1) "s" else ""),
          detail = "Pending channels will not be included in the ZIP.",
          names = pending_names
        ),
        if (length(tc_names)) list(
          severity = "warn", icon = "scale-balanced",
          title = paste0(length(tc_names), " Total Check warning",
                         if (length(tc_names) != 1) "s" else ""),
          detail = "Review split totals versus the analytical model total.",
          names = tc_names
        ),
        if (length(zero_names)) list(
          severity = "warn", icon = "triangle-exclamation",
          title = paste0(length(zero_names), " channel",
                         if (length(zero_names) != 1) "s" else "",
                         " with zero splits"),
          detail = "The channel processed, but no split columns were produced.",
          names = zero_names
        ),
        if ((merge_issues$count %||% 0L) > 0L) list(
          severity = "warn", icon = "code-branch",
          title = paste0(merge_issues$count, " merge lineage issue",
                         if (merge_issues$count != 1) "s" else ""),
          detail = "Some saved merges do not match the current processed splits. Reprocess the channel after importing the config, or review the merge names.",
          names = merge_issues$names
        ),
        if (rois_missing) list(
          severity = "warn", icon = "chart-line",
          title = "ROIs by Channel missing - seed file will not include ROI values",
          detail = paste0("The ZIP can still be downloaded, but ",
                          export_file_names$seed_indices,
                          " will export without ROI values until ROIs by Channel is loaded in Setup."),
          names = character(0),
          highlight = TRUE
        ),
        if (!rois_missing && (roi_issues$count %||% 0L) > 0L) list(
          severity = "warn", icon = "chart-line",
          title = paste0(roi_issues$count, " focus ROI coverage warning",
                         if (roi_issues$count != 1) "s" else ""),
          detail = "Some focus-period channels or geographies are missing ROI coverage.",
          names = roi_issues$names
        )
      ))

      has_blocker <- length(failed_names) > 0L || length(stale_names) > 0L
      has_warning <- length(issues) > 0L
      status <- if (has_blocker) "blocked" else if (has_warning) "review" else "ready"
      list(status = status, issues = issues)
    })

    output$summary_strip <- renderUI({
      mk_stat <- function(ico, value, label, color_key, tooltip,
                          is_always_colored = FALSE) {
        active <- is_always_colored || (is.numeric(value) && value > 0)
        key    <- if (active) color_key else "muted"
        div(class = "stat-strip-item",
            title = tooltip,
            div(class = paste("stat-strip-icon", paste0("stat-strip-icon-", key)),
                icon(ico, class = paste0("stat-icon-", key, "-c"))),
            div(tags$span(if (is.numeric(value)) format(value, big.mark = ",") else value,
                          class = paste0("stat-value-", key)),
                tags$span(label, class = "stat-strip-label")))
      }
      sep <- div(class = "stat-separator")
      split_counts <- split_period_counts()
      card(class = "mb-4",
           div(class = "d-flex align-items-stretch",
               mk_stat("circle-check", n_ok(), "Channels OK", "ok",
                       "Channels processed without warnings or blockers.",
                       is_always_colored = n_ok() > 0), sep,
               mk_stat("triangle-exclamation", n_warnings(), "Warnings", "warn",
                       "Processed channels that need review, such as Total Check mismatches or zero splits."), sep,
               mk_stat("circle-xmark", n_critical(), "Critical Issues", "error",
                       "Channels that are not processed yet and will not be included in the export."), sep,
               mk_stat("layer-group", format(n_splits(), big.mark = ","),
                       "Splits Matched", "blue",
                       "Total split columns currently available for export.",
                       is_always_colored = TRUE), sep,
               mk_stat("bullseye", format(split_counts$focus, big.mark = ","),
                       paste0("Focus Splits | Non-Focus ",
                              format(split_counts$nonfocus, big.mark = ",")),
                       "blue",
                       "Split count by reporting period: Focus splits are current-period columns; Non-Focus splits are _Before columns.",
                       is_always_colored = TRUE)))
    })

    output$export_issues <- renderUI({
      issue_state <- export_issue_summary()
      issues <- issue_state$issues
      if (!length(issues)) return(NULL)

      mk_issue <- function(x) {
        div(class = paste("export-issue-row", paste0("export-issue-", x$severity),
                          if (isTRUE(x$highlight)) "export-issue-highlight" else ""),
            div(class = "export-issue-icon", icon(x$icon)),
            div(class = "export-issue-copy",
                div(class = "export-issue-line",
                    tags$span(x$title, class = "export-issue-title"),
                    tags$span(if (x$severity == "error") "Action needed" else "Review",
                              class = paste("export-status-pill",
                                            paste0("export-status-", x$severity)))),
                tags$span(x$detail, class = "export-issue-detail"),
                if (length(x$names))
                  tags$span(compact_names(x$names), class = "export-issue-names")))
      }

      div(class = "export-issues",
          div(class = "export-issues-head",
              div(class = "card-header-inner",
                  icon("triangle-exclamation", class = "icon-blue-sm"),
                  tags$strong("Export Issues")),
              tags$span(paste0(length(issues), " item",
                               if (length(issues) != 1) "s" else ""),
                        class = "export-issues-count")),
          tagList(lapply(issues, mk_issue)))
    })

    output$channel_status <- renderUI({
      summ <- channel_summary()
      proc <- safe_process_qa()
      stale_nms <- proc$stale_names %||% character(0)
      failed_nms <- proc$failed_names %||% character(0)
      if (!length(summ))
        return(div(class = "ch-status-empty",
                   icon("layer-group", class = "icon-status-empty"),
                   tags$p("No channels configured.", class = "ch-status-empty-msg")))
      tagList(lapply(seq_along(summ), function(i) {
        ch <- summ[[i]]
        if (ch$name %in% failed_nms) {
          ic  <- icon("circle-xmark", class = "icon-ch-error")
          val <- tags$span("Failed", class = "export-status-pill export-status-error")
        } else if (ch$name %in% stale_nms) {
          ic  <- icon("rotate", class = "icon-ch-warn")
          val <- tags$span("Needs reprocess", class = "export-status-pill export-status-warn")
        } else if (!ch$processed) {
          ic  <- icon("circle", class = "icon-ch-empty")
          val <- tags$span("Not processed", class = "export-status-pill export-status-empty")
        } else if (ch$has_warning && ch$tc_mismatch) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("Total Check mismatch", class = "export-status-pill export-status-warn")
        } else if (ch$has_warning) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("0 splits found", class = "export-status-pill export-status-warn")
        } else {
          ic  <- icon("circle-check", class = "icon-ch-ok")
          val <- tags$span(paste0(format(ch$n_splits, big.mark = ","), " split",
                                  if (ch$n_splits != 1) "s" else ""),
                           class = "export-status-pill export-status-ok")
        }
        div(class = "ch-status-row",
            ic, tags$span(ch$name, class = "ch-status-name"), val)
      }))
    })

    file_dims <- reactive({
      export_snapshot()$file_dims
    })

    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      proc    <- safe_process_qa()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      n_stale <- proc$stale %||% 0L
      n_fail  <- proc$failed %||% 0L
      if (!n_total) return(NULL)
      if (n_stale > 0L || n_fail > 0L)
        tags$span(class = "badge-not-ready",
                  icon("circle-xmark", class = "icon-xs"),
                  paste0(" ", n_stale + n_fail, " blocked"))
      else if (n_ready == n_total)
        tags$span(class = "badge-ready", icon("circle-check", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
      else
        tags$span(class = "badge-not-ready",
                  icon("triangle-exclamation", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
    })

    # ── Export package contents — with Model Update summary (#9) ─────────
    output$export_contents <- renderUI({
      dims <- file_dims()
      snap <- export_snapshot()
      d    <- snap$data
      mode <- d$app_mode %||% "build"

      mk_file_row <- function(ico, icon_class, filename, description, dims_info) {
        div(class = "export-file-row",
            div(class = paste("export-file-icon", paste0("export-icon-", icon_class)),
                icon(ico, class = paste0("icon-export-", icon_class))),
            div(class = "flex-1-mw0",
                tags$span(filename, class = "export-file-name"),
                tags$span(description, class = "export-file-desc")),
            if (!is.null(dims_info))
              div(class = "export-file-dims",
                  tags$span(paste0(format(dims_info$rows, big.mark = ","),
                                   if (!is.null(dims_info$cols))
                                     paste0(" \u00d7 ", dims_info$cols) else ""),
                            class = "export-dims-value"),
                  tags$span(if (!is.null(dims_info$cols)) "rows \u00d7 cols" else "channels",
                            class = "export-dims-label"))
            else
              div(class = "export-file-dims",
                  tags$span("\u2014", class = "export-dims-na")))
      }

      # Model Update summary strip (#9)
      update_summary <- if (mode == "update" && !is.null(d$side_mapping_nonfocus)) {
        n_new      <- as.integer(n_splits())
        n_past     <- nrow(d$side_mapping_nonfocus)
        n_total_sp <- n_new + n_past
        div(class = "alert alert-info alert-sm p-2 mb-3",
            div(class = "d-flex gap-3 align-items-center",
                icon("rotate", class = "icon-blue-sm"),
                tags$strong("Model Update splits:"),
                tags$span(class = "badge-focus-sm",
                          paste0(format(n_new, big.mark = ","), " new")),
                tags$span(class = "badge-nonfocus-sm",
                          paste0(format(n_past, big.mark = ","), " past (non-focus)")),
                tags$span(class = "text-muted small",
                          paste0("Total: ", format(n_total_sp, big.mark = ",")))))
      } else NULL

      tagList(
        update_summary,
        div(class = "export-file-groups",
          div(class = "export-file-group-title",
              icon("table-cells", class = "icon-blue-sm"), "Model Dataset"),
          mk_file_row("table-cells",    "blue",   export_file_names$analytical_csv,
                      "IN/FIXED model variables + all split columns appended",
                      dims$analytical),
          mk_file_row("database",       "blue",   export_file_names$analytical_rdata,
                      "Same dataset as RData — load as AnalyticalDataset in next update",
                      dims$analytical),
          div(class = "export-file-group-title",
              icon("diagram-project", class = "icon-blue-sm"), "Mapping & Seeds"),
          mk_file_row("diagram-project","purple", export_file_names$side_mapping,
                      "Split-to-model mapping with PSO weight structure",
                      dims$side_map),
          mk_file_row("chart-column",   "green",  export_file_names$seed_indices,
                      "Activity, spend and ROI totals with split order per split",
                      dims$activity),
          mk_file_row("code-branch",    "teal",   export_file_names$split_composition,
                      "Split lineage: components, activity and spend per period",
                      dims$composition),
          div(class = "export-file-group-title",
              icon("gear", class = "icon-blue-sm"), "Configuration"),
          mk_file_row("gear",           "amber",  export_file_names$channel_config,
                      "Split order, merges, breaks and segment overrides",
                      dims$config)
        )
      )
    })

    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- clean_roi_columns(data()$channels_rois)

      if (!"MainModelVariableName" %in% names(roi_df))
        return(div(class = "roi-box-error",
                   div(class = "card-header-inner",
                       tags$span("ROIs file missing required column: MainModelVariableName",
                                 class = "roi-error-msg"))))

      snap <- export_snapshot()
      export_data <- snap$export_data
      if (!length(export_data)) return(NULL)

      has_geo_col  <- "Geography" %in% names(roi_df)
      normalize_mv <- function(x)
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)],
                              c("MainModelVariableName"))

      all_mvs <- dplyr::bind_rows(lapply(names(export_data), function(nm) {
        if (!length(focus_splits_for_export_item(export_data[[nm]]))) return(NULL)
        cfg <- snap$channels[[nm]]; mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble::tibble(Channel = nm, MainModelVariableName = mv,
                       mv_norm = normalize_mv(mv))
      }))
      if (!nrow(all_mvs)) return(NULL)

      roi_norm <- roi_df %>%
        dplyr::mutate(
          mv_norm = normalize_mv(MainModelVariableName),
          geo_val = if (has_geo_col) trimws(as.character(Geography %||% "")) else "")

      if (has_geo_col) {
        n_warn    <- 0L
        ch_blocks <- lapply(seq_len(nrow(all_mvs)), function(i) {
          nm      <- all_mvs$Channel[i]; mv_n <- all_mvs$mv_norm[i]
          gcfg <- snap$config
          rag_geos <- focus_geos_for_export_item(export_data[[nm]], gcfg)
          ch_roi      <- roi_norm[roi_norm$mv_norm == mv_n, , drop = FALSE]
          geo_entries <- ch_roi[nzchar(ch_roi$geo_val), , drop = FALSE]
          nat_entry   <- ch_roi[!nzchar(ch_roi$geo_val), , drop = FALSE]

          if (nrow(geo_entries) > 0) {
            matched_geos <- trimws(geo_entries$geo_val)
            missing_geos <- setdiff(rag_geos, matched_geos)
            if (length(missing_geos) > 0) n_warn <<- n_warn + 1L
            geo_rows  <- lapply(seq_len(nrow(geo_entries)), function(j) {
              geo     <- trimws(geo_entries$geo_val[j])
              roi_val <- if (length(roi_num_cols)) geo_entries[[roi_num_cols[1]]][j] else NA_real_
              div(class = "roi-geo-row", tags$span(geo, class = "roi-geo-name"),
                  if (!is.na(roi_val))
                    tags$span(paste0("ROI ", round(roi_val, 2)), class = "roi-geo-val"))
            })
            miss_rows <- lapply(head(missing_geos, 3), function(geo)
              div(class = "roi-geo-row",
                  tags$span(geo, class = "roi-geo-name text-muted"),
                  tags$span("missing", class = "roi-geo-miss-label")))
            badge <- if (!length(missing_geos))
              tags$span(paste0(length(matched_geos), " geos"), class = "badge-roi-ok")
            else
              tags$span(paste0(length(matched_geos), "/", length(rag_geos), " geos"),
                        class = "badge-roi-warn")
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"), badge),
                div(class = "roi-geo-list", geo_rows, miss_rows,
                    if (length(missing_geos) > 3)
                      tags$p(paste0("... and ", length(missing_geos) - 3, " more missing"),
                             class = "roi-warn-more")))
          } else if (nrow(nat_entry) > 0) {
            roi_val <- if (length(roi_num_cols)) nat_entry[[roi_num_cols[1]]][1] else NA_real_
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"),
                    if (!is.na(roi_val))
                      tags$span(paste0("ROI ", round(roi_val, 2)), class = "badge-roi-ok")
                    else tags$span("matched", class = "badge-roi-ok")))
          } else {
            n_warn <<- n_warn + 1L
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"),
                    tags$span("no ROI", class = "badge-roi-miss")))
          }
        })
        summary_bar <- if (n_warn == 0)
              div(class = "roi-box-ok mb-2",
                  div(class = "card-header-inner",
                  tags$span(paste0("All ", nrow(all_mvs), " focus channel(s) have ROI coverage."),
                            class = "roi-ok-msg")))
        else
          div(class = "roi-box-warn mb-2",
              div(class = "card-header-between",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_warn, " channel(s) with incomplete ROI"),
                                  class = "roi-warn-title")),
                  tags$span(paste0(nrow(all_mvs) - n_warn, "/", nrow(all_mvs), " complete"),
                            class = "badge-roi-count")))
        div(summary_bar, div(class = "roi-ch-list", ch_blocks))
      } else {
        roi_norms <- unique(roi_norm$mv_norm)
        checked   <- all_mvs %>% dplyr::mutate(.has_roi = mv_norm %in% roi_norms)
        n_total   <- nrow(checked); n_matched <- sum(checked$.has_roi)
        n_missing <- n_total - n_matched
        if (n_missing == 0) {
          div(class = "roi-box-ok",
              div(class = "card-header-inner",
                  tags$span(paste0("All ", n_total, " focus channel(s) matched with ROI values."),
                            class = "roi-ok-msg")))
        } else {
          missing_rows <- checked %>% dplyr::filter(!.has_roi) %>%
            dplyr::mutate(label = paste0(Channel, " -> ", MainModelVariableName)) %>%
            dplyr::pull(label)
          div(class = "roi-box-warn",
              div(class = "card-header-between mb-2",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_missing, " of ", n_total,
                                         " focus channel(s) have no ROI"), class = "roi-warn-title")),
                  tags$span(paste0(n_matched, "/", n_total, " matched"),
                            class = "badge-roi-count")),
              div(class = "roi-warn-list",
                  tagList(lapply(seq_along(head(missing_rows, 6)), function(i)
                    div(class = "roi-warn-row", div(class = "roi-warn-dot"),
                        tags$span(missing_rows[[i]], class = "roi-warn-name")))),
                  if (length(missing_rows) > 6)
                    tags$p(paste0("... and ", length(missing_rows) - 6, " more"),
                           class = "roi-warn-more")))
        }
      }
    })

    output$download_section <- renderUI({
      summ <- channel_summary()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      issue_state <- export_issue_summary()
      status <- issue_state$status
      if (n_ready == 0)
        return(div(class = "dl-empty", icon("hourglass-half", class = "icon-dl-empty"),
                   tags$p("Process channels first.", class = "dl-empty-msg")))
      div(class = paste("export-package-footer", paste0("export-package-footer-", status)),
          div(class = "export-download-action",
              downloadButton(session$ns("dl_zip"),
                             tagList(icon("file-zipper"), " Download All (ZIP)"),
                             class = "btn-primary btn-dl-main")),
          div(class = "dl-stats-row",
              tags$span(class = "dl-stat-item", icon("circle-check", class = "icon-stat-ok"),
                        paste0(n_ready, " channel", if (n_ready != 1) "s" else "")),
              tags$span(class = "dl-stat-item", icon("layer-group", class = "icon-stat-blue"),
                        paste0(format(n_splits(), big.mark = ","), " splits")),
              tags$span(class = "dl-stat-item", icon("file-csv", class = "icon-stat-muted"),
                        "6 files")))
    })

    # ═══════════════════════════════════════════════════════════════════
    # Dataset builders
    # ═══════════════════════════════════════════════════════════════════

    # ── 1. Analytical extended — CSV and RData contain the same data (#7) ─
    build_analytical_extended <- function(d, res_list, channels_list, gcfg,
                                          schema_metadata = NULL,
                                          snapshot = NULL) {
      if (is.null(d$analytical)) return(NULL)
      cross_cols <- gcfg$cross_cols %||% "Geography"
      cross_id   <- c(cross_cols, "Period")

      in_fixed_mv <- if (!is.null(d$details) &&
                         all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          dplyr::filter(!stringr::str_detect(
            stringr::str_to_lower(trimws(Type)), "none")) %>%
          dplyr::pull(VariableName) %>% unique()
      } else {
        mf <- unique(vapply(channels_list, \(c) c$model_variable %||% "", character(1)))
        mf[nzchar(mf)]
      }

      model_cols_an <- if (!is.null(schema_metadata) &&
                           !is.null(schema_metadata$name_lookup) &&
                           nrow(schema_metadata$name_lookup) > 0) {
        lookup  <- schema_metadata$name_lookup
        direct  <- intersect(in_fixed_mv, names(d$analytical))
        via_lkp <- lookup$OriginalName[
          lookup$VariableName %in% in_fixed_mv & !is.na(lookup$OriginalName)]
        unique(c(direct, via_lkp))
      } else {
        intersect(in_fixed_mv, names(d$analytical))
      }
      model_cols_an <- intersect(model_cols_an, names(d$analytical))

      id_cols_an   <- intersect(c(cross_cols, "Period", "BP_Year"), names(d$analytical))
      keep_an_cols <- union(id_cols_an, model_cols_an)
      selected_weight <- trimws(as.character(gcfg$weight_variable_name %||% ""))
      weight_col <- if (nzchar(selected_weight) && selected_weight %in% names(d$analytical)) {
        selected_weight
      } else {
        fallback_weight <- intersect("Weight Variable MMM", names(d$analytical))
        if (length(fallback_weight)) fallback_weight[1] else ""
      }
      if (nzchar(weight_col))
        keep_an_cols <- union(keep_an_cols, weight_col)

      # In Model Update mode, include non-focus split columns so CSV = RData (#7)
      if (!is.null(d$side_mapping_nonfocus) && nrow(d$side_mapping_nonfocus) > 0) {
        nonfocus_cols <- intersect(d$side_mapping_nonfocus$VariableSplit,
                                   names(d$analytical))
        keep_an_cols  <- union(keep_an_cols, nonfocus_cols)
      }

      result <- as.data.frame(d$analytical) %>%
        dplyr::select(dplyr::all_of(keep_an_cols))

      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        rag      <- as.data.frame(r$rag)
        join_key <- intersect(cross_id, names(rag))
        valid_jk <- intersect(join_key, intersect(names(rag), names(result)))
        if (!length(valid_jk)) next
        cfg_ch <- channels_list[[nm]] %||% list()
        split_cols <- if (!is.null(snapshot$export_data[[nm]])) {
          snapshot$export_data[[nm]]$final$VariableSplit
        } else {
          final_activity_splits(r, cfg_ch, nm)$VariableSplit
        }
        split_cols <- intersect(split_cols, names(rag))
        if (!length(split_cols)) next
        rag_sub  <- rag[, c(valid_jk, split_cols), drop = FALSE]
        conflict <- intersect(split_cols, names(result))
        if (length(conflict))
          names(rag_sub)[names(rag_sub) %in% conflict] <-
          paste0(names(rag_sub)[names(rag_sub) %in% conflict], "_", nm)
        result <- dplyr::left_join(result, rag_sub, by = valid_jk)
      }

      split_cols_out <- setdiff(names(result), keep_an_cols)
      numeric_split_cols <- split_cols_out[
        vapply(result[split_cols_out], is.numeric, logical(1))
      ]
      if (length(numeric_split_cols) > 0) {
        result[numeric_split_cols] <- lapply(result[numeric_split_cols], function(x) {
          x[is.na(x)] <- 0
          x
        })
      }
      result
    }

    build_activity_rois <- function(d, res_list, channels_list, gcfg,
                                    snapshot = NULL) {
      format_seed_for_indices <- function(df, roi_cols = character(0)) {
        if (is.null(df) || !nrow(df)) return(df)

        internal_match_cols <- setdiff(
          c("Sourced VariableName",
            roi_rae_key_columns(clean_roi_columns(d$channels_rois), d$all_rags)),
          "Geography"
        )
        df <- dplyr::select(df, -dplyr::any_of(setdiff(internal_match_cols, roi_cols)))

        if ("total_activity" %in% names(df))
          df <- dplyr::rename(df, Activity = total_activity)
        if ("total_spend" %in% names(df))
          df <- dplyr::rename(df, Spend = total_spend)

        roi_cols <- intersect(roi_cols, names(df))
        if (length(roi_cols) == 1L && !identical(roi_cols, "ROI")) {
          names(df)[names(df) == roi_cols] <- "ROI"
          roi_cols <- "ROI"
        }

        front_cols <- c(
          "MainModelVariableName",
          "Channel",
          intersect("Geography", names(df)),
          "VariableSplit",
          "Activity",
          "Spend",
          roi_cols,
          "SplitOrder"
        )
        dplyr::select(df, dplyr::any_of(front_cols), dplyr::everything())
      }

      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r   <- res_list[[nm]]
        cfg <- channels_list[[nm]]
        if (is.null(r) || is.null(r$rag)) return(NULL)
        cfg$model_metric <- r$model_metric %||% cfg$model_metric %||% "activity"
        model_metric <- normalize_model_metric(cfg$model_metric %||% "activity")
        roi_file <- clean_roi_columns(d$channels_rois)
        snap_ch <- NULL
        if (!is.null(snapshot) && !is.null(snapshot$export_data) &&
            !is.null(snapshot$export_data[[nm]])) {
          snap_ch <- ensure_channel_export_payload(snapshot$export_data[[nm]], d, gcfg)
        }

        rag_df     <- as.data.frame(r$rag)
        cross_cols <- r$cross_cols %||% gcfg$cross_cols %||% "Geography"
        cross_id   <- c(cross_cols, "Period")
        id_in_rag  <- intersect(cross_id, names(rag_df))
        geo_col    <- cross_cols[1]
        act_kw     <- cfg$activity_keyword %||% "Impressions"
        spend_kw   <- cfg$spend_keyword    %||% "Spend"
        metric_total_col <- if (identical(model_metric, "spend")) "total_spend" else "total_activity"

        split_order_str <- paste(cfg$split_columns %||% "VariableName", collapse = "|")

        channel_from_roi <- {
          rois <- roi_file
          if (!is.null(rois) &&
              all(c("MainModelVariableName", "Channel") %in% names(rois))) {
            mv       <- cfg$model_variable %||% nm
            rows_roi <- rois[trimws(rois$MainModelVariableName) == trimws(mv),
                             "Channel", drop = TRUE]
            rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(rows_roi))]
            if (length(rows_roi)) trimws(rows_roi[1]) else nm
          } else nm
        }

        metric_seed <- if (!is.null(snap_ch) &&
                           !is.null(snap_ch$canonical_totals) &&
                           nrow(snap_ch$canonical_totals$seed_focus_totals %||% tibble::tibble()) > 0) {
          snap_ch$canonical_totals$seed_focus_totals
        } else if (!is.null(snap_ch) &&
                   !is.null(snap_ch$rae_totals)) {
          snap_ch$rae_totals$seed
        } else {
          export_metric_totals_from_rae(d, cfg, gcfg, nm)$seed
        }
        if (!is.null(metric_seed) && nrow(metric_seed) > 0) {
          has_geo_roi_col <- !is.null(roi_file) &&
            "Geography" %in% names(roi_file)
          roi_match_cols <- setdiff(
            c("Sourced VariableName", roi_rae_key_columns(roi_file, d$all_rags)),
            "Geography"
          )
          roi_match_cols <- intersect(roi_match_cols, names(metric_seed))
          result <- metric_seed
          if (!"Geography" %in% names(result))
            result$Geography <- NA_character_
          if (!has_geo_roi_col && "Geography" %in% names(result))
            result$Geography <- NA_character_
          if (!"total_spend" %in% names(result))
            result$total_spend <- NA_real_
          if (!metric_total_col %in% names(result)) result[[metric_total_col]] <- NA_real_
          result <- result %>%
            dplyr::filter(!is.na(.data[[metric_total_col]]) & .data[[metric_total_col]] > 0) %>%
            dplyr::mutate(
              Channel = channel_from_roi,
              MainModelVariableName = cfg$model_variable %||% NA_character_,
              SplitOrder = split_order_str
            ) %>%
            dplyr::select(VariableSplit, Geography, total_activity, total_spend,
                          Channel, MainModelVariableName, SplitOrder,
                          dplyr::any_of(roi_match_cols))
          if (nrow(result)) return(result)
        }

        all_split_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_in_rag)
        final_splits <- if (!is.null(snap_ch)) {
          snap_ch$final
        } else {
          final_activity_splits(r, cfg, nm)
        }
        nonfocus_pattern <- "_Before(\\s+|_)"
        metric_cols <- intersect(final_splits$VariableSplit, all_split_cols)
        metric_cols <- metric_cols[!grepl(nonfocus_pattern, metric_cols, ignore.case = TRUE)]
        if (identical(model_metric, "spend")) {
          cost_cols <- metric_cols
          act_cols <- grep(act_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
          act_cols <- act_cols[!grepl(nonfocus_pattern, act_cols, ignore.case = TRUE)]
        } else {
          act_cols <- metric_cols
          cost_cols <- grep(spend_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
          cost_cols <- cost_cols[!grepl(nonfocus_pattern, cost_cols, ignore.case = TRUE)]
        }
        primary_cols <- if (identical(model_metric, "spend")) cost_cols else act_cols
        if (!length(primary_cols)) return(NULL)

        all_dedup_cols <- union(act_cols, cost_cols)
        cs_period <- if (geo_col %in% names(rag_df)) {
          rag_df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(c(geo_col, "Period")))) %>%
            dplyr::summarise(
              dplyr::across(dplyr::all_of(all_dedup_cols), \(x) max(x, na.rm = TRUE)),
              .groups = "drop") %>%
            as.data.frame()
        } else rag_df

        periods <- cs_period$Period
        has_geo_roi_col <- !is.null(roi_file) &&
          "Geography" %in% names(roi_file)

        if (has_geo_roi_col && geo_col %in% names(cs_period)) {
          geo_vals <- sort(unique(cs_period[[geo_col]]))
          geo_rows <- lapply(geo_vals, function(geo) {
            cs_geo <- cs_period[cs_period[[geo_col]] == geo, , drop = FALSE]
            get_geo_total <- function(col) {
              vals <- cs_geo[[col]]
              if (all(is.na(vals) | vals == 0)) return(0)
              sum(vals, na.rm = TRUE)
            }
            act_totals  <- if (length(act_cols)) sapply(act_cols, get_geo_total) else numeric(0)
            cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_geo_total) else numeric(0)
            primary_df_g <- tibble::tibble(
              VariableSplit = primary_cols, Geography = geo,
              key = stringr::str_remove_all(
                primary_cols,
                stringr::regex(if (identical(model_metric, "spend")) spend_kw else act_kw,
                               ignore_case = TRUE)
              ))
            act_df_g <- if (length(act_totals))
              tibble::tibble(total_activity = unname(act_totals),
                             key = stringr::str_remove_all(
                               act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
            else tibble::tibble(total_activity = numeric(0), key = character(0))
            cost_df_g <- if (length(cost_totals))
              tibble::tibble(total_spend = unname(cost_totals),
                             key = stringr::str_remove_all(
                               cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
            else tibble::tibble(total_spend = numeric(0), key = character(0))
            primary_df_g %>%
              dplyr::left_join(act_df_g, by = "key") %>%
              dplyr::left_join(cost_df_g, by = "key") %>% dplyr::select(-key) %>%
              dplyr::filter(!is.na(.data[[metric_total_col]]) & .data[[metric_total_col]] > 0) %>%
              dplyr::mutate(Channel               = channel_from_roi,
                            MainModelVariableName = cfg$model_variable %||% NA_character_)
          })
          result <- dplyr::bind_rows(geo_rows)
        } else {
          # STANDARD path — improved local/national detection (#5)
          # Uses tapply over ALL periods with 0.1% threshold instead of sampling 5
          get_total <- function(col) {
            vals <- cs_period[[col]]
            if (all(is.na(vals) | vals == 0)) return(0)
            is_local <- any(tapply(vals, periods, function(x) {
              nz <- x[!is.na(x) & x > 0]
              length(nz) >= 2 && diff(range(nz)) / max(nz) > 0.001
            }))
            if (is_local) sum(vals, na.rm = TRUE)
            else sum(tapply(vals, periods, function(x) {
              x_pos <- x[!is.na(x) & x > 0]
              if (length(x_pos)) max(x_pos) else 0
            }), na.rm = TRUE)
          }

          act_totals  <- if (length(act_cols)) sapply(act_cols, get_total) else numeric(0)
          cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_total) else numeric(0)
          primary_df_s <- tibble::tibble(
            VariableSplit = primary_cols, Geography = NA_character_,
            key = stringr::str_remove_all(
              primary_cols,
              stringr::regex(if (identical(model_metric, "spend")) spend_kw else act_kw,
                             ignore_case = TRUE)
            ))
          act_df_s <- if (length(act_totals))
            tibble::tibble(total_activity = unname(act_totals),
                           key = stringr::str_remove_all(
                             act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
          else tibble::tibble(total_activity = numeric(0), key = character(0))
          cost_df_s <- if (length(cost_totals))
            tibble::tibble(total_spend = unname(cost_totals),
                           key = stringr::str_remove_all(
                             cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
          else tibble::tibble(total_spend = numeric(0), key = character(0))
          result <- primary_df_s %>%
            dplyr::left_join(act_df_s, by = "key") %>%
            dplyr::left_join(cost_df_s, by = "key") %>% dplyr::select(-key) %>%
            dplyr::filter(!is.na(.data[[metric_total_col]]) & .data[[metric_total_col]] > 0) %>%
            dplyr::mutate(Channel               = channel_from_roi,
                          MainModelVariableName = cfg$model_variable %||% NA_character_)
        }

        if (!nrow(result)) return(NULL)
        result %>%
          dplyr::mutate(SplitOrder = split_order_str) %>%
          dplyr::select(VariableSplit, Geography, total_activity, total_spend,
                        Channel, MainModelVariableName, SplitOrder)
      }))

      if (!length(rows)) return(NULL)
      act_df <- dplyr::bind_rows(rows)

      if ("Geography" %in% names(act_df) && all(is.na(act_df$Geography)))
        act_df <- dplyr::select(act_df, -Geography)

      seed_roi_cols <- character(0)
      roi_file <- clean_roi_columns(d$channels_rois)
      if (!is.null(roi_file) && nrow(roi_file) > 0) {
        tryCatch({
          roi_df <- roi_file
          if (!"MainModelVariableName" %in% names(roi_df))
            return(format_seed_for_indices(act_df))

          roi_num_cols <- roi_value_columns(roi_df)
          if (!length(roi_num_cols))
            return(format_seed_for_indices(act_df))

          for (roi_col in roi_num_cols) {
            if (!is.numeric(roi_df[[roi_col]])) {
              roi_df[[roi_col]] <- suppressWarnings(as.numeric(
                gsub("%", "", gsub(",", "", as.character(roi_df[[roi_col]])))
              ))
            }
          }
          seed_roi_cols <- roi_num_cols

          src_col <- "Sourced VariableName"
          has_src_col <- src_col %in% names(roi_df) && src_col %in% names(act_df)
          has_geo_col <- "Geography" %in% names(roi_df) && "Geography" %in% names(act_df)
          roi_long_cols <- roi_rae_key_columns(roi_df, d$all_rags)
          roi_long_cols <- intersect(roi_long_cols, names(act_df))
          roi_long_cols <- setdiff(roi_long_cols, "Geography")
          unknown_roi_keys <- setdiff(
            setdiff(names(roi_df), c("MainModelVariableName", "Channel", "Geography",
                                     "Sourced VariableName", "VariableSplit", "SplitOrder",
                                     roi_num_cols)),
            names(d$all_rags %||% data.frame())
          )
          unknown_roi_keys <- unknown_roi_keys[!vapply(roi_df[unknown_roi_keys], is.numeric, logical(1))]
          if (length(unknown_roi_keys)) {
            showNotification(
              paste0("ROI key column(s) not found in RAE and ignored: ",
                     paste(head(unknown_roi_keys, 5), collapse = ", "),
                     if (length(unknown_roi_keys) > 5) " ..." else ""),
              type = "warning", duration = 12
            )
          }
          empty_roi <- setNames(as.list(rep(NA_real_, length(roi_num_cols))), roi_num_cols)

          roi_norm <- roi_df %>%
            dplyr::mutate(
              mv_norm = normalize_roi_mv(MainModelVariableName),
              geo_val = if ("Geography" %in% names(roi_df))
                normalize_roi_text(Geography) else "",
              sv_val = if (src_col %in% names(roi_df))
                normalize_roi_text(.data[[src_col]]) else ""
            )
          for (key_col in roi_long_cols) {
            roi_norm[[paste0(".key_", key_col)]] <- normalize_roi_text(roi_norm[[key_col]])
          }

          roi_by_mv <- split(roi_norm, roi_norm$mv_norm, drop = TRUE)
          ambiguous_matches <- character(0)
          row_value <- function(row, col) {
            if (!col %in% names(row)) return("")
            val <- row[[col]]
            if (!length(val)) return("")
            val[[1]]
          }
          named_value <- function(x, nm, default = "") {
            if (!length(nm) || is.na(nm) || !nzchar(nm)) return(default)
            if (is.null(x) || !nm %in% names(x)) return(default)
            val <- x[[nm]]
            if (!length(val) || is.na(val[[1]])) return(default)
            val[[1]]
          }
          roi_candidates_for_mv <- function(mv_norm) {
            if (!length(mv_norm) || is.na(mv_norm[[1]]) || !nzchar(mv_norm[[1]])) return(NULL)
            mv_norm <- mv_norm[[1]]
            if (!mv_norm %in% names(roi_by_mv)) return(NULL)
            roi_by_mv[[mv_norm]]
          }

          pick_roi <- function(cands, row) {
            if (is.null(cands) || !nrow(cands)) return(NULL)

            row_src <- if (has_src_col) normalize_roi_text(row_value(row, src_col)) else ""
            row_geo <- if (has_geo_col) normalize_roi_text(row_value(row, "Geography")) else ""
            row_keys <- stats::setNames(
              vapply(roi_long_cols, function(k) normalize_roi_text(row_value(row, k)), character(1)),
              roi_long_cols
            )
            key_sets <- list(
              c(if (has_src_col) "sv_val", paste0(".key_", roi_long_cols),
                if (has_geo_col) "geo_val"),
              c(if (has_src_col) "sv_val", paste0(".key_", roi_long_cols)),
              c(if (has_src_col) "sv_val"),
              character(0)
            )
            key_sets <- unique(lapply(key_sets, unique))

            match_one_level <- function(keys) {
              subset <- cands
              if (!length(keys)) {
                specific_cols <- c("sv_val", "geo_val", paste0(".key_", roi_long_cols))
                specific_cols <- intersect(specific_cols, names(subset))
                if (length(specific_cols)) {
                  specific_matrix <- vapply(specific_cols, function(k) {
                    nzchar(subset[[k]])
                  }, logical(nrow(subset)))
                  if (is.null(dim(specific_matrix))) {
                    specific_matrix <- matrix(specific_matrix, ncol = 1L)
                  }
                  general <- subset[rowSums(specific_matrix) == 0, , drop = FALSE]
                  if (nrow(general)) subset <- general
                }
                return(subset)
              }
              for (key in keys) {
                row_val <- switch(
                  key,
                  sv_val = row_src,
                  geo_val = row_geo,
                  {
                    source_col <- sub("^\\.key_", "", key)
                    named_value(row_keys, source_col)
                  }
                )
                if (!nzchar(row_val) || !key %in% names(subset)) return(subset[0, , drop = FALSE])
                subset <- subset[nzchar(subset[[key]]) & subset[[key]] == row_val, , drop = FALSE]
                if (!nrow(subset)) return(subset)
              }
              subset
            }

            for (keys in key_sets) {
              hit <- match_one_level(keys)
              if (!nrow(hit)) next
              hit <- dplyr::distinct(hit, dplyr::across(dplyr::all_of(roi_num_cols)), .keep_all = TRUE)
              if (nrow(hit) == 1L) return(as.list(hit[1, roi_num_cols, drop = FALSE]))
              ambiguous_matches <<- c(ambiguous_matches, paste0(row$Channel, " -> ", row$VariableSplit))
              return(NULL)
            }
            NULL
          }

          matched_rois <- lapply(seq_len(nrow(act_df)), function(i) {
            row <- act_df[i, , drop = FALSE]
            mv_norm <- normalize_roi_mv(row$MainModelVariableName %||% "")
            hit <- pick_roi(roi_candidates_for_mv(mv_norm), row)
            if (!is.null(hit)) return(hit)
            empty_roi
          })

          act_df <- dplyr::bind_cols(act_df, dplyr::bind_rows(matched_rois))

          if (length(ambiguous_matches)) {
            showNotification(
              tagList(
                tags$strong(paste0(length(unique(ambiguous_matches)), " ambiguous ROI match(es):")),
                tags$ul(class = "mt-1 ps-3 small",
                        lapply(head(unique(ambiguous_matches), 5), tags$li),
                        if (length(unique(ambiguous_matches)) > 5)
                          tags$li(paste0("... and ", length(unique(ambiguous_matches)) - 5, " more")))
              ),
              type = "warning", duration = 15
            )
          }

          drop_match_cols <- setdiff(c(src_col, roi_long_cols), "Geography")
          act_df <- dplyr::select(act_df, -dplyr::any_of(drop_match_cols))

          unmatched <- act_df %>%
            dplyr::filter(dplyr::if_any(dplyr::all_of(roi_num_cols), is.na)) %>%
            dplyr::select(dplyr::any_of(c("Channel", "MainModelVariableName", "Geography", "VariableSplit"))) %>%
            dplyr::distinct() %>%
            dplyr::arrange(Channel)

          if (nrow(unmatched) > 0) {
            msg_lines <- paste0(unmatched$Channel,
                                if ("Geography" %in% names(unmatched) &&
                                    any(nzchar(unmatched$Geography %||% "")))
                                  paste0(" / ", unmatched$Geography) else "",
                                " -> ", unmatched$MainModelVariableName)
            showNotification(
              tagList(tags$strong(paste0(nrow(unmatched), " ROI(s) not matched:")),
                      tags$ul(class = "mt-1 ps-3 small",
                              lapply(head(msg_lines, 5), tags$li),
                              if (nrow(unmatched) > 5)
                                tags$li(paste0("... and ", nrow(unmatched) - 5, " more")))),
              type = "warning", duration = 15)
          }
        }, error = function(e)
          showNotification(paste("ROI join error:", e$message),
                           type = "warning", duration = 15))
      }
      format_seed_for_indices(act_df, seed_roi_cols)
    }

    # Final export builders used by the ZIP and package preview.
    build_side_mapping_export <- function(res_list, channels_list = list(), d = NULL,
                                          snapshot = NULL) {
      standard_cols <- c("VariableSplit", "MainModelVariableName", "Weight",
                         "MinWeight", "MaxWeight", "rank")

      normalize_mapping <- function(df, nm, cfg = list()) {
        if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df)) {
          return(NULL)
        }
        out <- as.data.frame(df)
        out$VariableSplit <- normalize_export_split(out$VariableSplit)
        out <- out[!is.na(out$VariableSplit) & nzchar(out$VariableSplit), , drop = FALSE]
        if (!nrow(out)) return(NULL)

        if (!"MainModelVariableName" %in% names(out)) {
          if ("model_var" %in% names(out)) {
            out$MainModelVariableName <- out$model_var
          } else {
            out$MainModelVariableName <- cfg$model_variable %||% cfg$channel_name %||% nm
          }
        }
        out$MainModelVariableName <- normalize_export_split(out$MainModelVariableName)
        if (!"Weight" %in% names(out)) out$Weight <- 1
        if (!"MinWeight" %in% names(out)) out$MinWeight <- 0.5
        if (!"MaxWeight" %in% names(out)) out$MaxWeight <- 2
        if (!"rank" %in% names(out)) out$rank <- NA_real_
        out$Weight <- suppressWarnings(as.numeric(out$Weight))
        out$MinWeight <- suppressWarnings(as.numeric(out$MinWeight))
        out$MaxWeight <- suppressWarnings(as.numeric(out$MaxWeight))
        out$rank <- suppressWarnings(as.numeric(out$rank))

        out %>%
          dplyr::select(dplyr::any_of(standard_cols)) %>%
          dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
      }

      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        res <- res_list[[nm]]
        cfg <- channels_list[[nm]] %||% list()
        final <- if (!is.null(snapshot$export_data[[nm]])) {
          snapshot$export_data[[nm]]$final
        } else {
          final_activity_splits(res, cfg, nm)
        }
        if (!nrow(final)) return(NULL)

        out <- final %>%
          dplyr::select(VariableSplit, MainModelVariableName) %>%
          dplyr::mutate(
            Weight = 1,
            MinWeight = 0.5,
            MaxWeight = 2,
            rank = NA_real_
          )

        side_meta <- normalize_mapping(res$side_mapping, nm, cfg)
        if (!is.null(side_meta) && nrow(side_meta) > 0) {
          side_meta <- side_meta %>%
            dplyr::filter(.data$VariableSplit %in% out$VariableSplit) %>%
            dplyr::transmute(
              VariableSplit = .data$VariableSplit,
              Weight.side = .data$Weight,
              MinWeight.side = .data$MinWeight,
              MaxWeight.side = .data$MaxWeight,
              rank.side = .data$rank
            ) %>%
            dplyr::distinct(VariableSplit, .keep_all = TRUE)

          out <- out %>%
            dplyr::left_join(side_meta, by = "VariableSplit") %>%
            dplyr::mutate(
              Weight = dplyr::coalesce(.data$Weight.side, .data$Weight),
              MinWeight = dplyr::coalesce(.data$MinWeight.side, .data$MinWeight),
              MaxWeight = dplyr::coalesce(.data$MaxWeight.side, .data$MaxWeight),
              rank = dplyr::coalesce(.data$rank.side, .data$rank)
            ) %>%
            dplyr::select(dplyr::all_of(standard_cols))
        }

        out %>% dplyr::select(dplyr::all_of(standard_cols))
      }))

      result <- if (length(rows)) dplyr::bind_rows(rows) else NULL

      if (!is.null(d) &&
          !is.null(d$side_mapping_nonfocus) &&
          nrow(d$side_mapping_nonfocus) > 0) {
        nf <- normalize_mapping(
          d$side_mapping_nonfocus,
          nm = "nonfocus",
          cfg = list(model_variable = NA_character_)
        )
        if (!is.null(nf) && nrow(nf) > 0) {
          result <- if (is.null(result)) nf else dplyr::bind_rows(result, nf)
        }
      }

      if (is.null(result) || !nrow(result)) return(NULL)
      result %>%
        dplyr::arrange(.data$VariableSplit, .data$MainModelVariableName) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE) %>%
        dplyr::select(dplyr::all_of(standard_cols))
    }

    build_split_composition <- function(res_list, clean_list, channels_list, d = NULL,
                                        snapshot = NULL) {
      if (!length(res_list)) return(NULL)

      is_focus_split_name <- function(x) {
        x <- normalize_export_split(x)
        !is.na(x) & nzchar(x) & !grepl("_Before(\\s+|_)", x, ignore.case = TRUE)
      }

      metric_split_variants <- function(split_nm, cfg = list()) {
        split_nm <- normalize_export_split(split_nm)
        split_nm <- split_nm[!is.na(split_nm) & nzchar(split_nm)]
        if (!length(split_nm)) return(character(0))

        act_kw <- cfg$activity_keyword %||% ""
        spend_kw <- cfg$spend_keyword %||% ""
        swapped <- split_nm
        if (nzchar(act_kw) && nzchar(spend_kw)) {
          swapped <- c(
            swapped,
            stringr::str_replace(split_nm, stringr::regex(act_kw, ignore_case = TRUE), spend_kw),
            stringr::str_replace(split_nm, stringr::regex(spend_kw, ignore_case = TRUE), act_kw)
          )
        }
        unique(normalize_export_split(swapped))
      }

      make_metric_lookup <- function(df, metric, cfg = list()) {
        if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df) ||
            !metric %in% names(df)) {
          return(function(split_nm) NA_real_)
        }
        vals <- suppressWarnings(as.numeric(df[[metric]]))
        key_index <- new.env(parent = emptyenv(), hash = TRUE)
        for (i in seq_len(nrow(df))) {
          candidate_keys <- unique(unlist(
            lapply(metric_split_variants(df$VariableSplit[i], cfg), split_key_variants),
            use.names = FALSE
          ))
          candidate_keys <- candidate_keys[!is.na(candidate_keys) & nzchar(candidate_keys)]
          for (key in candidate_keys) {
            current <- get0(key, envir = key_index, ifnotfound = integer(),
                            inherits = FALSE)
            assign(key, unique(c(current, i)), envir = key_index)
          }
        }
        function(split_nm) {
          keys <- unique(unlist(lapply(metric_split_variants(split_nm, cfg), split_key_variants),
                                use.names = FALSE))
          keys <- keys[!is.na(keys) & nzchar(keys)]
          if (!length(keys)) return(NA_real_)
          idx <- unique(unlist(mget(keys, envir = key_index,
                                    ifnotfound = list(integer()),
                                    inherits = FALSE),
                               use.names = FALSE))
          if (!length(idx)) return(NA_real_)
          found <- vals[idx]
          found <- found[!is.na(found)]
          if (length(found)) sum(found, na.rm = TRUE) else NA_real_
        }
      }

      channel_from_roi <- function(nm, cfg) {
        rois <- if (!is.null(d)) clean_roi_columns(d$channels_rois) else NULL
        if (!is.null(rois) &&
            all(c("MainModelVariableName", "Channel") %in% names(rois))) {
          mv <- cfg$model_variable %||% nm
          rows_roi <- rois[trimws(as.character(rois$MainModelVariableName)) ==
                             trimws(as.character(mv)), "Channel", drop = TRUE]
          rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(as.character(rows_roi)))]
          if (length(rows_roi)) return(trimws(as.character(rows_roi[1])))
        }
        nm
      }

      rows <- Filter(Negate(is.null), lapply(names(channels_list), function(nm) {
        res <- res_list[[nm]]
        clean <- clean_list[[nm]] %||% list()
        cfg <- channels_list[[nm]] %||% list()
        snap_ch <- NULL
        if (!is.null(snapshot) && !is.null(snapshot$export_data) &&
            !is.null(snapshot$export_data[[nm]])) {
          snap_ch <- ensure_channel_export_payload(
            snapshot$export_data[[nm]],
            d,
            snapshot$config %||% list()
          )
        }
        final <- if (!is.null(snap_ch)) snap_ch$final else final_activity_splits(res, cfg, nm)
        if (!nrow(final)) return(NULL)

        pre_act <- if (!is.null(snap_ch)) snap_ch$pre_act else pre_merge_activity_splits(clean, cfg, nm)
        if (!nrow(pre_act)) pre_act <- final
        current_cost <- if (!is.null(snap_ch)) snap_ch$final_cost else final_spend_splits(res, cfg, nm)
        pre_cost <- if (!is.null(snap_ch)) snap_ch$pre_cost else pre_merge_spend_splits(clean, cfg, nm)

        resolved <- if (!is.null(snap_ch)) {
          snap_ch$merge_resolved
        } else {
          resolve_export_merge_map(cfg, final, pre_act)
        }
        merge_map <- resolved$map
        lineage <- merge_map

        ch_name <- export_channel_label(nm, cfg, d)
        mmv <- cfg$model_variable %||% nm
        canonical <- if (!is.null(snap_ch)) snap_ch$canonical_totals else NULL
        if (!is.null(canonical) &&
            nrow(canonical$component_focus_totals %||% tibble::tibble()) > 0 &&
            nrow(canonical$final_focus_totals %||% tibble::tibble()) > 0) {
          lineage <- canonical$merge_map
          lineage <- lineage %>%
            dplyr::filter(
              is_focus_split_name(.data$MergedSplitName),
              is_focus_split_name(.data$ComponentSplit)
            )
          if (!nrow(lineage)) return(NULL)
          component_metrics <- canonical$component_focus_totals %>%
            dplyr::rename(
              total_activity = Component_Activity,
              total_spend = Component_Spend
            )
          final_metrics <- canonical$final_focus_totals %>%
            dplyr::rename(
              total_activity = Activity,
              total_spend = Spend
            )
        } else {
          lineage <- lineage %>%
            dplyr::filter(
              is_focus_split_name(.data$MergedSplitName),
              is_focus_split_name(.data$ComponentSplit)
            )
          if (!nrow(lineage)) return(NULL)
          metric_pre_act <- if (!is.null(snap_ch) && !is.null(snap_ch$rae_totals))
            snap_ch$rae_totals$activity else pre_act
          metric_pre_cost <- if (!is.null(snap_ch) && !is.null(snap_ch$rae_totals))
            snap_ch$rae_totals$spend else pre_cost
          component_metrics <- dplyr::full_join(
            metric_pre_act %>% dplyr::select(VariableSplit, total_activity),
            metric_pre_cost %>% dplyr::select(VariableSplit, total_spend),
            by = "VariableSplit"
          ) %>%
            dplyr::filter(is_focus_split_name(.data$VariableSplit))
          final_metrics <- component_metrics
        }
        component_metrics <- component_metrics %>%
          dplyr::filter(is_focus_split_name(.data$VariableSplit))
        final_metrics <- final_metrics %>%
          dplyr::filter(is_focus_split_name(.data$VariableSplit))
        lookup_component_activity <- make_metric_lookup(component_metrics, "total_activity", cfg)
        lookup_component_spend <- make_metric_lookup(component_metrics, "total_spend", cfg)
        lookup_merged_activity <- make_metric_lookup(final_metrics, "total_activity", cfg)
        lookup_merged_spend <- make_metric_lookup(final_metrics, "total_spend", cfg)
        channel_metric <- normalize_model_metric(
          res$model_metric %||% cfg$model_metric %||% "activity"
        )

        lineage %>%
          dplyr::mutate(
            Channel = ch_name,
            MainModelVariableName = mmv,
            Component_Activity = vapply(.data$ComponentSplit, lookup_component_activity, numeric(1)),
            Component_Spend = vapply(.data$ComponentSplit, lookup_component_spend, numeric(1)),
            Merged_Activity = vapply(.data$MergedSplitName, lookup_merged_activity, numeric(1)),
            Merged_Spend = vapply(.data$MergedSplitName, lookup_merged_spend, numeric(1))
          ) %>%
          dplyr::mutate(
            Component_Activity = dplyr::coalesce(.data$Component_Activity, 0),
            Component_Spend = dplyr::coalesce(.data$Component_Spend, 0),
            Merged_Activity = dplyr::if_else(
              is.na(.data$Merged_Activity),
              ave(.data$Component_Activity, .data$MergedSplitName, FUN = sum),
              .data$Merged_Activity
            ),
            Merged_Spend = dplyr::if_else(
              is.na(.data$Merged_Spend),
              ave(.data$Component_Spend, .data$MergedSplitName, FUN = sum),
              .data$Merged_Spend
            ),
            Component_Pct = dplyr::if_else(
              if (identical(channel_metric, "spend")) {
                .data$Merged_Spend > 0
              } else {
                .data$Merged_Activity > 0
              },
              if (identical(channel_metric, "spend")) {
                round(.data$Component_Spend / .data$Merged_Spend * 100, 2)
              } else {
                round(.data$Component_Activity / .data$Merged_Activity * 100, 2)
              },
              NA_real_
            )
          )
      }))

      if (!length(rows)) return(NULL)
      result <- dplyr::bind_rows(rows)
      if (!nrow(result)) return(NULL)

      result %>%
        dplyr::arrange(
          .data$Channel,
          .data$MainModelVariableName,
          .data$MergedSplitName,
          dplyr::desc(.data$Component_Activity)
        ) %>%
        dplyr::select(
          Channel,
          MainModelVariableName,
          MergedSplitName,
          ComponentSplit,
          Component_Activity,
          Component_Pct,
          Component_Spend,
          `Total Activity` = Merged_Activity,
          `Total Spend` = Merged_Spend
        )
    }

    apply_scwa_flags <- function(df, flags = scwa_flags()) {
      if (is.null(df) || !nrow(df)) return(df)
      checked_scwa_keys <- names(flags)[flags]
      df %>%
        dplyr::mutate(
          SCWA = scwa_key(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName) %in%
            checked_scwa_keys
        )
    }

    scwa_channel_choices <- reactive({
      snap <- export_snapshot()
      candidates <- names(Filter(function(item) {
        if (!is.list(item)) return(FALSE)
        cfg <- item$cfg %||% list()
        length(extract_export_merges(cfg)) > 0 &&
          nrow(item$final %||% tibble::tibble()) > 0
      }, snap$export_data %||% list()))

      if (!length(candidates)) {
        return(c("Select a channel..." = ""))
      }

      labels <- vapply(candidates, function(nm) {
        export_channel_label(nm, snap$channels[[nm]] %||% list(), snap$data)
      }, character(1))
      labels <- make.unique(labels, sep = " - ")
      c("Select a channel..." = "", stats::setNames(candidates, labels))
    })

    scwa_split_composition <- reactive({
      nm <- input$scwa_channel_filter %||% ""
      if (!nzchar(nm)) return(NULL)
      snap <- export_snapshot()
      if (!nm %in% names(snap$channels)) return(NULL)
      cache_key <- paste0("scwa::", nm)
      cached <- scwa_cache_get(cache_key)
      if (!is.null(cached) && isTRUE(cached$hit)) return(cached$value)
      payload <- profile_export(paste0("SCWA channel ", nm), {
        single_snap <- snap
        single_snap$channels <- snap$channels[nm]
        single_snap$results <- snap$results[nm]
        single_snap$clean_results <- snap$clean_results[nm]
        single_snap$export_data <- snap$export_data[nm]
        ensure_export_payload(single_snap)
      })
      out <- build_split_composition(
        payload$results,
        payload$clean_results,
        payload$channels,
        payload$data,
        snapshot = payload
      )
      scwa_cache_set(cache_key, out)
      out
    })

    output$scwa_workaround <- renderUI({
      channel_choices <- scwa_channel_choices()
      current_channel <- input$scwa_channel_filter %||% ""
      if (!current_channel %in% unname(channel_choices)) current_channel <- ""
      df <- scwa_split_composition()
      flags <- isolate(scwa_flags())
      df <- apply_scwa_flags(df, flags)
      df <- if (!is.null(df)) as.data.frame(df) else NULL

      if (length(channel_choices) <= 1L) {
        return(div(class = "scwa-card scwa-card-empty",
                   div(class = "scwa-card-head",
                       div(class = "card-header-inner",
                           icon("wand-magic-sparkles", class = "icon-blue-sm"),
                           tags$strong("Splits Composition Workaround (SCWA)"))),
                   div(class = "scwa-empty",
                       icon("code-branch", class = "scwa-empty-icon"),
                       tags$span("No focus merges available for workaround review."))))
      }

      if (!nzchar(current_channel)) {
        rows_ui <- div(class = "scwa-empty scwa-empty-compact",
                       icon("circle-info", class = "scwa-empty-icon"),
                       tags$span("Select a channel to load workaround review."))
        n_merges <- 0L
        n_marked <- 0L
      } else if (is.null(df) || !nrow(df)) {
        rows_ui <- div(class = "scwa-empty scwa-empty-compact",
                       tags$span("No focus merges available for the selected channel."))
        n_merges <- 0L
        n_marked <- 0L
      } else {
        summary <- df %>%
          dplyr::group_by(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName) %>%
          dplyr::summarise(
            NumberOfSplits = dplyr::n_distinct(.data$ComponentSplit),
            TotalActivity = {
              vals <- .data$`Total Activity`[!is.na(.data$`Total Activity`)]
              if (length(vals)) max(vals) else 0
            },
            TotalSpend = {
              vals <- .data$`Total Spend`[!is.na(.data$`Total Spend`)]
              if (length(vals)) max(vals) else 0
            },
            SCWA = any(.data$SCWA, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName)

        n_merges <- nrow(summary)
        n_marked <- sum(summary$SCWA, na.rm = TRUE)

        rows_ui <- tagList(lapply(seq_len(nrow(summary)), function(i) {
          row <- summary[i, , drop = FALSE]
          key <- scwa_key(row$Channel, row$MainModelVariableName, row$MergedSplitName)
          comps <- df[
            df$Channel == row$Channel &
              df$MainModelVariableName == row$MainModelVariableName &
              df$MergedSplitName == row$MergedSplitName,
            , drop = FALSE
          ]
          comps <- comps %>%
            dplyr::arrange(dplyr::desc(.data$Component_Activity), .data$ComponentSplit)

          tags$details(class = "scwa-row",
            tags$summary(class = "scwa-summary",
              div(class = "scwa-main",
                  tags$span("+", class = "scwa-expand"),
                  tags$span(row$MergedSplitName, class = "scwa-merge-name"),
                  tags$span(paste(row$Channel, row$MainModelVariableName, sep = " | "),
                            class = "scwa-merge-meta")),
              div(class = "scwa-metrics",
                  tags$span(class = "scwa-pill", paste0(row$NumberOfSplits, " splits")),
                  tags$span(class = "scwa-metric",
                            tags$strong(format(round(row$TotalActivity, 2), big.mark = ",")),
                            tags$small(" Activity")),
                  tags$span(class = "scwa-metric",
                            tags$strong(format(round(row$TotalSpend, 2), big.mark = ",")),
                            tags$small(" Spend")),
                  tags$label(class = "scwa-check",
                    tags$input(
                      type = "checkbox",
                      checked = if (isTRUE(row$SCWA)) "checked" else NULL,
                      `data-key` = key,
                      onclick = "event.stopPropagation();",
                      onchange = sprintf(
                        "event.stopPropagation(); var card=this.closest('.scwa-card'); if(card){var total=card.querySelectorAll('.scwa-check input').length; var marked=card.querySelectorAll('.scwa-check input:checked').length; var count=card.querySelector('.scwa-count'); if(count){count.textContent=marked + '/' + total + ' marked';}} Shiny.setInputValue('%s', {key: this.dataset.key, value: this.checked, nonce: Math.random()}, {priority: 'event'});",
                        session$ns("scwa_toggle")
                      )
                    ),
                    tags$span("Need workaround")
                  ))),
            div(class = "scwa-components",
                div(class = "scwa-components-head",
                    tags$span("Component split"),
                    tags$span("Activity"),
                    tags$span("Spend"),
                    tags$span("Pct")),
                tagList(lapply(seq_len(nrow(comps)), function(j) {
                  comp <- comps[j, , drop = FALSE]
                  div(class = "scwa-component-row",
                      tags$span(comp$ComponentSplit, class = "scwa-component-name"),
                      tags$span(format(round(comp$Component_Activity, 2), big.mark = ",")),
                      tags$span(format(round(comp$Component_Spend, 2), big.mark = ",")),
                      tags$span(ifelse(is.na(comp$Component_Pct), "",
                                       paste0(round(comp$Component_Pct, 2), "%"))))
                })))
          )
        }))
      }

      div(class = "scwa-card",
          div(class = "scwa-card-head",
              div(class = "card-header-inner",
                  icon("wand-magic-sparkles", class = "icon-blue-sm"),
                  tags$strong("Splits Composition Workaround (SCWA)")),
              tags$span(paste0(n_marked, "/", n_merges, " marked"),
                        class = "scwa-count")),
          div(class = "scwa-filters",
              div(class = "scwa-filter",
                  tags$label("Channel"),
                  selectInput(session$ns("scwa_channel_filter"), NULL,
                              choices = channel_choices, selected = current_channel,
                              width = "100%"))),
          div(class = "scwa-table-head",
              tags$span("Merged split"),
              tags$span("Review")),
          div(class = "scwa-list", rows_ui))
    })

    output$dl_zip <- downloadHandler(
      filename = function()
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content = function(file) {
        tmp_dir <- file.path(tempdir(), paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        written <- character(0)

        snap          <- export_snapshot()
        d_snap        <- snap$data
        res_snap      <- snap$results
        clean_snap    <- snap$clean_results
        channels_snap <- snap$channels
        gcfg_snap     <- snap$config
        heavy_snap    <- NULL
        get_heavy_snap <- function() {
          if (is.null(heavy_snap)) heavy_snap <<- export_heavy_payload()
          heavy_snap
        }

        withProgress(message = "Building export files...", value = 0, {

          incProgress(0.15, message = "Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended(d_snap, res_snap, channels_snap, gcfg_snap,
                                            schema_metadata = d_snap$schema_metadata,
                                            snapshot = snap)
            if (!is.null(df) && nrow(df) > 0) {
              f_csv <- file.path(tmp_dir, export_file_names$analytical_csv)
              readr::write_csv(df, f_csv, na = ""); written <- c(written, f_csv)
              f_rdata <- file.path(tmp_dir, export_file_names$analytical_rdata)
              local({ AnalyticalDataset <- df; save(AnalyticalDataset, file = f_rdata) })
              written <- c(written, f_rdata)
            }
          }, error = \(e) showNotification(paste("Analytical error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.15, message = "Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping_export(res_snap, channels_snap, d_snap,
                                            snapshot = snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$side_mapping)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            } else {
              showNotification(paste0("No Activity splits available for ",
                                      export_file_names$side_mapping, "."),
                               type = "warning", duration = 6)
            }
          }, error = \(e) showNotification(paste("Side Mapping error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.20, message = "Seed for Indices...")
          tryCatch({
            payload_snap <- get_heavy_snap()
            df <- build_activity_rois(d_snap, res_snap, channels_snap, gcfg_snap,
                                      snapshot = payload_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$seed_indices)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Seed for Indices error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.20, message = "Split Composition...")
          tryCatch({
            payload_snap <- get_heavy_snap()
            df <- build_split_composition(res_snap, clean_snap, channels_snap, d_snap,
                                          snapshot = payload_snap)
            df <- apply_scwa_flags(df)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$split_composition)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Split composition error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.15, message = "Channel configuration...")
          tryCatch({
            df <- export_channels_csv(channels_snap, config())
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$channel_config)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Config error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.05, message = "Creating ZIP archive...")
        })

        if (!length(written)) {
          showNotification("No data available to export.", type = "warning")
          writeLines("no data", file); return()
        }
        tryCatch(
          zip::zipr(zipfile = file, files = basename(written), root = tmp_dir),
          error = function(e)
            showNotification(paste("ZIP creation failed:", conditionMessage(e)),
                             type = "error", duration = 10))
      }
    )
  })
}
