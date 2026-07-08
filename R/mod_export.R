# ═══════════════════════════════════════════════════════════════════════
# R/mod_export.R
# ═══════════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("summary_strip")),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "h-100",
        card_header(div(class = "card-header-inner",
                        icon("circle-check", class = "icon-blue-sm"),
                        "Channel Status")),
        uiOutput(ns("channel_status"))
      ),
      tagList(
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
          uiOutput(ns("download_section")))
      )
    )
  )
}

mod_export_server <- function(id, results, data, config, channels,
                              clean_results = reactive(list()),
                              process_qa = reactive(list())) {
  moduleServer(id, function(input, output, session) {

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
        dplyr::distinct(.data$VariableSplit, .data$MainModelVariableName, .keep_all = TRUE)
    }

    final_activity_splits <- function(res, cfg = list(), nm = "") {
      if (is.null(res)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric()
        ))
      }

      final <- activity_splits_from_rag(res, cfg, nm)
      if (!nrow(final)) return(final)

      diag_meta <- summarize_split_diagnostics(res$act_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_activity.diag = total_activity) %>%
        dplyr::distinct(.data$VariableSplit, .keep_all = TRUE)
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
                dplyr::distinct(.data$VariableSplit, .keep_all = TRUE),
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
        dplyr::distinct(.data$VariableSplit, .data$MainModelVariableName, .keep_all = TRUE)
    }

    pre_merge_activity_splits <- function(clean, cfg = list(), nm = "") {
      pre <- activity_splits_from_rag(clean, cfg, nm)
      if (nrow(pre)) return(pre)
      summarize_split_diagnostics(clean$act_diagnoses, nm, cfg) %>%
        dplyr::select(VariableSplit, MainModelVariableName, total_activity)
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
        if (is.null(m)) return(NULL)
        active <- isTRUE(m$active) || isTRUE(m$enabled) || isTRUE(m$checked)
        if (!active) return(NULL)
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

    export_merge_issue_summary <- reactive({
      res_list <- results()
      clean_list <- clean_results()
      ch_list <- channels()
      issues <- unlist(lapply(names(res_list), function(nm) {
        res <- res_list[[nm]]
        cfg <- ch_list[[nm]] %||% list()
        if (is.null(res) || !length(extract_export_merges(cfg))) return(character(0))
        resolved <- resolve_export_merge_map(
          cfg,
          final_activity_splits(res, cfg, nm),
          pre_merge_activity_splits(clean_list[[nm]] %||% list(), cfg, nm)
        )
        if (!length(resolved$issues)) return(character(0))
        paste0(nm, ": ", resolved$issues)
      }), use.names = FALSE)
      issues <- unique(issues[!is.na(issues) & nzchar(issues)])
      list(count = length(issues), names = issues)
    })

    channel_summary <- reactive({
      ch_names <- names(channels())
      res_list <- results()
      d        <- data()
      gcfg     <- config()

      an_periods_all <- if (!is.null(d$analytical) &&
                            "Period" %in% names(d$analytical))
        sort(unique(d$analytical$Period)) else NULL
      an_min_all <- if (length(an_periods_all)) min(an_periods_all) else NULL
      an_max_all <- if (length(an_periods_all)) max(an_periods_all) else NULL
      has_analytical <- !is.null(d$analytical) &&
        !is.null(gcfg$cross_cols) && length(an_periods_all) > 0

      lapply(ch_names, function(nm) {
        res <- res_list[[nm]]
        if (is.null(res)) return(list(name = nm, processed = FALSE,
                                      has_warning = FALSE,
                                      tc_mismatch = FALSE, n_splits = 0L))

        cfg_ch <- channels()[[nm]] %||% list()
        n_splits_total <- nrow(final_activity_splits(res, cfg_ch, nm))

        if (n_splits_total == 0L)
          return(list(name = nm, processed = TRUE, has_warning = TRUE,
                      tc_mismatch = FALSE, n_splits = 0L))

        tc_mismatch <- tryCatch({
          if (!has_analytical) return(FALSE)
          cfg_ch    <- channels()[[nm]]
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

          id_in_rag  <- intersect(cross_id, names(rag_scope))
          spend_kw   <- cfg_ch$spend_keyword %||% "Spend"
          all_num    <- setdiff(names(rag_scope)[sapply(rag_scope, is.numeric)], id_in_rag)
          split_cols <- all_num[!grepl(spend_kw, all_num, ignore.case = TRUE)]
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
      })
    }) %>% bindCache(
      paste(names(channels()), collapse = ","),
      paste(names(results()), collapse = ","),
      sum(vapply(results(), function(r) if (is.null(r$rag)) 0L else nrow(r$rag), integer(1))),
      nrow(data()$analytical %||% data.frame())
    )

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

    compact_names <- function(x, max_n = 4) {
      x <- unique(x[!is.na(x) & nzchar(trimws(x))])
      if (!length(x)) return("")
      out <- paste(head(x, max_n), collapse = ", ")
      if (length(x) > max_n) out <- paste0(out, " +", length(x) - max_n, " more")
      out
    }

    roi_issue_summary <- reactive({
      roi_df <- data()$channels_rois
      res_list <- results()
      if (is.null(roi_df) || !length(res_list)) {
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

      all_mvs <- dplyr::bind_rows(lapply(names(res_list), function(nm) {
        cfg <- channels()[[nm]]
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

          r <- res_list[[nm]]
          gcfg <- config()
          geo_col <- (r$cross_cols %||% gcfg$cross_cols %||% "Geography")[1]
          rag_geos <- if (!is.null(r) && !is.null(r$rag) &&
                           geo_col %in% names(as.data.frame(r$rag))) {
            sort(unique(as.data.frame(r$rag)[[geo_col]]))
          } else character(0)
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
      summ <- channel_summary()
      proc <- process_qa()
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
      merge_issues <- export_merge_issue_summary()
      rois_missing <- is.null(data()$channels_rois)
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
          detail = "The ZIP can still be downloaded, but seed_for_indices.csv will export without ROI values until ROIs by Channel is loaded in Setup.",
          names = character(0),
          highlight = TRUE
        ),
        if (!rois_missing && (roi_issues$count %||% 0L) > 0L) list(
          severity = "warn", icon = "chart-line",
          title = paste0(roi_issues$count, " ROI coverage warning",
                         if (roi_issues$count != 1) "s" else ""),
          detail = "Some channels or geographies are missing ROI coverage.",
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
      proc <- process_qa()
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
      res_list <- results(); d <- data(); gcfg <- config()
      total_splits <- 0L
      total_composition_rows <- 0L

      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        cfg <- channels()[[nm]] %||% list()
        final_splits <- final_activity_splits(r, cfg, nm)
        total_splits <- total_splits + nrow(final_splits)

        if (nrow(final_splits) > 0) {
          clean <- clean_results()[[nm]] %||% list()
          pre_splits <- pre_merge_activity_splits(clean, cfg, nm)
          resolved <- resolve_export_merge_map(cfg, final_splits, pre_splits)
          merged_names <- unique(resolved$map$MergedSplitName %||% character(0))
          total_composition_rows <- total_composition_rows +
            nrow(resolved$map) +
            sum(!final_splits$VariableSplit %in% merged_names)
        }
      }

      in_vars <- if (!is.null(d$details) &&
                     all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          dplyr::filter(!stringr::str_detect(
            stringr::str_to_lower(trimws(Type)), "none")) %>%
          dplyr::pull(VariableName) %>% unique()
      } else {
        mv <- unique(vapply(channels(), \(c) c$model_variable %||% "", character(1)))
        mv[nzchar(mv)]
      }

      id_an   <- length(intersect(c("Geography", "Product", "Period", "BP_Year"),
                                  names(d$analytical %||% list())))
      an_vars <- if (!is.null(d$analytical))
        length(intersect(in_vars, names(d$analytical))) else 0L
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical) else 0L
      an_cols <- id_an + an_vars + total_splits
      n_ch    <- length(channels())

      # In Model Update mode, add nonfocus split columns to the analytical dims
      nonfocus_n <- if (!is.null(d$side_mapping_nonfocus))
        nrow(d$side_mapping_nonfocus) else 0L

      list(
        analytical  = if (an_rows > 0) list(rows = an_rows,
                                            cols = an_cols + nonfocus_n) else NULL,
        side_map    = if (total_splits > 0) list(rows = total_splits + nonfocus_n,
                                                 cols = 6L) else NULL,
        activity    = if (total_splits > 0) list(rows = total_splits, cols = 7L) else NULL,
        composition = if (total_composition_rows > 0) {
          list(rows = total_composition_rows, cols = 9L)
        } else NULL,
        config      = if (n_ch > 0) list(rows = n_ch, cols = NULL) else NULL
      )
    })

    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      proc    <- process_qa()
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
      d    <- data()
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
          mk_file_row("table-cells",    "blue",   "analytical_splits_extended.csv",
                      "IN/FIXED model variables + all split columns appended",
                      dims$analytical),
          mk_file_row("database",       "blue",   "analytical_splits_extended.RData",
                      "Same dataset as RData — load as AnalyticalDataset in next update",
                      dims$analytical),
          div(class = "export-file-group-title",
              icon("diagram-project", class = "icon-blue-sm"), "Mapping & Seeds"),
          mk_file_row("diagram-project","purple", "side_model_mapping.csv",
                      "Split-to-model mapping with PSO weight structure",
                      dims$side_map),
          mk_file_row("chart-column",   "green",  "seed_for_indices.csv",
                      "Activity, spend and ROI totals with split order per split",
                      dims$activity),
          mk_file_row("code-branch",    "teal",   "split_composition.csv",
                      "Split lineage: components, activity and spend per period",
                      dims$composition),
          div(class = "export-file-group-title",
              icon("gear", class = "icon-blue-sm"), "Configuration"),
          mk_file_row("gear",           "amber",  "channel_config.csv",
                      "Split order, merges, breaks and segment overrides",
                      dims$config)
        )
      )
    })

    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- data()$channels_rois

      if (!"MainModelVariableName" %in% names(roi_df))
        return(div(class = "roi-box-error",
                   div(class = "card-header-inner",
                       tags$span("ROIs file missing required column: MainModelVariableName",
                                 class = "roi-error-msg"))))

      res_list <- results()
      if (!length(res_list)) return(NULL)

      has_geo_col  <- "Geography" %in% names(roi_df)
      normalize_mv <- function(x)
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)],
                              c("MainModelVariableName"))

      all_mvs <- dplyr::bind_rows(lapply(names(res_list), function(nm) {
        cfg <- channels()[[nm]]; mv <- cfg$model_variable %||% ""
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
          r       <- res_list[[nm]]; gcfg <- config()
          geo_col <- (r$cross_cols %||% gcfg$cross_cols %||% "Geography")[1]
          rag_geos <- if (!is.null(r) && !is.null(r$rag))
            sort(unique(as.data.frame(r$rag)[[geo_col]])) else character(0)
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
                  tags$span(paste0("All ", nrow(all_mvs), " channel(s) have ROI coverage."),
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
                  tags$span(paste0("All ", n_total, " channel(s) matched with ROI values."),
                            class = "roi-ok-msg")))
        } else {
          missing_rows <- checked %>% dplyr::filter(!.has_roi) %>%
            dplyr::mutate(label = paste0(Channel, " -> ", MainModelVariableName)) %>%
            dplyr::pull(label)
          div(class = "roi-box-warn",
              div(class = "card-header-between mb-2",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_missing, " of ", n_total,
                                         " channel(s) have no ROI"), class = "roi-warn-title")),
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
      summ    <- channel_summary()
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
                                          schema_metadata = NULL) {
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
      keep_an_cols <- union(keep_an_cols,
                            intersect("Weight Variable MMM", names(d$analytical)))

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
        split_cols <- final_activity_splits(r, cfg_ch, nm)$VariableSplit
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

    build_activity_rois <- function(d, res_list, channels_list, gcfg) {
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r   <- res_list[[nm]]
        cfg <- channels_list[[nm]]
        if (is.null(r) || is.null(r$rag)) return(NULL)

        rag_df     <- as.data.frame(r$rag)
        cross_cols <- r$cross_cols %||% gcfg$cross_cols %||% "Geography"
        cross_id   <- c(cross_cols, "Period")
        id_in_rag  <- intersect(cross_id, names(rag_df))
        geo_col    <- cross_cols[1]
        act_kw     <- cfg$activity_keyword %||% "Impressions"
        spend_kw   <- cfg$spend_keyword    %||% "Spend"

        split_order_str <- paste(cfg$split_columns %||% "VariableName", collapse = "|")

        channel_from_roi <- {
          rois <- d$channels_rois
          if (!is.null(rois) &&
              all(c("MainModelVariableName", "Channel") %in% names(rois))) {
            mv       <- cfg$model_variable %||% nm
            rows_roi <- rois[trimws(rois$MainModelVariableName) == trimws(mv),
                             "Channel", drop = TRUE]
            rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(rows_roi))]
            if (length(rows_roi)) trimws(rows_roi[1]) else nm
          } else nm
        }

        all_split_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_in_rag)
        final_splits <- final_activity_splits(r, cfg, nm)
        act_cols <- intersect(final_splits$VariableSplit, all_split_cols)
        nonfocus_pattern <- "_Before(\\s+|_)"
        act_cols <- act_cols[!grepl(nonfocus_pattern, act_cols, ignore.case = TRUE)]
        cost_cols <- grep(spend_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
        cost_cols <- cost_cols[!grepl(nonfocus_pattern, cost_cols, ignore.case = TRUE)]
        if (!length(act_cols)) return(NULL)

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
        has_geo_roi_col <- !is.null(d$channels_rois) &&
          "Geography" %in% names(d$channels_rois)

        if (has_geo_roi_col && geo_col %in% names(cs_period)) {
          geo_vals <- sort(unique(cs_period[[geo_col]]))
          geo_rows <- lapply(geo_vals, function(geo) {
            cs_geo <- cs_period[cs_period[[geo_col]] == geo, , drop = FALSE]
            get_geo_total <- function(col) {
              vals <- cs_geo[[col]]
              if (all(is.na(vals) | vals == 0)) return(0)
              sum(vals, na.rm = TRUE)
            }
            act_totals  <- sapply(act_cols, get_geo_total)
            cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_geo_total) else numeric(0)
            act_df_g <- tibble::tibble(
              VariableSplit = act_cols, Geography = geo,
              total_activity = unname(act_totals),
              key = stringr::str_remove_all(act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
            cost_df_g <- if (length(cost_totals))
              tibble::tibble(total_spend = unname(cost_totals),
                             key = stringr::str_remove_all(
                               cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
            else tibble::tibble(total_spend = numeric(0), key = character(0))
            act_df_g %>%
              dplyr::left_join(cost_df_g, by = "key") %>% dplyr::select(-key) %>%
              dplyr::filter(total_activity > 0) %>%
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

          act_totals  <- sapply(act_cols, get_total)
          cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_total) else numeric(0)
          act_df_s <- tibble::tibble(
            VariableSplit = act_cols, Geography = NA_character_,
            total_activity = unname(act_totals),
            key = stringr::str_remove_all(act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
          cost_df_s <- if (length(cost_totals))
            tibble::tibble(total_spend = unname(cost_totals),
                           key = stringr::str_remove_all(
                             cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
          else tibble::tibble(total_spend = numeric(0), key = character(0))
          result <- act_df_s %>%
            dplyr::left_join(cost_df_s, by = "key") %>% dplyr::select(-key) %>%
            dplyr::filter(total_activity > 0) %>%
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

      if (!is.null(d$channels_rois) && nrow(d$channels_rois) > 0) {
        tryCatch({
          roi_df <- d$channels_rois
          if (!"MainModelVariableName" %in% names(roi_df)) return(act_df)
          roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)], names(act_df))
          if (!length(roi_num_cols)) return(act_df)

          normalize_mv <- function(x)
            trimws(stringr::str_remove(as.character(x),
                                       stringr::regex("(_Total)+$", ignore_case = TRUE)))
          has_geo_col <- "Geography" %in% names(roi_df)
          src_col     <- "Sourced VariableName"
          has_src_col <- src_col %in% names(roi_df) &&
            any(!is.na(roi_df[[src_col]]) &
                  nzchar(trimws(as.character(roi_df[[src_col]]))), na.rm = TRUE)
          empty_roi <- setNames(as.list(rep(NA_real_, length(roi_num_cols))), roi_num_cols)

          roi_norm <- roi_df %>%
            dplyr::mutate(
              mv_norm = normalize_mv(MainModelVariableName),
              geo_val = if (has_geo_col) trimws(as.character(Geography %||% "")) else "",
              sv_val  = if (has_src_col)
                trimws(as.character(.data[[src_col]] %||% "")) else "")

          matched_rois <- lapply(seq_len(nrow(act_df)), function(i) {
            mv_norm <- normalize_mv(act_df$MainModelVariableName[i] %||% "")
            geo_val <- if (is.null(act_df$Geography[i]) || is.na(act_df$Geography[i])) ""
            else trimws(as.character(act_df$Geography[i]))
            vs <- trimws(as.character(act_df$VariableSplit[i]))

            cands <- roi_norm[roi_norm$mv_norm == mv_norm, , drop = FALSE]
            if (!nrow(cands)) return(empty_roi)

            if (nzchar(geo_val)) {
              m1 <- cands[cands$geo_val == geo_val, , drop = FALSE]
              if (nrow(m1) > 0) return(as.list(m1[1, roi_num_cols, drop = FALSE]))
            }
            if (has_src_col) {
              sv_cands <- cands[nzchar(cands$sv_val), , drop = FALSE]
              if (nrow(sv_cands) > 0) {
                pm <- startsWith(vs, sv_cands$sv_val)
                if (any(pm)) {
                  best <- sv_cands[pm, , drop = FALSE]
                  best <- best[which.max(nchar(best$sv_val)), , drop = FALSE]
                  return(as.list(best[1, roi_num_cols, drop = FALSE]))
                }
              }
            }
            m3 <- cands[!nzchar(cands$geo_val) & !nzchar(cands$sv_val), , drop = FALSE]
            if (nrow(m3) > 0) return(as.list(m3[1, roi_num_cols, drop = FALSE]))
            empty_roi
          })

          act_df <- dplyr::bind_cols(act_df, dplyr::bind_rows(matched_rois))

          unmatched <- act_df %>%
            dplyr::filter(dplyr::if_any(dplyr::all_of(roi_num_cols), is.na)) %>%
            dplyr::select(dplyr::any_of(c("Channel", "MainModelVariableName", "Geography"))) %>%
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
      act_df
    }

    # Final export builders used by the ZIP and package preview.
    build_side_mapping_export <- function(res_list, channels_list = list(), d = NULL) {
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
          dplyr::distinct(.data$VariableSplit, .data$MainModelVariableName,
                          .keep_all = TRUE)
      }

      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        res <- res_list[[nm]]
        cfg <- channels_list[[nm]] %||% list()
        final <- final_activity_splits(res, cfg, nm)
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
            dplyr::distinct(.data$VariableSplit, .keep_all = TRUE)

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
        dplyr::distinct(.data$VariableSplit, .data$MainModelVariableName,
                        .keep_all = TRUE) %>%
        dplyr::select(dplyr::all_of(standard_cols))
    }

    build_split_composition <- function(res_list, clean_list, channels_list, d = NULL) {
      if (!length(res_list)) return(NULL)

      lookup_metric <- function(split_nm, df, metric) {
        if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df) ||
            !metric %in% names(df)) {
          return(NA_real_)
        }
        keys <- split_key_variants(split_nm)
        cand <- df[vapply(df$VariableSplit, function(candidate) {
          length(intersect(keys, split_key_variants(candidate))) > 0
        }, logical(1)), , drop = FALSE]
        if (!nrow(cand)) return(NA_real_)
        vals <- suppressWarnings(as.numeric(cand[[metric]]))
        vals <- vals[!is.na(vals)]
        if (length(vals)) sum(vals, na.rm = TRUE) else NA_real_
      }

      channel_from_roi <- function(nm, cfg) {
        rois <- if (!is.null(d)) d$channels_rois else NULL
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

      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        res <- res_list[[nm]]
        clean <- clean_list[[nm]] %||% list()
        cfg <- channels_list[[nm]] %||% list()
        final <- final_activity_splits(res, cfg, nm)
        if (!nrow(final)) return(NULL)

        pre_act <- pre_merge_activity_splits(clean, cfg, nm)
        if (!nrow(pre_act)) pre_act <- final
        current_cost <- summarize_split_diagnostics(res$cost_diagnoses, nm, cfg)
        pre_cost <- summarize_split_diagnostics(clean$cost_diagnoses, nm, cfg)

        resolved <- resolve_export_merge_map(cfg, final, pre_act)
        merge_map <- resolved$map
        merged_names <- unique(merge_map$MergedSplitName %||% character(0))

        self_rows <- final %>%
          dplyr::filter(!.data$VariableSplit %in% merged_names) %>%
          dplyr::transmute(
            MergedSplitName = .data$VariableSplit,
            ComponentSplit = .data$VariableSplit
          )

        lineage <- dplyr::bind_rows(merge_map, self_rows)
        if (!nrow(lineage)) return(NULL)

        ch_name <- channel_from_roi(nm, cfg)
        mmv <- cfg$model_variable %||% nm

        lineage %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            Channel = ch_name,
            MainModelVariableName = mmv,
            Component_Activity = lookup_metric(.data$ComponentSplit, pre_act, "total_activity"),
            Component_Spend = lookup_metric(.data$ComponentSplit, pre_cost, "total_spend"),
            Merged_Activity = lookup_metric(.data$MergedSplitName, final, "total_activity"),
            Merged_Spend = lookup_metric(.data$MergedSplitName, current_cost, "total_spend")
          ) %>%
          dplyr::ungroup() %>%
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
              .data$Merged_Activity > 0,
              round(.data$Component_Activity / .data$Merged_Activity * 100, 2),
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
          Merged_Activity,
          Merged_Spend
        )
    }

    output$dl_zip <- downloadHandler(
      filename = function()
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content = function(file) {
        tmp_dir <- file.path(tempdir(), paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        written <- character(0)

        d_snap        <- data()
        res_snap      <- results()
        clean_snap    <- clean_results()
        channels_snap <- channels()
        gcfg_snap     <- config()

        withProgress(message = "Building export files...", value = 0, {

          incProgress(0.15, message = "Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended(d_snap, res_snap, channels_snap, gcfg_snap,
                                            schema_metadata = d_snap$schema_metadata)
            if (!is.null(df) && nrow(df) > 0) {
              f_csv <- file.path(tmp_dir, "analytical_splits_extended.csv")
              readr::write_csv(df, f_csv, na = ""); written <- c(written, f_csv)
              f_rdata <- file.path(tmp_dir, "analytical_splits_extended.RData")
              local({ AnalyticalDataset <- df; save(AnalyticalDataset, file = f_rdata) })
              written <- c(written, f_rdata)
            }
          }, error = \(e) showNotification(paste("Analytical error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.15, message = "Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping_export(res_snap, channels_snap, d_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "side_model_mapping.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            } else {
              showNotification("No Activity splits available for side_model_mapping.csv.",
                               type = "warning", duration = 6)
            }
          }, error = \(e) showNotification(paste("Side Mapping error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.20, message = "Seed for Indices...")
          tryCatch({
            df <- build_activity_rois(d_snap, res_snap, channels_snap, gcfg_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "seed_for_indices.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Seed for Indices error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.20, message = "Split Composition...")
          tryCatch({
            df <- build_split_composition(res_snap, clean_snap, channels_snap, d_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "split_composition.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Split composition error:", e$message),
                                           type = "warning", duration = 6))

          incProgress(0.15, message = "Channel configuration...")
          tryCatch({
            df <- export_channels_csv(channels_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "channel_config.csv")
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
