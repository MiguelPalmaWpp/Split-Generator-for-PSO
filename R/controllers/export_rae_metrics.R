# Calculate effective RAE activity/spend totals for export reconciliation.
# Reuse the channel's audited filters and period rather than raw unfiltered
# RAE rows; do not modify the uploaded source table.
register_export_rae_metrics <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    export_metric_totals_from_rae <- function(d, cfg = list(), gcfg = list(), nm = "",
                                              res = NULL) {
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
      if (length(vi) > 0 && !length(cfg$modeled_varname_include %||% character(0))) {
        vi <- expand_varname_include_with_spend(
          unique(source_data$VariableName),
          vi,
          cfg$spend_keyword %||% NULL
        )
      }
      if (length(vi) > 0) {
        vi <- expand_analytical_keys_to_variable_names(
          unique(source_data$VariableName), vi
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
        build_split_period_suffix(
          update_label,
          focus = FALSE,
          time_break_label = cfg$time_break_label %||% "",
          geo_label = cfg$geo_label %||% ""
        )
      }
      source_data$VariableSplit <- ifelse(
        period_tag == "focus",
        paste0(source_data$SplitName, "_", build_split_period_suffix(
          update_label,
          focus = TRUE,
          geo_label = cfg$geo_label %||% ""
        )),
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
      pair_key_map <- setNames(character(0), character(0))
      manifest <- res$split_manifest %||% tibble::tibble()
      if (nrow(manifest) &&
          all(c("Role", "VariableSplit", "PairedVariableSplit", "PairKey", "PeriodScope") %in% names(manifest))) {
        modeled_manifest <- manifest %>%
          dplyr::filter(.data$Role == "modeled") %>%
          dplyr::distinct(.data$VariableSplit, .data$PairedVariableSplit,
                          .data$PairKey, .data$PeriodScope, .keep_all = TRUE)
        if (nrow(modeled_manifest)) {
          canonical <- paste(
            modeled_manifest$PairKey,
            modeled_manifest$PeriodScope,
            sep = "||"
          )
          pair_key_map[modeled_manifest$VariableSplit] <- canonical
          paired_ok <- !is.na(modeled_manifest$PairedVariableSplit) &
            nzchar(modeled_manifest$PairedVariableSplit)
          pair_key_map[modeled_manifest$PairedVariableSplit[paired_ok]] <- canonical[paired_ok]
        }
      }
      split_pair_key <- function(x, keyword = "") {
        x <- as.character(x)
        mapped <- unname(pair_key_map[x])
        fallback <- if (nzchar(keyword %||% "")) {
          stringr::str_remove_all(x, stringr::regex(keyword, ignore_case = TRUE))
        } else x
        ifelse(!is.na(mapped) & nzchar(mapped), mapped, fallback)
      }
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

      if (identical(normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      ), "spend")) {
        seed <- spend %>%
          dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
          dplyr::mutate(
            key = split_pair_key(.data$VariableSplit, spend_kw)
          )
        if (nrow(seed)) {
          activity_focus <- activity %>%
            dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
            dplyr::mutate(
              key = split_pair_key(.data$VariableSplit, act_kw)
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
                    key = split_pair_key(.data$VariableSplit, act_kw)
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
                key = split_pair_key(.data$VariableSplit, spend_kw)
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
          key = split_pair_key(.data$VariableSplit, act_kw)
        )
      if (nrow(seed)) {
        spend_focus <- spend %>%
          dplyr::filter(!grepl("_Before(\\s+|_)", .data$VariableSplit, ignore.case = TRUE)) %>%
          dplyr::mutate(
            key = split_pair_key(.data$VariableSplit, spend_kw)
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
                  key = split_pair_key(.data$VariableSplit, spend_kw)
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
              key = split_pair_key(.data$VariableSplit, act_kw)
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
  }, envir = ctx$scope)
  invisible(NULL)
}
