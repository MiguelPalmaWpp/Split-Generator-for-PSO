register_export_split_payload <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    modeled_manifest_splits <- function(res) {
      manifest <- res$split_manifest %||% tibble::tibble()
      if (!nrow(manifest) ||
          !all(c("Role", "VariableSplit") %in% names(manifest))) {
        return(character(0))
      }
      unique(manifest$VariableSplit[
        manifest$Role == "modeled" &
          !is.na(manifest$VariableSplit) & nzchar(manifest$VariableSplit)
      ])
    }

    modeled_splits_from_manifest <- function(res, cfg = list(), nm = "") {
      manifest <- res$split_manifest %||% tibble::tibble()
      if (is.null(res$rag) || !nrow(manifest) ||
          !all(c("Role", "VariableSplit", "MetricRole") %in% names(manifest))) {
        return(tibble::tibble())
      }
      modeled <- manifest %>%
        dplyr::filter(.data$Role == "modeled") %>%
        dplyr::distinct(.data$VariableSplit, .data$MetricRole, .keep_all = TRUE)
      rag <- as.data.frame(res$rag)
      modeled <- modeled %>%
        dplyr::filter(.data$VariableSplit %in% names(rag))
      if (!nrow(modeled)) return(tibble::tibble())

      modeled$total_activity <- vapply(seq_len(nrow(modeled)), function(i) {
        if (!identical(modeled$MetricRole[[i]], "activity")) return(NA_real_)
        sum(suppressWarnings(as.numeric(rag[[modeled$VariableSplit[[i]]]])), na.rm = TRUE)
      }, numeric(1))
      modeled %>%
        dplyr::transmute(
          VariableSplit = .data$VariableSplit,
          MainModelVariableName = cfg$model_variable %||% cfg$channel_name %||% nm,
          total_activity = .data$total_activity
        )
    }

    final_activity_splits <- function(res, cfg = list(), nm = "") {
      if (is.null(res)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric()
        ))
      }

      manifest_final <- modeled_splits_from_manifest(res, cfg, nm)
      if (nrow(manifest_final)) return(manifest_final)

      if (identical(normalize_model_metric(
        res$modeled_role %||% cfg$modeled_role %||%
          res$model_metric %||% cfg$model_metric %||% "activity"
      ), "spend")) {
        spend_final <- final_spend_splits(res, cfg, nm)
        modeled_splits <- modeled_manifest_splits(res)
        if (length(modeled_splits)) {
          spend_final <- spend_final %>%
            dplyr::filter(.data$VariableSplit %in% modeled_splits)
        }
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
      modeled_splits <- modeled_manifest_splits(res)
      if (length(modeled_splits)) {
        final <- final %>%
          dplyr::filter(.data$VariableSplit %in% modeled_splits)
      }
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
      manifest_pre <- modeled_splits_from_manifest(clean, cfg, nm)
      if (nrow(manifest_pre)) return(manifest_pre)
      if (identical(normalize_model_metric(
        clean$modeled_role %||% cfg$modeled_role %||%
          clean$model_metric %||% cfg$model_metric %||% "activity"
      ), "spend")) {
        spend_pre <- pre_merge_spend_splits(clean, cfg, nm)
        modeled_splits <- modeled_manifest_splits(clean)
        if (length(modeled_splits)) {
          spend_pre <- spend_pre %>%
            dplyr::filter(.data$VariableSplit %in% modeled_splits)
        }
        return(spend_pre %>%
                 dplyr::mutate(total_activity = NA_real_) %>%
                 dplyr::select(dplyr::all_of(c("VariableSplit", "MainModelVariableName",
                                                "total_activity")),
                               dplyr::everything()))
      }
      pre <- activity_splits_from_rag(clean, cfg, nm)
      modeled_splits <- modeled_manifest_splits(clean)
      if (length(modeled_splits)) {
        pre <- pre %>% dplyr::filter(.data$VariableSplit %in% modeled_splits)
      }
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

    # Build or reuse one channel's derived export data from the immutable
    # snapshot. This keeps package preview and download calculations aligned.
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
      cfg_effective$model_metric <- item$res$modeled_role %||%
        cfg_effective$modeled_role %||% item$res$model_metric %||%
        cfg_effective$model_metric %||% "activity"

      item$rae_totals <- if (isTRUE(has_processed_splits)) {
        export_metric_totals_from_rae(
          d, cfg_effective, gcfg %||% list(), item$name %||% "", item$res
        )
      } else {
        empty_export_metric_totals()
      }
      item$canonical_totals <- build_canonical_export_totals(
        item$rae_totals,
        item$merge_resolved %||% list(
          map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character()),
          issues = character(0)
        ),
        model_metric = item$res$modeled_role %||% item$cfg$modeled_role %||%
          item$res$model_metric %||% item$cfg$model_metric %||% "activity"
      )
      item
    }

    # Prepare all required channel payloads once for the current export snapshot.
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
          model_metric <- normalize_model_metric(
            res$modeled_role %||% cfg_ch$modeled_role %||%
              res$model_metric %||% cfg_ch$model_metric %||% "activity"
          )
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
      build_export_file_dimensions(
        export_data = export_data,
        details = d$details,
        analytical = d$analytical,
        side_mapping_nonfocus = d$side_mapping_nonfocus,
        channels = ch_list,
        roi_data = clean_roi_columns(d$channels_rois)
      )
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
