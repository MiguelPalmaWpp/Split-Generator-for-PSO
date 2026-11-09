# Prepare per-channel export snapshots from clean (pre-SAP) and final results.
# Derived metric/merge data is cached for preview and download reuse.
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

    role_splits_from_manifest <- function(res, role) {
      manifest <- res$split_manifest %||% tibble::tibble()
      rag <- res$rag
      if (is.null(rag) || !nrow(manifest) ||
          !all(c("Role", "VariableSplit") %in% names(manifest))) {
        return(tibble::tibble(VariableSplit = character()))
      }
      splits <- unique(as.character(manifest$VariableSplit[
        manifest$Role == role & !is.na(manifest$VariableSplit) &
          nzchar(manifest$VariableSplit)
      ]))
      tibble::tibble(VariableSplit = intersect(splits, names(rag)))
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
    # Lazily resolve one channel's merge lineage and canonical RAE totals.
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
    # Fill only missing derived fields; preserve the immutable input snapshot.
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

      # Use the same exact-key, modeled-role reconciliation as Process.
      # A failed validation requires review instead of silently passing.
      check <- tryCatch(
        build_canonical_total_check(
          analytical = d$analytical,
          all_rags = d$all_rags,
          result = res,
          cfg = cfg_ch,
          cross_cols = res$cross_cols %||% gcfg$cross_cols %||% "Geography",
          schema_metadata = d$schema_metadata,
          tolerance = 0.01
        ),
        error = function(e) {
          pso_public_error(
            e, paste0("export.total_check.", nm),
            "Total Check could not be completed. Review the channel in Process."
          )
          list(status = "error")
        }
      )
      tc_mismatch <- !identical(check$status %||% "error", "ok")

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
