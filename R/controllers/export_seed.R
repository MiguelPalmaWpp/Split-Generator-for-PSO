# Build Seed For Indices from processed split totals and optional ROI matches.
# Keep its columns stable even when a channel has no matching ROI value.
register_export_seed <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Combine modeled activity/spend rows with channel-level ROI metadata for
    # the file written by Export, using a prepared snapshot when available.
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
        cfg$model_metric <- r$modeled_role %||% cfg$modeled_role %||%
          r$model_metric %||% cfg$model_metric %||% "activity"
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
          export_metric_totals_from_rae(d, cfg, gcfg, nm, r)$seed
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
 # STANDARD path - improved local/national detection (#5)
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
          roi_available_long_cols <- setdiff(roi_rae_key_columns(roi_df, d$all_rags), "Geography")
          roi_long_cols <- intersect(roi_available_long_cols, names(act_df))
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
          empty_roi <- c(
            setNames(as.list(rep(NA_real_, length(roi_num_cols))), roi_num_cols),
            list(.ROIChannel = NA_character_)
          )

          roi_norm <- roi_df %>%
            dplyr::mutate(
              mv_norm = normalize_roi_mv(MainModelVariableName),
              geo_val = if ("Geography" %in% names(roi_df))
                normalize_roi_text(Geography) else "",
              sv_val = if (src_col %in% names(roi_df))
                normalize_roi_text(.data[[src_col]]) else ""
            )
          for (key_col in roi_available_long_cols) {
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
              omitted_specific <- setdiff(
                intersect(c("sv_val", "geo_val", paste0(".key_", roi_available_long_cols)), names(cands)),
                keys
              )
              if (length(omitted_specific)) {
                has_more_specific_roi <- vapply(omitted_specific, function(k) {
                  any(nzchar(cands[[k]] %||% ""), na.rm = TRUE)
                }, logical(1))
                if (any(has_more_specific_roi)) next
              }
              hit <- match_one_level(keys)
              if (!nrow(hit)) next
              hit <- dplyr::distinct(hit, dplyr::across(dplyr::all_of(roi_num_cols)), .keep_all = TRUE)
              if (nrow(hit) == 1L) {
                matched <- as.list(hit[1, roi_num_cols, drop = FALSE])
                matched$.ROIChannel <- if ("Channel" %in% names(hit)) {
                  trimws(as.character(hit$Channel[[1]] %||% NA_character_))
                } else {
                  NA_character_
                }
                return(matched)
              }
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
          if (".ROIChannel" %in% names(act_df)) {
            roi_channel <- trimws(as.character(act_df$.ROIChannel))
            act_df$Channel <- ifelse(!is.na(roi_channel) & nzchar(roi_channel),
                                     roi_channel, act_df$Channel)
            act_df <- dplyr::select(act_df, -dplyr::any_of(".ROIChannel"))
          }

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
          showNotification(pso_public_error(
            e, "export.roi_join", "ROI values could not be joined. Review the ROI file."
          ), type = "warning", duration = 15))
      }
      format_seed_for_indices(act_df, seed_roi_cols)
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
