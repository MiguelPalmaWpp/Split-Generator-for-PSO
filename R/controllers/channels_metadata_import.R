# Preview and apply Splits Metadata imports within current channel authority.
# Inactive VOF variables and stale SAP references are not silently restored.
register_channels_metadata_import <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Reconcile parsed rows with current Media Index channels while preserving
    # valid manual channels and imported legacy configuration where possible.
    apply_config_import <- function(parsed_config) {
      req(parsed_config)
      profile_step("config_import_apply", {
      tryCatch({
        df  <- config_df_from_pending(parsed_config)
        
        if (is.null(df) || !all(c("Channel", "Type") %in% names(df))) {
          showNotification("Splits Metadata file missing Channel or Type columns.",
                           type = "error"); return()
        }

        # Reconcile against the current index before restoring imported rows,
        # so stale files cannot reintroduce inactive generated channels.
        current_mi <- tryCatch(media_index(), error = function(e) NULL)
        if (!is.null(current_mi) && length(current_mi$channels %||% list()))
          sync_channels_to_media_index(current_mi, notify = TRUE)
        
        config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
        merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
        break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
        rename_rows <- df[trimws(df$Type) == "Rename", , drop = FALSE]
        
        is_new_format  <- all(c("Name", "Splits") %in% names(df))
        has_date_cols  <- all(c("MinPeriod", "MaxPeriod") %in% names(df))
        n_restored <- n_imported <- n_built <- n_breaks <- n_merges <- n_skipped <- 0L
        n_stale <- 0L
        stale_merge_refs <- list()
        n_inactive <- 0L
        an         <- data()$analytical
        affected_channels <- character(0)
        update_label_mismatch <- character(0)
        if ("UpdateLabel" %in% names(config_rows)) {
          imported_labels <- unique(trimws(as.character(config_rows$UpdateLabel %||% "")))
          imported_labels <- imported_labels[!is.na(imported_labels) & nzchar(imported_labels)]
          current_label <- trimws(as.character((config()$update_label %||% "")[[1]]))
          update_label_mismatch <- setdiff(imported_labels, current_label)
        } else if (nrow(merge_rows) > 0) {
          txt <- paste(c(merge_rows$Name %||% "", merge_rows$Splits %||% ""), collapse = " ")
          matches <- gregexpr("_Before\\s+[^_|]+",
                              txt, ignore.case = TRUE, perl = TRUE)[[1]]
          if (!identical(matches[1], -1L)) {
            inferred_labels <- regmatches(txt, list(matches))[[1]]
            inferred_labels <- sub("^_Before\\s+", "", inferred_labels, ignore.case = TRUE)
            inferred_labels <- unique(inferred_labels[nzchar(inferred_labels)])
            current_label <- trimws(as.character((config()$update_label %||% "")[[1]]))
            update_label_mismatch <- setdiff(inferred_labels, current_label)
          }
        }
        
        recover_dates <- function(nm, row_idx) {
          saved_min <- as.Date(NA); saved_max <- as.Date(NA)
          period_token <- stringr::str_match(
            trimws(as.character(nm %||% "")),
            stringr::regex("\\s+--p\\s+(-?\\d{8}-?)", ignore_case = TRUE)
          )[, 2]
          if (is.na(period_token)) period_token <- ""
          if (has_date_cols) {
            min_str <- trimws(config_rows$MinPeriod[row_idx] %||% "")
            max_str <- trimws(config_rows$MaxPeriod[row_idx] %||% "")
            saved_min <- tryCatch(
              if (nzchar(min_str)) parse_vof_period_context(min_str, period_token, "min") else as.Date(NA),
              error = \(e) as.Date(NA))
            saved_max <- tryCatch(
              if (nzchar(max_str)) parse_vof_period_context(max_str, period_token, "max") else as.Date(NA),
              error = \(e) as.Date(NA))
          }
          if (is.na(saved_min) || is.na(saved_max)) {
            vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
            if (!is.null(vof_d) &&
                all(c("MainModelVariableName", "MinPeriod", "MaxPeriod") %in% names(vof_d))) {
              vof_ch <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
              if (nrow(vof_ch) > 0) {
                raw_mins <- vof_ch$MinPeriod[!is.na(vof_ch$MinPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MinPeriod)))]
                raw_maxs <- vof_ch$MaxPeriod[!is.na(vof_ch$MaxPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MaxPeriod)))]
                vof_mins <- if (length(raw_mins)) parse_vof_period_context(raw_mins, period_token, "min") else as.Date(NA)
                vof_maxs <- if (length(raw_maxs)) parse_vof_period_context(raw_maxs, period_token, "max") else as.Date(NA)
                vof_mins <- vof_mins[!is.na(vof_mins)]; vof_maxs <- vof_maxs[!is.na(vof_maxs)]
                if (length(vof_mins)) saved_min <- min(vof_mins)
                if (length(vof_maxs)) saved_max <- max(vof_maxs)
              }
            }
          }
          list(min_p = if (!is.na(saved_min)) saved_min else
            if (!is.null(an)) min(an$Period, na.rm = TRUE) else NULL,
            max_p = if (!is.na(saved_max)) saved_max else
              if (!is.null(an)) max(an$Period, na.rm = TRUE) else NULL)
        }

        build_channel_from_config_row <- function(nm, row_idx, splits) {
          dates <- recover_dates(nm, row_idx)
          row <- config_rows[row_idx, , drop = FALSE]
          if (nm %in% names(rv$available_channels)) {
            base_cfg <- rv$available_channels[[nm]]
            cfg <- base_cfg
            cfg$split_columns    <- splits
            cfg$dimension_breaks <- list()
            cfg$dimension_aliases <- list()
            cfg$saved_merges     <- list()
            cfg$min_period       <- dates$min_p
            cfg$max_period       <- dates$max_p
            cfg$config_imported  <- TRUE
            cfg$model_metric     <- cfg$model_metric %||% "activity"
            cfg <- apply_config_keywords(cfg, row)
            return(reconcile_channel_metric_keywords(cfg, base_cfg))
          }

          varname_include <- get_varname_include_fallback(nm)
          act_kw <- detect_activity_keyword(trimws(stringr::str_remove(
            stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$")))
          spend_kw <- detect_spend_keyword(data()$all_rags, varname_include)
          actual_mv <- if (!is.null(an)) {
            an_num <- names(an)[sapply(an, is.numeric)]
            saved_mv <- if ("ModelVariable" %in% names(row))
              trimws(as.character(row$ModelVariable[[1]] %||% "")) else ""
            if (nzchar(saved_mv) && saved_mv %in% an_num) saved_mv
            else if (nm %in% an_num) nm
            else {
              cand <- paste0(nm, "_Total_Total_Total")
              if (cand %in% an_num) cand
              else { pm <- an_num[startsWith(an_num, nm)]; if (length(pm)) pm[1] else nm }
            }
          } else nm

          roi_info <- lookup_roi(actual_mv, nm)
          cfg <- list(
            channel_name = nm, model_variable = actual_mv,
            varname_include = varname_include, analytical_varkeys = actual_mv,
            min_period = dates$min_p, max_period = dates$max_p,
            segment_overrides = list(), activity_keyword = act_kw,
            model_metric = "activity",
            spend_keyword = spend_kw, split_columns = splits,
            saved_merges = list(), dimension_breaks = list(), dimension_aliases = list(),
            roi = roi_info$value, source = "manual",
            geo_label = "", vof_geo_group = "", vof_geo_order = NA_integer_,
            vof_geo_group_size = 0L, vof_geo_signature = "all",
            media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
            sub_channel = "", effect = "", config_imported = TRUE)
          cfg <- apply_config_keywords(cfg, row)
          reconcile_channel_metric_keywords(cfg)
        }

        for (i in seq_len(nrow(config_rows))) {
          nm     <- config_rows$Channel[i]
          if (!nzchar(trimws(nm %||% ""))) {
            n_skipped <- n_skipped + 1L
            next
          }
          if (length(inactive_vof_channel_names(nm))) {
            if (nm %in% names(rv$channels)) {
              rv$channels[[nm]] <- NULL
              dirty_channels[[nm]] <- NULL
            }
            n_inactive <- n_inactive + 1L
            next
          }
          splits <- Filter(nzchar, trimws(strsplit(config_rows$SplitOrder[i], "\\|")[[1]]))
          if (!length(splits)) splits <- c("VariableName")
          
          if (nm %in% names(rv$channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_restored <- n_restored + 1L
          } else if (nm %in% names(rv$available_channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_imported <- n_imported + 1L
          } else {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_built <- n_built + 1L
          }
          if (!identical(rv$channels[[nm]]$source %||% "", "vof") &&
              !nzchar(rv$channels[[nm]]$time_break_label %||% "")) {
            inferred_tbr <- infer_time_break_from_config_merges(nm, merge_rows)
            if (nzchar(inferred_tbr))
              rv$channels[[nm]]$time_break_label <- inferred_tbr
          }
          if (nzchar(rv$channels[[nm]]$time_break_label %||% "")) {
            rv$channels[[nm]]$geo_label <- ""
          }
          affected_channels <- c(affected_channels, nm)
        }
        
        if (nrow(break_rows) > 0) {
          for (i in seq_len(nrow(break_rows))) {
            nm  <- break_rows$Channel[i]
            col <- trimws(break_rows$SplitOrder[i] %||% "")
            if (!nm %in% names(rv$channels) || !nzchar(col)) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (is_new_format) {
              part_names_raw <- break_rows$Name[i]  %||% ""
              sep_n_raw      <- break_rows$Splits[i] %||% ""
              part_names <- Filter(nzchar, trimws(strsplit(part_names_raw, "\\|")[[1]]))
              sep_n      <- strsplit(sep_n_raw, "\\|")[[1]]
              sep     <- if (length(sep_n) >= 1 && nzchar(sep_n[1])) sep_n[1] else default_break_separator
              n_parts <- if (length(sep_n) >= 2) as.integer(trimws(sep_n[2])) else length(part_names)
            } else {
              break_info <- strsplit(
                break_rows$BreakInfo[i] %||% "", "\\|")[[1]]
              sep     <- if (length(break_info) >= 1 && nzchar(break_info[1])) break_info[1] else default_break_separator
              n_parts <- if (length(break_info) >= 2) as.integer(trimws(break_info[2])) else 2L
              part_names <- if (length(break_info) > 2) {
                Filter(nzchar, trimws(break_info[3:length(break_info)]))
              }
              else paste0(col, "_", LETTERS[seq_len(n_parts)])
            }
            if (is.na(n_parts) || n_parts < 1L) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (length(part_names) != n_parts || any(!nzchar(part_names))) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing <- rv$channels[[nm]]$dimension_breaks %||% list()
            if (any(sapply(existing, \(b) b$column == col))) {
              n_skipped <- n_skipped + 1L
              next
            }
            break_missing_raw <- if ("BreakMissingPartValue" %in% names(break_rows)) {
              trimws(as.character(break_rows$BreakMissingPartValue[i] %||% ""))
            } else {
              ""
            }
            if (is.na(break_missing_raw) ||
                !identical(tolower(break_missing_raw), "unknown")) {
              rv$channels[[nm]]$legacy_break_missing_part_migrated <- TRUE
            }
            rv$channels[[nm]]$break_missing_part_value <-
              canonical_break_missing_part_value()
            rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
              column = col, separator = sep, n_parts = n_parts,
              names = part_names,
              missing_part_value = canonical_break_missing_part_value())))
            n_breaks <- n_breaks + 1L
          }
        }

        n_renames <- 0L
        if (nrow(rename_rows) > 0) {
          for (i in seq_len(nrow(rename_rows))) {
            nm <- rename_rows$Channel[i]
            source <- if ("RenameSource" %in% names(rename_rows)) {
              trimws(rename_rows$RenameSource[i] %||% "")
            } else {
              ""
            }
            alias <- if ("RenameAlias" %in% names(rename_rows)) {
              trimws(rename_rows$RenameAlias[i] %||% "")
            } else {
              ""
            }
            if (!nzchar(source)) source <- trimws(rename_rows$SplitOrder[i] %||% "")
            if (!nzchar(alias)) alias <- trimws(rename_rows$Name[i] %||% "")
            if (!nm %in% names(rv$channels) || !nzchar(source) ||
                !nzchar(alias) || identical(source, alias)) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing <- rv$channels[[nm]]$dimension_aliases %||% list()
            existing_sources <- vapply(existing, \(a) a$source %||% "", character(1))
            existing_aliases <- vapply(existing, \(a) a$alias %||% "", character(1))
            if (source %in% existing_sources || alias %in% existing_aliases) {
              n_skipped <- n_skipped + 1L
              next
            }
            md <- main_data()
            if (!is.null(md) && alias %in% names(md) && !identical(alias, source)) {
              n_skipped <- n_skipped + 1L
              next
            }
            splits <- rv$channels[[nm]]$split_columns %||% character(0)
            splits[splits == source] <- alias
            rv$channels[[nm]]$split_columns <- unique(splits[nzchar(splits)])
            rv$channels[[nm]]$dimension_aliases <- c(existing, list(list(
              source = source,
              alias = alias
            )))
            n_renames <- n_renames + 1L
          }
        }
        
        if (nrow(merge_rows) > 0) {
          for (i in seq_len(nrow(merge_rows))) {
            nm <- merge_rows$Channel[i]
            if (!nm %in% names(rv$channels)) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (is_new_format) {
              merge_name <- trimws(merge_rows$Name[i]  %||% "")
              merged_raw <- trimws(merge_rows$Splits[i] %||% "")
            } else {
              merge_name <- trimws(merge_rows$SplitOrder[i] %||% "")
              merged_raw <- trimws(merge_rows$BreakInfo[i]  %||% "")
            }
            merged <- parse_merge_splits_config(merged_raw)
            if (!nzchar(merge_name) || !length(merged)) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing       <- rv$channels[[nm]]$saved_merges %||% list()
            existing_names <- vapply(existing, \(m) m$new_name %||% "", character(1))
            if (merge_name %in% existing_names) {
              n_skipped <- n_skipped + 1L
              next
            }
            max_id   <- if (length(existing))
              max(vapply(existing, \(m) m$id %||% 0L, integer(1))) else 0L
            cfg_ch   <- rv$channels[[nm]]
            stale_labels <- stale_time_break_merge(cfg_ch, merge_name, merged_raw)
            is_stale <- length(stale_labels) > 0L
            if (is_stale) {
              n_stale <- n_stale + 1L
              stale_merge_refs[[length(stale_merge_refs) + 1L]] <- list(
                channel = nm,
                merge_name = merge_name,
                labels = stale_labels
              )
            }
            act_kw   <- cfg_ch$activity_keyword %||% "Impressions"
            spend_kw <- cfg_ch$spend_keyword    %||% "Spend"
            merge_metric <- normalize_model_metric(
              if ("ModelMetric" %in% names(merge_rows))
                merge_rows$ModelMetric[i] %||% cfg_ch$model_metric %||% "activity"
              else cfg_ch$model_metric %||% "activity"
            )
            new_spend <- stringr::str_replace_all(
              merge_name, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
            if (new_spend == merge_name) new_spend <- paste0(merge_name, "_", spend_kw)
            merge_view <- infer_merge_view(merge_name, merged_raw)
            rv$channels[[nm]]$saved_merges <- c(existing, list(list(
              id = max_id + 1L, new_name = merge_name, merged = as.list(merged),
              view = merge_view, spend_merged = list(), new_spend_name = new_spend,
              metric = merge_metric,
              active = !is_stale, stale = is_stale,
              stale_labels = stale_labels,
              saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
            if (!is_stale) n_merges <- n_merges + 1L
          }
        }
        
        if (is.null(rv$selected) && length(rv$channels) > 0)
          rv$selected <- names(rv$channels)[1]
        clear_manager_channel_cache()
        clear_preview_caches(cards = TRUE)
        
        parts <- c(
          if (n_restored > 0) paste0(n_restored, " channel(s) updated"),
          if (n_imported > 0) paste0(n_imported, " imported from VOF"),
          if (n_built    > 0) paste0(n_built,    " rebuilt from MFF"),
          if (n_breaks   > 0) paste0(n_breaks,   " break(s) loaded"),
          if (n_renames  > 0) paste0(n_renames,  " rename(s) loaded"),
          if (n_merges   > 0) paste0(n_merges,   " merge(s) loaded"),
          if (n_stale    > 0) paste0(n_stale,    " stale merge reference(s) held for review"),
          if (n_inactive > 0) paste0(n_inactive, " inactive VOF channel(s) ignored"),
          if (n_skipped  > 0) paste0(n_skipped,  " skipped"))
        showNotification(paste(parts, collapse = " \u2014 "),
                         type = "message", duration = 5)
        if (length(update_label_mismatch)) {
          showNotification(
            paste0("This Splits Metadata was created with ",
                   paste(update_label_mismatch, collapse = ", "),
                   "; current setup is ",
                   if (nzchar(config()$update_label %||% "")) config()$update_label else "(blank)",
                   ". Merges may not reproduce."),
            type = "warning",
            duration = 12
          )
        }
        affected_channels <- unique(affected_channels[nzchar(affected_channels)])
        if (length(affected_channels)) {
          next_id <- isolate(config_import_event_id()) + 1L
          config_import_event_id(next_id)
          config_import_event(list(id = next_id, channels = affected_channels))
        }
      }, error = \(e)
      showNotification(pso_public_error(
        e, "channels.metadata", "Channel metadata could not be applied. Review the imported file."
      ), type = "error", duration = 8))
      })
    }

    # Count overwrites, additions, skips, and stale references before apply.
    preview_config_import <- function(parsed_config) {
      df <- config_df_from_pending(parsed_config)
      if (is.null(df) || !all(c("Channel", "Type") %in% names(df))) {
        stop("Splits Metadata file missing Channel or Type columns.")
      }
        config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
        merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
        break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
        rename_rows <- df[trimws(df$Type) == "Rename", , drop = FALSE]
        current_label <- trimws(as.character((config()$update_label %||% "")[[1]]))
        update_label_mismatch <- character(0)
        if ("UpdateLabel" %in% names(config_rows)) {
          imported_labels <- unique(trimws(as.character(config_rows$UpdateLabel %||% "")))
          imported_labels <- imported_labels[!is.na(imported_labels) & nzchar(imported_labels)]
          update_label_mismatch <- setdiff(imported_labels, current_label)
        } else if (nrow(merge_rows) > 0) {
          txt <- paste(c(merge_rows$Name %||% "", merge_rows$Splits %||% ""), collapse = " ")
          matches <- gregexpr("_Before\\s+[^_|]+", txt, ignore.case = TRUE, perl = TRUE)[[1]]
          if (!identical(matches[1], -1L)) {
            inferred_labels <- regmatches(txt, list(matches))[[1]]
            inferred_labels <- sub("^_Before\\s+", "", inferred_labels, ignore.case = TRUE)
            inferred_labels <- unique(inferred_labels[nzchar(inferred_labels)])
            update_label_mismatch <- setdiff(inferred_labels, current_label)
          }
        }
      ch_names <- config_rows$Channel
      ch_names <- ch_names[!is.na(ch_names) & nzchar(trimws(ch_names))]
      inactive_names <- inactive_vof_channel_names(ch_names)
      list(
        updated  = sum(ch_names %in% names(rv$channels)),
        imported = sum(!ch_names %in% names(rv$channels) &
                         ch_names %in% names(rv$available_channels)),
        rebuilt  = sum(!ch_names %in% names(rv$channels) &
                         !ch_names %in% names(rv$available_channels) &
                         !ch_names %in% inactive_names),
        breaks   = nrow(break_rows),
        renames  = nrow(rename_rows),
        merges   = nrow(merge_rows),
        skipped  = sum(is.na(config_rows$Channel) |
                         !nzchar(trimws(config_rows$Channel %||% ""))) +
          length(inactive_names),
        inactive = length(inactive_names),
        total    = length(ch_names),
        update_label_mismatch = update_label_mismatch
      )
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
