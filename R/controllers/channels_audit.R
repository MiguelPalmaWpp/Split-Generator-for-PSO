# Build the selected channel's audit from effective VOF/Details connection,
# filters, dates, roles, and ROI state. This is a diagnostic view, not an
# independent filter source for Process.
register_channels_audit <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    mk_info_row <- function(label, value) {
      div(class = "info-row",
          tags$span(label, class = "info-row-label"),
          div(class = "info-row-value", value))
    }

    fmt_audit_count <- function(x) {
      x <- suppressWarnings(as.numeric(x %||% 0))
      if (is.na(x)) x <- 0
      format(round(x), big.mark = ",", scientific = FALSE)
    }

    source_label <- function(cfg) {
      if (isTRUE(cfg$config_imported)) return("Imported from Config")
      switch(cfg$source %||% "vof",
             vof = "Auto-configured from VOF",
             keyword_fallback = "From MFF (keyword match)",
             "From MFF / Manual")
    }

    summarize_useful_longitudinals <- function(cfg, max_values = 8L) {
      sm <- tryCatch(data()$schema_metadata, error = \(e) NULL)
      useful_long <- sm$useful_long %||% character(0)
      name_lookup <- sm$name_lookup
      analytical_keys <- cfg$analytical_varkeys %||% character(0)

      if (is.null(name_lookup) || !length(useful_long) || !length(analytical_keys)) {
        return(list(rows = list(), count = 0L))
      }

      rows <- lapply(useful_long, function(dim) {
        vals <- tryCatch(
          get_useful_long_values(analytical_keys, name_lookup, dim),
          error = \(e) character(0)
        )
        vals <- unique(trimws(as.character(vals)))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (!length(vals)) return(NULL)

        shown <- head(vals, max_values)
        more <- length(vals) - length(shown)
        div(class = "audit-longitudinal-item",
            tags$strong(dim),
            tags$span(": "),
            tags$span(paste(shown, collapse = ", ")),
            if (more > 0) tags$span(paste0(" +", more, " more"), class = "audit-muted"))
      })
      rows <- Filter(Negate(is.null), rows)

      list(rows = rows, count = length(rows))
    }

    # Build the compact audit model from file matches, effective filters,
    # metric roles, ROI, TimeBreak, GeoLabel, and saved SAP state.
    # Collect the fields shown in the compact audit and expanded detail.
    selected_channel_audit <- function(nm, cfg) {
      d <- data()
      md <- d$all_rags
      an <- d$analytical
      source <- selected_channel_source(nm, cfg, include_spend = TRUE)
      cache_key <- pso_cache_key(
        "channels-audit",
        nm %||% "",
        channel_config_signature(cfg),
        source$key,
        if (!is.null(an)) paste(names(an), collapse = "|") else "",
        rois_signature()
      )
      cached_audit <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached_audit)) return(cached_audit)

      notes <- character(0)
      warnings <- character(0)
      blockers <- character(0)
      activity_rows <- 0L
      spend_rows <- 0L
      date_mode <- "unavailable"
      date_message <- ""
      date_rows <- 0L

      if (identical(cfg$source %||% "", "vof") &&
          (cfg$vof_rows_discarded %||% 0L) > 0L) {
        notes <- c(notes, paste0(
          "Excluded ", cfg$vof_rows_discarded,
          " VOF row(s) because ModelDetails marks them as not modelled (NONE)."
        ))
      }
      legacy_tbr <- trimws(as.character(cfg$legacy_time_break_label %||% ""))
      canonical_tbr <- trimws(as.character(cfg$time_break_label %||% ""))
      if (identical(cfg$source %||% "", "vof") &&
          nzchar(legacy_tbr) && !identical(tolower(legacy_tbr), tolower(canonical_tbr))) {
        notes <- c(notes, paste0(
          "Legacy TimeBreak label corrected from ", legacy_tbr,
          " to ", if (nzchar(canonical_tbr)) canonical_tbr else "None", "."
        ))
      }

      if (is.null(md)) {
        blockers <- c(blockers, "RAE Datafile is not loaded.")
      } else if (!"VariableName" %in% names(md)) {
        blockers <- c(blockers, "RAE Datafile has no VariableName column.")
      } else {
        scoped <- source$data
        date_mode <- source$mode
        date_message <- source$message %||% ""
        date_rows <- nrow(scoped)
        if (nzchar(date_message) && identical(date_mode, "channel_range_fallback")) {
          notes <- c(notes, date_message)
        }

        vn <- trimws(as.character(scoped$VariableName %||% character(0)))
        act_kw <- cfg$activity_keyword %||% ""
        spend_kw <- cfg$spend_keyword %||% ""
        if (nzchar(act_kw)) {
          activity_rows <- sum(grepl(act_kw, vn, ignore.case = TRUE), na.rm = TRUE)
        }
        if (nzchar(spend_kw)) {
          spend_rows <- sum(grepl(spend_kw, vn, ignore.case = TRUE), na.rm = TRUE)
        }

        role_vars <- resolve_role_rae_variables(unique(vn), cfg)
        modeled_rows <- sum(tolower(vn) %in% tolower(role_vars$modeled), na.rm = TRUE)
        for_indices_rows <- sum(tolower(vn) %in% tolower(role_vars$for_indices), na.rm = TRUE)
        if (modeled_rows == 0L) {
          blockers <- c(blockers, "No RAE rows match the modeled variable for this channel and date range.")
        }
        if ((cfg$role_pair_status %||% "") %in% c("Matched", "Partial") &&
            modeled_rows > 0L && for_indices_rows == 0L) {
          warnings <- c(warnings, "ForIndices is connected but has no RAE rows in the effective scope.")
        }
        if (identical(cfg$role_pair_status %||% "", "Partial")) {
          warnings <- c(warnings, "ForIndices has partial RAE coverage; some modeled source variables have no compatible counterpart.")
        }
        if (identical(cfg$role_pair_status %||% "", "Ambiguous")) {
          warnings <- c(warnings, "ForIndices has multiple candidates and will not be inferred automatically.")
        }

        if (identical(tolower(spend_kw), "spend")) {
          indexed_rae <- d$all_rags_indexed
          available_variable_names <- if (inherits(indexed_rae, "pso_rae_index") &&
                                          !is.null(indexed_rae$variables)) {
            indexed_rae$variables$VariableName
          } else {
            unique(trimws(as.character(md$VariableName)))
          }
          vi_expanded_cost <- expand_varname_include_with_spend(
            available_variable_names,
            cfg$varname_include %||% character(0),
            "Cost"
          )
          cost_candidate <- any(grepl("Cost", vi_expanded_cost, ignore.case = TRUE))
          if (isTRUE(cost_candidate)) {
            warnings <- c(warnings, "RAE appears to contain Cost for this family, but Spend keyword is set to Spend.")
          }
        }
      }

      model_var <- cfg$model_variable %||% ""
      model_ok <- !is.null(an) && nzchar(model_var) && model_var %in% names(an)
      if (is.null(an)) {
        blockers <- c(blockers, "Analytical Dataset is not loaded.")
      } else if (!model_ok) {
        blockers <- c(blockers, paste0("Model variable not found in Analytical: ", model_var))
      }

      roi_val <- effective_roi_cached(nm, cfg)
      if (is.na(roi_val)) {
        warnings <- c(warnings, "ROI is not loaded. Processing can continue; Seed For Indices.csv will be exported without ROI values.")
      }

      gcfg <- tryCatch(config(), error = \(e) list())
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        blockers <- c(blockers, "Global Parameters are not configured.")
      }

      n_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1)))
      n_stale_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m)
        isTRUE(m$stale), logical(1)))
      if (n_merges > 0) {
        notes <- c(notes, paste0(n_merges, " saved merge(s) will be applied during Process."))
      }
      if (n_stale_merges > 0) {
        warnings <- c(warnings, paste0(
          n_stale_merges,
          " stale merge reference(s) are held for review and will not be applied."
        ))
      }

      roi_missing <- is.na(roi_val)
      status <- if (length(blockers)) "Blocked" else if (length(warnings) || length(notes)) "Needs review" else "Ready"
      status_class <- switch(status, Ready = "audit-ready", `Needs review` = "audit-review", Blocked = "audit-blocked")
      status_reason <- if (length(blockers)) {
        blockers[[1]]
      } else if (length(warnings)) {
        warn <- warnings[[1]]
        if (grepl("^ROI is (missing|not loaded)", warn)) {
          "ROI data is not loaded for this channel."
        } else if (grepl("^No Cost/Spend rows", warn)) {
          "Spend/Cost rows were not found with the current keyword."
        } else if (grepl("Cost for this family", warn)) {
          "Spend keyword may need review because Cost exists for this family."
        } else {
          warn
        }
      } else if (n_merges > 0) {
        paste0(n_merges, " saved merge(s) will be applied during Process.")
      } else if (length(notes)) {
        notes[[1]]
      } else {
        "Channel audit passed."
      }
      out <- list(
        status = status,
        status_reason = status_reason,
        status_class = status_class,
        activity_rows = activity_rows,
        spend_rows = spend_rows,
        date_rows = date_rows,
        date_mode = date_mode,
        date_message = date_message,
        model_ok = model_ok,
        roi = roi_val,
        roi_missing = roi_missing,
        n_merges = n_merges,
        n_breaks = length(cfg$dimension_breaks %||% list()),
        warnings = unique(warnings),
        blockers = unique(blockers),
        notes = unique(notes)
      )
      pso_cache_set(performance_cache, cache_key, out)
      out
    }

    audit_pill <- function(label, value, tone = c("neutral", "ok", "warn", "error", "info"),
                           detail = NULL) {
      tone <- match.arg(tone)
      div(class = paste("channel-audit-metric channel-audit-row", paste0("audit-row-", tone)),
          tags$span(label, class = "channel-audit-label"),
          tags$strong(value, class = "channel-audit-value"),
          if (!is.null(detail) && nzchar(detail))
            tags$span(detail, class = "channel-audit-reason"))
    }

    render_channel_audit <- function(nm, cfg, audit = NULL) {
      audit <- audit %||% selected_channel_audit(nm, cfg)
      roi_text <- if (is.na(audit$roi)) "Missing" else paste0(round(audit$roi, 1))
      time_text <- if (nzchar(cfg$time_break_label %||% "")) cfg$time_break_label else "None"
      geo_label_raw <- normalize_geo_label(cfg$geo_label %||% "")
      geo_text <- if (nzchar(geo_label_raw)) {
        paste("GeoLabel", sub("^GeoLabel", "", geo_label_raw))
      } else {
        "None"
      }
      spend_label <- cfg$spend_keyword %||% "Spend"
      status_tone <- switch(audit$status, Ready = "ok", `Needs review` = "warn", Blocked = "error")
      source_text <- source_label(cfg)
      connection_text <- cfg$connection_status %||% "Not audited"
      connection_tone <- switch(connection_text,
                                Matched = "ok",
                                Partial = "warn",
                                `Missing RAE` = "error",
                                `Missing Analytical` = "error",
                                Ambiguous = "warn",
                                "neutral")
      metric_text <- if (identical(normalize_model_metric(cfg$model_metric %||% "activity"), "spend"))
        "Spend/Cost" else "Activity"
      status_items <- unique(c(
        audit$blockers,
        audit$warnings,
        audit$notes
      ))
      if (!length(status_items)) {
        status_items <- "Channel is ready: data, model variable, date range and ROI checks passed."
      }
      messages_notes <- setdiff(audit$notes, status_items)
        div(class = "channel-audit-enterprise",
          tagList(
        div(class = "channel-audit-summary channel-audit-metrics",
            audit_pill("Status", audit$status, status_tone),
            audit_pill("Source", source_text, if (isTRUE(cfg$config_imported)) "info" else "neutral"),
            audit_pill("Connection", connection_text, connection_tone),
            audit_pill("Model metric", metric_text, if (identical(metric_text, "Spend/Cost")) "info" else "neutral"),
            audit_pill("Activity rows", fmt_audit_count(audit$activity_rows),
                       if (audit$activity_rows > 0) "ok" else "error"),
            audit_pill(paste0(spend_label, " rows"), fmt_audit_count(audit$spend_rows),
                       if (audit$spend_rows > 0) "ok" else "warn"),
            audit_pill("ROI", roi_text, if (isTRUE(audit$roi_missing)) "error" else "ok"),
            audit_pill("TimeBreak", time_text, if (nzchar(cfg$time_break_label %||% "")) "info" else "neutral"),
            audit_pill("GeoLabel", geo_text, if (nzchar(geo_label_raw)) "info" else "neutral"),
            audit_pill("Merges", audit$n_merges, if (audit$n_merges > 0) "info" else "neutral")),
        div(class = paste("channel-audit-status-explain", paste0("audit-explain-", status_tone)),
            div(class = "channel-audit-status-title",
                icon(if (identical(audit$status, "Blocked")) "circle-xmark"
                     else if (identical(audit$status, "Needs review")) "triangle-exclamation"
                     else "circle-check"),
                tags$strong(if (identical(audit$status, "Ready")) "Audit passed"
                            else paste("Why this status"))),
            tags$ul(class = "channel-audit-status-list",
                    lapply(status_items, \(msg) tags$li(msg)))),
        if (length(audit$blockers) || length(audit$warnings) ||
            length(messages_notes) || nzchar(geo_label_raw))
          div(class = "channel-audit-messages",
              tagList(lapply(audit$blockers, \(msg)
                div(class = "channel-audit-message audit-msg-error",
                    icon("circle-xmark"), tags$span(msg)))),
              tagList(lapply(audit$warnings[!grepl("^ROI is (missing|not loaded)", audit$warnings)], \(msg)
                div(class = "channel-audit-message audit-msg-warn",
                    icon("triangle-exclamation"), tags$span(msg)))),
              tagList(lapply(messages_notes, \(msg)
                div(class = "channel-audit-message audit-msg-info",
                    icon("circle-info"), tags$span(msg)))),
              if (nzchar(geo_label_raw))
                div(class = "channel-audit-message audit-msg-info",
                    icon("circle-info"),
                    tags$span("GeoLabel is applied because this same time partition exists with multiple geography rules.")))
          )
        )
    }

    normalize_model_var <- function(x) {
      trimws(stringr::str_remove(as.character(x),
                                 stringr::regex("(_Total)+$", ignore_case = TRUE)))
    }

    # Match ROI rows to the configured model variable and channel context.
    lookup_roi <- function(model_var, fallback = NULL, cfg = NULL) {
      rois <- rois_clean()
      if (is.null(rois) || !"MainModelVariableName" %in% names(rois)) {
        return(list(value = NA_real_, channel = NA_character_))
      }

      candidates <- unique(normalize_model_var(c(model_var, fallback)))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (!length(candidates)) return(list(value = NA_real_, channel = NA_character_))

      roi_norm <- rois
      roi_norm$.mv_norm <- normalize_model_var(roi_norm$MainModelVariableName)
      rows <- roi_norm[roi_norm$.mv_norm %in% candidates, , drop = FALSE]
      if (!nrow(rows)) return(list(value = NA_real_, channel = NA_character_))

      roi_meta <- c("MainModelVariableName", "Channel", "Geography",
                    "Sourced VariableName", "VariableSplit", "SplitOrder")
      roi_num <- names(rows)[
        stringr::str_detect(names(rows), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE)) |
          vapply(rows, is.numeric, logical(1))
      ]
      roi_num <- setdiff(unique(roi_num), roi_meta)
      roi_key_cols <- tryCatch({
        md <- data()$all_rags
        reserved <- c(roi_meta, roi_num, "VariableName", "VariableValue", "Period")
        setdiff(intersect(names(rows), names(md)), reserved)
      }, error = \(e) character(0))

      if (!is.null(cfg) && "Sourced VariableName" %in% names(rows)) {
        vi <- unique(trimws(as.character(cfg$varname_include %||% character(0))))
        vi <- vi[!is.na(vi) & nzchar(vi)]
        if (length(vi)) {
          src_vals <- trimws(as.character(rows[["Sourced VariableName"]]))
          src_rows <- rows[tolower(src_vals) %in% tolower(vi), , drop = FALSE]
          if (nrow(src_rows)) rows <- src_rows
        }
      }

      if (!is.null(cfg) && length(roi_key_cols)) {
        sm <- tryCatch(data()$schema_metadata, error = \(e) NULL)
        nl <- sm$name_lookup
        useful_long <- sm$useful_long %||% character(0)
        for (key_col in intersect(roi_key_cols, useful_long)) {
          dim_vals <- tryCatch(
            get_useful_long_values(cfg$analytical_varkeys %||% character(0), nl, key_col),
            error = \(e) character(0)
          )
          dim_vals <- unique(trimws(as.character(dim_vals)))
          dim_vals <- dim_vals[!is.na(dim_vals) & nzchar(dim_vals)]
          if (!length(dim_vals)) next
          row_vals <- trimws(as.character(rows[[key_col]]))
          matched <- rows[tolower(row_vals) %in% tolower(dim_vals), , drop = FALSE]
          if (nrow(matched)) rows <- matched
        }
      }

      for (roi_col in roi_num) {
        if (!is.numeric(rows[[roi_col]])) {
          rows[[roi_col]] <- suppressWarnings(as.numeric(
            gsub("%", "", gsub(",", "", as.character(rows[[roi_col]])))
          ))
        }
      }
      roi_val <- if (length(roi_num)) {
        vals <- rows[[roi_num[1]]]
        vals <- vals[!is.na(vals)]
        if (length(vals)) mean(vals) else NA_real_
      } else NA_real_

      roi_channel <- if ("Channel" %in% names(rows)) {
        vals <- trimws(as.character(rows$Channel))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals)) vals[1] else NA_character_
      } else NA_character_

      list(value = roi_val, channel = roi_channel)
    }

    effective_roi <- function(cfg, fallback = NULL) {
      stored <- cfg$roi %||% NA_real_
      resolved <- lookup_roi(cfg$model_variable %||% fallback, fallback, cfg)$value
      if (!is.na(resolved)) return(resolved)
      stored
    }

    effective_roi_cached <- function(nm, cfg) {
      key <- paste(
        nm %||% "",
        cfg$model_variable %||% "",
        paste(cfg$analytical_varkeys %||% character(0), collapse = "|"),
        paste(cfg$varname_include %||% character(0), collapse = "|"),
        rois_signature(),
        sep = "::"
      )
      cached <- cache_get(channel_roi_cache, key)
      if (!is.null(cached)) return(cached)
      value <- effective_roi(cfg, nm)
      cache_set(channel_roi_cache, key, value, max_items = 300L)
      value
    }
    
    get_varname_include_fallback <- function(nm) {
      vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
      if (!is.null(vof_d) &&
          all(c("MainModelVariableName", "AnalyticalVariableName") %in% names(vof_d))) {
        vof_rows <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
        if (nrow(vof_rows) > 0) {
          an_vn <- unique(vof_rows$AnalyticalVariableName[
            !is.na(vof_rows$AnalyticalVariableName) &
              nzchar(vof_rows$AnalyticalVariableName)])
          if (length(an_vn) > 0) {
            an_vn_clean <- unique(stringr::str_remove(an_vn, "_Total(_Total)*$"))
            return(unique(c(an_vn_clean, nm)))
          }
        }
      }
      base_vn <- trimws(stringr::str_remove(
        stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$"))
      if (!nzchar(base_vn)) base_vn <- nm
      act_kw   <- detect_activity_keyword(base_vn)
      broad_vn <- trimws(stringr::str_remove(
        base_vn,
        stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)))
      if (nzchar(broad_vn) && broad_vn != base_vn) c(base_vn, broad_vn) else base_vn
    }

    channel_name_key <- function(x) {
      tolower(stringr::str_squish(trimws(as.character(x %||% ""))))
    }

    raw_vof_channel_names <- function() {
      vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
      if (is.null(vof_d) || !"MainModelVariableName" %in% names(vof_d)) {
        return(character(0))
      }
      vals <- unique(trimws(as.character(vof_d$MainModelVariableName)))
      vals[!is.na(vals) & nzchar(vals)]
    }

    inactive_vof_channel_names <- function(candidates = raw_vof_channel_names()) {
      candidates <- unique(as.character(candidates %||% character(0)))
      if (!length(candidates)) return(character(0))
      raw_keys <- channel_name_key(raw_vof_channel_names())
      available_keys <- channel_name_key(names(rv$available_channels))
      candidate_keys <- channel_name_key(candidates)
      candidates[candidate_keys %in% raw_keys & !candidate_keys %in% available_keys]
    }

    # Reconcile generated channels with the latest index while preserving
    # valid manual channels and removing generated channels rejected by Details.
    sync_channels_to_media_index <- function(mi, notify = FALSE) {
      if (is.null(mi) || is.null(mi$channels)) return(character(0))
      inherited <- tryCatch(update_sap(), error = function(e) list())
      if (identical(tryCatch(data()$app_mode, error = function(e) "build"), "update") &&
          length(inherited)) {
        for (channel in intersect(names(inherited), names(mi$channels))) {
          mi$channels[[channel]]$saved_merges <- inherited[[channel]]
        }
      }
      rv$available_channels <- mi$channels
      raw_vof_keys <- channel_name_key(raw_vof_channel_names())
      stale_existing <- names(rv$channels)[vapply(names(rv$channels), function(nm) {
        cfg <- rv$channels[[nm]]
        is_vof_channel <- identical(cfg$source %||% "", "vof") ||
          channel_name_key(nm) %in% raw_vof_keys
        is_vof_channel && !nm %in% names(mi$channels)
      }, logical(1))]
      if (length(stale_existing)) {
        for (nm in stale_existing) {
          rv$channels[[nm]] <- NULL
          dirty_channels[[nm]] <- NULL
        }
        if (!is.null(rv$selected) && rv$selected %in% stale_existing)
          rv$selected <- if (length(rv$channels)) names(rv$channels)[1] else NULL
        if (isTRUE(notify)) {
          showNotification(
            paste0(length(stale_existing),
                   " stale VOF channel(s) removed from the refreshed Media Index."),
            type = "message", duration = 6
          )
        }
      }
      stale_existing
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
