# Register Setup's file/schema contract helpers and upload summary renderer.
# These helpers inspect inputs without changing the stored source datasets.
register_setup_contracts <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Report missing structural columns before a file reaches index building.
    validate_required_cols <- function(df, label) {
      missing <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing) > 0) {
        showNotification(paste0(label, " is missing required columns: ",
                                paste(missing, collapse = ", ")),
                         type = "error", duration = 15)
        return(FALSE)
      }
      TRUE
    }

    output$batch_upload_summary <- renderUI({
      issues <- rv$upload_issues
      has_loaded <- length(rv$file_meta) > 0
      has_issues <- !is.null(issues) && (
        length(issues$unrecognized %||% list()) > 0 ||
          length(issues$duplicates %||% list()) > 0 ||
          length(issues$errors %||% list()) > 0
      )
      if (!has_loaded && !has_issues) return(NULL)
      issues <- issues %||% list()

      recognized_items <- lapply(intersect(base_file_kinds, names(rv$file_meta)), function(kind) {
        meta <- rv$file_meta[[kind]]
        contract <- rv$file_contracts[[kind]]
        contract_text <- loaded_file_contract_text(kind)
        list(
          label = base_file_labels[[kind]],
          file = meta$name,
          stats = paste(c(
            if (!is.null(meta$rows)) paste0(format(meta$rows, big.mark = ","), " rows"),
            if (!is.null(meta$cols)) paste0(format(meta$cols, big.mark = ","), " cols"),
            format_file_size(meta$size)
          ), collapse = " | "),
          detail = paste(c(
            paste0("Loaded via ", meta$source, " at ", meta$loaded_at),
            if (nzchar(contract_text)) contract_text,
            if (length(contract$warnings %||% character(0)))
              paste0(length(contract$warnings), " warning(s)")
          ), collapse = " | ")
        )
      })
      missing_items <- lapply(setdiff(required_base_file_kinds, names(rv$file_meta)), function(kind) {
        list(main = base_file_labels[[kind]], sub = "Not loaded yet.")
      })
      optional_missing_items <- lapply(setdiff(optional_base_file_kinds, names(rv$file_meta)), function(kind) {
        list(
          main = base_file_labels[[kind]],
          sub = "Optional. Seed For Indices will export without ROI values."
        )
      })
      duplicate_items <- lapply(issues$duplicates %||% list(), function(x) {
        list(
          main = paste0(x$file, " skipped as duplicate ", x$label),
          sub  = paste0("Kept: ", x$kept_file)
        )
      })
      unrecognized_items <- lapply(issues$unrecognized %||% list(), function(x) {
        list(
          main = paste0(x$file, " from ", x$source %||% "upload"),
          sub  = paste0("Not recognized | ", x$reason)
        )
      })
      error_items <- lapply(issues$errors %||% list(), function(x) {
        list(
          main = paste0(x$file, " -> ", x$label),
          sub  = paste0("Load failed | ", x$error)
        )
      })
      warning_items <- lapply(contract_warning_items(), function(x) {
        list(
          main = x$title,
          sub = x$message,
          action = x$action %||% NULL,
          examples = x$examples %||% character(0)
        )
      })

      has_review <- length(missing_items) > 0 || length(optional_missing_items) > 0 ||
        length(duplicate_items) > 0 || length(unrecognized_items) > 0 ||
        length(error_items) > 0 || length(warning_items) > 0
      review_count <- length(missing_items) + length(optional_missing_items) +
        length(duplicate_items) + length(unrecognized_items) +
        length(error_items) + length(warning_items)

      tags$div(
        class = "batch-upload-summary",
        tags$div(
          class = "batch-summary-head",
          tags$div(
            class = "batch-summary-title",
            icon(if (has_review) "triangle-exclamation" else "circle-check"),
            tags$div(
              tags$strong(if (has_review) "Files loaded with review items" else "Files ready"),
              tags$span(paste0(length(recognized_items), " loaded",
                               if (has_review) paste0(" | ", review_count, " to review") else ""))
            )
          ),
          tags$div(
            class = "batch-summary-badges",
            batch_summary_badge("Loaded", length(recognized_items), "ok"),
            if (length(missing_items))
              batch_summary_badge("Required missing", length(missing_items), "error"),
            if (length(optional_missing_items))
              batch_summary_badge("ROI missing", length(optional_missing_items), "warn"),
            if (length(warning_items))
              batch_summary_badge("Warnings", length(warning_items), "warn"),
            if (length(duplicate_items))
              batch_summary_badge("Skipped", length(duplicate_items), "warn"),
            if (length(unrecognized_items) || length(error_items))
              batch_summary_badge("Errors", length(unrecognized_items) + length(error_items), "error")
          )
        ),
        tags$div(
          class = "batch-summary-body",
          if (length(recognized_items))
            tags$div(
              class = "batch-loaded-grid",
              lapply(recognized_items, function(item) {
                tags$div(
                  class = "batch-loaded-item",
                  title = item$detail,
                  icon("circle-check"),
                  tags$div(
                    class = "batch-loaded-copy",
                    tags$div(class = "batch-loaded-label", item$label),
                    tags$div(class = "batch-loaded-file", item$file),
                    tags$div(class = "batch-loaded-stats", item$stats)
                  )
                )
              })
            ),
          batch_summary_section("Missing required files", missing_items, "warn"),
          batch_summary_section("Optional", optional_missing_items, "warn"),
          batch_summary_section("Review", warning_items, "warn"),
          batch_summary_section("Skipped duplicates", duplicate_items, "warn"),
          batch_summary_section("Not recognized", unrecognized_items, "error"),
          batch_summary_section("Load errors", error_items, "error")
        )
      )
    })

    read_csv_header <- function(path, nrows = 5) {
      clean_data_columns(
        data.table::fread(path, nrows = nrows, data.table = FALSE,
                          stringsAsFactors = FALSE, showProgress = FALSE)
      )
    }

    read_first_rdata_object <- function(path) {
      e <- new.env()
      load(path, envir = e)
      get(ls(e)[1], envir = e)
    }

    reset_update_outputs <- function() {
      rv$analytical_combined   <- NULL
      rv$side_mapping_nonfocus <- NULL
      rv$update_status         <- "pending"
    }

    required_base_file_kinds <- c("main", "analytical", "vof", "details")
    optional_base_file_kinds <- c("rois")
    base_file_kinds <- c(required_base_file_kinds, optional_base_file_kinds)

    base_file_input_ids <- c(
      main       = "file_main",
      analytical = "file_analytical",
      vof        = "file_vof",
      details    = "file_details",
      rois       = "file_rois"
    )

    format_file_size <- function(bytes) {
      if (is.null(bytes) || is.na(bytes)) return("size unknown")
      if (bytes >= 1024^2)
        return(paste0(round(bytes / 1024^2, 1), " MB"))
      if (bytes >= 1024)
        return(paste0(round(bytes / 1024, 1), " KB"))
      paste0(bytes, " B")
    }

    base_file_labels <- c(
      main       = "RAE Datafile",
      analytical = "Analytical Dataset",
      vof        = "VOF Metadata",
      details    = "ModelDetails",
      rois       = "ROIs by Channel"
    )

    required_files_ready <- function() {
      !is.null(rv$main_data) && !is.null(rv$analytical) &&
        !is.null(rv$vof_data) && !is.null(rv$details)
    }

    normalize_contract_key <- function(x) {
      x <- trimws(as.character(x))
      x <- stringr::str_to_upper(x)
      gsub("\\s+", " ", x)
    }

    normalize_contract_model_var <- function(x) {
      out <- trimws(as.character(x))
      out <- stringr::str_replace_all(out, "\\s+", " ")
      out <- stringr::str_remove(out, stringr::regex("(_Total)+$", ignore_case = TRUE))
      stringr::str_to_lower(out)
    }

    date_contract <- function(df, col = "Period") {
      if (is.null(df) || !col %in% names(df)) {
        return(list(has_date = FALSE, min = NULL, max = NULL, invalid = 0L))
      }
      raw <- df[[col]]
      parsed <- if (inherits(raw, "Date")) raw else parse_period_robust(raw)
      non_empty <- !is.na(raw) & nzchar(trimws(as.character(raw)))
      invalid <- sum(non_empty & is.na(parsed), na.rm = TRUE)
      valid <- parsed[!is.na(parsed)]
      list(
        has_date = length(valid) > 0,
        min = if (length(valid)) min(valid) else NULL,
        max = if (length(valid)) max(valid) else NULL,
        invalid = invalid
      )
    }

    vof_date_contract <- function(df) {
      if (is.null(df)) return(list(invalid = 0L, min = NULL, max = NULL))
      cols <- intersect(c("MinPeriod", "MaxPeriod"), names(df))
      if (!length(cols)) return(list(invalid = 0L, min = NULL, max = NULL))
      parsed <- lapply(cols, function(col) {
        raw <- df[[col]]
        p <- parse_period_robust(raw)
        non_empty <- !is.na(raw) & nzchar(trimws(as.character(raw)))
        list(parsed = p, invalid = sum(non_empty & is.na(p), na.rm = TRUE))
      })
      all_dates <- do.call(c, lapply(parsed, `[[`, "parsed"))
      all_dates <- all_dates[!is.na(all_dates)]
      list(
        invalid = sum(vapply(parsed, `[[`, integer(1), "invalid")),
        min = if (length(all_dates)) min(all_dates) else NULL,
        max = if (length(all_dates)) max(all_dates) else NULL
      )
    }

    roi_columns_detected <- function(df) {
      if (is.null(df)) return(character(0))
      cols <- names(df)
      cols[stringr::str_detect(cols, stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE))]
    }

    # Summarize the loaded file's dimensions, dates, and recognized schema for
    # the persistent Setup validation record.
    build_file_contract <- function(kind, rule = NULL) {
      df <- switch(kind,
                   main = rv$main_data,
                   analytical = rv$analytical,
                   vof = rv$vof_data,
                   details = rv$details_raw,
                   rois = rv$channels_rois,
                   NULL)
      label <- base_file_labels[[kind]] %||% kind
      if (is.null(df)) {
        return(list(
          kind = kind, label = label, loaded = FALSE, rule = rule %||% "",
          rows = 0L, cols = 0L, key_columns = character(0),
          date_min = NULL, date_max = NULL, warnings = character(0),
          blockers = if (kind %in% required_base_file_kinds) "Not loaded" else character(0),
          built_at = format(Sys.time(), "%Y-%m-%d %H:%M")
        ))
      }

      req_cols <- switch(kind,
                         main = REQUIRED_COLS,
                         analytical = "Period",
                         vof = c("AnalyticalVariableName", "MainModelVariableName", "MinPeriod", "MaxPeriod"),
                         details = model_details_required_cols,
                         rois = "MainModelVariableName",
                         character(0))
      opt_cols <- switch(kind,
                         vof = c("Geography", "Geographies", "Metric", "Effect", "MediaChannel", "SubChannel"),
                         rois = c("ROI", "Geography", "Sourced VariableName", "Channel"),
                         analytical = analytical_dim_cols,
                         character(0))
      cc <- column_contract(df, req_cols, opt_cols)
      warnings <- character(0)
      blockers <- cc$missing_required
      date_info <- if (kind == "vof") vof_date_contract(df) else date_contract(df, "Period")

      if (kind == "main") {
        if (!"VariableValue" %in% names(df)) blockers <- c(blockers, "VariableValue")
        if (!isTRUE(date_info$has_date)) blockers <- c(blockers, "No valid Period values")
        if (date_info$invalid > 0) warnings <- c(warnings, paste0(date_info$invalid, " RAE Period value(s) could not be parsed"))
        vals <- suppressWarnings(as.numeric(df$VariableValue))
        if (!any(is.finite(vals), na.rm = TRUE)) blockers <- c(blockers, "VariableValue has no numeric values")
      } else if (kind == "analytical") {
        sig <- validate_analytical_signature(df)
        if (!isTRUE(sig$ok)) blockers <- c(blockers, sig$reason)
        if (!isTRUE(date_info$has_date)) blockers <- c(blockers, "No valid Period values")
        if (date_info$invalid > 0) warnings <- c(warnings, paste0(date_info$invalid, " Analytical Period value(s) could not be parsed"))
      } else if (kind == "vof") {
        if (!any(c("Geography", "Geographies") %in% names(df))) blockers <- c(blockers, "Geography or Geographies")
        if (date_info$invalid > 0) warnings <- c(warnings, paste0(date_info$invalid, " VOF Min/Max Period value(s) could not be parsed"))
        if (!"Metric" %in% names(df)) warnings <- c(warnings, "Metric column not found; Activity/Spend keywords will be inferred")
      } else if (kind == "details") {
        type_chr <- stringr::str_to_lower(trimws(as.character(df$Type %||% character(0))))
        n_modeled <- sum(stringr::str_detect(type_chr, "\\b(in|fixed)\\b"), na.rm = TRUE)
        if (n_modeled == 0) warnings <- c(warnings, "No IN/FIXED variables detected after filtering ModelDetails")
      } else if (kind == "rois") {
        roi_cols <- roi_columns_detected(df)
        if (!length(roi_cols)) blockers <- c(blockers, "ROI column")
        if (!is.null(rv$main_data)) {
          supported_meta <- c("MainModelVariableName", "Channel", "Geography",
                              "Sourced VariableName", "VariableSplit", "SplitOrder", roi_cols)
          candidate_keys <- setdiff(names(df), supported_meta)
          unknown_keys <- setdiff(candidate_keys, names(rv$main_data))
          unknown_keys <- unknown_keys[!vapply(df[unknown_keys], is.numeric, logical(1))]
          if (length(unknown_keys)) {
            warnings <- c(warnings, paste0(
              "ROI key column(s) not found in RAE: ",
              paste(head(unknown_keys, 5), collapse = ", "),
              if (length(unknown_keys) > 5) " ..." else ""
            ))
          }
        }
      }

      blockers <- unique(blockers[nzchar(as.character(blockers))])
      warnings <- unique(warnings[nzchar(as.character(warnings))])
      key_columns <- unique(c(cc$present_required, cc$present_optional))
      list(
        kind = kind,
        label = label,
        loaded = TRUE,
        rule = rule %||% "",
        rows = nrow(df),
        cols = ncol(df),
        key_columns = key_columns,
        date_min = date_info$min,
        date_max = date_info$max,
        warnings = warnings,
        blockers = blockers,
        built_at = format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    }

    update_file_contract <- function(kind, rule = NULL) {
      contracts <- rv$file_contracts
      contracts[[kind]] <- build_file_contract(kind, rule)
      rv$file_contracts <- contracts
      refresh_cross_file_warnings()
    }

    clear_file_contract <- function(kind) {
      contracts <- rv$file_contracts
      contracts[[kind]] <- NULL
      rv$file_contracts <- contracts
      refresh_cross_file_warnings()
    }

    contract_warning_items <- function() {
      items <- list()
      for (kind in names(rv$file_contracts)) {
        contract <- rv$file_contracts[[kind]]
        for (msg in contract$warnings %||% character(0)) {
          items[[length(items) + 1L]] <- list(
            title = paste0(contract$label, " warning"),
            message = msg
          )
        }
        for (msg in contract$blockers %||% character(0)) {
          items[[length(items) + 1L]] <- list(
            title = paste0(contract$label, " blocker"),
            message = msg
          )
        }
      }
      for (msg in rv$cross_file_warnings %||% list()) {
        items[[length(items) + 1L]] <- msg
      }
      items
    }

    refresh_cross_file_warnings <- function() {
      warnings <- list()
      add_warning <- function(title, message, action = NULL, examples = character(0)) {
        warnings[[length(warnings) + 1L]] <<- list(
          title = title,
          message = message,
          action = action,
          examples = examples
        )
      }
      if (!is.null(rv$vof_data) && !is.null(rv$analytical) &&
          "AnalyticalVariableName" %in% names(rv$vof_data)) {
        vof_vars <- unique(unlist(strsplit(as.character(rv$vof_data$AnalyticalVariableName), "\\|")))
        vof_vars <- trimws(vof_vars[!is.na(vof_vars) & nzchar(vof_vars)])
        missing_an <- setdiff(normalize_contract_key(vof_vars), normalize_contract_key(names(rv$analytical)))
        if (length(missing_an)) {
          sample <- vof_vars[normalize_contract_key(vof_vars) %in% head(missing_an, 5)]
          add_warning(
            paste0(length(missing_an), " VOF variable",
                   if (length(missing_an) != 1) "s are" else " is",
                   " missing from Analytical"),
            "The affected modeled connection cannot be confirmed in Analytical.",
            "Check AnalyticalVariableName in VOF against the exact Analytical column name.",
            head(sample, 3)
          )
        }
      }
      if (!is.null(rv$vof_data) && !is.null(rv$main_data) &&
          "AnalyticalVariableName" %in% names(rv$vof_data) &&
          "VariableName" %in% names(rv$main_data)) {
        vof_vars <- unique(unlist(strsplit(as.character(rv$vof_data$AnalyticalVariableName), "\\|")))
        vof_vars <- trimws(vof_vars[!is.na(vof_vars) & nzchar(vof_vars)])
        rae_vars <- unique(trimws(as.character(rv$main_data$VariableName)))
        rae_vars <- rae_vars[!is.na(rae_vars) & nzchar(rae_vars)]
        matched_vof <- vof_vars[vapply(vof_vars, function(v) {
          expanded <- expand_analytical_keys_to_variable_names(rae_vars, v)
          any(normalize_contract_key(expanded) %in% normalize_contract_key(rae_vars))
        }, logical(1))]
        missing_rae <- setdiff(normalize_contract_key(vof_vars), normalize_contract_key(matched_vof))
        if (length(missing_rae)) {
          sample <- vof_vars[normalize_contract_key(vof_vars) %in% head(missing_rae, 5)]
          add_warning(
            paste0(length(missing_rae), " VOF variable",
                   if (length(missing_rae) != 1) "s have" else " has",
                   " no RAE source"),
            "Splits cannot be reconstructed from RAE for these variables.",
            "Compare AnalyticalVariableName in VOF with VariableName in RAE.",
            head(sample, 3)
          )
        }
      }
      if (!is.null(rv$details_raw) && !is.null(rv$analytical) &&
          all(c("Type", "VariableName") %in% names(rv$details_raw))) {
        d <- rv$details_raw
        type_chr <- stringr::str_to_lower(trimws(as.character(d$Type)))
        modeled <- unique(trimws(as.character(d$VariableName[stringr::str_detect(type_chr, "\\b(in|fixed)\\b")])))
        modeled <- modeled[!is.na(modeled) & nzchar(modeled)]
        analytical_names <- names(rv$analytical)
        schema_lookup <- rv$schema_metadata$name_lookup %||% data.frame()
        schema_names <- unique(c(
          schema_lookup$OriginalName %||% character(0),
          schema_lookup$VariableName %||% character(0)
        ))
        vof_names <- if (!is.null(rv$vof_data)) {
          unique(c(
            rv$vof_data$MainModelVariableName %||% character(0),
            rv$vof_data$AnalyticalVariableName %||% character(0)
          ))
        } else character(0)
        allowed_names <- unique(c(analytical_names, schema_names, vof_names))
        allowed_norm <- unique(c(
          normalize_contract_key(allowed_names),
          normalize_contract_key(normalize_contract_model_var(allowed_names))
        ))
        modeled_norm <- normalize_contract_key(modeled)
        modeled_mv_norm <- normalize_contract_key(normalize_contract_model_var(modeled))
        missing_mask <- !(modeled_norm %in% allowed_norm | modeled_mv_norm %in% allowed_norm)
        missing_details <- normalize_contract_key(modeled[missing_mask])
        if (length(missing_details)) {
          sample <- modeled[normalize_contract_key(modeled) %in% head(missing_details, 5)]
          add_warning(
            paste0(length(missing_details), " modeled variable",
                   if (length(missing_details) != 1) "s are" else " is",
                   " not connected"),
            "The IN/FIXED variable was not found in Analytical, VOF, or a usable longitudinal mapping.",
            "Confirm the ModelDetails VariableName and its corresponding Analytical or VOF name.",
            head(sample, 3)
          )
        }
      }
      if (!is.null(rv$channels_rois) && !is.null(rv$vof_data) &&
          all(c("MainModelVariableName") %in% names(rv$channels_rois)) &&
          "MainModelVariableName" %in% names(rv$vof_data)) {
        roi_mv <- unique(trimws(as.character(rv$channels_rois$MainModelVariableName)))
        roi_mv <- roi_mv[!is.na(roi_mv) & nzchar(roi_mv)]
        vof_mv <- unique(trimws(as.character(rv$vof_data$MainModelVariableName)))
        extra_roi <- setdiff(normalize_contract_key(roi_mv), normalize_contract_key(vof_mv))
        if (length(extra_roi)) {
          sample <- roi_mv[normalize_contract_key(roi_mv) %in% head(extra_roi, 5)]
          add_warning(
            paste0(length(extra_roi), " ROI entr",
                   if (length(extra_roi) != 1) "ies do" else "y does",
                   " not match VOF"),
            "These ROI values will not be assigned to a VOF channel.",
            "Match MainModelVariableName exactly between ROIs by Channel and VOF.",
            head(sample, 3)
          )
        }
      }
      rv$cross_file_warnings <- warnings
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
