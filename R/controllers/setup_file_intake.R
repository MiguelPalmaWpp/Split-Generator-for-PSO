# Register file readers and state updates for manual/batch Setup uploads.
# Source tables stay in Setup reactiveValues; derived signatures and indexes
# are refreshed when an upload is replaced or removed.
register_setup_file_intake <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    loaded_file_contract_text <- function(kind) {
      contract <- rv$file_contracts[[kind]]
      if (is.null(contract)) return("")
      present <- contract$key_columns %||% character(0)
      txt <- paste0("keys: ", paste(head(present, 5), collapse = ", "))
      if (length(present) > 5) txt <- paste0(txt, " +", length(present) - 5)
      if (length(contract$blockers %||% character(0))) {
        txt <- paste0(txt, " | blockers: ", paste(contract$blockers, collapse = ", "))
      }
      if (!is.null(contract$date_min) && !is.null(contract$date_max)) {
        txt <- paste0(txt, " | dates: ", format(contract$date_min), " -> ", format(contract$date_max))
      }
      txt
    }

    batch_summary_badge <- function(label, count, type = "neutral") {
      tags$span(
        class = paste("batch-summary-badge", paste0("batch-summary-badge-", type)),
        tags$span(class = "batch-summary-badge-count", count),
        label
      )
    }

    batch_summary_section <- function(title, items, type = "neutral") {
      if (!length(items)) return(NULL)
      tags$div(
        class = "batch-summary-section",
        tags$div(class = "batch-summary-section-title", title),
        tags$div(
          class = "batch-summary-list",
          lapply(items, function(item) {
            tags$div(
              class = paste("batch-summary-row", paste0("batch-summary-row-", type)),
              icon(switch(type,
                          ok = "circle-check",
                          warn = "triangle-exclamation",
                          error = "circle-xmark",
                          muted = "circle-info",
                          "circle-info"),
                   class = "batch-summary-row-icon"),
              tags$div(
                class = "batch-summary-row-text",
                tags$div(class = "batch-summary-row-main", item$main),
                if (!is.null(item$sub) && nzchar(item$sub))
                  tags$div(class = "batch-summary-row-sub", item$sub),
                if (!is.null(item$action) && nzchar(item$action))
                  tags$div(class = "batch-summary-row-action",
                           tags$strong("Review: "), item$action),
                if (length(item$examples %||% character(0)))
                  tags$div(
                    class = "batch-summary-row-examples",
                    tags$span("Examples"),
                    lapply(head(item$examples, 3), function(example)
                      tags$code(example))
                  )
              )
            )
          })
        )
      )
    }

    set_upload_issues <- function(unrecognized = list(), duplicates = list(),
                                  errors = list()) {
      rv$upload_issues <- list(
        unrecognized = unrecognized,
        duplicates = duplicates,
        errors = errors,
        last_upload_at = format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    }

    set_manual_upload_summary <- function(kind, file_row, ok, reason,
                                          error = NULL, status = NULL) {
      label <- base_file_labels[[kind]] %||% kind
      if (isTRUE(ok)) {
        set_upload_issues()
      } else {
        set_upload_issues(
          unrecognized = list(list(
            file = file_row$name,
            ext = tolower(tools::file_ext(file_row$name)),
            reason = reason,
            source = label
          )),
          errors = if (!is.null(error)) list(list(
            file = file_row$name,
            kind = kind,
            label = label,
            error = error
          )) else list()
        )
      }
    }

    set_file_meta <- function(kind, file_row, rows = NULL, cols = NULL,
                              source = "manual", rule = NULL) {
      pso_cache_reset(performance_cache)
      rv$media_index       <- NULL
      rv$validation_status <- "pending"
      rv$file_meta[[kind]] <- list(
        name      = file_row$name,
        size      = file_row$size,
        rows      = rows,
        cols      = cols,
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
        source    = source
      )
      current_data <- switch(
        kind,
        main = rv$main_data,
        analytical = rv$analytical,
        vof = rv$vof_data,
        details = rv$details,
        rois = rv$channels_rois,
        NULL
      )
      rv$file_signatures[[kind]] <- data_signature(current_data)
      update_file_contract(kind, rule %||% source)
    }

    clear_file_meta <- function(kind) {
      meta <- rv$file_meta
      meta[[kind]] <- NULL
      rv$file_meta <- meta
      signatures <- rv$file_signatures
      signatures[[kind]] <- NULL
      rv$file_signatures <- signatures
      clear_file_contract(kind)
    }

    render_loaded_file_status <- function(kind) {
      meta <- rv$file_meta[[kind]]
      if (is.null(meta)) {
        return(div(class = "file-card-empty", "No file loaded"))
      }

      dims <- c(
        if (!is.null(meta$rows)) paste0(format(meta$rows, big.mark = ","), " rows"),
        if (!is.null(meta$cols)) paste0(format(meta$cols, big.mark = ","), " cols")
      )
      detail <- paste(c(
        format_file_size(meta$size),
        dims,
        paste0("via ", meta$source),
        meta$loaded_at
      ), collapse = " | ")

      div(
        class = "file-card-loaded",
        icon("circle-check", class = "file-card-loaded-icon"),
        div(
          class = "file-card-file-text",
          div(class = "file-card-file-name", title = meta$name, meta$name),
          div(class = "file-card-file-meta", detail)
        ),
        tags$button(
          type = "button",
          class = "file-card-remove",
          title = "Remove file",
          onclick = sprintf(
            "Shiny.setInputValue('%s','%s',{priority:'event'});",
            ns("remove_base_file"), kind
          ),
          icon("xmark")
        )
      )
    }

    reset_file_input <- function(input_id) {
      session$sendCustomMessage("resetFileInput", list(id = ns(input_id)))
    }

    clear_base_file <- function(kind) {
      switch(kind,
             main = {
               rv$main_data <- NULL
               rv$main_data_indexed <- NULL
             },
             analytical = {
               rv$analytical       <- NULL
               rv$dates_df         <- NULL
               rv$analytical_rag   <- NULL
               rv$cross_cols       <- NULL
               rv$schema_metadata  <- NULL
             },
             vof = {
               rv$vof_data <- NULL
             },
             details = {
               rv$details_raw <- NULL
               rv$details <- NULL
             },
             rois = {
               rv$channels_rois <- NULL
             },
             return(FALSE)
      )

      rv$media_index       <- NULL
      rv$validation_status <- "pending"
      mi_signature(NULL)
      pso_cache_reset(performance_cache)
      reset_update_outputs()
      clear_file_meta(kind)
      reset_file_input(base_file_input_ids[[kind]])
      reset_file_input("file_base_bundle")
      TRUE
    }

    # Include raw ModelDetails status values so IN/FIXED/NONE changes rebuild
    # the index even when the filtered Details table remains unchanged.
    # Include raw ModelDetails so changing only a NONE/IN/FIXED row rebuilds
    # the index instead of reusing a stale channel set.
    media_index_signature <- function() {
      kinds <- c(required_base_file_kinds, optional_base_file_kinds)
      pieces <- vapply(kinds, function(kind) {
        meta <- rv$file_meta[[kind]]
        if (is.null(meta)) return(paste0(kind, ":missing"))
        paste(kind, meta$name, meta$size, meta$rows %||% "", meta$cols %||% "",
              meta$loaded_at %||% "", sep = ":")
      }, character(1))
      details_contract <- if (!is.null(rv$details_raw) &&
                              all(c("VariableName", "Type") %in% names(rv$details_raw))) {
        pso_cache_key(
          "model-details-contract",
          nrow(rv$details_raw),
          names(rv$details_raw),
          rv$details_raw$VariableName,
          rv$details_raw$Type
        )
      } else {
        "details-contract:missing"
      }
      pso_cache_key("setup-files", pieces, rv$file_signatures, details_contract)
    }

    # Load the RAE source once, then publish both the untouched table and its
    # VariableName lookup index for channel-level filtering.
    load_main_base <- function(file_row, preloaded = NULL, source = "manual") {
      ext <- tools::file_ext(file_row$name)
      df <- preloaded %||% read_main_data(file_row$datapath, ext)
      df <- clean_data_columns(df)
      if (!validate_required_cols(df, "RAE Datafile")) return(FALSE)
      rv$main_data <- df
      set_file_meta("main", file_row, nrow(rv$main_data), ncol(rv$main_data), source,
                    rule = "Matched REQUIRED_COLS")
      rv$main_data_indexed <- pso_profile(
        "setup.rae_index",
        build_indexed_rae(rv$main_data)
      )
      if (!identical(source, "batch")) {
        showNotification(paste0("RAE Datafile loaded - ",
                                format(nrow(rv$main_data), big.mark = ","),
                                " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    analytical_dim_cols <- setdiff(REQUIRED_COLS, c("VariableName", "Period", "VariableValue"))
    model_details_required_cols <- c(
      "Type", "VariableName", "FunctionForm", "Normalization", "MinMaxAdjustment"
    )

    validate_analytical_signature <- function(df) {
      cols <- names(df)
      period_pos <- match("Period", cols)
      if (is.na(period_pos)) {
        return(list(ok = FALSE, reason = "Analytical Dataset must contain Period"))
      }
      left_cols <- if (period_pos > 1L) cols[seq_len(period_pos - 1L)] else character(0)
      matched_dims <- intersect(left_cols, analytical_dim_cols)
      if (!length(matched_dims)) {
        return(list(
          ok = FALSE,
          reason = paste0(
            "Analytical Dataset needs Period with at least one RAE dimension to its left: ",
            paste(analytical_dim_cols, collapse = ", ")
          )
        ))
      }
      list(
        ok = TRUE,
        reason = paste0("Period found after dimension(s): ", paste(matched_dims, collapse = ", "))
      )
    }

    validate_model_details_signature <- function(df) {
      missing <- setdiff(model_details_required_cols, names(df))
      if (length(missing)) {
        return(list(
          ok = FALSE,
          reason = paste0("ModelDetails missing columns: ", paste(missing, collapse = ", "))
        ))
      }
      list(ok = TRUE, reason = "ModelDetails required columns matched")
    }

    validate_rois_signature <- function(df) {
      if (!"MainModelVariableName" %in% names(df)) {
        return(list(ok = FALSE, reason = "ROIs by Channel must contain MainModelVariableName"))
      }
      roi_cols <- names(df)[stringr::str_detect(names(df), stringr::regex("\\bROI\\b", ignore_case = TRUE))]
      if (!length(roi_cols)) {
        return(list(ok = FALSE, reason = "ROIs by Channel must contain an ROI column"))
      }
      list(ok = TRUE, reason = paste0("ROI column found: ", roi_cols[1]))
    }

    load_analytical_base <- function(file_row, preloaded = NULL, source = "manual") {
      df <- preloaded %||% read_first_rdata_object(file_row$datapath)
      df <- clean_data_columns(df)
      df <- df[, !duplicated(names(df), fromLast = TRUE)]
      sig <- validate_analytical_signature(df)
      if (!isTRUE(sig$ok)) {
        showNotification(sig$reason, type = "error", duration = 12)
        return(FALSE)
      }
      if ("Period" %in% names(df))
        df <- df %>% dplyr::mutate(Period = parse_period_robust(Period))
      rv$analytical <- tibble::as_tibble(df) %>% dplyr::ungroup()
      rv$dates_df   <- rv$analytical %>% dplyr::distinct(Period) %>% dplyr::arrange(Period)
      tryCatch({
        sch                <- infer_schema(rv$analytical)
        rv$schema_metadata <- build_schema_metadata(rv$analytical, sch)
        rv$cross_cols <- if (length(rv$schema_metadata$xs_dims) > 0)
          rv$schema_metadata$xs_dims else auto_detect_cross_cols(rv$analytical)
        rv$analytical_rag <- rv$analytical %>%
          dplyr::distinct(dplyr::across(dplyr::any_of(c(rv$cross_cols, "Period"))))
      }, error = \(e) {
        showNotification(pso_public_error(
          e, "setup.schema", "Analytical schema could not be inferred; using detected cross-sections."
        ),
                         type = "warning", duration = 6)
        rv$cross_cols <- auto_detect_cross_cols(rv$analytical)
      })
      reset_update_outputs()
      set_file_meta("analytical", file_row, nrow(rv$analytical), ncol(rv$analytical), source,
                    rule = sig$reason)
      if (!identical(source, "batch")) {
        showNotification(paste0("Analytical loaded - ",
                                format(nrow(rv$analytical), big.mark = ","),
                                " rows"),
                         type = "message", duration = 5)
      }
      TRUE
    }

    load_vof_base <- function(file_row, preloaded = NULL, source = "manual") {
      vof_raw <- preloaded %||%
        as.data.frame(data.table::fread(file_row$datapath, data.table = FALSE,
                                        stringsAsFactors = FALSE,
                                        showProgress = FALSE))
      vof_raw <- clean_data_columns(vof_raw)
      req_fixed <- c("AnalyticalVariableName", "MainModelVariableName",
                     "MinPeriod", "MaxPeriod")
      miss_vof <- setdiff(req_fixed, names(vof_raw))
      if (!any(c("Geography", "Geographies") %in% names(vof_raw)))
        miss_vof <- c(miss_vof, "Geography (or Geographies)")
      if (length(miss_vof) > 0) {
        showNotification(paste0("VOF missing required columns: ",
                                paste(miss_vof, collapse = ", ")),
                         type = "error", duration = 15)
        return(FALSE)
      }
      rv$vof_data <- vof_raw
      set_file_meta("vof", file_row, nrow(rv$vof_data), ncol(rv$vof_data), source,
                    rule = "VOF required columns matched")
      if (!identical(source, "batch")) {
        showNotification(paste0("VOF loaded - ", format(nrow(vof_raw), big.mark = ","),
                                " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    # Keep both raw and modeled-only Details: the index needs NONE rows to
    # prevent Analytical fallback from reactivating excluded variables.
    load_details_base <- function(file_row, preloaded = NULL, source = "manual") {
      details_raw <- preloaded %||%
        data.table::fread(file_row$datapath, data.table = FALSE,
                          showProgress = FALSE)
      details_raw <- clean_data_columns(details_raw)
      sig <- validate_model_details_signature(details_raw)
      if (!isTRUE(sig$ok)) {
        showNotification(sig$reason,
                         type = "error", duration = 10)
        return(FALSE)
      }
      rv$details_raw <- tibble::as_tibble(details_raw)
      rv$details <- details_raw %>%
        dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "none"))
      set_file_meta("details", file_row, nrow(rv$details_raw), ncol(rv$details_raw), source,
                    rule = sig$reason)
      n_in <- sum(stringr::str_detect(stringr::str_to_lower(trimws(rv$details$Type)),
                                      "\\b(in|fixed)\\b"), na.rm = TRUE)
      if (!identical(source, "batch")) {
        showNotification(paste0("ModelDetails loaded - ", n_in,
                                " Type='IN'/'FIXED' variables"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    load_rois_base <- function(file_row, preloaded = NULL, source = "manual") {
      ext <- tolower(tools::file_ext(file_row$name))
      rv$channels_rois <- preloaded %||% {
        if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(file_row$datapath)
        else
          data.table::fread(file_row$datapath, data.table = FALSE, showProgress = FALSE)
      }
      rv$channels_rois <- clean_data_columns(rv$channels_rois)
      sig <- validate_rois_signature(rv$channels_rois)
      if (!isTRUE(sig$ok)) {
        rv$channels_rois <- NULL
        showNotification(sig$reason, type = "error", duration = 10)
        return(FALSE)
      }
      if (!is.null(rv$main_data)) {
        roi_cols <- names(rv$channels_rois)[
          stringr::str_detect(names(rv$channels_rois), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE))
        ]
        supported_meta <- c("MainModelVariableName", "Channel", "Geography",
                            "Sourced VariableName", "VariableSplit", "SplitOrder", roi_cols)
        candidate_keys <- setdiff(names(rv$channels_rois), supported_meta)
        unknown_keys <- setdiff(candidate_keys, names(rv$main_data))
        unknown_keys <- unknown_keys[!vapply(rv$channels_rois[unknown_keys], is.numeric, logical(1))]
        if (length(unknown_keys)) {
          showNotification(
            paste0("ROI key column(s) not found in RAE and ignored: ",
                   paste(head(unknown_keys, 5), collapse = ", "),
                   if (length(unknown_keys) > 5) " ..." else ""),
            type = "warning", duration = 12
          )
        }
      }
      set_file_meta("rois", file_row, nrow(rv$channels_rois), ncol(rv$channels_rois), source,
                    rule = sig$reason)
      if (!identical(source, "batch")) {
        showNotification(paste0("ROIs loaded - ", nrow(rv$channels_rois), " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    detect_base_file <- function(file_row) {
      ext <- tolower(tools::file_ext(file_row$name))
      nm  <- stringr::str_to_lower(file_row$name)

      if (ext == "zip") {
        df <- clean_data_columns(read_main_data(file_row$datapath, ext))
        return(list(kind = "main", data = df,
                    reason = "ZIP file read as RAE Datafile"))
      }

      if (ext == "rdata") {
        obj <- read_first_rdata_object(file_row$datapath)
        if (is.data.frame(obj)) {
          obj <- clean_data_columns(obj)
          sig <- validate_analytical_signature(obj)
          if (isTRUE(sig$ok))
            return(list(kind = "analytical", data = obj,
                        reason = sig$reason))
          return(list(kind = NA_character_, data = NULL,
                      reason = sig$reason))
        }
        return(list(kind = NA_character_, data = NULL,
                    reason = "RData first object is not a data.frame"))
      }

      if (ext %in% c("xlsx", "xls")) {
        df <- clean_data_columns(readxl::read_excel(file_row$datapath, n_max = 5))
        sig <- validate_rois_signature(df)
        if (isTRUE(sig$ok))
          return(list(kind = "rois", data = NULL,
                      reason = sig$reason))
        return(list(kind = NA_character_, data = NULL,
                    reason = sig$reason))
      }

      if (ext == "csv") {
        hdr <- read_csv_header(file_row$datapath, nrows = 5)
        cols <- names(hdr)
        if (all(REQUIRED_COLS %in% cols))
          return(list(kind = "main", data = NULL,
                      reason = "Matched REQUIRED_COLS"))
        if (all(c("AnalyticalVariableName", "MainModelVariableName",
                  "MinPeriod", "MaxPeriod") %in% cols) &&
            any(c("Geography", "Geographies") %in% cols))
          return(list(kind = "vof", data = NULL,
                      reason = "CSV VOF required columns"))
        if (all(model_details_required_cols %in% cols))
          return(list(kind = "details", data = NULL,
                      reason = "CSV ModelDetails required columns"))
        if (stringr::str_detect(nm, "model.?details"))
          return(list(kind = NA_character_, data = NULL,
                      reason = paste0("ModelDetails filename but missing columns: ",
                                      paste(setdiff(model_details_required_cols, cols),
                                            collapse = ", "))))
        roi_sig <- validate_rois_signature(hdr)
        if (isTRUE(roi_sig$ok))
          return(list(kind = "rois", data = NULL,
                      reason = roi_sig$reason))
        return(list(kind = NA_character_, data = NULL,
                    reason = "CSV columns did not match any base file signature"))
      }

      list(kind = NA_character_, data = NULL,
           reason = paste0("Unsupported extension: .", ext))
    }

    load_base_by_kind <- function(kind, file_row, preloaded = NULL, source = "manual") {
      switch(kind,
             main       = load_main_base(file_row, preloaded, source),
             analytical = load_analytical_base(file_row, preloaded, source),
             vof        = load_vof_base(file_row, preloaded, source),
             details    = load_details_base(file_row, preloaded, source),
             rois       = load_rois_base(file_row, preloaded, source),
             FALSE)
    }

    # Shared entry point for single-file uploads. Detection and validation
    # happen before the file-specific loader updates reactive state.
    load_manual_base_file <- function(kind, file_row, progress_message = NULL) {
      label <- base_file_labels[[kind]] %||% kind
      if (!operation_status$is_busy()) {
        operation_status$start(
          "setup-upload", "Loading data file",
          c("Inspecting files", "Identifying file types", "Reading data",
            "Validating contracts", "Building Media Variable Index"),
          1L, file_row$name
        )
      }
      operation_status$update("Inspecting files", 0.12, file_row$name,
                              list(operation_item(file_row$name, "Processing", label)))
      det <- tryCatch(detect_base_file(file_row), error = function(e) {
        list(kind = NA_character_, data = NULL, reason = pso_public_error(
          e, "setup.detect_file", "File type could not be identified."
        ))
      })

      if (is.na(det$kind) || !identical(det$kind, kind)) {
        detected_label <- if (!is.na(det$kind) && det$kind %in% names(base_file_labels))
          base_file_labels[[det$kind]] else "no base file type"
        reason <- paste0(
          label, " expected, but this upload matched ", detected_label,
          ". ", det$reason %||% "No matching signature."
        )
        set_manual_upload_summary(kind, file_row, FALSE, reason)
        showNotification(reason, type = "warning", duration = 10)
        operation_status$fail("The selected file was not loaded.", reason,
                              list(operation_item(file_row$name, "Failed", reason)))
        return(FALSE)
      }

      operation_status$update("Reading data", 0.45, label)
      run_load <- function() load_base_by_kind(kind, file_row, det$data, source = "manual")
      ok <- tryCatch({
        if (!is.null(progress_message)) {
          withProgress(message = progress_message, value = 0.2, {
            incProgress(0.5, detail = "Loading...")
            run_load()
          })
        } else {
          run_load()
        }
      }, error = function(e) {
        public_error <- pso_public_error(
          e, paste0("setup.upload.", kind),
          paste0(label, " could not be loaded. Check the file format and columns.")
        )
        set_manual_upload_summary(kind, file_row, FALSE, det$reason, public_error)
        showNotification(public_error, type = "error", duration = 12)
        operation_status$fail(paste0(label, " could not be loaded."), public_error,
                              list(operation_item(file_row$name, "Failed", public_error)))
        FALSE
      })

      set_manual_upload_summary(
        kind, file_row, isTRUE(ok), det$reason,
        error = if (isTRUE(ok)) NULL else paste(label, "failed validation")
      )
      if (isTRUE(ok)) {
        operation_status$update(
          "Validating contracts", 0.72, paste0(label, " loaded"),
          list(operation_item(file_row$name, "Completed", label))
        )
        if (required_files_ready()) {
          operation_status$update("Building Media Variable Index", 0.86,
                                  "Connecting the validated files")
        } else {
          operation_status$complete(
            paste0(label, " loaded successfully. Required files are still pending."),
            items = list(operation_item(file_row$name, "Completed", label))
          )
        }
      }
      ok
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
