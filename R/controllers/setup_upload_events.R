# Register file-input, batch-upload, removal, and workflow-tab observers.
# The operation modal reports long reads; upload summaries remain visible in
# Setup after the modal closes.
register_setup_upload_events <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    for (kind in base_file_kinds) {
      local({
        k <- kind
        output[[paste0(k, "_file_status")]] <- renderUI(render_loaded_file_status(k))
      })
    }
    
    # Keep downstream tabs unavailable until the required source files load.
    observe({
      session$sendCustomMessage("setTabsDisabled", list(disabled = !required_files_ready()))
    })
    
    observeEvent(input$period_preset, {
      if (!isTRUE((input$app_mode %||% "build") == "update")) {
        build_period_preset(input$period_preset %||% "last52")
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$app_mode, {
      rv$analytical_combined   <- NULL
      rv$side_mapping_nonfocus <- NULL
      rv$update_status         <- "pending"

      if (isTRUE(input$app_mode == "update")) {
        build_period_preset(input$period_preset %||% build_period_preset())
        updateRadioButtons(session, "period_preset", selected = "all")
      } else {
        updateRadioButtons(session, "period_preset", selected = build_period_preset())
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$setup_workflow_tab, {
      mode <- input$setup_workflow_tab %||% "build"
      updateTextInput(session, "app_mode", value = mode)
      if (identical(mode, "update")) {
        build_period_preset("all")
        updateRadioButtons(session, "period_preset", selected = "all")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$file_base_bundle, {
      req(input$file_base_bundle)
      files <- input$file_base_bundle
      detected <- list()
      duplicates <- list()
      unrecognized <- list()
      errors <- list()
      if (operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning")
        return()
      }
      op_items <- lapply(files$name, operation_item)
      operation_status$start(
        "setup-upload", "Loading data files",
        c("Inspecting files", "Identifying file types", "Reading data",
          "Validating contracts", "Building Media Variable Index"),
        nrow(files), paste0(nrow(files), " file(s) received")
      )

      withProgress(message = "Detecting base files...", value = 0, {
        for (i in seq_len(nrow(files))) {
          file_row <- files[i, , drop = FALSE]
          op_items[[i]] <- operation_item(file_row$name, "Processing", "Identifying file type")
          operation_status$update("Identifying file types", 0.08 + 0.22 * i / nrow(files),
                                  file_row$name, op_items)
          incProgress(0.05, detail = paste("Inspecting", file_row$name))
          det <- tryCatch(detect_base_file(file_row), error = function(e) {
            list(kind = NA_character_, data = NULL, reason = pso_public_error(
              e, "setup.detect_file", "File type could not be identified."
            ))
          })

          if (is.na(det$kind) || !det$kind %in% base_file_kinds) {
            unrecognized[[length(unrecognized) + 1L]] <- list(
              file = file_row$name,
              ext = tolower(tools::file_ext(file_row$name)),
              reason = det$reason %||% "Unsupported or missing required columns",
              source = "Base Files"
            )
            op_items[[i]] <- operation_item(file_row$name, "Review", "File type not recognized")
            next
          }
          if (!is.null(detected[[det$kind]])) {
            duplicates[[length(duplicates) + 1L]] <- list(
              file = file_row$name,
              kind = det$kind,
              label = base_file_labels[[det$kind]],
              kept_file = detected[[det$kind]]$file$name,
              reason = det$reason %||% "Duplicate detected type"
            )
            op_items[[i]] <- operation_item(file_row$name, "Skipped", "Duplicate file type")
            next
          }
          detected[[det$kind]] <- list(file = file_row, data = det$data,
                                       reason = det$reason)
        }

        for (kind in base_file_kinds) {
          item <- detected[[kind]]
          if (is.null(item)) next
          incProgress(0.10, detail = paste("Loading", base_file_labels[[kind]]))
          item_idx <- match(item$file$name, files$name)
          operation_status$update("Reading data", 0.35 + 0.30 * which(base_file_kinds == kind) / length(base_file_kinds),
                                  base_file_labels[[kind]], op_items)
          ok <- tryCatch(load_base_by_kind(kind, item$file, item$data, source = "batch"),
                         error = function(e) {
                           errors[[length(errors) + 1L]] <<- list(
                             file = item$file$name,
                             kind = kind,
                             label = base_file_labels[[kind]],
                             error = pso_public_error(
                               e, paste0("setup.batch_upload.", kind),
                               "File could not be loaded. Check its format and columns."
                             )
                           )
                           FALSE
                         })
          if (!isTRUE(ok)) {
            if (!any(vapply(errors, function(x) identical(x$file, item$file$name), logical(1)))) {
              errors[[length(errors) + 1L]] <- list(
                file = item$file$name,
                kind = kind,
                label = base_file_labels[[kind]],
                error = paste(base_file_labels[[kind]], "failed validation")
              )
            }
            op_items[[item_idx]] <- operation_item(item$file$name, "Failed", paste(base_file_labels[[kind]], "failed validation"))
          } else {
            op_items[[item_idx]] <- operation_item(item$file$name, "Completed", base_file_labels[[kind]])
          }
        }

        set_upload_issues(
          unrecognized = unrecognized,
          duplicates = duplicates,
          errors = errors
        )

        has_review <- length(unrecognized) > 0 || length(duplicates) > 0 || length(errors) > 0
        operation_status$update("Validating contracts", 0.76,
                                "Reviewing loaded file contracts", op_items)
        if (required_files_ready()) {
          operation_status$update("Building Media Variable Index", 0.86,
                                  "Connecting the validated files", op_items)
        } else {
          warnings <- c(
            if (length(unrecognized)) paste0(length(unrecognized), " file(s) not recognized"),
            if (length(duplicates)) paste0(length(duplicates), " duplicate file(s) skipped"),
            if (length(errors)) paste0(length(errors), " file(s) failed validation"),
            "Required files are still missing"
          )
          operation_status$complete(
            paste0(sum(vapply(op_items, function(x) x$status == "Completed", logical(1))),
                   " file(s) loaded."),
            warnings = warnings, items = op_items
          )
        }
      })
    }, ignoreInit = TRUE)

    observeEvent(input$remove_base_file, {
      kind <- input$remove_base_file
      req(kind %in% base_file_kinds)
      if (isTRUE(clear_base_file(kind))) {
        showNotification("File removed. Upload it again to replace it.",
                         type = "message", duration = 3)
      }
    }, ignoreInit = TRUE)
    
    # Individual upload handlers share the same detection and contract path.
    observeEvent(input$file_main, {
      req(input$file_main)
      size <- round(input$file_main$size / 1024^2, 1)
      load_manual_base_file(
        "main",
        input$file_main,
        paste0("Loading RAE Datafile (", size, " MB)...")
      )
    })
    
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      load_manual_base_file("analytical", input$file_analytical)
    })
    
    observeEvent(input$file_vof, {
      req(input$file_vof)
      load_manual_base_file("vof", input$file_vof)
    })
    
    observeEvent(input$file_details, {
      req(input$file_details)
      load_manual_base_file("details", input$file_details)
    })
    
    observeEvent(input$file_rois, {
      req(input$file_rois)
      load_manual_base_file("rois", input$file_rois)
    })
    
    observeEvent(input$file_update_zip, {
      req(input$file_update_zip$datapath)
      tryCatch({
        package <- read_model_update_zip(input$file_update_zip$datapath)
        rv$update_zip <- package
        rv$update_zip_name <- input$file_update_zip$name
        rv$past_analytical_splits <- package$analytical
        rv$past_side_mapping <- package$side_mapping
        rv$update_sap <- model_update_sap_by_channel(package$metadata)
        rv$analytical_combined <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status <- "pending"
        if (!is.null(rv$mainvars_mapping)) {
          inferred <- infer_previous_update_id(rv$past_side_mapping, rv$mainvars_mapping)
          if (nzchar(inferred)) updateTextInput(session, "past_update_id", value = inferred)
        }
        showNotification("Previous results ZIP validated and loaded.",
                         type = "message", duration = 5)
      }, error = function(e) {
        rv$update_zip <- NULL
        rv$past_analytical_splits <- NULL
        rv$past_side_mapping <- NULL
        rv$update_sap <- list()
        rv$update_status <- "error"
        showNotification(pso_public_error(
          e, "setup.results_zip", "Results ZIP could not be loaded. Check the package files."
        ),
                         type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)

    for (kind in base_file_kinds) {
      local({
        k <- kind
        observeEvent(input[[paste0("update_file_", k)]], {
          file_row <- input[[paste0("update_file_", k)]]
          req(file_row)
          load_manual_base_file(k, file_row)
        }, ignoreInit = TRUE)
      })
    }
    
    # MainVars mappings can supply update identifiers from their column names.
    observeEvent(input$file_mainvars_mapping, {
      req(input$file_mainvars_mapping)
      tryCatch({
        df <- readxl::read_excel(input$file_mainvars_mapping$datapath)
        rv$mainvars_mapping      <- df
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status         <- "pending"
        
        inferred <- infer_previous_update_id(rv$past_side_mapping, df)
        if (nzchar(inferred)) updateTextInput(session, "past_update_id", value = inferred)
        showNotification(paste0("MainVars Mapping loaded - ",
                                format(nrow(df), big.mark = ","), " rows, ", ncol(df),
                                " columns.", if (nzchar(inferred))
                                  paste0(" Previous ID detected: ", inferred, ".") else
                                    " Previous ID needs review."),
                         type = if (nzchar(inferred)) "message" else "warning", duration = 6)
      }, error = \(e) showNotification(pso_public_error(
        e, "setup.mainvars", "MainVars Mapping could not be loaded. Check the workbook."
      ),
                                       type = "error", duration = 10))
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
