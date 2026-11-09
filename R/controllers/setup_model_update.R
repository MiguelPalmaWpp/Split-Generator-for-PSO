# Register the Model Update workflow: identify prior ZIP history, map current
# variables, and prepare inherited SAP. The UI still marks this flow as under
# development pending representative-data validation.
register_setup_model_update <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Use the ZIP-derived ID only when unambiguous; otherwise require input.
    resolved_previous_update_id <- function() {
      manual <- trimws(input$past_update_id %||% "")
      if (nzchar(manual)) return(manual)
      if (is.null(rv$update_zip) || is.null(rv$mainvars_mapping)) return("")
      infer_previous_update_id(rv$past_side_mapping, rv$mainvars_mapping)
    }

    update_inputs_ready <- reactive({
      previous_id <- resolved_previous_update_id()
      isTRUE(input$app_mode == "update")               &&
        !is.null(rv$update_zip)                         &&
        !is.null(rv$past_analytical_splits)            &&
        !is.null(rv$past_side_mapping)                 &&
        !is.null(rv$mainvars_mapping)                  &&
        !is.null(rv$analytical)                        &&
        nzchar(previous_id) && previous_id %in% names(rv$mainvars_mapping) &&
        nzchar(trimws(input$current_update_id %||% "")) &&
        trimws(input$current_update_id) %in% names(rv$mainvars_mapping) &&
        nzchar(trimws(input$update_label_update %||% "")) &&
        required_files_ready()
    })

    effective_update_sap <- reactive({
      if (!identical(input$app_mode, "update")) return(list())
      previous_id <- resolved_previous_update_id()
      relabeled <- relabel_model_update_sap(
        rv$update_sap, rv$update_zip$previous_label %||% "",
        input$update_label_update %||% ""
      )
      remap_model_update_sap(
        relabeled, rv$update_zip$metadata %||% NULL, rv$mainvars_mapping,
        previous_id,
        trimws(input$current_update_id %||% ""),
        rv$media_index$channels %||% list()
      )
    })
    
    observeEvent(update_inputs_ready(), {
      if (!update_inputs_ready()) {
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        if (rv$update_status != "pending") rv$update_status <- "pending"
      }
    }, ignoreNULL = FALSE)

    observeEvent(list(input$past_update_id, input$current_update_id,
                      input$update_label_update), {
      reset_update_outputs()
    }, ignoreInit = TRUE)

    observeEvent(input$btn_process_update, {
      if (!update_inputs_ready()) {
        showNotification("Load the ZIP, current model files, and update identifiers first.",
                         type = "warning", duration = 6)
        return()
      }
      if (isolate(isTRUE(upd_processing()))) return()
      upd_processing(TRUE)
      rv$update_status <- "processing"
      
      tryCatch({
        past_upd_id    <- resolved_previous_update_id()
        past_lbl       <- rv$update_zip$previous_label %||% ""
        current_upd_id <- trimws(input$current_update_id)
        current_lbl    <- trimws(input$update_label_update)
        
        if (!past_upd_id %in% names(rv$mainvars_mapping)) {
          showNotification(paste0("Column '", past_upd_id, "' not found in MainVars Mapping. ",
                                  "Available: ", paste(names(rv$mainvars_mapping), collapse = ", ")),
                           type = "error", duration = 10)
          rv$update_status <- "error"; upd_processing(FALSE); return()
        }
        if (!current_upd_id %in% names(rv$mainvars_mapping)) {
          showNotification(paste0("Column '", current_upd_id, "' not found in MainVars Mapping. ",
                                  "Available: ", paste(names(rv$mainvars_mapping), collapse = ", ")),
                           type = "error", duration = 10)
          rv$update_status <- "error"; upd_processing(FALSE); return()
        }
        
        withProgress(message = "Processing Model Update...", value = 0, {
          
          incProgress(0.15, detail = "Joining side mappings...")
          side_mapping_joined <- rv$past_side_mapping %>%
            dplyr::mutate(
              past_var = stringr::str_remove(MainModelVariableName, stringr::fixed("____"))) %>%
            dplyr::left_join(rv$mainvars_mapping, by = setNames(past_upd_id, "past_var")) %>%
            dplyr::filter(!is.na(.data[[current_upd_id]])) %>%
            dplyr::mutate(NewSplitName = model_update_history_name(
              VariableSplit, past_lbl, current_lbl))
          
          if (!nrow(side_mapping_joined)) {
            showNotification(paste0("No matching variables. Verify Past Update ID '",
                                    past_upd_id, "' matches MainModelVariableName values."),
                             type = "warning", duration = 12)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          n_matched <- nrow(side_mapping_joined)
          incProgress(0.15, detail = paste0("Found ", n_matched, " variables to rename..."))
          
          cross_id <- c(rv$cross_cols %||% "Geography", "Period")
          id_cols  <- intersect(cross_id, names(rv$past_analytical_splits))
          if (!length(id_cols)) {
            showNotification("No cross-sectional columns found in Past Analytical Splits.",
                             type = "error", duration = 10)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          metadata_sap <- rv$update_zip$metadata
          sap_components <- if (!is.null(metadata_sap) && nrow(metadata_sap)) {
            unlist(lapply(metadata_sap$Splits[metadata_sap$Type == "Merge"], function(x) {
              trimws(strsplit(as.character(x), " ||| ", fixed = TRUE)[[1]])
            }), use.names = FALSE)
          } else character(0)
          history_split_names <- unique(c(side_mapping_joined$VariableSplit,
                                           sap_components,
                                           metadata_sap$Name[metadata_sap$Type == "Merge"]))
          splits_available <- intersect(history_split_names,
                                        names(rv$past_analytical_splits))
          if (!length(splits_available)) {
            showNotification("No matching split columns found in Past Analytical Splits.",
                             type = "error", duration = 10)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          # Reject duplicate split names before joining historical and current data.
          existing_split_cols <- setdiff(names(rv$analytical), id_cols)
          new_split_names     <- unique(side_mapping_joined$NewSplitName)
          split_conflicts     <- intersect(new_split_names, existing_split_cols)
          if (length(split_conflicts) > 0) {
            showNotification(
              paste0(length(split_conflicts), " past split name(s) conflict with ",
                     "existing analytical columns: ",
                     paste(head(split_conflicts, 3), collapse = ", "),
                     if (length(split_conflicts) > 3)
                       paste0(" ... +", length(split_conflicts) - 3) else ""),
              type = "warning", duration = 12)
          }
          
          incProgress(0.25, detail = paste0("Renaming ", length(splits_available),
                                            " past splits to Before ", current_lbl, "..."))
          
          # Roll historical and current versions into the shared Before split.
          history_name_map <- data.frame(
            VariableSplit = splits_available,
            NewSplitName = model_update_history_name(splits_available, past_lbl, current_lbl),
            stringsAsFactors = FALSE
          )
          analytical_nonfocus <- rv$past_analytical_splits %>%
            dplyr::select(dplyr::all_of(c(id_cols, splits_available))) %>%
            tidyr::pivot_longer(cols = -dplyr::all_of(id_cols),
                                names_to = "VariableSplit", values_to = "Value") %>%
            dplyr::left_join(history_name_map, by = "VariableSplit") %>%
            tidyr::pivot_wider(id_cols     = dplyr::all_of(id_cols),
                               names_from  = NewSplitName,
                               values_from = Value,
                               values_fn   = sum)
          
          # Resolve duplicate column names before joining the two periods.
          id_cols_an    <- intersect(id_cols, names(rv$analytical))
          incoming_cols <- setdiff(names(analytical_nonfocus), id_cols_an)
          col_conflicts <- intersect(setdiff(names(rv$analytical), id_cols_an), incoming_cols)
          if (length(col_conflicts) > 0) {
            showNotification(
              paste0(length(col_conflicts), " column(s) already exist in Analytical ",
                     "and will be skipped from past splits: ",
                     paste(head(col_conflicts, 3), collapse = ", "),
                     if (length(col_conflicts) > 3)
                       paste0(" ... +", length(col_conflicts) - 3) else ""),
              type = "warning", duration = 12)
            # Current Analytical values take precedence for conflicting columns.
            analytical_nonfocus <- analytical_nonfocus %>%
              dplyr::select(-dplyr::any_of(col_conflicts))
          }
          
          n_nonfocus_cols <- ncol(analytical_nonfocus) - length(id_cols_an)
          incProgress(0.25, detail = paste0("Joining ", nrow(analytical_nonfocus),
                                            " rows x ", n_nonfocus_cols,
                                            " non-focus columns..."))
          
          rv$analytical_combined <- rv$analytical %>%
            dplyr::left_join(analytical_nonfocus, by = id_cols_an)
          
          incProgress(0.15, detail = "Building non-focus side mapping...")
          rv$side_mapping_nonfocus <- side_mapping_joined %>%
            dplyr::distinct(NewSplitName, .keep_all = TRUE) %>%
            dplyr::select(NewSplitName, dplyr::all_of(current_upd_id),
                          dplyr::any_of(c("Weight", "MinWeight", "MaxWeight", "rank"))) %>%
            dplyr::rename(VariableSplit         = NewSplitName,
                          MainModelVariableName = dplyr::all_of(current_upd_id))
          
          incProgress(0.05, detail = "Done!")
          rv$update_status <- "done"
          showNotification(
            paste0("Model Update processed: ",
                   nrow(rv$side_mapping_nonfocus), " non-focus splits added (",
                   ncol(rv$analytical_combined) - ncol(rv$analytical),
                   " new columns)."),
            type = "message", duration = 6)
        })
        
      }, error = function(e) {
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status         <- "error"
        showNotification(pso_public_error(
          e, "setup.model_update", "Model Update could not be completed. Review its inputs."
        ),
                         type = "error", duration = 12)
      })
      upd_processing(FALSE)
    })
    
    output$update_status_badge <- renderUI({
      switch(rv$update_status,
             pending    = tags$span(class = "badge-not-ready",
                                    icon("clock", class = "icon-xs"), " In development"),
             processing = tags$span(class = "badge-not-ready",
                                    icon("spinner", class = "icon-xs"), " Processing..."),
             done       = tags$span(class = "badge-ready",
                                    icon("circle-check", class = "icon-xs"), " Ready"),
             error      = tags$span(class = "badge-error",
                                    icon("circle-xmark", class = "icon-xs"), " Error"),
             NULL)
    })
    
    output$update_processing_ui <- renderUI({
      status <- rv$update_status
      if (status == "pending") return(NULL)
      if (status == "processing")
        return(div(class = "alert alert-info alert-sm p-2 mt-2",
                   icon("spinner"), " Processing past splits..."))
      if (status == "error")
        return(div(class = "alert alert-danger alert-sm p-2 mt-2",
                   "Processing failed. Check Update IDs and file columns."))
      if (status == "done" && !is.null(rv$side_mapping_nonfocus))
        return(div(class = "alert alert-success alert-sm p-2 mt-2",
                   div(class = "d-flex gap-3",
                       tags$span(icon("circle-check"),
                                 paste0(" ", nrow(rv$side_mapping_nonfocus),
                                        " non-focus splits")),
                       tags$span(icon("table-columns"),
                                 paste0(" ", ncol(rv$analytical_combined) -
                                          ncol(rv$analytical), " columns added")))))
      NULL
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
