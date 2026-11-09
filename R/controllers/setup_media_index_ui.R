# Register Media Variable Index rebuild and display. Setup owns the reactive
# publication/cache; the service computes channels from raw ModelDetails.
register_setup_media_index_ui <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    observe({
      req(rv$main_data, rv$analytical, rv$vof_data, rv$details)
      if (isolate(isTRUE(mi_building()))) return()
      sig <- media_index_signature()
      if (!is.null(isolate(mi_signature())) && identical(isolate(mi_signature()), sig) &&
          !is.null(isolate(rv$media_index))) {
        return()
      }
      mi_building(TRUE)
      tryCatch({
        cache_key <- pso_cache_key("media-index", sig)
        mi <- pso_cache_get(performance_cache, cache_key)
        if (is.null(mi)) {
          mi <- pso_profile(
            "setup.media_index",
            build_media_index(main_data       = rv$main_data,
                              analytical      = rv$analytical,
                              vof_df          = rv$vof_data,
                              model_details   = rv$details_raw,
                              channels_rois   = rv$channels_rois,
                              cross_cols      = rv$cross_cols %||% "Geography",
                              schema_metadata = rv$schema_metadata)
          )
          pso_cache_set(performance_cache, cache_key, mi)
        }
        index_issue <- validate_media_index_result(mi)
        if (!is.null(index_issue)) stop(index_issue, call. = FALSE)
        rv$media_index <- mi
        mi_signature(sig)
        if (identical(operation_status$current_id(), "setup-upload")) {
          mi_warnings <- unlist(mi$warnings %||% character(0), use.names = FALSE)
          operation_status$complete(
            paste0("Files validated and Media Variable Index built with ",
                   mi$summary$total_channels %||% 0L, " channel(s)."),
            warnings = mi_warnings
          )
        }
        if (mi$summary$total_channels > 0)
          showNotification(paste0("Media Index - ", mi$summary$total_channels,
                                  " channels (", mi$summary$from_vof, " VOF, ",
                                  mi$summary$from_fallback, " keyword)"),
                           type = "message", duration = 5)
        else
          showNotification("Media Index built but 0 channels found.",
                           type = "warning", duration = 8)
      }, error = function(e) {
        rv$media_index <- NULL
        mi_signature(NULL)
        public_error <- pso_public_error(
          e, "setup.media_index", "Media Variable Index could not be built. Review the loaded files."
        )
        showNotification(public_error, type = "error", duration = 10)
        if (identical(operation_status$current_id(), "setup-upload")) {
          operation_status$fail("Media Variable Index could not be built.", public_error)
        }
      })
      mi_building(FALSE)
    })
    
    output$media_index_display <- renderUI({
      mi_building()
      if (isTRUE(mi_building()))
        return(div(class = "mi-box-pending",
                   div(class = "mi-box-pending-inner", tags$span("Building Media Index..."))))
      mi <- rv$media_index
      if (is.null(mi))
        return(div(class = "mi-box-na",
                   div(class = "mi-box-na-inner",
                       tags$span("Upload the 4 required files to auto-build the index. ROIs by Channel can be added for ROI values."))))
      if (mi$summary$total_channels == 0)
        return(div(class = "alert alert-warning alert-sm p-3",
                   "No channels found. Check ModelDetails has IN/FIXED variables."))
      summary <- mi$summary
      cm <- mi$connection_map
      issue_rows <- if (!is.null(cm) && nrow(cm))
        cm[cm$ConnectionStatus != "Matched", , drop = FALSE] else NULL
      issue_count <- if (is.null(issue_rows)) 0L else nrow(issue_rows)
      xs <- summary$xs_dims %||% character(0)
      key <- summary$useful_long %||% character(0)
      disc <- summary$discarded_long %||% character(0)
      metric_card <- function(value, label, icon_name, tone = "neutral") {
        div(class = paste("mi-kpi", paste0("is-", tone)),
            div(class = "mi-kpi-icon", icon(icon_name)),
            div(class = "mi-kpi-copy",
                tags$strong(value, class = "mi-kpi-value"),
                tags$span(label, class = "mi-kpi-label")))
      }
      div(
        class = "mi-overview",
        div(class = "mi-overview-head",
            div(
              tags$span("CONNECTION MAP", class = "mi-eyebrow"),
              tags$strong(paste0(summary$total_channels, " channels ready"),
                          class = "mi-overview-title"),
              tags$span("Model variables connected to RAE sources and ROI metadata.",
                        class = "mi-overview-subtitle")),
            tags$span(if (issue_count) paste0(issue_count, " to review") else "All connections ready",
                      class = paste("mi-status-pill", if (issue_count) "is-review" else "is-ready"))),
        div(class = "mi-kpi-grid",
            metric_card(summary$total_channels, "Channels", "layer-group", "primary"),
            metric_card(summary$connection_matched %||% 0, "Matched", "link", "success"),
            metric_card(paste0(summary$with_roi, "/", summary$total_channels),
                        "ROI coverage", "chart-line",
                        if ((summary$with_roi %||% 0) == summary$total_channels) "success" else "neutral"),
            metric_card(issue_count, "Needs review", "triangle-exclamation",
                        if (issue_count) "warning" else "success")),
        div(class = "mi-source-strip",
            tags$span(class = "mi-source-label", "Sources"),
            tags$span(class = "mi-chip", paste0(summary$from_vof, " VOF")),
            if ((summary$from_fallback %||% 0) > 0)
              tags$span(class = "mi-chip", paste0(summary$from_fallback, " fallback")),
            tags$span(class = "mi-chip", paste0(summary$details_modelled_variables %||% 0,
                                                  " modeled")),
            if ((summary$vof_rows_discarded %||% 0) > 0)
              tags$span(class = "mi-chip is-muted",
                        paste0(summary$vof_rows_discarded, " VOF excluded")),
            tags$span(class = "mi-chip is-muted",
                      paste0(summary$details_inactive_variables %||% 0, " inactive")),
            tags$span(class = "mi-key-type", paste0("Key: ", summary$var_key_type))),
        if (length(xs) || length(key) || length(disc))
          div(class = "mi-schema-line",
              if (length(xs))
                div(tags$span("Cross-section", class = "mi-schema-label"),
                    tags$strong(paste(xs, collapse = ", "))),
              if (length(key))
                div(tags$span("Split key", class = "mi-schema-label"),
                    tags$strong(paste(key, collapse = ", "))),
              if (length(disc))
                div(class = "mi-schema-muted",
                    tags$span("Excluded totals", class = "mi-schema-label"),
                    tags$span(paste(disc, collapse = ", ")))),
        if (issue_count > 0) {
          shown <- head(issue_rows, 4)
          div(class = "mi-review-panel",
              div(class = "mi-review-head",
                  div(icon("triangle-exclamation"),
                      tags$strong("Connection review"),
                      tags$span(paste0(issue_count, " unresolved"))),
                  if (issue_count > nrow(shown))
                    tags$span(paste0("+", issue_count - nrow(shown), " more"),
                              class = "mi-review-more")),
              div(class = "mi-review-list",
                  lapply(seq_len(nrow(shown)), function(i) {
                    div(class = "mi-review-row",
                        tags$span(shown$ConnectionStatus[i], class = "mi-review-status"),
                        tags$span(shown$MainModelVariableName[i], class = "mi-review-name"))
                  })))
        }
      )
    })

    output$model_update_summary <- renderUI({
      pkg <- rv$update_zip
      if (is.null(pkg)) return(div(class = "alert alert-secondary alert-sm",
                                   "Load the previous results ZIP and current model files to review readiness."))
      current_id <- trimws(input$current_update_id %||% "")
      previous_id <- resolved_previous_update_id()
      inherited_sap <- effective_update_sap()
      inherited_channels <- names(inherited_sap)
      current_channels <- names(rv$media_index$channels %||% list())
      missing_channels <- if (length(current_channels))
        setdiff(inherited_channels, current_channels) else character(0)
      unresolved <- attr(inherited_sap, "unresolved_channels") %||% character(0)
      div(class = "alert alert-light border mt-2",
          tags$strong("Previous package validated"),
          tags$ul(class = "mb-1 mt-2",
            tags$li(paste0("ZIP: ", rv$update_zip_name)),
            tags$li(paste0("Historical label: ",
                            if (nzchar(pkg$previous_label)) pkg$previous_label else "not detected")),
            tags$li(paste0("Previous ID: ",
                            if (nzchar(previous_id)) previous_id else "needs review")),
            tags$li(paste0("Current ID: ", if (nzchar(current_id)) current_id else "pending")),
            tags$li(paste0("SAP: ", length(inherited_channels), " channel(s), ",
                            sum(lengths(inherited_sap)), " aggregation(s)")),
            if (length(missing_channels) || length(unresolved))
              tags$li(class = "text-warning",
                      paste0("Channels requiring review: ",
                             paste(unique(c(missing_channels, unresolved)), collapse = ", "))),
            if (sum(lengths(inherited_sap)) > 0L)
              tags$li("SAP split references are validated during Process; unmatched rules remain in Review.")
          ))
    })

    output$update_action_ui <- renderUI({
      ready <- isTRUE(tryCatch(update_inputs_ready(), error = function(e) FALSE))
      actionButton(ns("btn_process_update"), "Prepare Model Update",
                   class = "btn-primary", icon = icon("gears"),
                   disabled = if (!ready || isTRUE(upd_processing())) "disabled")
    })
    
    output$update_label_ui <- renderUI({
      mode   <- input$app_mode %||% "build"
      preset <- input$period_preset %||% "last52"
      if (mode == "update") {
        return(NULL)
      } else {
        if (preset == "all") return(NULL)
        val <- switch(preset, last52 = "Last52w", last13 = "Last13w",
                      custom = isolate(input$update_label %||% "Last52w"))
        textInput(ns("update_label"), "Update Label", value = val)
      }
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
