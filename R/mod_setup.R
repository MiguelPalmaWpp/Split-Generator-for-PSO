# -----------------------------------------------------------------------
# R/mod_setup.R
# Setup owns file intake, shared configuration, index lifecycle, and
# cross-file validation. Index construction lives in a pure domain service.
# -----------------------------------------------------------------------

mod_setup_ui <- function(id) build_setup_ui(id)

# -----------------------------------------------------------------------
# Server: own uploaded sources and return data/config/index reactives to the
# other modules. Controllers register in dependency order below.
# -----------------------------------------------------------------------
mod_setup_server <- function(id, performance_cache = NULL, operation_status = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    performance_cache <- ensure_performance_cache(performance_cache)
    operation_status <- operation_status %||% new_operation_status(session)
    
    rv <- reactiveValues(
      main_data              = NULL,
      main_data_indexed      = NULL,
      analytical             = NULL,
      analytical_rag         = NULL,
      dates_df               = NULL,
      details_raw            = NULL,
      details                = NULL,
      channels_rois          = NULL,
      vof_data               = NULL,
      cross_cols             = NULL,
      schema_metadata        = NULL,
      validation_status      = "pending",
      media_index            = NULL,
      update_zip             = NULL,
      update_sap             = list(),
      update_zip_name        = "",
      past_analytical_splits = NULL,
      past_side_mapping      = NULL,
      mainvars_mapping       = NULL,
      analytical_combined    = NULL,
      side_mapping_nonfocus  = NULL,
      update_status          = "pending",
      file_meta              = list(),
      file_signatures        = list(),
      file_contracts         = list(),
      cross_file_warnings    = list(),
      upload_issues          = NULL
    )
    
    mi_building    <- reactiveVal(FALSE)
    mi_signature   <- reactiveVal(NULL)
    upd_processing <- reactiveVal(FALSE)
    build_period_preset <- reactiveVal("last52")
    controller_context <- list(
      scope = environment(), input = input, output = output, session = session,
      ns = ns, rv = rv, performance_cache = performance_cache,
      operation_status = operation_status, mi_building = mi_building,
      mi_signature = mi_signature, upd_processing = upd_processing,
      build_period_preset = build_period_preset
    )
    
    register_setup_contracts(controller_context)

    register_setup_file_intake(controller_context)

    register_setup_upload_events(controller_context)
    
    # Model Update combines current-period data with prior splits and resolves
    # naming conflicts before producing the combined Analytical dataset.
    register_setup_model_update(controller_context)
    
    # Build the Media Variable Index from the current files and raw Details.
    register_setup_media_index_ui(controller_context)
    
    output$custom_dates_ui <- renderUI({
      req(input$period_preset == "custom",
          (input$app_mode %||% "build") != "update")
      default_start <- if (!is.null(rv$dates_df)) {
        s <- sort(rv$dates_df$Period)
        if (length(s) >= 52) s[length(s) - 51] else s[1]
      } else Sys.Date() - 365
      default_end <- if (!is.null(rv$dates_df)) max(rv$dates_df$Period) else Sys.Date()
      div(class = "mt-2",
          layout_columns(col_widths = c(6, 6),
                         dateInput(ns("start_report_date"), "Start Date",
                                   value = default_start),
                         dateInput(ns("end_report_date"),   "End Date",
                                   value = default_end)))
    })
    
    period_dates <- reactive({
      preset <- input$period_preset %||% "last52"
      if (preset == "custom") {
        req(input$start_report_date, input$end_report_date)
        return(list(start = as.Date(input$start_report_date),
                    end   = as.Date(input$end_report_date)))
      }
      req(rv$dates_df)
      s <- sort(rv$dates_df$Period); n <- length(s); mx <- s[n]
      switch(preset,
             last52 = list(start = if (n >= 52) s[n-51] else s[1], end = mx),
             last13 = list(start = if (n >= 13) s[n-12] else s[1], end = mx),
             all    = list(start = s[1], end = mx))
    })

    period_preset_label <- function(preset) {
      switch(preset %||% "last52",
             last52 = "Last 52w",
             last13 = "Last 13w",
             all = "All Period",
             custom = "Custom",
             preset %||% "Last 52w")
    }

    output$reporting_period_preview <- renderUI({
      mode <- input$app_mode %||% "build"
      if (mode == "update") return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates) || is.null(dates$start) || is.null(dates$end)) {
        return(div(class = "global-muted-box period-preview period-preview-empty",
                   div(class = "global-muted-box-inner",
                       tags$span(class = "global-muted-title", "Reporting period"),
                       tags$span(class = "global-muted-text",
                                 "Upload Analytical Dataset to preview the reporting dates."))))
      }
      periods <- if (!is.null(rv$dates_df) && "Period" %in% names(rv$dates_df)) {
        p <- rv$dates_df$Period
        sum(p >= dates$start & p <= dates$end, na.rm = TRUE)
      } else NA_integer_
      div(class = "global-muted-box period-preview",
          div(class = "global-muted-box-inner",
              tags$span(class = "global-muted-title", "Reporting period"),
              tags$span(class = "code-tag-blue",
                        period_preset_label(input$period_preset %||% "last52")),
              tags$span(class = "code-tag",
                        paste0(format(dates$start), " -> ", format(dates$end))),
              tags$span(class = "global-muted-text",
                        if (!is.na(periods)) paste0(format(periods, big.mark = ","), " period(s)") else "periods pending")))
    })

    is_vof_style_value <- function(x) {
      x_chr <- trimws(as.character(x))
      is.na(x) | !nzchar(x_chr) | toupper(x_chr) %in% "<--VOF-->"
    }

    weight_smart_enabled <- function() {
      val <- input$weight_smart_filter
      is.null(val) || isTRUE(val) || identical(val, "smart")
    }

    weight_variable_candidates <- reactive({
      details <- rv$details_raw
      an <- rv$analytical
      if (is.null(details) || is.null(an) || !"VariableName" %in% names(details)) {
        return(list(candidates = character(0), all_available = character(0), smart_empty = TRUE))
      }
      details <- as.data.frame(details)
      vars <- unique(trimws(as.character(details$VariableName)))
      vars <- vars[!is.na(vars) & nzchar(vars)]
      all_available <- intersect(vars, names(an))

      smart_filter <- weight_smart_enabled()
      if (!smart_filter) {
        return(list(candidates = all_available, all_available = all_available, smart_empty = FALSE))
      }

      has_dims <- all(c("Campaign", "Outlet", "Creative") %in% names(details))
      if (has_dims) {
        vof_rows <- is_vof_style_value(details$Campaign) &
          is_vof_style_value(details$Outlet) &
          is_vof_style_value(details$Creative)
      } else {
        vof_rows <- rep(TRUE, nrow(details))
      }
      weight_rows <- stringr::str_detect(
        trimws(as.character(details$VariableName)),
        stringr::regex("Weight", ignore_case = TRUE)
      )
      candidates <- unique(trimws(as.character(details$VariableName[vof_rows & weight_rows])))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      candidates <- intersect(candidates, names(an))
      list(candidates = candidates, all_available = all_available, smart_empty = !length(candidates))
    })

    observe({
      cands <- weight_variable_candidates()$candidates
      selected <- input$weight_variable_name %||% ""
      preferred <- intersect("Weight Variable MMM", cands)
      next_selection <- if (length(preferred)) preferred[1] else if (length(cands)) cands[1] else ""
      if (nzchar(next_selection) && !identical(selected, next_selection)) {
        updateSelectizeInput(session, "weight_variable_name", selected = next_selection)
      }
    })

    output$weight_variable_ui <- renderUI({
      info <- weight_variable_candidates()
      choices <- info$candidates
      smart_enabled <- weight_smart_enabled()
      if (!length(choices) && !smart_enabled) {
        choices <- info$all_available
      }
      selected <- input$weight_variable_name %||% ""
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- if ("Weight Variable MMM" %in% choices) "Weight Variable MMM"
        else if (length(choices)) choices[1] else ""
      }
      div(class = "global-muted-box weight-smart-box",
          div(class = "weight-smart-header",
              div(class = "weight-smart-title",
                  tags$strong("Weight Variable MMM")),
              tags$span("Required", class = "weight-required-badge")),
          div(class = "weight-smart-grid",
              div(class = "weight-smart-field",
                  tags$label("Weight variable", class = "weight-smart-label"),
                  selectizeInput(ns("weight_variable_name"), NULL,
                                 choices = choices, selected = selected,
                                 options = list(placeholder = "Select Weight variable..."))),
              div(class = "weight-smart-field weight-smart-mode",
                  tags$label("Search mode", class = "weight-smart-label"),
                  div(class = "ds-pill-group weight-mode-pills",
                      radioButtons(ns("weight_smart_filter"), NULL,
                                   choices = c("Smart Filter" = "smart",
                                               "All Details variables" = "all"),
                                   selected = if (smart_enabled) "smart" else "all",
                                   inline = TRUE)))),
          div(class = "global-muted-text weight-smart-note",
              "Used in analytical_splits_extended when the selected column exists in Analytical."),
          if (smart_enabled && isTRUE(info$smart_empty))
            div(class = "weight-smart-warning alert alert-warning alert-sm",
                "No Weight candidates found with VOF-style details. Switch Search mode to All Details variables."),
          if (nzchar(selected) && !is.null(rv$analytical) && !selected %in% names(rv$analytical))
            div(class = "weight-smart-warning alert alert-warning alert-sm",
                "Selected variable is not in Analytical and will not be exported."))
    })
    
    # Compare loaded file contracts using the active Build or Update rules.
    register_setup_validation_views(controller_context)
    
    # Return the shared data and configuration reactives to the app server.
    list(
      data = reactive({
        mode <- input$app_mode %||% "build"
        list(
          all_rags              = rv$main_data,
          all_rags_indexed      = rv$main_data_indexed,
          data_signature        = media_index_signature(),
          analytical            = if (mode == "update" && !is.null(rv$analytical_combined))
            rv$analytical_combined else rv$analytical,
          analytical_rag        = rv$analytical_rag,
          dates_df              = rv$dates_df,
          details_raw           = rv$details_raw,
          details               = rv$details,
               channels_rois         = rv$channels_rois,
               vof_data              = rv$vof_data,
               schema_metadata       = rv$schema_metadata,
               variable_connection_map = rv$media_index$connection_map %||% NULL,
               variable_role_map       = rv$media_index$role_map %||% list(),
               file_contracts        = rv$file_contracts,
               cross_file_warnings   = rv$cross_file_warnings,
          side_mapping_nonfocus = if (mode == "update") rv$side_mapping_nonfocus else NULL,
          app_mode              = mode)
      }),
      config = reactive({
        preset <- input$period_preset %||% "last52"
        mode   <- input$app_mode %||% "build"
        dates  <- tryCatch(period_dates(), error = \(e) NULL)
        list(update_label      = if (mode == "update") input$update_label_update %||% ""
             else if (preset == "all") "" else (input$update_label %||% "Last52w"),
             start_report_date = if (!is.null(dates)) dates$start else NULL,
             end_report_date   = if (!is.null(dates)) dates$end   else NULL,
             cross_cols        = rv$cross_cols,
             period_preset     = preset,
             weight_variable_enabled = TRUE,
             weight_variable_name = input$weight_variable_name %||% "",
             weight_variable_smart_filter = weight_smart_enabled(),
             required_ready = required_files_ready(),
             roi_optional_missing = is.null(rv$channels_rois),
             file_contracts = rv$file_contracts,
             cross_file_warnings = rv$cross_file_warnings,
             app_mode          = mode)
      }),
      media_index       = reactive(rv$media_index),
      update_sap        = effective_update_sap,
      schema_metadata   = reactive(rv$schema_metadata),
      validation_status = reactive(rv$validation_status),
      qa_status = reactive({
        loaded <- vapply(base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
        required_loaded <- vapply(required_base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
        base_ready <- required_files_ready()
        missing <- c(main = "RAE Datafile", analytical = "Analytical Dataset",
                     vof = "VOF Metadata", details = "ModelDetails")[!required_loaded]
        optional_missing <- c(rois = "ROIs by Channel")[is.null(rv$file_meta$rois)]
        list(
          files_loaded      = sum(loaded),
          files_total       = length(base_file_kinds),
          required_loaded   = sum(required_loaded),
          required_total    = length(required_base_file_kinds),
          files_ready       = base_ready,
          missing_files     = unname(missing),
          optional_missing_files = unname(optional_missing),
          rois_loaded       = !is.null(rv$channels_rois),
          roi_optional_missing = is.null(rv$channels_rois),
          file_contracts     = rv$file_contracts,
          cross_file_warnings = rv$cross_file_warnings,
          validation_status = rv$validation_status %||% "pending",
          media_ready       = !is.null(rv$media_index)
        )
      })
    )
  })
}
