# Build independent Setup workflows without changing the module's input IDs.
# Model Update is flagged for validation; Model Refresh is a placeholder.
build_setup_ui <- function(id) {
  ns <- NS(id)
  
  # Reuse one file-card structure across batch/manual Build and Update inputs.
  mk_file_card <- function(num, title, formats, input_ui, kind = NULL) {
    div(class = "file-card",
        div(class = "file-card-header",
            tags$span(num,   class = "file-card-num"),
            tags$span(title, class = "file-card-title")),
        tags$span(formats, class = "file-card-formats"),
        div(class = "mt-auto",
            input_ui,
            if (!is.null(kind)) uiOutput(ns(paste0(kind, "_file_status")))))
  }
  
  tagList(
    div(style = "display:none;", textInput(ns("app_mode"), NULL, "build")),
    navset_tab(
      id = ns("setup_workflow_tab"),
      nav_panel(
        "Model Build", value = "build",
    div(
      class = "setup-flow",

    card(
      class = "setup-files-card setup-files-section", fill = FALSE,
      card_header("Data Files"),
      div(
        class = "batch-file-card-wrap mb-3",
        mk_file_card("All", "Base Files", ".csv .zip .RData .xlsx .xls",
                     div(class = "batch-file-input",
                         fileInput(
                           ns("file_base_bundle"),
                           NULL,
                           accept = c(".csv", ".zip", ".RData", ".xlsx", ".xls"),
                           multiple = TRUE,
                           placeholder = "Select files"
                         ))),
        uiOutput(ns("batch_upload_summary"))
      ),
      layout_columns(
        col_widths = c(4, 4, 4), class = "mb-3",
        mk_file_card("1", "RAE Datafile",    ".csv .zip",
                     fileInput(ns("file_main"),       NULL, accept = c(".csv", ".zip")),
                     kind = "main"),
        mk_file_card("2", "Analytical Dataset", ".RData",
                     fileInput(ns("file_analytical"), NULL, accept = ".RData"),
                     kind = "analytical"),
        mk_file_card("3", "VOF Metadata",       ".csv",
                     fileInput(ns("file_vof"),        NULL, accept = ".csv"),
                     kind = "vof")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        mk_file_card("4", "ModelDetails",    ".csv",
                     fileInput(ns("file_details"), NULL, accept = ".csv"),
                     kind = "details"),
        mk_file_card("5", "ROIs by Channel", ".csv .xlsx",
                     fileInput(ns("file_rois"),    NULL, accept = c(".csv", ".xlsx")),
                     kind = "rois"),
        div()
      )
    ),
    
    uiOutput(ns("validation_card")),

    card(
      class = "global-params-card setup-global-card",
      card_header("Global Parameters"),
      div(
        class = "setup-global-grid",
        div(
          class = "setup-global-main",
          uiOutput(ns("update_label_ui")),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'update'", ns("app_mode")),
          div(class = "mb-2",
              tags$label("Reporting Period", class = "form-label"),
              div(class = "ds-pill-group",
                  radioButtons(ns("period_preset"), NULL,
                               choices  = c("Last 52w" = "last52", "Last 13w" = "last13",
                                            "All Period" = "all", "Custom" = "custom"),
                               selected = "last52", inline = TRUE)))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'update'", ns("app_mode")),
          div(class = "alert alert-info alert-sm p-2 mb-2",
              icon("circle-info"), " ",
              tags$strong("All Period — fixed in Model Update."),
              " All data in RAE Datafile is treated as focus period.",
              " Past data already carries the Before label from processing.")
        ),
          uiOutput(ns("custom_dates_ui")),
          uiOutput(ns("weight_variable_ui"))
        ),
        div(
          class = "setup-global-side",
          div(
            class = "setup-suffix-panel",
            div(class = "setup-panel-title", "Column Suffix Preview"),
            uiOutput(ns("suffix_preview"))
          ),
          div(
            class = "setup-period-panel",
            div(class = "setup-panel-title", "Reporting Period Preview"),
            uiOutput(ns("reporting_period_preview"))
          )
        )
      ),
      div(
        class = "setup-global-footer",
        uiOutput(ns("cross_section_info")),
        uiOutput(ns("validation_alerts"))
      )
    ),
    
    card(
      class = "setup-index-section",
      card_header(
        div(class = "setup-index-heading",
            icon("diagram-project"),
            div(tags$strong("Media Variable Index"),
                tags$span("Connection health across model inputs")))
      ),
      uiOutput(ns("media_index_display"))
    )
        )
      ),
      nav_panel(
        "Model Update", value = "update",
        div(class = "setup-flow",
          div(class = "alert alert-warning alert-sm p-2 mb-3",
            icon("flask"), " ",
            tags$strong("In development."),
            " Awaiting validation with representative previous results and current model files."
          ),
          card(class = "setup-files-card setup-files-section", fill = FALSE,
            card_header(div(class = "card-header-between",
              div(class = "card-header-inner", icon("clock-rotate-left", class = "icon-blue-sm"),
                  "Previous Results"),
              uiOutput(ns("update_status_badge")))),
            p(class = "text-muted small", "Start from a previous Deep Dives Splits results ZIP."),
            layout_columns(col_widths = c(6, 6),
              mk_file_card("ZIP", "Previous Results ZIP", ".zip",
                fileInput(ns("file_update_zip"), NULL, accept = ".zip")),
              mk_file_card("Map", "MainVars Mapping", ".xlsx .xls",
                fileInput(ns("file_mainvars_mapping"), NULL, accept = c(".xlsx", ".xls"))))
          ),
          card(class = "setup-files-card setup-files-section", fill = FALSE,
            card_header("Current Model Files"),
            layout_columns(col_widths = c(4, 4, 4), class = "mb-3",
              mk_file_card("1", "RAE Datafile", ".csv .zip",
                fileInput(ns("update_file_main"), NULL, accept = c(".csv", ".zip"))),
              mk_file_card("2", "Analytical Dataset", ".RData",
                fileInput(ns("update_file_analytical"), NULL, accept = ".RData")),
              mk_file_card("3", "VOF Metadata", ".csv",
                fileInput(ns("update_file_vof"), NULL, accept = ".csv"))),
            layout_columns(col_widths = c(4, 4, 4),
              mk_file_card("4", "ModelDetails", ".csv",
                fileInput(ns("update_file_details"), NULL, accept = ".csv")),
              mk_file_card("5", "ROIs by Channel", ".csv .xlsx",
                fileInput(ns("update_file_rois"), NULL, accept = c(".csv", ".xlsx"))),
              div())
          ),
          card(class = "setup-files-card setup-files-section", fill = FALSE,
            card_header("Update Details"),
            layout_columns(col_widths = c(4, 4, 4),
              div(tags$label("Previous Update ID", class = "form-label"),
                  textInput(ns("past_update_id"), NULL,
                            placeholder = "Detected from ZIP and MainVars Mapping")),
              div(tags$label("Current Update ID", class = "form-label"),
                  textInput(ns("current_update_id"), NULL, placeholder = "e.g. Update15")),
              div(tags$label("Current Label", class = "form-label"),
                  textInput(ns("update_label_update"), NULL, placeholder = "e.g. 2026Q3"))),
            uiOutput(ns("model_update_summary")),
            uiOutput(ns("update_action_ui")),
            uiOutput(ns("update_processing_ui"))
          )
        )
      ),
      nav_panel(
        "Model Refresh", value = "refresh",
        div(
          class = "setup-workflow-placeholder",
          icon("arrows-rotate"),
          tags$h2("Model Refresh is in development"),
          tags$p("This workflow is being prepared and is not available yet.")
        )
      )
    )
  )
}
