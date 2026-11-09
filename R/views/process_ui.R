# Build the namespaced Process layout. Controllers supply dynamic metric tabs,
# diagnostics, SAP actions, and per-channel status through these placeholders.
build_process_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),

    tagList(
      card(
        card_header("Run Processing"),
        div(class = "process-run-panel",
            div(class = "process-field-block",
                selectInput(ns("channel_select"), "Select Channel", choices = NULL)),
            uiOutput(ns("role_summary_ui")),
            div(class = "process-run-actions",
                actionButton(ns("btn_one"), "Process Selected",
                             class = "btn-success btn-sm process-action-primary operation-trigger"),
                actionButton(ns("btn_all"), "Process All",
                             class = "btn-warning btn-sm process-action-primary operation-trigger"),
                actionButton(ns("btn_failed"), "Reprocess Failed Only",
                             class = "btn-outline-danger btn-sm process-action-secondary operation-trigger"),
                actionButton(ns("btn_changed"), "Reprocess Changed",
                             class = "btn-outline-secondary btn-sm process-action-secondary operation-trigger"))),
        hr(class = "hr-sm"),
        div(class = "process-run-summary",
            uiOutput(ns("batch_summary")),
            uiOutput(ns("error_summary"))),
        uiOutput(ns("status"))
      ),
      uiOutput(ns("merge_history_card"))
    ),

    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "flex-fill", uiOutput(ns("channel_pos"))),
          actionButton(ns("btn_next"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      uiOutput(ns("process_tabs"))
    )
  )
}
