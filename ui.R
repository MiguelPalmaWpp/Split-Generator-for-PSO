ui <- page_fluid(
  title = APP_TITLE,
  theme   = wpp_theme,
  padding = 0,
  lang    = "en",
 # --- External assets ---
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  includeScript("www/custom.js"),

  div(
    id = "operation-status-overlay",
    class = "operation-status-overlay",
    hidden = "hidden",
    `aria-hidden` = "true",
    div(
      id = "operation-status-dialog",
      class = "operation-status-dialog status-running",
      div(
        class = "operation-status-head",
        div(class = "operation-status-symbol"),
        div(class = "operation-status-heading",
            tags$span("OPERATION STATUS", class = "operation-status-eyebrow"),
            tags$strong(id = "operation-status-title", "Working")),
        tags$span(id = "operation-status-badge", class = "operation-status-badge is-running",
                  "Running")
      ),
      div(
        class = "operation-status-body",
        div(class = "operation-status-progress-row",
            tags$span(id = "operation-status-stage", class = "operation-status-stage", "Preparing operation")),
        div(class = "operation-status-meta",
            tags$span(id = "operation-status-detail", class = "operation-status-detail", ""),
            tags$span(id = "operation-status-elapsed", "0.0s")),
        div(id = "operation-status-counts", class = "operation-status-counts", hidden = "hidden"),
        div(id = "operation-status-items", class = "operation-status-items", hidden = "hidden"),
        div(id = "operation-status-summary", class = "operation-status-summary", hidden = "hidden"),
        tags$details(
          id = "operation-status-technical",
          class = "operation-status-technical",
          hidden = "hidden",
          tags$summary("Technical details"),
          tags$pre(id = "operation-status-technical-text")
        )
      ),
      div(class = "operation-status-foot",
          actionButton("operation_status_minimize", "Minimize",
                       icon = icon("window-minimize"),
                       class = "btn-outline-secondary btn-sm operation-status-minimize",
                       onclick = "window.minimizeOperationStatus && window.minimizeOperationStatus();"),
          actionButton("operation_status_close", "Close",
                       class = "btn-outline-secondary btn-sm operation-status-close",
                       onclick = "window.closeOperationStatus && window.closeOperationStatus();"))
    )
  ),
  div(
    id = "operation-status-compact",
    class = "operation-status-compact",
    hidden = "hidden",
    onclick = "window.restoreOperationStatus && window.restoreOperationStatus();",
    div(class = "operation-status-compact-copy",
        tags$strong(id = "operation-status-compact-title", "Processing"),
        tags$span(id = "operation-status-compact-detail", "Operation in progress")),
    tags$span(icon("up-right-and-down-left-from-center"), class = "operation-status-compact-open")
  ),
 # --- App header ---
  tags$header(class = "wpp-app-header",
              tags$div(class = "wpp-header-brand",
                       wpp_logo(height = "86px")
              ),
              app_center,
              tags$div(class = "wpp-header-right",
                       wpp_logo(height = "86px")
              )
  ),
 # --- Main navigation ---
  div(class = "wpp-main-nav",
      div(
        class = "splits-metadata-global",
        div(
          class = "splits-metadata-global-title",
          icon("database"),
          tags$span("Splits Metadata"),
          uiOutput("splits_metadata_status")
        ),
        div(
          class = "splits-metadata-global-actions",
          downloadButton("dl_splits_metadata",
                         label = tagList(icon("download"), "Download"),
                         class = "btn-outline-secondary btn-sm splits-metadata-btn"),
          div(
            class = "splits-metadata-upload",
            fileInput("splits_metadata_file", NULL,
                      accept = c(".csv", ".tsv", ".txt"),
                      buttonLabel = "Import",
                      placeholder = "No file selected",
                      width = "100%")
          )
        )
      ),
      navset_underline(
        id = "main_tabs",
        nav_panel(
          title = tagList(tags$span("1", class = "tab-step"), icon("upload"),   " Setup"),
          value = "setup",    mod_setup_ui("setup")),
        nav_panel(
          title = tagList(tags$span("2", class = "tab-step"), icon("sliders"),  " Channels"),
          value = "channels", mod_channels_ui("channels")),
        nav_panel(
          title = tagList(tags$span("3", class = "tab-step"), icon("play"),     " Process"),
          value = "process",  mod_process_ui("process")),
        nav_panel(
          title = tagList(tags$span("4", class = "tab-step"), icon("download"), " Export"),
          value = "export",   mod_export_ui("export"))
      )
  )
)
