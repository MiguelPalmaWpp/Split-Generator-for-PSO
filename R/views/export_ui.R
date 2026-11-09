# Build the namespaced Export shell for readiness, SCWA, and ZIP controls.
# File calculations and download handling remain in the module/controllers.
build_export_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("summary_strip")),
    div(
      class = "export-main-layout",
      card(
        class = "export-channel-panel",
        card_header(div(class = "card-header-inner",
                        icon("circle-check", class = "icon-blue-sm"),
                        "Channel Status")),
        uiOutput(ns("channel_status"))
      ),
      div(
        class = "export-right-stack",
        uiOutput(ns("scwa_workaround")),
        card(
          class = "export-package-panel",
          card_header(div(
            class = "card-header-between",
            div(class = "card-header-inner",
                icon("file-zipper", class = "icon-blue-sm"),
                "Export Package"),
            uiOutput(ns("readiness_badge"))
          )),
          uiOutput(ns("export_issues")),
          div(class = "export-contents-pad", uiOutput(ns("export_contents"))),
          uiOutput(ns("roi_coverage")),
          hr(class = "hr-export"),
          uiOutput(ns("download_section"))
        )
      )
    )
  )
}
