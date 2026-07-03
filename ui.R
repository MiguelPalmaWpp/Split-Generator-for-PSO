ui <- page_fluid(
  theme   = wpp_theme,
  padding = 0,
  lang    = "en",
  
  # ── External assets ───────────────────────────────────────────────────
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$script(src = "custom.js"),
  
  # ── App header ────────────────────────────────────────────────────────
  tags$header(class = "wpp-app-header",
              tags$div(class = "wpp-header-brand",
                       wpp_logo(height = "74px")
              ),
              app_center,
              tags$div(class = "wpp-header-right",
                       wpp_logo(height = "74px")
              )
  ),
  
  # ── Main navigation ───────────────────────────────────────────────────
  div(class = "wpp-main-nav",
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
