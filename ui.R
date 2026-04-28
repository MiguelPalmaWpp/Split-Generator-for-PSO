ui <- page_fluid(
  theme   = wpp_theme,
  padding = 0,
  lang    = "en",
  
  # ── DT pagination fix ─────────────────────────────────────
  dt_pagination_fix,
  
  # ── Header: branding only, no tabs ────────────────────────
  tags$header(
    class = "wpp-app-header",
    tags$div(class = "wpp-header-brand", wpp_logo()),
    app_center,
    tags$div(class = "wpp-header-right",  wpp_logo())
  ),
  
  # ── Tab strip: separated from header, AWB style ───────────
  div(
    class = "wpp-main-nav",
    navset_underline(
      id = "main_tabs",
      
      nav_panel(
        title = tagList(tags$span("1", class = "tab-step"), icon("upload"),       " Upload"),
        mod_upload_ui("upload")
      ),
      nav_panel(
        title = tagList(tags$span("2", class = "tab-step"), icon("gear"),         " Config"),
        mod_config_ui("config")
      ),
      nav_panel(
        title = tagList(tags$span("3", class = "tab-step"), icon("sliders"),      " Channels"),
        mod_channels_ui("channels")
      ),
      nav_panel(
        title = tagList(tags$span("4", class = "tab-step"), icon("play"),         " Process"),
        mod_process_ui("process")
      ),
      nav_panel(
        title = tagList(tags$span("5", class = "tab-step"), icon("circle-check"), " Validate"),
        mod_validate_ui("validate")
      ),
      nav_panel(
        title = tagList(tags$span("6", class = "tab-step"), icon("download"),     " Export"),
        mod_export_ui("export")
      )
    )
  )
)