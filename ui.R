ui <- page_navbar(
  title    = wpp_logo(),
  theme    = wpp_theme,
  fillable = FALSE,
  lang     = "en",
  
  nav_panel(title = tagList(tags$span("1", class="tab-step"), icon("upload"),       " Upload"),    mod_upload_ui("upload")),
  nav_panel(title = tagList(tags$span("2", class="tab-step"), icon("gear"),         " Config"),    mod_config_ui("config")),
  nav_panel(title = tagList(tags$span("3", class="tab-step"), icon("sliders"),      " Channels"),  mod_channels_ui("channels")),
  nav_panel(title = tagList(tags$span("4", class="tab-step"), icon("play"),         " Process"),   mod_process_ui("process")),
  nav_panel(title = tagList(tags$span("5", class="tab-step"), icon("circle-check"), " Validate"),  mod_validate_ui("validate")),
  nav_panel(title = tagList(tags$span("6", class="tab-step"), icon("download"),     " Export"),    mod_export_ui("export")),
  
  # Título centrado entre dos spacers — flex nativo, sin solapamiento
  nav_spacer(),
  nav_item(app_center),
  nav_spacer(),
  
  # Logo derecho
  nav_item(tags$div(class = "navbar-logo-right", wpp_logo())),
  nav_item(dt_pagination_fix)
)