server <- function(input, output, session) {
  
  data            <- mod_upload_server("upload")
  config          <- mod_config_server("config", data)
  channels_module <- mod_channels_server("channels", data)
  
  results_rv <- mod_process_server(
    "process",
    data          = data,
    config        = config,
    channels      = channels_module$channels,
    update_merges = channels_module$update_merges
  )
  
  mod_validate_server("validate", results_rv)
  mod_export_server("export", data, results_rv, config,
                    channels = channels_module$channels)
  
  # Notify when app is ready 
  session$onFlushed(function() {
    showNotification(
      tagList(icon("circle-check"), " App ready"),
      type = "message", duration = 2
    )
  }, once = TRUE)
  
  # Free memory when session ends
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}