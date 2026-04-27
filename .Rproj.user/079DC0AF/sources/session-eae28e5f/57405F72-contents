server <- function(input, output, session) {
  data     <- mod_upload_server("upload")
  config   <- mod_config_server("config", data)
  channels <- mod_channels_server("channels", data)
  
  results_rv <- mod_process_server(
    "process",
    data     = data,
    config   = config,
    channels = channels
  )
  
  mod_validate_server("validate", results_rv)
  mod_export_server("export", data, results_rv, config)
}

