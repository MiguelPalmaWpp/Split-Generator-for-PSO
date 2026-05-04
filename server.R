
server <- function(input, output, session) {
  
  setup_module    <- mod_setup_server("setup")
  channels_module <- mod_channels_server("channels", setup_module$data , setup_module$config)
  
  results_rv <- mod_process_server(
    "process",
    data          = setup_module$data,
    config        = setup_module$config,
    channels      = channels_module$channels,
    update_merges = channels_module$update_merges
  )
  
  mod_validate_server("validate", results_rv)
  mod_export_server("export", setup_module$data, results_rv,
                    setup_module$config, channels = channels_module$channels)
  observe({
    status     <- setup_module$validation_status()
    is_blocked <- identical(status, "red")
    session$sendCustomMessage("setTabsDisabled", list(disabled = is_blocked))
  })
  
  session$onFlushed(function() {
    showNotification(tagList(icon("circle-check"), " App ready"),
                     type = "message", duration = 2)
  }, once = TRUE)
  
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}