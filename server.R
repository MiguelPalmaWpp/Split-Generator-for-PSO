# ═══════════════════════════════════════════════════════════════════════
# server.R
# ═══════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── 1. Setup ─────────────────────────────────────────────────────────────
  setup_module <- mod_setup_server("setup")
  
  # ── 2. Channels ───────────────────────────────────────────────────────────
  channels_module <- mod_channels_server(
    "channels",
    data        = setup_module$data,
    media_index = setup_module$media_index
  )
  
  # ── 3. Process ────────────────────────────────────────────────────────────
  process_module <- mod_process_server(
    "process",
    data          = setup_module$data,
    config        = setup_module$config,
    channels      = channels_module$channels,
    update_merges = channels_module$update_merges
  )
  
  # ── 4. Export ─────────────────────────────────────────────────────────────
  mod_export_server(
    "export",
    results       = process_module$results,        
    clean_results = process_module$clean_results,  
    data          = setup_module$data,
    config        = setup_module$config,
    channels      = channels_module$channels
  )
  
  # ── Tab blocking — only on validation error ───────────────────────────────
  observe({
    status     <- setup_module$validation_status()
    is_blocked <- identical(status, "red")
    session$sendCustomMessage("setTabsDisabled", list(disabled = is_blocked))
  })
  
  # ── App ready notification ────────────────────────────────────────────────
  session$onFlushed(function() {
    showNotification(
      tagList(icon("circle-check"), " App ready"),
      type = "message", duration = 2)
  }, once = TRUE)
  
  # ── Cleanup on session end ────────────────────────────────────────────────
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}