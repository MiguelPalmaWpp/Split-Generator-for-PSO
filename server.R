# ═══════════════════════════════════════════════════════════════════════
# server.R
# ═══════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── 1. Setup ──────────────────────────────────────────────────────────
  setup_module <- mod_setup_server("setup")
  
  # ── 2. Channels ───────────────────────────────────────────────────────
  channels_module <- mod_channels_server(
    "channels",
    data        = setup_module$data,
    media_index = setup_module$media_index,
    config      = setup_module$config
  )

  splits_metadata_pending <- reactiveVal(NULL)
  splits_metadata_status  <- reactiveVal("current")

  output$dl_splits_metadata <- downloadHandler(
    filename = function()
      paste0("splits_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      cfg_data <- isolate(channels_module$channels())
      if (!length(cfg_data)) {
        readr::write_csv(data.frame(), file)
        return()
      }
      df <- export_splits_metadata_csv(cfg_data, isolate(setup_module$config()))
      readr::write_csv(df, file, na = "")
      splits_metadata_status("current")
    }
  )

  output$splits_metadata_status <- renderUI({
    status <- splits_metadata_status()
    cls <- switch(status,
                  needs_reprocess = "warn",
                  mismatch = "warn",
                  "ok")
    label <- switch(status,
                    needs_reprocess = "Needs reprocess",
                    mismatch = "Update label mismatch",
                    "Current")
    tags$span(label, class = paste("splits-metadata-badge", cls))
  })

  observeEvent(input$splits_metadata_file, {
    req(input$splits_metadata_file$datapath)
    tryCatch({
      parsed <- channels_module$read_splits_metadata_file(input$splits_metadata_file$datapath)
      preview <- channels_module$preview_splits_metadata_import(parsed)
      splits_metadata_pending(list(parsed = parsed, preview = preview))
      mismatch <- preview$update_label_mismatch %||% character(0)
      if (length(mismatch)) splits_metadata_status("mismatch")

      badge <- function(value, label) {
        tags$span(paste(value, label), class = "splits-metadata-preview-badge")
      }
      showModal(modalDialog(
        title = tagList(icon("database"), " Import Splits Metadata"),
        div(
          class = "splits-metadata-preview",
          div(
            class = "splits-metadata-preview-grid",
            badge(preview$total, "channel configs"),
            badge(preview$breaks, "breaks"),
            badge(preview$renames %||% 0L, "renames"),
            badge(preview$merges, "merges"),
            badge(preview$updated, "will overwrite"),
            badge(preview$imported + preview$rebuilt, "new/imported"),
            badge(preview$skipped, "skipped")
          ),
          if (length(mismatch)) {
            div(
              class = "splits-metadata-preview-warning",
              strong("Update label mismatch"),
              tags$p(
                paste0(
                  "This metadata was created with ",
                  paste(mismatch, collapse = ", "),
                  "; current setup is ",
                  setup_module$config()$update_label %||% "(blank)",
                  ". Merges may not reproduce until labels match."
                )
              )
            )
          } else {
            div(
              class = "splits-metadata-preview-note",
              "Import will update channels, breaks, renames and saved merges. It will not process automatically."
            )
          }
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("btn_apply_splits_metadata", "Apply Import", class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    }, error = function(e) {
      splits_metadata_pending(NULL)
      showNotification(paste("Splits Metadata import error:", e$message),
                       type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$btn_apply_splits_metadata, {
    pending <- splits_metadata_pending()
    req(pending$parsed)
    preview <- pending$preview %||% list()
    tryCatch({
      channels_module$apply_splits_metadata_import(pending$parsed)
      removeModal()
      splits_metadata_pending(NULL)
      if (length(preview$update_label_mismatch %||% character(0))) {
        splits_metadata_status("mismatch")
      } else {
        splits_metadata_status("needs_reprocess")
      }
      showNotification("Splits Metadata imported. Review channels, then process when ready.",
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Splits Metadata apply error:", e$message),
                       type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)
  
  # ── 3. Process ────────────────────────────────────────────────────────
  process_module <- mod_process_server(
    "process",
    data          = setup_module$data,
    config        = setup_module$config,
    channels      = channels_module$channels,
    update_merges = channels_module$update_merges,
    config_import_event = channels_module$config_import_event
  )

  observe({
    qa <- process_module$qa_status()
    if (is.null(qa) || !length(channels_module$channels())) return()
    all_current <- (qa$total %||% 0L) > 0L &&
      (qa$processed %||% 0L) >= (qa$total %||% 0L) &&
      (qa$pending %||% 0L) == 0L &&
      (qa$stale %||% 0L) == 0L &&
      (qa$failed %||% 0L) == 0L
    if (isTRUE(all_current)) {
      splits_metadata_status("current")
    }
  })
  
  # ── 4. Export ─────────────────────────────────────────────────────────
  mod_export_server(
    "export",
    results       = process_module$results,
    clean_results = process_module$clean_results,
    data          = setup_module$data,
    config        = setup_module$config,
    channels      = channels_module$channels,
    process_qa    = process_module$qa_status
  )

  # ── App ready notification ────────────────────────────────────────────
  session$onFlushed(function() {
    showNotification(
      tagList(icon("circle-check"), " App ready"),
      type = "message", duration = 2)
  }, once = TRUE)
  
  # ── Cleanup on session end ────────────────────────────────────────────
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}
