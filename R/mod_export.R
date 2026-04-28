# ═══════════════════════════════════════════════════════════════════
# R/mod_export.R
# ═══════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── Export status ─────────────────────────────────────────────
    uiOutput(ns("export_status")),
    
    # ── Download cards ────────────────────────────────────────────
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Card 1: Analytical Extended
      card(
        card_header("Analytical Splits Extended"),
        tags$p(
          class = "text-muted small mb-3",
          "Original analytical dataset with all split columns appended.",
          "One row per Geography × Product × Period."
        ),
        uiOutput(ns("info_analytical")),
        hr(style = "margin:10px 0;"),
        actionButton(ns("preview_analytical"), "Preview",
                     icon  = icon("eye"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2"),
        downloadButton(ns("dl_analytical"), "Download CSV",
                       class = "btn-primary w-100")
      ),
      
      # Card 2: Side Mapping
      card(
        card_header("Side Model Mapping"),
        tags$p(
          class = "text-muted small mb-3",
          "Split-to-model-variable mapping with PSO weight structure."
        ),
        uiOutput(ns("info_side_mapping")),
        hr(style = "margin:10px 0;"),
        actionButton(ns("preview_side_mapping"), "Preview",
                     icon  = icon("eye"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2"),
        downloadButton(ns("dl_side_mapping"), "Download CSV",
                       class = "btn-primary w-100")
      ),
      
      # Card 3: Activity, Cost & ROIs
      card(
        card_header("Activity, Cost & ROIs"),
        tags$p(
          class = "text-muted small mb-3",
          "Activity and spend totals per split, enriched with ROI data."
        ),
        uiOutput(ns("info_activity_cost")),
        hr(style = "margin:10px 0;"),
        actionButton(ns("preview_activity_cost"), "Preview",
                     icon  = icon("eye"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2"),
        downloadButton(ns("dl_activity_cost"), "Download CSV",
                       class = "btn-primary w-100")
      )
    ),
    
    # ── Feature 5: Download All as ZIP ────────────────────────────
    div(
      style = "margin-top:12px;",
      downloadButton(
        ns("dl_all_zip"),
        label = tagList(icon("file-zipper"), " Download All (ZIP)"),
        class = "btn-primary w-100"
      ),
      tags$p(
        class = "text-muted small text-center mt-1",
        "Analytical Extended + Side Mapping + Activity & ROIs + Config JSON"
      )
    ),
    
    # ── Preview panel ─────────────────────────────────────────────
    uiOutput(ns("preview_panel"))
  )
}

# ── Server ────────────────────────────────────────────────────────
mod_export_server <- function(id, data, results, config,
                              channels = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Track which preview is active
    active_preview <- reactiveVal(NULL)
    
    observeEvent(input$preview_analytical,   { active_preview("analytical")   })
    observeEvent(input$preview_side_mapping, { active_preview("side_mapping")  })
    observeEvent(input$preview_activity_cost,{ active_preview("activity_cost") })
    
    # ── Export status panel ────────────────────────────────────────
    output$export_status <- renderUI({
      res_list <- results()
      d        <- data()
      
      n_done  <- length(res_list)
      
      channel_rows <- if (n_done > 0) {
        lapply(names(res_list), function(nm) {
          r        <- res_list[[nm]]
          n_splits <- if (!is.null(r$activity_spend)) nrow(r$activity_spend) else 0L
          div(
            style = paste0(
              "display:flex; align-items:center; gap:12px;",
              "padding:6px 0; border-bottom:1px solid #f0f0f0;",
              "font-size:13px;"
            ),
            icon("circle-check", style = "color:#2ecc71; font-size:14px;"),
            tags$span(tags$strong(nm), style = "flex:1; color:#2c3e50;"),
            tags$span(
              paste0(n_splits, " splits"),
              style = paste0(
                "background:#EBF3FB; color:#5B9BD5;",
                "padding:2px 8px; border-radius:10px;",
                "font-size:11px; font-weight:600;"
              )
            )
          )
        })
      } else {
        list(tags$p(class = "text-muted small mb-0",
                    "No channels processed yet."))
      }
      
      warning_ui <- if (n_done == 0)
        div(class = "alert alert-warning mb-0 mt-2",
            icon("triangle-exclamation"),
            " No channels have been processed. Go to the Process tab first.")
      
      card(
        card_header("Export Status"),
        div(
          style = "display:flex; align-items:center; gap:16px; margin-bottom:12px;",
          div(
            style = paste0(
              "background:", if (n_done > 0) "#e8f5e9" else "#fff8e1", ";",
              "border-radius:8px; padding:10px 20px;",
              "display:flex; align-items:center; gap:10px;"
            ),
            icon(if (n_done > 0) "check" else "clock",
                 style = paste0(
                   "color:", if (n_done > 0) "#2ecc71" else "#f39c12",
                   "; font-size:20px;"
                 )),
            div(
              tags$strong(paste0(n_done, " channel(s) ready"),
                          style = "font-size:15px; color:#2c3e50; display:block;"),
              tags$small("processed and available for export",
                         style = "color:#6c757d;")
            )
          )
        ),
        div(style = "max-height:200px; overflow-y:auto;",
            tagList(channel_rows)),
        if (!is.null(warning_ui)) warning_ui
      )
    })
    
    # ── Core reactives ─────────────────────────────────────────────
    
    final_analytical <- reactive({
      d        <- data(); req(d$analytical)
      res_list <- results(); req(length(res_list) > 0)
      
      cross_cols_used <- NULL
      for (r in res_list) {
        if (!is.null(r$cross_cols)) { cross_cols_used <- r$cross_cols; break }
      }
      cross_cols_used <- cross_cols_used %||% config()$cross_cols %||% "Geography"
      cross_id        <- c(cross_cols_used, "Period")
      
      # Use data.table for memory-efficient joins
      result_dt <- data.table::as.data.table(d$analytical)
      
      for (r in res_list) {
        r_cross   <- r$cross_cols %||% cross_cols_used
        r_key     <- c(r_cross, "Period")
        join_cols <- intersect(names(result_dt), r_key)
        
        rag_dt    <- data.table::as.data.table(r$rag)
        result_dt <- merge(result_dt, rag_dt, by = join_cols, all.x = TRUE)
        
        # Fill NAs only in new numeric columns
        new_cols     <- setdiff(names(rag_dt), join_cols)
        num_new_cols <- new_cols[sapply(result_dt[, .SD, .SDcols = new_cols],
                                        is.numeric)]
        for (col in num_new_cols)
          data.table::set(result_dt, which(is.na(result_dt[[col]])), col, 0L)
        
        gc()
      }
      
      as_tibble(result_dt)
    })
    
    final_side_mapping <- reactive({
      res_list <- results(); req(length(res_list) > 0)
      
      map(res_list, \(r) r$side_mapping) %>%
        bind_rows() %>%
        mutate(
          MainModelVariableName = case_when(
            !str_detect(MainModelVariableName, "_Total_Total") &
              !str_detect(MainModelVariableName, "___") ~
              paste0(MainModelVariableName, "___"),
            .default = MainModelVariableName
          ),
          .srt = if_else(str_detect(VariableSplit, "_Before"), 1L, 2L)
        ) %>%
        arrange(.srt, MainModelVariableName, VariableSplit) %>%
        select(-.srt)
    })
    
    activity_cost <- reactive({
      d        <- data()
      res_list <- results(); req(length(res_list) > 0)
      ac <- map(res_list, \(r) r$activity_spend) %>% bind_rows()
      if (!is.null(d$channels_rois))
        ac <- ac %>% left_join(d$channels_rois, by = "Channel")
      ac
    })
    
    # ── File info (rows × columns) ─────────────────────────────────
    file_info_ui <- function(df_reactive) {
      renderUI({
        tryCatch({
          df <- df_reactive()
          div(
            style = "display:flex; gap:12px; margin-bottom:4px;",
            div(
              style = paste0(
                "background:#f4f6f9; border-radius:6px;",
                "padding:6px 12px; text-align:center;"
              ),
              tags$strong(format(nrow(df), big.mark = ","),
                          style = "font-size:16px; color:#2c3e50; display:block;"),
              tags$small("rows", style = "color:#6c757d;")
            ),
            div(
              style = paste0(
                "background:#f4f6f9; border-radius:6px;",
                "padding:6px 12px; text-align:center;"
              ),
              tags$strong(ncol(df),
                          style = "font-size:16px; color:#2c3e50; display:block;"),
              tags$small("columns", style = "color:#6c757d;")
            )
          )
        }, error = function(e) {
          tags$p(class = "text-muted small mb-0", "Process channels first.")
        })
      })
    }
    
    output$info_analytical   <- file_info_ui(final_analytical)
    output$info_side_mapping <- file_info_ui(final_side_mapping)
    output$info_activity_cost <- file_info_ui(activity_cost)
    
    # ── Preview panel ──────────────────────────────────────────────
    output$preview_panel <- renderUI({
      ap <- active_preview()
      if (is.null(ap)) return(NULL)
      
      df <- tryCatch({
        switch(ap,
               "analytical"   = final_analytical(),
               "side_mapping" = final_side_mapping(),
               "activity_cost" = activity_cost())
      }, error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0)
        return(div(class = "alert alert-warning mt-3",
                   "No data available yet. Process channels first."))
      
      title <- switch(ap,
                      "analytical"    = "Analytical Splits Extended",
                      "side_mapping"  = "Side Model Mapping",
                      "activity_cost" = "Activity, Cost & ROIs")
      
      card(
        style = "margin-top:16px;",
        card_header(
          div(
            style = "display:flex; align-items:center; justify-content:space-between;",
            tags$span(paste("Preview —", title)),
            div(
              tags$small(
                style = "color:#8a9bb0;",
                paste0("Showing 10 of ", format(nrow(df), big.mark = ","),
                       " rows × ", ncol(df), " columns")
              ),
              actionButton(session$ns("close_preview"),
                           icon("xmark"),
                           class = "btn btn-link btn-sm p-0 ms-3",
                           style = "color:#adb5bd;")
            )
          )
        ),
        div(
          style = "overflow-x:auto;",
          DT::renderDT(
            df %>% head(10),
            options  = list(scrollX = TRUE, pageLength = 10,
                            dom = "t", ordering = FALSE),
            rownames = FALSE
          )
        )
      )
    })
    
    observeEvent(input$close_preview, { active_preview(NULL) })
    
    # ── Individual downloads ───────────────────────────────────────
    output$dl_analytical <- downloadHandler(
      filename = \() paste0("AnalyticalDataset_Splits_Extended_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = \(f) readr::write_csv(final_analytical(), f)
    )
    
    output$dl_side_mapping <- downloadHandler(
      filename = \() paste0("Side_Model_Mapping_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = \(f) readr::write_csv(final_side_mapping(), f)
    )
    
    output$dl_activity_cost <- downloadHandler(
      filename = \() paste0("Activity_Cost_ROIs_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = \(f) readr::write_csv(activity_cost(), f)
    )
    
    # ── Feature 5: Download All as ZIP ────────────────────────────
    output$dl_all_zip <- downloadHandler(
      filename = function() {
        paste0("Split_Generator_Export_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        ts      <- format(Sys.time(), "%Y%m%d_%H%M%S")
        tmp_dir <- file.path(tempdir(), paste0("sg_export_", ts))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        written <- character(0)
        
        # Helper: write CSV safely, collect path
        write_csv_safe <- function(data_fn, fname) {
          tryCatch({
            fpath <- file.path(tmp_dir, fname)
            readr::write_csv(isolate(data_fn()), fpath)
            written <<- c(written, fpath)
          }, error = \(e) {
            message("ZIP export warning [", fname, "]: ", conditionMessage(e))
          })
        }
        
        # 1. Analytical Extended
        write_csv_safe(
          final_analytical,
          paste0("AnalyticalDataset_Splits_Extended_", ts, ".csv")
        )
        
        # 2. Side Model Mapping
        write_csv_safe(
          final_side_mapping,
          paste0("Side_Model_Mapping_", ts, ".csv")
        )
        
        # 3. Activity, Cost & ROIs
        write_csv_safe(
          activity_cost,
          paste0("Activity_Cost_ROIs_", ts, ".csv")
        )
        
        # 4. Channel config JSON (only if channels reactive is provided)
        if (!is.null(channels)) {
          tryCatch({
            cfg_data <- isolate(channels())
            if (length(cfg_data) > 0) {
              fpath <- file.path(tmp_dir, paste0("split_config_", ts, ".json"))
              jsonlite::write_json(cfg_data, fpath,
                                   pretty      = TRUE,
                                   auto_unbox  = TRUE,
                                   null        = "null")
              written <- c(written, fpath)
            }
          }, error = \(e) {
            message("ZIP export warning [config JSON]: ", conditionMessage(e))
          })
        }
        
        # Nothing to zip?
        if (length(written) == 0) {
          writeLines("No files could be generated. Process channels first.",
                     file)
          return()
        }
        
        # Create ZIP — change to temp dir so archive has no path prefixes
        old_wd <- setwd(tmp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(
          zipfile = file,
          files   = basename(written)
        )
        
        message("ZIP export: ", length(written), " file(s) packaged.")
      }
    )
    
  })
}