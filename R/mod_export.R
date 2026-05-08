# ════════════════════════════════════════════════════════════════
# R/mod_export.R  — replaces mod_validate + mod_export
# ════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ── Section 1: Summary KPIs ────────────────────────────────────
    uiOutput(ns("summary_kpis")),
    
    # ── Section 2: Channel Status ──────────────────────────────────
    card(
      card_header(
        div(style = "display:flex; align-items:center; gap:8px;",
            icon("list-check", style = "color:#5B9BD5;"),
            "Channel Status")
      ),
      uiOutput(ns("channel_status"))
    ),
    
    # ── Section 3: Export ──────────────────────────────────────────
    card(
      card_header(
        div(style = "display:flex; align-items:center; gap:8px;",
            icon("file-zipper", style = "color:#5B9BD5;"),
            "Export")
      ),
      div(style = "padding:16px 0 8px;",
          uiOutput(ns("export_contents"))
      ),
      hr(style = "margin:8px 0 16px;"),
      uiOutput(ns("download_section"))
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────
mod_export_server <- function(id, results, data, config, channels) {
  moduleServer(id, function(input, output, session) {
    
    # ── Channel-level summaries ────────────────────────────────────
    channel_summary <- reactive({
      ch_names <- names(channels())
      res_list <- results()
      
      lapply(ch_names, function(nm) {
        res <- res_list[[nm]]
        
        processed <- !is.null(res)
        
        n_splits_focus <- if (processed && !is.null(res$act_diagnoses))
          nrow(res$act_diagnoses[res$act_diagnoses$period == "focus", ])
        else 0L
        
        has_warning <- processed && n_splits_focus == 0
        
        list(
          name          = nm,
          processed     = processed,
          has_warning   = has_warning,
          n_splits      = n_splits_focus
        )
      })
    })
    
    # ── KPI aggregates ─────────────────────────────────────────────
    # Reemplaza las 4 líneas actuales de n_ok, n_warnings, n_critical, n_splits
    
    n_ok <- reactive({
      summ <- channel_summary()
      if (!length(summ)) return(0L)
      sum(vapply(summ, function(x) isTRUE(x$processed) && !isTRUE(x$has_warning), logical(1)))
    })
    
    n_warnings <- reactive({
      summ <- channel_summary()
      if (!length(summ)) return(0L)
      sum(vapply(summ, function(x) isTRUE(x$processed) && isTRUE(x$has_warning), logical(1)))
    })
    
    n_critical <- reactive({
      summ <- channel_summary()
      if (!length(summ)) return(0L)
      sum(vapply(summ, function(x) !isTRUE(x$processed), logical(1)))
    })
    
    n_splits <- reactive({
      summ <- channel_summary()
      if (!length(summ)) return(0L)
      sum(vapply(summ, function(x) as.integer(x$n_splits %||% 0L), integer(1)))
    })
    
    # ── Summary KPIs ───────────────────────────────────────────────
    output$summary_kpis <- renderUI({
      
      mk_kpi <- function(value, label, ico, color, bg, border) {
        div(
          style = paste0(
            "background:", bg, "; border:1px solid ", border, ";",
            "border-radius:10px; padding:22px 16px;",
            "display:flex; flex-direction:column; align-items:center;",
            "gap:8px; text-align:center;"
          ),
          icon(ico, style = paste0("color:", color, "; font-size:26px;")),
          tags$span(value,
                    style = paste0("font-size:36px; font-weight:700; color:", color, "; line-height:1;")),
          tags$span(label,
                    style = "font-size:12px; color:#64748b; font-weight:500;")
        )
      }
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        style = "margin-bottom:20px;",
        
        mk_kpi(n_ok(), "Channels OK",
               "circle-check", "#16a34a", "#f0fdf4", "#86efac"),
        
        mk_kpi(n_warnings(), "Warnings",
               "triangle-exclamation",
               if (n_warnings() > 0) "#d97706" else "#94a3b8",
               if (n_warnings() > 0) "#fffbeb" else "#f8fafc",
               if (n_warnings() > 0) "#fde68a" else "#e2e8f0"),
        
        mk_kpi(n_critical(), "Critical Issues",
               "circle-xmark",
               if (n_critical() > 0) "#dc2626" else "#94a3b8",
               if (n_critical() > 0) "#fef2f2" else "#f8fafc",
               if (n_critical() > 0) "#fca5a5" else "#e2e8f0"),
        
        mk_kpi(format(n_splits(), big.mark = ","), "Splits Matched",
               "layer-group", "#5B9BD5", "#EBF3FB", "#90caf9")
      )
    })
    
    # ── Channel Status ─────────────────────────────────────────────
    output$channel_status <- renderUI({
      summ <- channel_summary()
      
      if (!length(summ))
        return(div(
          class = "text-center py-4", style = "color:#94a3b8;",
          icon("circle-info",
               style = "font-size:28px; display:block; margin-bottom:8px; color:#cbd5e1;"),
          tags$p(style = "font-size:13px;",
                 "No channels configured. Go to the Channels tab to add channels.")
        ))
      
      tagList(lapply(summ, function(ch) {
        
        if (!ch$processed) {
          ico         <- icon("circle-xmark",
                              style = "color:#dc2626; font-size:14px; flex-shrink:0;")
          badge_text  <- "Not processed"
          badge_bg    <- "#fef2f2"; badge_col <- "#dc2626"
        } else if (ch$has_warning) {
          ico         <- icon("triangle-exclamation",
                              style = "color:#d97706; font-size:14px; flex-shrink:0;")
          badge_text  <- "Warning: 0 focus splits"
          badge_bg    <- "#fffbeb"; badge_col <- "#d97706"
        } else {
          ico         <- icon("circle-check",
                              style = "color:#16a34a; font-size:14px; flex-shrink:0;")
          badge_text  <- paste0(format(ch$n_splits, big.mark = ","), " splits")
          badge_bg    <- "#f0fdf4"; badge_col <- "#16a34a"
        }
        
        div(
          style = paste0(
            "display:flex; align-items:center; gap:12px;",
            "padding:11px 16px; border-bottom:1px solid #f1f5f9;"
          ),
          ico,
          tags$span(ch$name,
                    style = "flex:1; font-size:13px; font-weight:600; color:#1e293b;"),
          tags$span(badge_text,
                    style = paste0(
                      "background:", badge_bg, "; color:", badge_col, ";",
                      "font-size:12px; font-weight:600;",
                      "padding:3px 12px; border-radius:10px;"))
        )
      }))
    })
    
    # ── File dimensions (fast — uses metadata, no full build) ──────
    file_dims <- reactive({
      res_list <- results()
      d        <- data()
      gcfg     <- config()
      
      cross_id         <- c(gcfg$cross_cols %||% "Geography", "Period")
      total_splits     <- 0L
      total_split_cols <- 0L
      
      for (nm in names(res_list)) {
        r <- res_list[[nm]]
        if (is.null(r)) next
        if (!is.null(r$side_mapping))
          total_splits <- total_splits + nrow(r$side_mapping)
        rag       <- as.data.frame(r$rag)
        id_in_rag <- intersect(cross_id, names(rag))
        total_split_cols <- total_split_cols +
          length(setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag))
      }
      
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical) else 0L
      an_cols <- if (!is.null(d$analytical)) ncol(d$analytical) + total_split_cols else 0L
      n_ch    <- length(channels())
      
      list(
        analytical = if (an_rows > 0) list(rows = an_rows, cols = an_cols) else NULL,
        side_map   = if (total_splits > 0) list(rows = total_splits, cols = 6L) else NULL,
        activity   = if (total_splits > 0) list(rows = total_splits, cols = 5L) else NULL,
        config     = if (n_ch > 0) list(rows = n_ch, cols = NULL) else NULL
      )
    })
    
    # ── Export Contents UI ─────────────────────────────────────────
    output$export_contents <- renderUI({
      dims <- file_dims()
      
      mk_file_card <- function(ico, title, desc, dims_info, color, bg) {
        div(
          style = paste0(
            "background:#f8fafc; border:1px solid #e2e8f0;",
            "border-radius:8px; padding:18px 14px; text-align:center;"
          ),
          div(
            style = paste0(
              "width:44px; height:44px; background:", bg, ";",
              "border-radius:10px; display:flex; align-items:center;",
              "justify-content:center; margin:0 auto 10px;"
            ),
            icon(ico, style = paste0("color:", color, "; font-size:18px;"))
          ),
          tags$strong(title,
                      style = "font-size:12.5px; color:#1e293b; display:block; margin-bottom:4px;"),
          tags$p(desc,
                 style = "font-size:11px; color:#94a3b8; margin-bottom:10px; line-height:1.4;"),
          if (!is.null(dims_info))
            div(
              style = "display:flex; gap:6px; justify-content:center; flex-wrap:wrap;",
              tags$span(
                style = paste0(
                  "background:", bg, "; color:", color, ";",
                  "padding:2px 10px; border-radius:6px; font-size:11.5px; font-weight:600;"
                ),
                paste0(format(dims_info$rows, big.mark = ","), " rows")
              ),
              if (!is.null(dims_info$cols))
                tags$span(
                  style = paste0(
                    "background:#f1f5f9; color:#475569;",
                    "padding:2px 10px; border-radius:6px; font-size:11.5px; font-weight:600;"
                  ),
                  paste0(dims_info$cols, " cols")
                )
            )
          else
            tags$span(style = "color:#cbd5e1; font-size:12px;",
                      icon("clock", style = "font-size:11px;"), " Awaiting processing")
        )
      }
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        mk_file_card("table-cells",
                     "Analytical Splits Extended",
                     "Original Analytical with all split columns appended. One row per Geography \u00d7 Product \u00d7 Period.",
                     dims$analytical, "#5B9BD5", "#EBF3FB"),
        
        mk_file_card("diagram-project",
                     "Side Model Mapping",
                     "Split-to-model-variable mapping with PSO weight structure.",
                     dims$side_map, "#7c3aed", "#f5f3ff"),
        
        mk_file_card("chart-column",
                     "Activity, Cost & ROIs",
                     "Activity and spend totals per split, enriched with ROI data.",
                     dims$activity, "#059669", "#ecfdf5"),
        
        mk_file_card("gear",
                     "Channel Config & Merges",
                     "Full channel configuration including merges, breaks and segment overrides.",
                     dims$config, "#d97706", "#fffbeb")
      )
    })
    
    # ── Download Section UI ────────────────────────────────────────
    output$download_section <- renderUI({
      summ    <- channel_summary()
      
      if (!length(summ))
        return(div(
          class = "text-center py-3",
          icon("circle-info",
               style = "font-size:24px; color:#94a3b8; display:block; margin-bottom:8px;"),
          tags$p(style = "color:#94a3b8; font-size:13px; margin-bottom:0;",
                 "Process at least one channel before exporting.")
        ))
      
      n_ready <- sum(sapply(summ, `[[`, "processed"))
      n_total <- length(summ)
      
      if (n_ready == 0)
        return(div(
          class = "text-center py-3",
          icon("circle-info",
               style = "font-size:24px; color:#94a3b8; display:block; margin-bottom:8px;"),
          tags$p(style = "color:#94a3b8; font-size:13px; margin-bottom:0;",
                 "Process at least one channel before exporting.")
        ))
      
      div(
        style = "text-align:center; padding-bottom:8px;",
        
        # Warning if some channels not processed
        if (n_ready < n_total)
          div(class = "alert alert-warning p-2 mb-3",
              style = "font-size:12px; text-align:left;",
              icon("triangle-exclamation"),
              paste0(" ", n_ready, " of ", n_total, " channel(s) processed. ",
                     n_total - n_ready, " will not be included.")),
        
        # Files included
        div(
          style = paste0(
            "background:#f8fafc; border:1px solid #e2e8f0;",
            "border-radius:8px; padding:12px 16px; margin-bottom:16px;",
            "text-align:left;"
          ),
          tags$strong("Contents of the ZIP:",
                      style = "font-size:12px; color:#1e293b; display:block; margin-bottom:8px;"),
          div(
            style = "display:flex; flex-wrap:wrap; gap:6px;",
            lapply(
              c("analytical_splits_extended.csv",
                "side_model_mapping.csv",
                "activity_cost_rois.csv",
                "channel_config.csv"),
              function(f) {
                tags$span(
                  style = paste0(
                    "background:white; border:1px solid #e2e8f0;",
                    "border-radius:5px; padding:3px 10px;",
                    "font-size:11.5px; color:#475569; font-family:monospace;"
                  ),
                  icon("file-csv", style = "color:#5B9BD5; font-size:10px; margin-right:3px;"),
                  f
                )
              }
            )
          )
        ),
        
        # Download button
        downloadButton(
          session$ns("dl_zip"),
          tagList(icon("file-zipper"), " Download All (ZIP)"),
          class = "btn-primary",
          style = paste0(
            "font-size:14px; padding:12px 0; border-radius:8px;",
            "width:100%; max-width:400px;"
          )
        ),
        
        tags$p(
          style = "font-size:11px; color:#94a3b8; margin-top:10px; margin-bottom:0;",
          paste0(n_ready, " channel(s) \u2014 ",
                 format(n_splits(), big.mark = ","), " total splits")
        )
      )
    })
    
    # ── Dataset builders (called only at download time) ────────────
    
    build_analytical_extended <- function() {
      d        <- data()
      res_list <- results()
      gcfg     <- config()
      if (is.null(d$analytical)) return(NULL)
      
      cross_cols <- gcfg$cross_cols %||% "Geography"
      cross_id   <- c(cross_cols, "Period")
      result     <- as.data.frame(d$analytical)
      
      for (nm in names(res_list)) {
        r <- res_list[[nm]]
        if (is.null(r)) next
        
        rag         <- as.data.frame(r$rag)
        is_rag_flag <- isTRUE(r$is_rag)
        join_key    <- if (is_rag_flag) intersect(cross_id, names(rag)) else "Period"
        id_in_rag   <- intersect(cross_id, names(rag))
        split_cols  <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        if (!length(split_cols)) next
        
        rag_sub  <- rag[, c(join_key, split_cols), drop = FALSE]
        conflict <- intersect(split_cols, names(result))
        if (length(conflict))
          names(rag_sub)[names(rag_sub) %in% conflict] <-
          paste0(names(rag_sub)[names(rag_sub) %in% conflict], "_", nm)
        
        result <- left_join(result, rag_sub, by = join_key)
      }
      
      result[is.na(result)] <- 0
      result
    }
    
    build_side_mapping <- function() {
      res_list <- results()
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r) || is.null(r$side_mapping) || !nrow(r$side_mapping)) return(NULL)
        r$side_mapping %>% mutate(Channel = nm)
      }))
      if (!length(rows)) return(NULL)
      bind_rows(rows)
    }
    
    build_activity_rois <- function() {
      d        <- data()
      res_list <- results()
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r) || is.null(r$activity_spend) || !nrow(r$activity_spend)) return(NULL)
        r$activity_spend
      }))
      if (!length(rows)) return(NULL)
      
      act_df <- bind_rows(rows)
      
      if (!is.null(d$channels_rois) && nrow(d$channels_rois) > 0) {
        tryCatch({
          by_cols <- intersect(names(act_df), names(d$channels_rois))
          if (length(by_cols))
            act_df <- left_join(act_df, d$channels_rois, by = by_cols)
        }, error = function(e) NULL)
      }
      
      act_df
    }
    
    # ── ZIP Download Handler ───────────────────────────────────────
    output$dl_zip <- downloadHandler(
      filename = function() {
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        
        tmp_dir <- file.path(tempdir(),
                             paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        written <- character(0)
        
        withProgress(message = "Preparing export...", value = 0, {
          
          # 1. Analytical Splits Extended
          incProgress(0.25, message = "Building Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "analytical_splits_extended.csv")
              readr::write_csv(df, f, na = "")
              written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Analytical error:", e$message), type = "warning", duration = 6))
          
          # 2. Side Model Mapping
          incProgress(0.25, message = "Building Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "side_model_mapping.csv")
              readr::write_csv(df, f, na = "")
              written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Side Mapping error:", e$message), type = "warning", duration = 6))
          
          # 3. Activity, Cost & ROIs
          incProgress(0.25, message = "Building Activity, Cost & ROIs...")
          tryCatch({
            df <- build_activity_rois()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "activity_cost_rois.csv")
              readr::write_csv(df, f, na = "")
              written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Activity error:", e$message), type = "warning", duration = 6))
          
          # 4. Channel Config & Merges
          incProgress(0.25, message = "Exporting configuration...")
          tryCatch({
            df <- export_channels_csv(channels())
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "channel_config.csv")
              readr::write_csv(df, f, na = "")
              written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Config error:", e$message), type = "warning", duration = 6))
        })
        
        if (!length(written)) {
          showNotification("No data available to export.", type = "warning")
          writeLines("no data", file)
          return()
        }
        
        zip(zipfile = file, files = written, flags = "-j")
      }
    )
    
  })
}