# ════════════════════════════════════════════════════════════════
# R/mod_export.R
# ════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # ── Summary strip ──────────────────────────────────────────────
    uiOutput(ns("summary_strip")),
    
    # ── Two-column layout ─────────────────────────────────────────
    layout_columns(
      col_widths = c(4, 8),
      
      # Left: Channel Status
      card(
        style = "height:100%;",
        card_header(
          div(style = "display:flex; align-items:center; gap:8px;",
              icon("circle-check", style = "color:#5B9BD5; font-size:13px;"),
              "Channel Status")
        ),
        uiOutput(ns("channel_status"))
      ),
      
      # Right: Export + Download
      tagList(
        card(
          card_header(
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              div(style = "display:flex; align-items:center; gap:8px;",
                  icon("file-zipper", style = "color:#5B9BD5; font-size:13px;"),
                  "Export Package"),
              uiOutput(ns("readiness_badge"))
            )
          ),
          div(style = "padding:16px 0 8px;",
              uiOutput(ns("export_contents"))
          ),
          # ROI coverage indicator
          uiOutput(ns("roi_coverage")),
          hr(style = "margin:8px 0 16px;"),
          uiOutput(ns("download_section"))
        )
      )
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
      d        <- data()
      gcfg     <- config()
      
      lapply(ch_names, function(nm) {
        res <- res_list[[nm]]
        if (is.null(res))
          return(list(name = nm, processed = FALSE,
                      has_warning = FALSE, tc_mismatch = FALSE, n_splits = 0L))
        
        n_splits_total <- if (!is.null(res$side_mapping) && nrow(res$side_mapping) > 0)
          nrow(res$side_mapping)
        else if (!is.null(res$act_diagnoses))
          n_distinct(res$act_diagnoses$VariableSplit %||% character(0))
        else 0L
        
        if (n_splits_total == 0L)
          return(list(name = nm, processed = TRUE, has_warning = TRUE,
                      tc_mismatch = FALSE, n_splits = 0L))
        
        # Quick Total Check — geographic channels only
        tc_mismatch <- tryCatch({
          if (is.null(d$analytical) || is.null(gcfg$cross_cols)) return(FALSE)
          if (!isTRUE(res$is_rag)) return(FALSE)
          
          cfg        <- channels()[[nm]]
          cross_cols <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
          cross_id   <- c(cross_cols, "Period")
          geo_col    <- cross_cols[1]
          
          an_periods <- sort(unique(d$analytical$Period))
          an_min_p   <- min(an_periods)
          an_max_p   <- max(an_periods)
          
          model_df  <- build_model_total(d$analytical, cross_id,
                                         cfg$model_variables, cfg$break_dates)
          model_sum <- sum(
            model_df$ModelTotal[model_df$Period >= an_min_p &
                                  model_df$Period <= an_max_p],
            na.rm = TRUE)
          if (model_sum == 0) return(FALSE)
          
          rag_df    <- as.data.frame(res$rag)
          rag_scope <- rag_df[rag_df$Period >= an_min_p & rag_df$Period <= an_max_p, ]
          id_in_rag  <- intersect(cross_id, names(rag_scope))
          split_cols <- setdiff(names(rag_scope)[sapply(rag_scope, is.numeric)], id_in_rag)
          if (!length(split_cols)) return(FALSE)
          
          splits_sum <- sum(rowSums(rag_scope[, split_cols, drop = FALSE], na.rm = TRUE))
          abs(model_sum - splits_sum) / max(abs(model_sum), 1) > 0.05
          
        }, error = function(e) FALSE)
        
        list(
          name        = nm,
          processed   = TRUE,
          has_warning = tc_mismatch,
          tc_mismatch = tc_mismatch,
          n_splits    = n_splits_total
        )
      })
    })
    
    # ── KPI aggregates ─────────────────────────────────────────────
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
    
    # ── Summary strip ──────────────────────────────────────────────
    output$summary_strip <- renderUI({
      
      mk_stat <- function(ico, value, label, active_color, active_bg,
                          zero_color = "#94a3b8", zero_bg = "#f8fafc",
                          is_always_colored = FALSE) {
        active  <- is_always_colored || as.numeric(value) > 0
        i_color <- if (active) active_color else zero_color
        v_color <- if (active) active_color else "#94a3b8"
        bg      <- if (active) active_bg    else zero_bg
        
        div(
          style = "display:flex; align-items:center; gap:12px; padding:16px 24px; flex:1;",
          div(
            style = paste0(
              "width:40px; height:40px; border-radius:10px; flex-shrink:0;",
              "background:", bg, ";",
              "display:flex; align-items:center; justify-content:center;"
            ),
            icon(ico, style = paste0("color:", i_color, "; font-size:17px;"))
          ),
          div(
            tags$span(
              if (is.numeric(value)) format(value, big.mark = ",") else value,
              style = paste0("font-size:26px; font-weight:700; color:", v_color,
                             "; display:block; line-height:1;")),
            tags$span(label,
                      style = "font-size:11.5px; color:#94a3b8; font-weight:500; display:block; margin-top:3px;")
          )
        )
      }
      
      card(
        style = "margin-bottom:16px;",
        div(
          style = "display:flex; align-items:stretch;",
          mk_stat("circle-check", n_ok(), "Channels OK",
                  "#16a34a", "#f0fdf4", is_always_colored = n_ok() > 0),
          div(style = "width:1px; background:#e2e8f0; margin:12px 0; flex-shrink:0;"),
          mk_stat("triangle-exclamation", n_warnings(), "Warnings",
                  "#d97706", "#fffbeb"),
          div(style = "width:1px; background:#e2e8f0; margin:12px 0; flex-shrink:0;"),
          mk_stat("circle-xmark", n_critical(), "Critical Issues",
                  "#dc2626", "#fef2f2"),
          div(style = "width:1px; background:#e2e8f0; margin:12px 0; flex-shrink:0;"),
          mk_stat("layer-group",
                  format(n_splits(), big.mark = ","), "Splits Matched",
                  "#5B9BD5", "#EBF3FB", is_always_colored = TRUE)
        )
      )
    })
    
    # ── Channel Status ─────────────────────────────────────────────
    output$channel_status <- renderUI({
      summ <- channel_summary()
      
      if (!length(summ))
        return(div(
          style = "padding:24px 0; text-align:center; color:#94a3b8;",
          icon("layer-group",
               style = "font-size:24px; display:block; margin-bottom:8px; color:#e2e8f0;"),
          tags$p("No channels configured.", style = "font-size:13px; margin:0;")
        ))
      
      tagList(lapply(seq_along(summ), function(i) {
        ch      <- summ[[i]]
        is_last <- i == length(summ)
        
        if (!ch$processed) {
          ic  <- icon("circle", style = "color:#e2e8f0; font-size:14px; flex-shrink:0;")
          val <- tags$span("Not processed", style = "font-size:11.5px; color:#e2e8f0;")
        } else if (ch$has_warning && ch$tc_mismatch) {
          ic  <- icon("triangle-exclamation",
                      style = "color:#d97706; font-size:14px; flex-shrink:0;")
          val <- tags$span("Total Check mismatch",
                           style = "font-size:11.5px; color:#d97706; font-weight:600;")
        } else if (ch$has_warning) {
          ic  <- icon("triangle-exclamation",
                      style = "color:#d97706; font-size:14px; flex-shrink:0;")
          val <- tags$span("0 splits found",
                           style = "font-size:11.5px; color:#d97706; font-weight:600;")
        } else {
          ic  <- icon("circle-check",
                      style = "color:#16a34a; font-size:14px; flex-shrink:0;")
          val <- tags$span(
            paste0(format(ch$n_splits, big.mark = ","), " split",
                   if (ch$n_splits != 1) "s" else ""),
            style = "font-size:11.5px; color:#5B9BD5; font-weight:600;")
        }
        
        div(
          style = paste0(
            "display:flex; align-items:center; gap:12px; padding:11px 0;",
            if (!is_last) "border-bottom:1px solid #f8fafc;" else ""
          ),
          ic,
          tags$span(ch$name,
                    style = "flex:1; font-size:13px; color:#1e293b; font-weight:500;"),
          val
        )
      }))
    })
    
    # ── File dimensions ────────────────────────────────────────────
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
        rag        <- as.data.frame(r$rag)
        id_in_rag  <- intersect(cross_id, names(rag))
        total_split_cols <- total_split_cols +
          length(setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag))
      }
      
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical)        else 0L
      an_cols <- if (!is.null(d$analytical)) ncol(d$analytical) + total_split_cols else 0L
      n_ch    <- length(channels())
      
      list(
        analytical = if (an_rows > 0) list(rows = an_rows, cols = an_cols) else NULL,
        side_map   = if (total_splits > 0) list(rows = total_splits, cols = 6L) else NULL,
        activity   = if (total_splits > 0) list(rows = total_splits, cols = 5L) else NULL,
        config     = if (n_ch > 0) list(rows = n_ch, cols = NULL) else NULL
      )
    })
    
    # ── Readiness badge ────────────────────────────────────────────
    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, function(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      if (!n_total) return(NULL)
      
      if (n_ready == n_total)
        tags$span(
          style = "background:#dcfce7; color:#166534; font-size:11px; font-weight:600; padding:3px 10px; border-radius:10px;",
          icon("circle-check", style = "font-size:10px;"),
          paste0(" ", n_ready, "/", n_total, " ready"))
      else
        tags$span(
          style = "background:#fff3cd; color:#92400e; font-size:11px; font-weight:600; padding:3px 10px; border-radius:10px;",
          icon("triangle-exclamation", style = "font-size:10px;"),
          paste0(" ", n_ready, "/", n_total, " ready"))
    })
    
    # ── Export Contents ────────────────────────────────────────────
    output$export_contents <- renderUI({
      dims <- file_dims()
      
      mk_file_row <- function(ico, filename, description,
                              dims_info, color, bg, is_last = FALSE) {
        div(
          style = paste0(
            "display:flex; align-items:center; gap:14px; padding:13px 0;",
            if (!is_last) "border-bottom:1px solid #f8fafc;" else ""
          ),
          div(
            style = paste0(
              "width:36px; height:36px; border-radius:8px;",
              "background:", bg, "; flex-shrink:0;",
              "display:flex; align-items:center; justify-content:center;"
            ),
            icon(ico, style = paste0("color:", color, "; font-size:15px;"))
          ),
          div(
            style = "flex:1; min-width:0;",
            tags$span(filename,
                      style = paste0(
                        "font-size:12px; font-weight:600; color:#1e293b;",
                        "display:block; font-family:'JetBrains Mono', monospace;",
                        "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;")),
            tags$span(description,
                      style = "font-size:11.5px; color:#94a3b8; display:block; margin-top:2px;")
          ),
          if (!is.null(dims_info))
            div(
              style = "text-align:right; flex-shrink:0;",
              tags$span(
                paste0(format(dims_info$rows, big.mark = ","),
                       if (!is.null(dims_info$cols)) paste0(" \u00d7 ", dims_info$cols) else ""),
                style = "font-size:11.5px; font-weight:600; color:#475569; display:block;"),
              tags$span(
                if (!is.null(dims_info$cols)) "rows \u00d7 cols" else "channels",
                style = "font-size:10.5px; color:#cbd5e1; display:block;"))
          else
            div(style = "text-align:right; flex-shrink:0;",
                tags$span("\u2014", style = "font-size:12px; color:#e2e8f0;"))
        )
      }
      
      div(
        mk_file_row("table-cells",    "analytical_splits_extended.csv",
                    "Analytical with all split columns appended",
                    dims$analytical, "#5B9BD5", "#EBF3FB"),
        mk_file_row("diagram-project", "side_model_mapping.csv",
                    "Split-to-model mapping with PSO weight structure",
                    dims$side_map, "#7c3aed", "#f5f3ff"),
        mk_file_row("chart-column",    "activity_cost_rois.csv",
                    "Activity and spend totals enriched with ROI data",
                    dims$activity, "#059669", "#ecfdf5"),
        mk_file_row("gear",            "channel_config.csv",
                    "Configuration, merges, breaks and segment overrides",
                    dims$config, "#d97706", "#fffbeb", is_last = TRUE)
      )
    })
    
    # ── ROI coverage indicator ─────────────────────────────────────
    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- data()$channels_rois
      
      # Validate required columns
      if (!all(c("Channel", "MainModelVariableName") %in% names(roi_df)))
        return(div(
          style = paste0(
            "background:#fef2f2; border:1px solid #fca5a5;",
            "border-radius:8px; padding:10px 14px; margin:0 0 4px;"
          ),
          div(style = "display:flex; align-items:center; gap:8px;",
              icon("circle-xmark", style = "color:#dc2626; font-size:14px;"),
              tags$span(
                paste0("ROIs file missing required column(s): ",
                       paste(setdiff(c("Channel", "MainModelVariableName"),
                                     names(roi_df)), collapse = ", ")),
                style = "font-size:12.5px; color:#991b1b; font-weight:600;"
              ))
        ))
      
      # Collect Channel + MainModelVariableName from processed results
      res_list <- results()
      if (!length(res_list)) return(NULL)
      
      all_combos <- bind_rows(lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r$side_mapping) || !nrow(r$side_mapping)) return(NULL)
        r$side_mapping %>%
          select(MainModelVariableName) %>%
          distinct() %>%
          mutate(Channel = nm)
      }))
      
      if (!nrow(all_combos)) return(NULL)
      
      # Check coverage
      checked <- all_combos %>%
        left_join(
          roi_df %>%
            select(Channel, MainModelVariableName) %>%
            mutate(.has_roi = TRUE),
          by = c("Channel", "MainModelVariableName")
        )
      
      n_total   <- nrow(all_combos)
      n_matched <- sum(!is.na(checked$.has_roi))
      n_missing <- n_total - n_matched
      
      if (n_missing == 0) {
        div(
          style = paste0(
            "background:#f0fdf4; border:1px solid #86efac;",
            "border-radius:8px; padding:10px 14px; margin:0 0 4px;"
          ),
          div(style = "display:flex; align-items:center; gap:8px;",
              icon("circle-check", style = "color:#16a34a; font-size:14px;"),
              tags$span(
                paste0("All ", n_total, " model variable(s) matched with ROI values."),
                style = "font-size:12.5px; color:#166534; font-weight:600;"
              ))
        )
      } else {
        missing_rows <- checked %>%
          filter(is.na(.has_roi)) %>%
          mutate(label = paste0(Channel, "  \u2192  ", MainModelVariableName)) %>%
          pull(label)
        
        div(
          style = paste0(
            "background:#fffbeb; border:1px solid #fde68a;",
            "border-radius:8px; padding:12px 16px; margin:0 0 4px;"
          ),
          div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
            div(style = "display:flex; align-items:center; gap:8px;",
                icon("triangle-exclamation", style = "color:#d97706; font-size:15px;"),
                tags$strong(
                  paste0(n_missing, " of ", n_total, " model variable(s) have no ROI"),
                  style = "font-size:13px; color:#92400e;"
                )),
            tags$span(
              paste0(n_matched, "/", n_total, " matched"),
              style = paste0(
                "background:#fef3c7; color:#92400e; font-size:11.5px;",
                "font-weight:600; padding:2px 10px; border-radius:10px;"
              )
            )
          ),
          div(
            style = "border-top:1px solid #fde68a; padding-top:8px;",
            tagList(lapply(seq_along(head(missing_rows, 6)), function(i) {
              div(
                style = paste0(
                  "display:flex; align-items:center; gap:8px; padding:4px 0;",
                  if (i < min(6, length(missing_rows)))
                    "border-bottom:1px solid #fef3c7;" else ""
                ),
                div(style = "width:6px; height:6px; border-radius:50%; background:#d97706; flex-shrink:0;"),
                tags$span(missing_rows[[i]],
                          style = "font-size:12px; color:#92400e; font-family:monospace;")
              )
            })),
            if (length(missing_rows) > 6)
              tags$p(
                paste0("... and ", length(missing_rows) - 6, " more"),
                style = "font-size:11.5px; color:#b45309; margin:6px 0 0;")
          )
        )
      }
    })
    
    # ── Download Section ───────────────────────────────────────────
    output$download_section <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, function(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      
      if (n_ready == 0)
        return(div(
          style = "text-align:center; padding:20px; color:#94a3b8;",
          icon("hourglass-half",
               style = "font-size:20px; color:#cbd5e1; display:block; margin-bottom:8px;"),
          tags$p("Process channels first.", style = "font-size:13px; margin:0;")
        ))
      
      div(
        style = "text-align:center; padding-bottom:8px;",
        
        if (n_ready < n_total)
          div(class = "alert alert-warning p-2 mb-3",
              style = "font-size:12px; text-align:left;",
              icon("triangle-exclamation"),
              paste0(" ", n_ready, " of ", n_total, " channel(s) processed. ",
                     n_total - n_ready, " will not be included.")),
        
        downloadButton(
          session$ns("dl_zip"),
          tagList(icon("file-zipper"), " Download All  (ZIP)"),
          class = "btn-primary",
          style = "font-size:14px; padding:13px 0; border-radius:8px; width:100%; max-width:400px;"
        ),
        
        div(
          style = "display:flex; justify-content:center; gap:20px; margin-top:10px; padding-top:10px; border-top:1px solid #f1f5f9;",
          tags$span(style = "display:flex; align-items:center; gap:5px; font-size:11.5px; color:#94a3b8;",
                    icon("circle-check", style = "color:#16a34a; font-size:11px;"),
                    paste0(n_ready, " channel", if (n_ready != 1) "s" else "")),
          tags$span(style = "display:flex; align-items:center; gap:5px; font-size:11.5px; color:#94a3b8;",
                    icon("layer-group", style = "color:#5B9BD5; font-size:11px;"),
                    paste0(format(n_splits(), big.mark = ","), " splits")),
          tags$span(style = "display:flex; align-items:center; gap:5px; font-size:11.5px; color:#94a3b8;",
                    icon("file-csv", style = "color:#94a3b8; font-size:11px;"),
                    "4 files")
        )
      )
    })
    
    # ══ Dataset builders ═══════════════════════════════════════════
    
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
        valid_jk    <- intersect(join_key, intersect(names(rag), names(result)))
        if (!length(valid_jk)) next
        
        id_in_rag  <- intersect(cross_id, names(rag))
        split_cols <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        if (!length(split_cols)) next
        
        rag_sub  <- rag[, c(valid_jk, split_cols), drop = FALSE]
        conflict <- intersect(split_cols, names(result))
        if (length(conflict))
          names(rag_sub)[names(rag_sub) %in% conflict] <-
          paste0(names(rag_sub)[names(rag_sub) %in% conflict], "_", nm)
        
        result <- left_join(result, rag_sub, by = valid_jk)
      }
      
      result[is.na(result)] <- 0
      result
    }
    
    build_side_mapping <- function() {
      res_list <- results()
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r) || is.null(r$side_mapping) || !nrow(r$side_mapping)) return(NULL)
        r$side_mapping %>% mutate(Channel = nm) %>% select(Channel, everything())
      }))
      if (!length(rows)) return(NULL)
      bind_rows(rows)
    }
    
    build_activity_rois <- function() {
      d        <- data()
      res_list <- results()
      gcfg     <- config()
      
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r   <- res_list[[nm]]
        cfg <- channels()[[nm]]
        if (is.null(r) || is.null(r$rag)) return(NULL)
        
        rag_df     <- as.data.frame(r$rag)
        cross_cols <- r$cross_cols %||% gcfg$cross_cols %||% "Geography"
        cross_id   <- c(cross_cols, "Period")
        id_in_rag  <- intersect(cross_id, names(rag_df))
        geo_col    <- cross_cols[1]
        
        act_kw   <- cfg$activity_keyword %||% "Impressions"
        spend_kw <- cfg$spend_keyword    %||% "Spend"
        
        all_split_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_in_rag)
        act_cols  <- grep(act_kw,   all_split_cols, ignore.case = TRUE, value = TRUE)
        cost_cols <- grep(spend_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
        if (!length(act_cols)) return(NULL)
        
        # ── Dedup by cross-section × Period (remove product duplication) ──
        if (geo_col %in% names(rag_df)) {
          cs_period <- rag_df %>%
            group_by(across(all_of(c(geo_col, "Period")))) %>%
            summarise(
              across(all_of(all_split_cols), \(x) max(x, na.rm = TRUE)),
              .groups = "drop"
            ) %>%
            as.data.frame()
        } else {
          cs_period <- rag_df
        }
        
        # ── Compute total per split: national vs local ─────────────────
        # National (same value per geo per period): use one value per period
        # Local (different values per geo):         sum all geos per period
        get_total <- function(col) {
          periods      <- sort(unique(cs_period$Period))
          test_periods <- head(periods, min(5, length(periods)))
          
          is_local <- FALSE
          for (p in test_periods) {
            vals     <- cs_period[[col]][cs_period$Period == p]
            non_zero <- vals[!is.na(vals) & vals > 0]
            if (length(non_zero) >= 2 &&
                diff(range(non_zero)) / max(non_zero) > 0.01) {
              is_local <- TRUE; break
            }
          }
          
          if (is_local) {
            sum(cs_period[[col]], na.rm = TRUE)
          } else {
            total <- 0L
            for (p in periods) {
              vals     <- cs_period[[col]][cs_period$Period == p]
              non_zero <- vals[!is.na(vals) & vals > 0]
              total    <- total + if (length(non_zero) > 0) max(non_zero) else 0
            }
            total
          }
        }
        
        act_totals  <- sapply(act_cols,  get_total)
        cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_total) else numeric(0)
        
        # ── One row per VariableSplit ───────────────────────────────────
        act_df <- tibble(
          VariableSplit  = act_cols,
          total_activity = unname(act_totals),
          key = str_remove_all(act_cols, regex(act_kw, ignore_case = TRUE))
        )
        
        cost_df <- if (length(cost_totals))
          tibble(
            total_spend = unname(cost_totals),
            key = str_remove_all(cost_cols, regex(spend_kw, ignore_case = TRUE))
          )
        else
          tibble(total_spend = numeric(0), key = character(0))
        
        result <- act_df %>%
          left_join(cost_df, by = "key") %>%
          select(-key) %>%
          filter(total_activity > 0) %>%
          mutate(Channel = nm, MainModelVariableName = NA_character_)
        
        # Add MainModelVariableName from side_mapping
        if (!is.null(r$side_mapping) && nrow(r$side_mapping) > 0) {
          sm <- r$side_mapping %>%
            select(VariableSplit, MainModelVariableName) %>%
            distinct(VariableSplit, .keep_all = TRUE)
          result <- result %>%
            left_join(sm, by = "VariableSplit", suffix = c("", "_sm")) %>%
            mutate(
              MainModelVariableName = coalesce(MainModelVariableName_sm,
                                               MainModelVariableName)
            ) %>%
            select(-any_of("MainModelVariableName_sm"))
        }
        
        if (!nrow(result)) return(NULL)
        result %>%
          select(VariableSplit, total_activity, total_spend,
                 Channel, MainModelVariableName)
      }))
      
      if (!length(rows)) return(NULL)
      act_df <- bind_rows(rows)
      
      # ── ROI enrichment — join by Channel + MainModelVariableName ────
      if (!is.null(d$channels_rois) && nrow(d$channels_rois) > 0) {
        tryCatch({
          roi_df    <- d$channels_rois
          join_cols <- c("Channel", "MainModelVariableName")
          
          missing_join <- setdiff(join_cols, names(roi_df))
          if (length(missing_join) > 0) {
            showNotification(
              paste0("ROIs file missing column(s): ",
                     paste(missing_join, collapse = ", ")),
              type = "warning", duration = 8)
          } else {
            roi_num_cols <- setdiff(
              names(roi_df)[sapply(roi_df, is.numeric)],
              names(act_df)
            )
            if (length(roi_num_cols) > 0) {
              roi_clean <- roi_df %>%
                select(all_of(c(join_cols, roi_num_cols)))
              act_df <- left_join(act_df, roi_clean, by = join_cols)
              
              # ── Detect unmatched rows ──────────────────────────────
              unmatched <- act_df %>%
                filter(if_any(all_of(roi_num_cols), is.na)) %>%
                distinct(Channel, MainModelVariableName) %>%
                arrange(Channel)
              
              if (nrow(unmatched) > 0) {
                msg_lines <- paste0(unmatched$Channel, " \u2192 ",
                                    unmatched$MainModelVariableName)
                showNotification(
                  tagList(
                    icon("triangle-exclamation",
                         style = "color:#d97706; margin-right:6px;"),
                    tags$strong(paste0(nrow(unmatched), " ROI(s) not matched:")),
                    tags$ul(
                      style = "margin:6px 0 0; padding-left:16px; font-size:12px;",
                      lapply(head(msg_lines, 5), tags$li),
                      if (nrow(unmatched) > 5)
                        tags$li(paste0("... and ", nrow(unmatched) - 5, " more"))
                    )
                  ),
                  type = "warning", duration = 15
                )
              }
            }
          }
        }, error = function(e)
          showNotification(paste("ROI join error:", e$message),
                           type = "warning", duration = 6))
      }
      
      act_df
    }
    
    # ── ZIP Download ───────────────────────────────────────────────
    output$dl_zip <- downloadHandler(
      filename = function() {
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        
        tmp_dir <- file.path(tempdir(), paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        written <- character(0)
        
        withProgress(message = "Building export files...", value = 0, {
          
          incProgress(0.25, message = "Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "analytical_splits_extended.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Analytical error:", e$message), type = "warning", duration = 6))
          
          incProgress(0.25, message = "Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "side_model_mapping.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Side Mapping error:", e$message), type = "warning", duration = 6))
          
          incProgress(0.25, message = "Activity, Cost & ROIs...")
          tryCatch({
            df <- build_activity_rois()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "activity_cost_rois.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Activity error:", e$message), type = "warning", duration = 6))
          
          incProgress(0.2, message = "Channel configuration...")
          tryCatch({
            df <- export_channels_csv(channels())
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "channel_config.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(
            paste("Config error:", e$message), type = "warning", duration = 6))
          
          incProgress(0.05, message = "Creating ZIP archive...")
        })
        
        if (!length(written)) {
          showNotification("No data available to export.", type = "warning")
          writeLines("no data", file); return()
        }
        
        # zip package: cross-platform, no system zip executable required
        tryCatch(
          zip::zipr(
            zipfile = file,
            files   = basename(written),
            root    = tmp_dir
          ),
          error = function(e)
            showNotification(paste("ZIP creation failed:", conditionMessage(e)),
                             type = "error", duration = 10)
        )
      }
    )
    
  })
}