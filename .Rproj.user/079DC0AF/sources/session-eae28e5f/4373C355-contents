# ══════════════════════════════════════════════════════════════════
# R/mod_process.R
# ══════════════════════════════════════════════════════════════════

mod_process_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    
    tagList(
      card(
        card_header("Run Processing"),
        selectInput(ns("channel_select"), "Select Channel",
                    choices = NULL),
        actionButton(ns("btn_one"), "Process Selected",
                     class = "btn-success btn-sm w-100 mb-2"),
        actionButton(ns("btn_all"), "Process All",
                     class = "btn-warning btn-sm w-100"),
        hr(),
        uiOutput(ns("status"))
      ),
      uiOutput(ns("merge_history_card"))
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          style = "display:flex; align-items:center; gap:10px;",
          actionButton(ns("btn_prev"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm",
                       style = "padding:3px 8px;"),
          div(style = "flex:1;", uiOutput(ns("channel_pos"))),
          actionButton(ns("btn_next"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm",
                       style = "padding:3px 8px;")
        )
      ),
      navset_card_underline(
        nav_panel(
          "Activity",
          uiOutput(ns("period_filter_ui")),
          uiOutput(ns("activity_kpis")),
          uiOutput(ns("threshold_ui")),
          uiOutput(ns("merge_toolbar")),
          DTOutput(ns("diag_act"))
        ),
        nav_panel("Spend",       DTOutput(ns("diag_cost"))),
        nav_panel("Total Check", DTOutput(ns("diag_check")))
      )
    )
  )
}

mod_process_server <- function(id, data, config, channels) {
  moduleServer(id, function(input, output, session) {
    
    # ── State ─────────────────────────────────────────────────
    # reactiveVal(list) ensures reliable reactive propagation
    results_rv   <- reactiveVal(list())
    original_rv  <- reactiveVal(list())
    merge_log_rv <- reactiveVal(list())
    history_rv   <- reactiveVal(list())
    cs_selected  <- reactiveVal("ALL")   # selected cross-section
    
    # ── Accessors ─────────────────────────────────────────────
    get_res  <- function(nm) results_rv()[[nm]]
    set_res  <- function(nm, val) {
      x <- isolate(results_rv())
      x[[nm]] <- val
      results_rv(x)
    }
    get_orig <- function(nm) original_rv()[[nm]]
    set_orig <- function(nm, val) {
      x <- isolate(original_rv())
      x[[nm]] <- val
      original_rv(x)
    }
    get_log  <- function(nm) merge_log_rv()[[nm]] %||% list()
    set_log  <- function(nm, val) {
      x <- isolate(merge_log_rv())
      x[[nm]] <- val
      merge_log_rv(x)
    }
    get_hist <- function(nm) history_rv()[[nm]] %||% list()
    set_hist <- function(nm, val) {
      x <- isolate(history_rv())
      x[[nm]] <- val
      history_rv(x)
    }
    
    # ── Helpers ───────────────────────────────────────────────
    
    valid_nm <- function(nm) {
      !is.null(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)
    }
    
    make_export_buttons <- function(prefix, nm) {
      fname <- paste0(prefix, "_", nm, "_",
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
      list(
        list(extend = "csv",   text = "Download CSV",
             filename = fname, className = "dt-button",
             exportOptions = list(modifier = list(page = "all"))),
        list(extend = "excel", text = "Download Excel",
             filename = fname, className = "dt-button",
             exportOptions = list(modifier = list(page = "all")))
      )
    }
    
    fmt_compact <- function(x) {
      x <- as.numeric(x)
      dplyr::case_when(
        abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"),
        abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
        abs(x) >= 1e3 ~ paste0(round(x / 1e3, 0), "K"),
        TRUE          ~ formatC(round(x), format = "f",
                                digits = 0, big.mark = ",")
      )
    }
    
    strip_common_prefix <- function(names_vec) {
      if (length(names_vec) <= 1) return(names_vec)
      parts      <- strsplit(names_vec, "_")
      min_len    <- min(sapply(parts, length))
      if (min_len == 0) return(names_vec)
      common_len <- 0L
      for (i in seq_len(min_len)) {
        if (length(unique(sapply(parts, `[[`, i))) == 1L)
          common_len <- i
        else break
      }
      if (common_len == 0L) return(names_vec)
      sapply(parts, function(p) {
        rest <- p[(common_len + 1):length(p)]
        if (!length(rest)) paste(p, collapse = "_")
        else               paste(rest, collapse = "_")
      })
    }
    
    build_model_total <- function(analytical, cross_id,
                                  model_variables, break_dates) {
      n_vars        <- length(model_variables)
      break_dates_d <- as.Date(break_dates %||% character(0))
      base <- analytical %>%
        select(all_of(cross_id)) %>%
        mutate(ModelTotal = 0)
      for (i in seq_len(n_vars)) {
        mv <- model_variables[i]
        if (!mv %in% names(analytical)) next
        seg_start <- if (i == 1)      as.Date("1900-01-01")
        else                           break_dates_d[i - 1] + 1
        seg_end   <- if (i == n_vars) as.Date("2999-12-31")
        else                           break_dates_d[i]
        seg_vals  <- analytical %>%
          filter(Period >= seg_start, Period <= seg_end) %>%
          select(all_of(cross_id), model_val = !!sym(mv))
        base <- base %>%
          left_join(seg_vals, by = cross_id) %>%
          mutate(ModelTotal = if_else(!is.na(model_val),
                                      model_val, ModelTotal)) %>%
          select(-model_val)
      }
      base
    }
    
    # ── Cross-section choices (only for all_rags channels) ────
    cross_section_choices <- reactive({
      nm  <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      res <- results_rv()[[nm]]
      # Only show selector for all_rags (is_rag = FALSE)
      if (is.null(res) || isTRUE(res$is_rag)) return(NULL)
      cross_cols_r <- res$cross_cols %||% "Geography"
      rag_df       <- as.data.frame(res$rag)
      cross_data   <- rag_df[, cross_cols_r, drop = FALSE]
      cross_keys   <- do.call(paste,
                              c(as.list(cross_data), list(sep = " / ")))
      sort(unique(cross_keys))
    })
    
    # Reset cross-section when channel changes
    observeEvent(input$channel_select, {
      cs_selected("ALL")
    }, ignoreInit = TRUE)
    
    # Update cs_selected when selector changes
    observeEvent(input$cs_select, {
      val <- input$cs_select %||% "ALL"
      cs_selected(if (nzchar(val)) val else "ALL")
    }, ignoreInit = TRUE)
    
    # ── Sync channel selector ────────────────────────────────
    observe({
      updateSelectInput(session, "channel_select",
                        choices = names(channels()))
    })
    
    # ── Prev / Next ───────────────────────────────────────────
    observeEvent(input$btn_prev, {
      nms <- names(channels())
      if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur > 1)
        updateSelectInput(session, "channel_select",
                          selected = nms[cur - 1])
    })
    observeEvent(input$btn_next, {
      nms <- names(channels())
      if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur < length(nms))
        updateSelectInput(session, "channel_select",
                          selected = nms[cur + 1])
    })
    
    output$channel_pos <- renderUI({
      nms <- names(channels())
      nm  <- input$channel_select
      if (!length(nms) || !valid_nm(nm)) return(NULL)
      idx <- which(nms == nm)
      if (!length(idx)) idx <- 0L
      tagList(
        tags$strong(nm, style = "font-size:13px; color:#2c3e50;"),
        tags$span(paste0(" (", idx, " / ", length(nms), ")"),
                  style = "font-size:11px; color:#8a9bb0;")
      )
    })
    
    # ── Status ────────────────────────────────────────────────
    output$status <- renderUI({
      all_res    <- results_rv()
      ch_names   <- names(channels())
      done       <- ch_names[sapply(ch_names,
                                    function(n) !is.null(all_res[[n]]))]
      pending_ch <- setdiff(ch_names, done)
      tagList(
        if (length(done))
          tags$p(class = "text-success small",
                 icon("check"), paste(length(done), "processed")),
        if (length(pending_ch))
          tags$p(class = "text-warning small",
                 icon("clock"), paste(length(pending_ch), "pending"))
      )
    })
    
    # ── Run engine ────────────────────────────────────────────
    run_one <- function(nm) {
      d    <- data()
      cfg  <- channels()[[nm]]
      req(cfg)
      gcfg <- config()
      src  <- cfg$data_source %||% "all_transformed"
      
      if (src == "all_rags" && is.null(d$all_rags)) {
        showNotification(paste0(nm, ": all_RAGs not uploaded."),
                         type = "error", duration = 8)
        return()
      }
      if (src == "all_transformed" && is.null(d$all_transformed)) {
        showNotification(paste0(nm, ": all_transformed not uploaded."),
                         type = "error", duration = 8)
        return()
      }
      if (is.null(d$analytical)) {
        showNotification("AnalyticalDataset not uploaded.",
                         type = "error", duration = 10)
        return()
      }
      if (is.null(d$dates_df)) {
        showNotification("dates_df is NULL.",
                         type = "error", duration = 10)
        return()
      }
      n <- length(cfg$model_variables)
      if (length(cfg$break_dates) != n - 1) {
        showNotification(
          paste0(nm, ": expected ", n - 1, " break date(s), got ",
                 length(cfg$break_dates)),
          type = "error", duration = 8)
        return()
      }
      
      # Compute inside withProgress
      res_stored <- NULL
      err_stored <- NULL
      
      withProgress(message = paste("Processing", nm, "..."),
                   value = 0.5, {
                     tryCatch({
                       res_stored <- process_channel(
                         all_transformeds  = d$all_transformed,
                         all_rags          = d$all_rags,
                         analytical        = d$analytical,
                         dates_df          = d$dates_df,
                         cfg               = cfg,
                         cross_cols        = gcfg$cross_cols %||% "Geography",
                         start_report_date = gcfg$start_report_date,
                         end_report_date   = gcfg$end_report_date,
                         update_label      = gcfg$update_label
                       )
                     }, error = function(e) {
                       err_stored <<- conditionMessage(e)
                     })
                   })
      
      # Assign results OUTSIDE withProgress so reactive
      # invalidation propagates correctly to all outputs
      if (!is.null(err_stored)) {
        showNotification(paste(nm, "error:", err_stored),
                         type = "error", duration = 12)
        return()
      }
      if (!is.null(res_stored)) {
        set_res(nm,  res_stored)
        set_orig(nm, res_stored)
        set_log(nm,  list())
        set_hist(nm, list())
        showNotification(paste(nm, "processed successfully."),
                         type = "message")
      }
    }
    
    observeEvent(input$btn_one, {
      nm <- req(input$channel_select)
      req(valid_nm(nm))
      run_one(nm)
    })
    observeEvent(input$btn_all, {
      walk(names(channels()), run_one)
    })
    
    # ── Period filter UI with cross-section selector ──────────
    output$period_filter_ui <- renderUI({
      nm  <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      res <- results_rv()[[nm]]
      if (is.null(res) || nrow(res$act_diagnoses) == 0)
        return(NULL)
      
      diag    <- res$act_diagnoses
      n_focus <- sum(diag$period == "focus",    na.rm = TRUE)
      n_nf    <- sum(diag$period == "nonfocus", na.rm = TRUE)
      n_all   <- nrow(diag)
      choices <- cross_section_choices()
      cs_val  <- cs_selected()
      
      tagList(
        
        # Cross-section selector — only for all_rags channels
        if (!is.null(choices)) {
          div(
            style = paste0(
              "background:#f4f6f9; border-radius:8px;",
              "padding:10px 16px; margin-bottom:8px;",
              "display:flex; align-items:center; gap:12px;"),
            tags$span(
              icon("globe", style = "color:#5B9BD5;
                 font-size:13px;"),
              tags$strong(" Cross-section:",
                          style = "font-size:13px; color:#333;
                                 white-space:nowrap;")),
            div(
              style = "flex:1;",
              selectInput(
                session$ns("cs_select"), NULL,
                choices  = c("ALL (sum all geos)" = "ALL",
                             choices),
                selected = cs_val,
                width    = "100%"
              )
            ),
            tags$span(
              style = "font-size:11px; color:#6c757d;
                     white-space:nowrap;",
              if (cs_val == "ALL")
                tagList(icon("sigma"), " Sum all")
              else
                tagList(icon("filter"), " Specific")
            )
          )
        },
        
        # Period filter
        div(
          style = paste0(
            "background:#f4f6f9; border-radius:8px;",
            "padding:10px 16px; margin-bottom:10px;",
            "display:flex; align-items:center; gap:16px;"),
          tags$span(
            icon("filter", style = "color:#5B9BD5;"),
            tags$strong(" View:",
                        style = "font-size:13px; color:#333;")),
          radioButtons(session$ns("period_filter"), NULL,
                       choices  = c("Focus"     = "focus",
                                    "Non-Focus" = "nonfocus",
                                    "All"       = "all"),
                       selected = "focus", inline = TRUE),
          div(
            style = "display:flex; gap:8px; font-size:12px;",
            tags$span(
              style = "background:#EBF3FB; color:#5B9BD5;
                     padding:2px 8px; border-radius:10px;
                     font-weight:600;",
              paste0("Focus: ", n_focus)),
            tags$span(
              style = "background:#f4f6f9; color:#6c757d;
                     padding:2px 8px; border-radius:10px;
                     font-weight:600; border:1px solid #dee2e6;",
              paste0("Non-Focus: ", n_nf)),
            tags$span(
              style = "background:#fff3cd; color:#856404;
                     padding:2px 8px; border-radius:10px;
                     font-weight:600;",
              paste0("Total: ", n_all))
          )
        )
      )
    })
    
    # ── Current activity data ─────────────────────────────────
    # all_transformed (is_rag=TRUE): use pre-computed act_diagnoses
    # all_rags (is_rag=FALSE): recompute from RAG with cross-section
    current_act_data <- reactive({
      nm         <- req(input$channel_select)
      res        <- req(results_rv()[[nm]])
      filter_val <- input$period_filter %||% "focus"
      
      if (isTRUE(res$is_rag)) {
        
        # ── all_transformed: national, 1 row per period ───────
        df <- res$act_diagnoses
        if (is.null(df) || nrow(df) == 0)
          return(tibble(VariableSplit = character()))
        if (filter_val == "focus")
          df <- df[df$period == "focus",    ]
        if (filter_val == "nonfocus")
          df <- df[df$period == "nonfocus", ]
        df %>%
          filter(total_activity > 0) %>%
          select(-any_of(c("seg", "period", "model_var"))) %>%
          mutate(across(where(is.numeric), \(x) round(x, 4)))
        
      } else {
        
        # ── all_rags: geographic, recompute from RAG ──────────
        cfg          <- channels()[[nm]]
        gcfg         <- config()
        cross_cols_r <- res$cross_cols %||% "Geography"
        rag_df       <- as.data.frame(res$rag)
        selected_cs  <- cs_selected()
        start_d      <- as.Date(gcfg$start_report_date)
        end_d        <- as.Date(gcfg$end_report_date)
        
        # Apply cross-section filter
        if (selected_cs == "ALL") {
          num_cols_r <- names(rag_df)[sapply(rag_df, is.numeric)]
          rag_df <- aggregate(
            rag_df[, num_cols_r, drop = FALSE],
            by  = list(Period = rag_df$Period),
            FUN = function(x) sum(x, na.rm = TRUE)
          )
          # Restore Date class lost by aggregate
          rag_df$Period <- as.Date(rag_df$Period,
                                   origin = "1970-01-01")
        } else {
          cross_data <- rag_df[, cross_cols_r, drop = FALSE]
          cross_key  <- do.call(paste,
                                c(as.list(cross_data),
                                  list(sep = " / ")))
          rag_df <- rag_df[cross_key == selected_cs, ]
        }
        
        if (nrow(rag_df) == 0)
          return(tibble(VariableSplit = character()))
        
        # Apply period filter
        rag_df <- switch(
          filter_val,
          "focus"    = rag_df[rag_df$Period >= start_d &
                                rag_df$Period <= end_d, ],
          "nonfocus" = rag_df[rag_df$Period < start_d, ],
          rag_df
        )
        
        if (nrow(rag_df) == 0)
          return(tibble(VariableSplit = character()))
        
        # Find activity columns (keyword is in split column names)
        act_kw   <- cfg$activity_keyword %||% "Clicks"
        id_cols  <- intersect(
          if (selected_cs == "ALL") "Period"
          else c(cross_cols_r, "Period"),
          names(rag_df)
        )
        act_cols <- grep(act_kw, names(rag_df),
                         ignore.case = TRUE, value = TRUE)
        
        if (!length(act_cols))
          return(tibble(VariableSplit = character()))
        
        df <- splits_summary(
          rag_df[, c(id_cols, act_cols), drop = FALSE],
          "activity"
        )
        
        if (is.null(df) || nrow(df) == 0)
          return(tibble(VariableSplit = character()))
        
        df %>%
          filter(total_activity > 0) %>%
          select(-any_of(c("seg", "period", "model_var"))) %>%
          mutate(across(where(is.numeric), \(x) round(x, 4)))
      }
    })
    
    # ── Current spend data ────────────────────────────────────
    current_spend_data <- reactive({
      nm  <- req(input$channel_select)
      res <- req(results_rv()[[nm]])
      cost_df <- res$cost_diagnoses
      if (is.null(cost_df) || nrow(cost_df) == 0) return(NULL)
      cost_df %>%
        filter(total_spend > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
    })
    
    # ── KPI cards ─────────────────────────────────────────────
    output$activity_kpis <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_rv()[[nm]])) return(NULL)
      df <- current_act_data()
      if (nrow(df) == 0) return(NULL)
      
      threshold     <- input$threshold_pct %||% 5
      total_splits  <- nrow(df)
      above_thresh  <- sum(df$pct_total_activity >= threshold,
                           na.rm = TRUE)
      below_thresh  <- sum(df$pct_total_activity <  threshold,
                           na.rm = TRUE)
      channel_total <- sum(df$total_activity, na.rm = TRUE)
      
      kpis <- list(
        list(label = "Total splits",
             value = total_splits,
             icon  = "layer-group",
             color = "#5B9BD5", bg = "#EBF3FB"),
        list(label = paste0("Above ", threshold, "%"),
             value = above_thresh,
             icon  = "arrow-up",
             color = "#2ecc71", bg = "#e8f5e9"),
        list(label = paste0("Below ", threshold, "% (review)"),
             value = below_thresh,
             icon  = "arrow-down",
             color = "#e74c3c", bg = "#fdecea"),
        list(label = "Channel total",
             value = fmt_compact(channel_total),
             icon  = "chart-bar",
             color = "#5B9BD5", bg = "#EBF3FB")
      )
      
      do.call(layout_columns, c(
        list(col_widths = c(3, 3, 3, 3),
             style = "margin-bottom:10px;"),
        lapply(kpis, function(k) {
          div(
            style = paste0(
              "background:", k$bg, ";",
              "border-left:3px solid ", k$color, ";",
              "border-radius:6px; padding:10px 14px;",
              "display:flex; align-items:center; gap:10px;"),
            icon(k$icon, style = paste0("color:", k$color,
                                        "; font-size:20px;")),
            div(
              tags$strong(k$value,
                          style = "font-size:20px; color:#2c3e50;
                                 display:block; line-height:1.2;"),
              tags$small(k$label,
                         style = "color:#6c757d; font-size:11px;")
            )
          )
        })
      ))
    })
    
    # ── Threshold UI ──────────────────────────────────────────
    output$threshold_ui <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_rv()[[nm]])) return(NULL)
      div(
        style = paste0("background:#f4f6f9; border-radius:8px;",
                       "padding:10px 16px; margin-bottom:10px;",
                       "display:flex; align-items:center;",
                       "gap:12px; flex-wrap:wrap;"),
        tags$span(
          icon("sliders", style = "color:#5B9BD5;"),
          tags$strong(" Small split threshold:",
                      style = "font-size:13px; color:#333;
                             white-space:nowrap;")),
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(style = "width:70px;",
              numericInput(session$ns("threshold_pct"), NULL,
                           value = input$threshold_pct %||% 5,
                           min = 0, max = 100, step = 0.5)),
          tags$span("%",
                    style = "color:#6c757d; font-size:13px;
                           font-weight:600;")
        ),
        tags$span(
          style = "font-size:11px; color:#adb5bd;",
          icon("circle-info"),
          " Splits below threshold shown in red.", tags$br(),
          " Select manually based on grouping logic.")
      )
    })
    
    # ── Merge toolbar ─────────────────────────────────────────
    output$merge_toolbar <- renderUI({
      nm  <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      res <- results_rv()[[nm]]
      
      if (is.null(res))
        return(tags$p(class = "text-muted small mb-2",
                      icon("info-circle"),
                      " Process a channel first."))
      
      selected <- input$diag_act_rows_selected %||% integer(0)
      n_sel    <- length(selected)
      
      if (n_sel == 0)
        return(tags$p(class = "text-muted small mb-2",
                      icon("hand-pointer"),
                      " Select rows in the table to merge splits."))
      
      df             <- current_act_data()
      selected_names <- df$VariableSplit[selected]
      display_names  <- strip_common_prefix(selected_names)
      
      combined <- df %>%
        filter(VariableSplit %in% selected_names) %>%
        summarise(act   = sum(total_activity,     na.rm = TRUE),
                  pct   = sum(pct_total_activity, na.rm = TRUE),
                  weeks = max(num_weeks_activity, na.rm = TRUE))
      
      div(
        style = paste0(
          "background:#EBF3FB; border:1px solid #5B9BD5;",
          "border-radius:8px; padding:12px 16px;",
          "margin-bottom:12px;"),
        layout_columns(
          col_widths = c(4, 4, 4),
          tagList(
            tags$div(
              icon("object-group", style = "color:#5B9BD5;"),
              tags$strong(paste0(" ", n_sel, " splits selected"),
                          style = "color:#5B9BD5; font-size:13px;")),
            tags$div(class = "mt-1",
                     tags$small(paste0(
                       "Combined: ", fmt_compact(combined$act),
                       " (", round(combined$pct, 2), "%)",
                       " | Max weeks: ", combined$weeks))),
            tags$div(class = "mt-1",
                     tags$small(
                       style = "color:#6c757d; font-size:11px;",
                       paste(display_names, collapse = " + ")))
          ),
          textInput(session$ns("merge_name"),
                    tags$small("New split name"),
                    placeholder = "e.g. Channel_Small_Other",
                    width = "100%"),
          tagList(
            actionButton(session$ns("btn_merge"),
                         tagList(icon("link"), " Merge"),
                         class = "btn-primary btn-sm w-100 mb-1"),
            actionButton(session$ns("btn_clear"),
                         tagList(icon("xmark"), " Clear"),
                         class = "btn-outline-secondary btn-sm w-100")
          )
        )
      )
    })
    
    # ── Execute merge ─────────────────────────────────────────
    observeEvent(input$btn_merge, {
      nm       <- req(input$channel_select)
      req(valid_nm(nm))
      selected <- req(input$diag_act_rows_selected)
      new_name <- trimws(input$merge_name %||% "")
      
      if (!nchar(new_name)) {
        showNotification("Enter a name for the merged split.",
                         type = "warning")
        return()
      }
      
      res         <- results_rv()[[nm]]
      req(res)
      cfg         <- channels()[[nm]]
      view_filter <- input$period_filter %||% "focus"
      df          <- current_act_data()
      
      selected_splits <- df$VariableSplit[selected]
      rag_cols        <- intersect(selected_splits, names(res$rag))
      
      if (!length(rag_cols)) {
        showNotification("Selected splits not found in RAG.",
                         type = "warning")
        return()
      }
      
      # Save for undo
      hist <- get_hist(nm)
      set_hist(nm, c(hist, list(get_res(nm))))
      
      # 1. Update RAG
      new_rag             <- res$rag
      new_rag[[new_name]] <- rowSums(
        new_rag[, rag_cols, drop = FALSE], na.rm = TRUE)
      new_rag <- new_rag[, setdiff(names(new_rag), rag_cols)]
      
      # 2. Recompute act_diagnoses from existing stats
      selected_diag <- res$act_diagnoses %>%
        filter(VariableSplit %in% selected_splits,
               period == view_filter)
      
      merged_act <- tibble(
        VariableSplit         = new_name,
        total_activity        = sum(selected_diag$total_activity,
                                    na.rm = TRUE),
        pct_total_activity    = NA_real_,
        num_weeks_activity    = max(selected_diag$num_weeks_activity,
                                    na.rm = TRUE),
        max_index             = NA_real_,
        min_consecutive_weeks = max(
          selected_diag$min_consecutive_weeks, na.rm = TRUE),
        sd             = NA_real_,
        min            = min(selected_diag$min,         na.rm = TRUE),
        quartile_1     = mean(selected_diag$quartile_1,  na.rm = TRUE),
        median         = mean(selected_diag$median,      na.rm = TRUE),
        quartile_3     = mean(selected_diag$quartile_3,  na.rm = TRUE),
        max_no_outlier = max(selected_diag$max_no_outlier,
                             na.rm = TRUE),
        max            = max(selected_diag$max,          na.rm = TRUE)
      ) %>%
        bind_cols(selected_diag %>% slice(1) %>%
                    select(any_of(c("seg", "period", "model_var"))))
      
      new_act_diag <- res$act_diagnoses %>%
        filter(!VariableSplit %in% selected_splits) %>%
        bind_rows(merged_act) %>%
        group_by(period) %>%
        mutate(grand_p = sum(total_activity, na.rm = TRUE),
               pct_total_activity = round(
                 total_activity / pmax(grand_p, 1) * 100, 4)) %>%
        ungroup() %>% select(-grand_p)
      
      # 3. activity_spend
      merged_as <- res$activity_spend %>%
        filter(VariableSplit %in% selected_splits) %>%
        summarise(VariableSplit         = new_name,
                  total_activity        = sum(total_activity,
                                              na.rm = TRUE),
                  total_spend           = sum(total_spend,
                                              na.rm = TRUE),
                  Channel               = first(Channel),
                  MainModelVariableName = first(
                    MainModelVariableName))
      new_act_spend <- res$activity_spend %>%
        filter(!VariableSplit %in% selected_splits) %>%
        bind_rows(merged_as)
      
      # 4. side_mapping
      merged_sm <- res$side_mapping %>%
        filter(VariableSplit %in% selected_splits) %>%
        slice(1) %>% mutate(VariableSplit = new_name)
      new_side_map <- res$side_mapping %>%
        filter(!VariableSplit %in% selected_splits) %>%
        bind_rows(merged_sm)
      
      # 5. cost_diagnoses
      spend_splits   <- str_replace_all(
        selected_splits,
        regex(cfg$activity_keyword, ignore_case = TRUE),
        cfg$spend_keyword)
      new_spend_name <- str_replace_all(
        new_name,
        regex(cfg$activity_keyword, ignore_case = TRUE),
        cfg$spend_keyword)
      if (new_spend_name == new_name)
        new_spend_name <- paste0(new_name, "_", cfg$spend_keyword)
      
      matching_cost <- intersect(spend_splits,
                                 res$cost_diagnoses$VariableSplit)
      
      new_cost_diag <- tryCatch({
        if (!length(matching_cost)) {
          res$cost_diagnoses
        } else {
          sel_cost <- res$cost_diagnoses %>%
            filter(VariableSplit %in% matching_cost)
          mc_row <- tibble(
            VariableSplit         = new_spend_name,
            total_spend           = sum(sel_cost$total_spend,
                                        na.rm = TRUE),
            pct_total_spend       = NA_real_,
            num_weeks_spend       = max(sel_cost$num_weeks_spend,
                                        na.rm = TRUE),
            min_consecutive_weeks = max(
              sel_cost$min_consecutive_weeks, na.rm = TRUE),
            sd             = NA_real_,
            min            = min(sel_cost$min,         na.rm = TRUE),
            quartile_1     = mean(sel_cost$quartile_1,  na.rm = TRUE),
            median         = mean(sel_cost$median,      na.rm = TRUE),
            quartile_3     = mean(sel_cost$quartile_3,  na.rm = TRUE),
            max_no_outlier = max(sel_cost$max_no_outlier,
                                 na.rm = TRUE),
            max            = max(sel_cost$max,          na.rm = TRUE),
            max_index      = NA_real_,
            period         = sel_cost$period[1]    %||% "focus",
            seg            = sel_cost$seg[1]       %||% NA_integer_,
            model_var      = sel_cost$model_var[1] %||% NA_character_
          )
          cc <- res$cost_diagnoses %>%
            filter(!VariableSplit %in% matching_cost) %>%
            bind_rows(mc_row)
          if ("period" %in% names(cc)) {
            cc %>% group_by(period) %>%
              mutate(gp = sum(total_spend, na.rm = TRUE),
                     pct_total_spend = round(
                       total_spend / pmax(gp, 1) * 100, 4)) %>%
              ungroup() %>% select(-gp)
          } else {
            gt <- sum(cc$total_spend, na.rm = TRUE)
            cc %>% mutate(pct_total_spend = round(
              total_spend / pmax(gt, 1) * 100, 4))
          }
        }
      }, error = \(e) {
        showNotification(
          paste("Spend mirror failed:", conditionMessage(e)),
          type = "warning", duration = 8)
        res$cost_diagnoses
      })
      
      # 6. Store
      set_res(nm, list(
        rag            = new_rag,
        cross_cols     = res$cross_cols,
        is_rag         = res$is_rag,
        ref_cross      = res$ref_cross,
        activity_spend = new_act_spend,
        side_mapping   = new_side_map,
        act_diagnoses  = new_act_diag,
        cost_diagnoses = new_cost_diag
      ))
      
      # 7. Log
      log <- get_log(nm)
      set_log(nm, c(log, list(list(
        merged         = selected_splits,
        new_name       = new_name,
        view           = view_filter,
        spend_merged   = matching_cost,
        new_spend_name = new_spend_name
      ))))
      
      updateTextInput(session, "merge_name", value = "")
      showNotification(
        paste0("Merged ", length(selected_splits),
               " [", toupper(view_filter), "] splits",
               if (length(matching_cost) > 0)
                 paste0(" + ", length(matching_cost), " spend"),
               " -> ", new_name),
        type = "message")
    })
    
    observeEvent(input$btn_clear, {
      DT::dataTableProxy(session$ns("diag_act")) %>%
        DT::selectRows(NULL)
    })
    
    # ── Undo ──────────────────────────────────────────────────
    observeEvent(input$btn_undo, {
      nm <- req(input$channel_select)
      req(valid_nm(nm))
      hist <- get_hist(nm)
      if (!length(hist)) {
        showNotification("No merges to undo.", type = "warning")
        return()
      }
      set_res(nm,  hist[[length(hist)]])
      set_hist(nm, hist[-length(hist)])
      log <- get_log(nm)
      if (length(log) > 0) set_log(nm, log[-length(log)])
      showNotification("Last merge undone.", type = "message")
    })
    
    # ── Merge history card ────────────────────────────────────
    output$merge_history_card <- renderUI({
      nm  <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      log  <- get_log(nm)
      hist <- get_hist(nm)
      if (!length(log)) return(NULL)
      
      card(
        card_header("Merge History"),
        tagList(
          if (length(hist) > 0)
            actionButton(session$ns("btn_undo"),
                         tagList(icon("rotate-left"),
                                 " Undo Last Merge"),
                         class = "btn-outline-secondary btn-sm
                                w-100 mb-2"),
          lapply(rev(seq_along(log)), function(i) {
            m  <- log[[i]]
            vb <- switch(
              m$view %||% "all",
              "focus" = tags$span(
                style = "background:#EBF3FB; color:#5B9BD5;
                       padding:1px 6px; border-radius:8px;
                       font-size:10px;", "FOCUS"),
              "nonfocus" = tags$span(
                style = "background:#f4f6f9; color:#6c757d;
                       padding:1px 6px; border-radius:8px;
                       font-size:10px; border:1px solid #dee2e6;",
                "NON-FOCUS"),
              tags$span(
                style = "background:#fff3cd; color:#856404;
                       padding:1px 6px; border-radius:8px;
                       font-size:10px;", "ALL")
            )
            div(
              class = "mb-2 p-2",
              style = paste0(
                "background:#f4f6f9; border-radius:6px;",
                "border-left:3px solid #5B9BD5; font-size:12px;",
                if (i == length(log)) "border-left-width:4px;"
                else ""),
              div(
                class = "d-flex align-items-center gap-2 mb-1",
                icon("arrow-right", style = "color:#5B9BD5;"),
                tags$strong(m$new_name), vb,
                if (i == length(log))
                  tags$span("latest",
                            style = "font-size:9px; color:#8a9bb0;
                                   margin-left:auto;")
              ),
              tags$div(class = "text-muted",
                       paste(strip_common_prefix(m$merged),
                             collapse = " + ")),
              if (length(m$spend_merged) > 0)
                tags$div(class = "text-muted",
                         style = "font-size:11px;",
                         paste0("Spend -> ", m$new_spend_name))
            )
          }),
          hr(),
          actionButton(session$ns("btn_reset_merges"),
                       tagList(icon("trash"), " Reset All Merges"),
                       class = "btn-outline-danger btn-sm w-100")
        )
      )
    })
    
    observeEvent(input$btn_reset_merges, {
      nm <- req(input$channel_select)
      req(valid_nm(nm))
      orig <- get_orig(nm)
      req(orig)
      set_res(nm,  orig)
      set_log(nm,  list())
      set_hist(nm, list())
      showNotification(paste("All merges reset for", nm),
                       type = "message")
    })
    
    # ── Activity table ────────────────────────────────────────
    output$diag_act <- DT::renderDT({
      nm <- req(input$channel_select)
      req(results_rv()[[nm]])
      df        <- current_act_data()
      req(nrow(df) > 0)
      threshold <- input$threshold_pct %||% 5
      max_pct   <- max(df$pct_total_activity, na.rm = TRUE)
      if (!is.finite(max_pct) || max_pct == 0) max_pct <- 1
      
      df_display <- df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      
      num_fmt <- intersect(
        c("sd", "min", "quartile_1", "median",
          "quartile_3", "max_no_outlier", "max"),
        names(df_display))
      
      col_defs <- list(
        list(className = "dt-left", targets = 0),
        list(
          targets = which(names(df_display) ==
                            "total_activity") - 1,
          render  = JS(
            "function(d,t){if(t!=='display')return d;",
            "var n=parseFloat(d);",
            "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
            "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
            "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
            "return n.toLocaleString();}")
        )
      )
      
      dt <- df_display %>%
        datatable(
          selection  = list(mode = "multiple", target = "row"),
          extensions = "Buttons",
          options    = list(
            scrollX      = TRUE,
            pageLength   = 15,
            initComplete = dt_blue_callback,
            dom          = "Bfrtip",
            buttons      = make_export_buttons("activity", nm),
            columnDefs   = col_defs
          ),
          rownames = FALSE
        )
      
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "",
                                    digits = 0, mark = ",")
      dt %>%
        formatStyle(
          "pct_total_activity",
          background         = styleColorBar(c(0, max_pct), "#EBF3FB"),
          backgroundSize     = "100% 90%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center") %>%
        formatStyle(
          "pct_total_activity",
          color = styleInterval(threshold, c("#dc3545", "#333")))
      
    }, server = FALSE)
    
    # ── Spend table ───────────────────────────────────────────
    output$diag_cost <- DT::renderDT({
      nm <- req(input$channel_select)
      req(results_rv()[[nm]])
      cost_df <- current_spend_data()
      
      if (is.null(cost_df) || nrow(cost_df) == 0) {
        cfg <- channels()[[nm]]
        return(datatable(
          data.frame(Info = paste0(
            "No spend data. Check keyword: '",
            cfg$spend_keyword %||% "Spend", "'")),
          options  = list(initComplete = dt_blue_callback),
          rownames = FALSE))
      }
      
      df_display <- cost_df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      
      num_fmt <- intersect(
        c("sd", "min", "quartile_1", "median",
          "quartile_3", "max_no_outlier", "max"),
        names(df_display))
      
      dt <- df_display %>%
        datatable(
          extensions = "Buttons",
          options    = list(
            scrollX      = TRUE,
            pageLength   = 15,
            initComplete = dt_blue_callback,
            dom          = "Bfrtip",
            buttons      = make_export_buttons("spend", nm),
            columnDefs   = list(
              list(className = "dt-left", targets = 0),
              list(
                targets = which(names(df_display) ==
                                  "total_spend") - 1,
                render  = JS(
                  "function(d,t){if(t!=='display')return d;",
                  "var n=parseFloat(d);",
                  "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
                  "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
                  "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
                  "return n.toLocaleString();}")
              )
            )
          ),
          rownames = FALSE
        )
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "",
                                    digits = 0, mark = ",")
      dt
    })
    
    # ── Total Check ───────────────────────────────────────────
    output$diag_check <- DT::renderDT({
      nm  <- req(input$channel_select)
      res <- req(results_rv()[[nm]])
      d   <- data()
      req(d$analytical)
      cfg        <- channels()[[nm]]
      cross_cols <- res$cross_cols %||%
        config()$cross_cols %||% "Geography"
      cross_id   <- c(cross_cols, "Period")
      
      valid_vars <- intersect(cfg$model_variables,
                              names(d$analytical))
      validate(need(length(valid_vars) > 0,
                    paste("Model variables not found:",
                          paste(cfg$model_variables,
                                collapse = ", "))))
      
      model_side <- build_model_total(
        d$analytical, cross_id,
        cfg$model_variables, cfg$break_dates)
      
      rag_df      <- as.data.frame(res$rag)
      splits_side <- rag_df %>%
        select(all_of(cross_id), where(is.numeric)) %>%
        mutate(SplitsTotal = rowSums(
          select(., where(is.numeric)), na.rm = TRUE)) %>%
        select(all_of(cross_id), SplitsTotal)
      
      check_df <- model_side %>%
        left_join(splits_side, by = cross_id) %>%
        mutate(
          SplitsTotal = replace_na(SplitsTotal, 0),
          Diff        = ModelTotal - SplitsTotal,
          Status      = if_else(abs(Diff) < 0.01,
                                "OK", "Mismatch")
        ) %>%
        filter(ModelTotal > 0 | SplitsTotal > 0) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4))) %>%
        arrange(Status, across(all_of(cross_cols)), Period)
      
      check_df %>%
        datatable(
          extensions = "Buttons",
          options    = list(
            scrollX      = TRUE,
            pageLength   = 25,
            initComplete = dt_blue_callback,
            dom          = "Bfrtip",
            buttons      = make_export_buttons("total_check", nm)
          ),
          rownames = FALSE
        ) %>%
        formatStyle("Status",
                    backgroundColor = styleEqual(
                      c("OK", "Mismatch"),
                      c("#d4edda", "#f8d7da")))
    })
    
    # ── Return ────────────────────────────────────────────────
    results_rv
  })
}