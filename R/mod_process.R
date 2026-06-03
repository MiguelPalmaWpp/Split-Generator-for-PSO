# ═══════════════════════════════════════════════════════════════════════
# R/mod_process.R
# ═══════════════════════════════════════════════════════════════════════

mod_process_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    
    tagList(
      card(
        card_header("Run Processing"),
        selectInput(ns("channel_select"), "Select Channel", choices = NULL),
        actionButton(ns("btn_one"), "Process Selected",
                     class = "btn-success btn-sm w-100 mb-2"),
        actionButton(ns("btn_all"), "Process All",
                     class = "btn-warning btn-sm w-100"),
        hr(class = "hr-sm"),
        uiOutput(ns("status")),
        hr(class = "hr-sm"),
        downloadButton(ns("dl_config_process"),
                       label = tagList(" Download config CSV"),
                       class = "btn-outline-secondary btn-sm w-100")
      ),
      uiOutput(ns("merge_history_card"))
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "flex-fill", uiOutput(ns("channel_pos"))),
          actionButton(ns("btn_next"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      navset_card_underline(
        nav_panel("Activity",
                  uiOutput(ns("period_filter_ui")),
                  uiOutput(ns("activity_kpis")),
                  uiOutput(ns("threshold_ui")),
                  uiOutput(ns("merge_plan_toolbar")),
                  uiOutput(ns("merge_toolbar")),
                  DTOutput(ns("diag_act"))),
        nav_panel("Spend",       DTOutput(ns("diag_cost"))),
        nav_panel("Total Check", DTOutput(ns("diag_check")))
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_process_server <- function(id, data, config, channels,
                               update_merges = NULL) {
  moduleServer(id, function(input, output, session) {
    
    results_store   <- reactiveValues()
    original_store  <- reactiveValues()
    merge_log_store <- reactiveValues()
    history_store   <- reactiveValues()
    clean_store     <- reactiveValues()
    results_trigger  <- reactiveVal(0L)
    is_batch_processing <- reactiveVal(FALSE)  
    
    get_res  <- function(nm) results_store[[nm]]
    set_res  <- function(nm, val) {
      results_store[[nm]] <- val
      # ── OPT: only trigger UI update when not in batch mode
      if (!isTRUE(is_batch_processing()))
        results_trigger(isolate(results_trigger()) + 1L)
    }
    get_orig <- function(nm) original_store[[nm]]
    set_orig <- function(nm, val) { original_store[[nm]] <- val }
    get_log  <- function(nm) merge_log_store[[nm]] %||% list()
    set_log  <- function(nm, val) { merge_log_store[[nm]] <- val }
    get_hist <- function(nm) history_store[[nm]] %||% list()
    set_hist <- function(nm, val) { history_store[[nm]] <- val }
    
    valid_nm <- function(nm)
      !is.null(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)
    
    make_export_buttons <- function(prefix, nm) {
      fname <- paste0(prefix, "_", nm, "_",
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
      list(
        list(extend = "csv",   text = "Download CSV",   filename = fname,
             className = "dt-button",
             exportOptions = list(modifier = list(page = "all"))),
        list(extend = "excel", text = "Download Excel", filename = fname,
             className = "dt-button",
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
                                digits = 0, big.mark = ","))
    }
    
    strip_common_prefix <- function(names_vec) {
      if (length(names_vec) <= 1) return(names_vec)
      parts      <- strsplit(names_vec, "_")
      min_len    <- min(sapply(parts, length))
      if (min_len == 0) return(names_vec)
      common_len <- 0L
      for (i in seq_len(min_len)) {
        if (length(unique(sapply(parts, `[[`, i))) == 1L)
          common_len <- i else break
      }
      if (common_len == 0L) return(names_vec)
      sapply(parts, function(p) {
        rest <- p[(common_len + 1):length(p)]
        if (!length(rest)) paste(p, collapse = "_")
        else paste(rest, collapse = "_")
      })
    }
    
    get_merge_name_parts <- function(selected_names) {
      selected_names <- selected_names[
        !is.na(selected_names) & nzchar(selected_names)]
      if (length(selected_names) <= 1)
        return(list(prefix = "", suffix = ""))
      common_prefix_chars <- function(strings) {
        chars   <- lapply(strings, \(s) strsplit(s, "")[[1]])
        min_len <- min(sapply(chars, length)); n <- 0L
        for (i in seq_len(min_len)) {
          if (length(unique(sapply(chars, `[[`, i))) == 1L) n <- i else break
        }
        if (n == 0L) return("")
        raw <- substr(strings[1], 1, n)
        pos <- max(gregexpr("_", raw)[[1]])
        if (pos < 1L) "" else substr(raw, 1, pos)
      }
      common_suffix_chars <- function(strings) {
        revs    <- sapply(strings,
                          \(s) paste(rev(strsplit(s, "")[[1]]), collapse = ""))
        chars   <- lapply(revs, \(s) strsplit(s, "")[[1]])
        min_len <- min(sapply(chars, length)); n <- 0L
        for (i in seq_len(min_len)) {
          if (length(unique(sapply(chars, `[[`, i))) == 1L) n <- i else break
        }
        if (n == 0L) return("")
        raw <- paste(rev(strsplit(substr(revs[1], 1, n), "")[[1]]),
                     collapse = "")
        pos <- min(gregexpr("_", raw)[[1]])
        if (pos < 1L || pos > nchar(raw)) "" else substr(raw, pos, nchar(raw))
      }
      prefix  <- common_prefix_chars(selected_names)
      suffix  <- common_suffix_chars(selected_names)
      min_len <- min(nchar(selected_names), na.rm = TRUE)
      if (is.finite(min_len) && nchar(prefix) + nchar(suffix) >= min_len)
        suffix <- ""
      list(prefix = prefix, suffix = suffix)
    }
    
    # ── Channel selector ───────────────────────────────────────────────────
    observe({
      updateSelectInput(session, "channel_select", choices = names(channels()))
    })
    
    observeEvent(input$btn_prev, {
      nms <- names(channels()); if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur > 1)
        updateSelectInput(session, "channel_select", selected = nms[cur - 1])
    })
    observeEvent(input$btn_next, {
      nms <- names(channels()); if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur < length(nms))
        updateSelectInput(session, "channel_select", selected = nms[cur + 1])
    })
    
    output$channel_pos <- renderUI({
      nms <- names(channels()); nm <- input$channel_select
      if (!length(nms) || !valid_nm(nm)) return(NULL)
      idx <- which(nms == nm); if (!length(idx)) idx <- 0L
      tagList(
        tags$strong(nm, class = "ch-editor-name"),
        tags$span(paste0(" (", idx, " / ", length(nms), ")"),
                  class = "ch-editor-counter"))
    })
    
    # ── Status panel — debounced ───────────────────────────────────────────
    status_trigger <- reactive({
      results_trigger()
      names(channels())
      input$channel_select
    }) %>% debounce(300)
    
    output$status <- renderUI({
      status_trigger()
      ch_names <- names(channels())
      if (!length(ch_names))
        return(tags$p(class = "text-muted small mt-2",
                      "No channels configured."))
      tagList(lapply(ch_names, function(nm) {
        processed <- !is.null(results_store[[nm]])
        n_merges  <- length(get_log(nm))
        is_sel    <- identical(input$channel_select, nm)
        saved_m   <- channels()[[nm]]$saved_merges %||% list()
        n_saved   <- sum(vapply(saved_m, \(m) isTRUE(m$active), logical(1)))
        div(
          class = paste("status-item", if (is_sel) "selected" else ""),
          onclick = paste0("Shiny.setInputValue('", session$ns("ch_click"),
                           "','", nm, "',{priority:'event'});"),
          if (processed)
            icon("circle-check", class = "icon-success-sm")
          else
            icon("circle", class = "icon-empty-status"),
          tags$span(nm, class = "status-item-name"),
          div(class = "status-badges",
              if (processed && n_merges > 0)
                tags$span(paste0(n_merges, "m"), class = "badge-merge-count"),
              if (n_saved > 0)
                tags$span(paste0(n_saved, " saved"), class = "badge-saved",
                          title = paste0(n_saved,
                                         " merge(s) — auto-applied on process")))
        )
      }))
    })
    
    observeEvent(input$ch_click, {
      req(nzchar(input$ch_click %||% ""))
      updateSelectInput(session, "channel_select", selected = input$ch_click)
    }, ignoreInit = TRUE)
    
    output$dl_config_process <- downloadHandler(
      filename = function()
        paste0("channel_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        cfg_data <- isolate(channels())
        if (!length(cfg_data)) {
          readr::write_csv(data.frame(), file); return()
        }
        readr::write_csv(export_channels_csv(cfg_data), file, na = "")
      }
    )
    
    # ── run_one ────────────────────────────────────────────────────────────
    run_one <- function(nm) {
      d    <- data()
      cfg  <- channels()[[nm]]; req(cfg)
      gcfg <- config()
      
      if (is.null(d$all_rags)) {
        showNotification(paste0(nm, ": All RAGs data not uploaded."),
                         type = "error", duration = 8); return()
      }
      if (is.null(d$analytical)) {
        showNotification("AnalyticalDataset not uploaded.",
                         type = "error", duration = 10); return()
      }
      if (is.null(d$dates_df)) {
        showNotification("dates_df is NULL.",
                         type = "error", duration = 10); return()
      }
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        showNotification("Reporting period not configured. Check Setup tab.",
                         type = "error", duration = 10); return()
      }
      if (is.null(gcfg$cross_cols)) {
        showNotification("Cross-sections not detected. Upload Analytical first.",
                         type = "error", duration = 8); return()
      }
      
      model_var <- cfg$model_variable %||% ""
      if (!nzchar(model_var)) {
        showNotification(paste0(nm, ": model_variable not configured."),
                         type = "error", duration = 8); return()
      }
      if (!model_var %in% names(d$analytical)) {
        showNotification(paste0(nm, ": model variable '", model_var,
                                "' not found in AnalyticalDataset."),
                         type = "error", duration = 10); return()
      }
      
      res_stored <- NULL; err_stored <- NULL
      
      withProgress(message = paste0("Processing: ", nm), value = 0, {
        tryCatch({
          res_stored <- process_channel(
            all_rags          = d$all_rags,
            analytical        = d$analytical,
            dates_df          = d$dates_df,
            cfg               = cfg,
            cross_cols        = gcfg$cross_cols %||% "Geography",
            start_report_date = gcfg$start_report_date,
            end_report_date   = gcfg$end_report_date,
            update_label      = gcfg$update_label,
            dimension_breaks  = cfg$dimension_breaks  %||% list(),
            segment_overrides = cfg$segment_overrides %||% list(),
            min_period        = cfg$min_period,
            max_period        = cfg$max_period,
            schema_metadata   = d$schema_metadata,
            progress_cb = function(detail, value = NULL) {
              if (!is.null(value)) setProgress(value, detail = detail)
              else incProgress(0, detail = detail)
            }
          )
        }, error = function(e) { err_stored <<- conditionMessage(e) })
      })
      
      if (!is.null(err_stored)) {
        showNotification(paste(nm, "error:", err_stored),
                         type = "error", duration = 12)
        gc(verbose = FALSE, full = FALSE); return()   
      }
      
      if (!is.null(res_stored)) {
        active_saved <- Filter(\(m) isTRUE(m$active),
                               cfg$saved_merges %||% list())
        n_applied <- 0L; n_skipped <- 0L
        
        clean_store[[nm]] <- res_stored
        
        if (length(active_saved) > 0) {
          withProgress(message = paste0("Applying ",
                                        length(active_saved),
                                        " merge(s) from config..."),
                       value = 0.5, {
                         for (m in active_saved) {
                           rag_before <- names(res_stored$rag)
                           res_stored <- tryCatch(
                             apply_single_merge(res_stored, m, cfg),
                             error = function(e) res_stored)
                           if (m$new_name %in% names(res_stored$rag) &&
                               !m$new_name %in% rag_before)
                             n_applied <- n_applied + 1L
                           else
                             n_skipped <- n_skipped + 1L
                         }
                       })
        }
        
        set_res(nm, res_stored); set_orig(nm, res_stored)
        set_log(nm, list());     set_hist(nm, list())
        
        msg <- paste0(nm, " processed",
                      if (n_applied > 0)
                        paste0(" + ", n_applied, " merge(s) from config")
                      else "")
        if (n_skipped > 0)
          msg <- paste0(msg, " (", n_skipped,
                        " skipped \u2014 column names may have changed)")
        showNotification(msg,
                         type = if (n_skipped > 0 && n_applied == 0)
                           "warning" else "message",
                         duration = 4)
        rm(res_stored); gc(verbose = FALSE, full = FALSE)   # ── OPT: was full=TRUE
      }
    }
    
    observeEvent(input$btn_one, {
      nm <- req(input$channel_select); req(valid_nm(nm)); run_one(nm)
    })
    
    # ── Process All — optimized ────────────────────────────────────────────
    observeEvent(input$btn_all, {
      nms <- names(channels())
      if (!length(nms)) {
        showNotification("No channels configured.", type = "warning"); return()
      }
      
      d    <- data()
      gcfg <- config()
      ch   <- channels()   # ── OPT: snapshot once — not per iteration
      
      if (is.null(d$all_rags)) {
        showNotification("All RAGs not uploaded.", type = "error"); return()
      }
      if (is.null(d$analytical)) {
        showNotification("Upload AnalyticalDataset first.",
                         type = "error"); return()
      }
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        showNotification("Configure reporting period first.",
                         type = "error"); return()
      }
      if (is.null(gcfg$cross_cols)) {
        showNotification("Cross-sections not detected. Upload Analytical first.",
                         type = "error"); return()
      }
      
      to_process   <- nms[vapply(nms, \(nm) is.null(results_store[[nm]]),
                                 logical(1))]
      already_done <- length(nms) - length(to_process)
      
      if (!length(to_process)) {
        showNotification(
          paste0("All ", length(nms), " channels already processed."),
          type = "message", duration = 5)
        return()
      }
      
      n_total  <- length(to_process)
      n_ok     <- 0L; n_skipped <- 0L; n_err <- 0L
      err_msgs <- character(0); info_msgs <- character(0)
      t_start  <- proc.time()
      
      # ── OPT: pre-compute constants used in every iteration ────────────
      cross_cols_val <- gcfg$cross_cols %||% "Geography"
      has_vn_col     <- "VariableName" %in% names(d$all_rags)
      
      # ── OPT: suppress intermediate UI triggers ────────────────────────
      # Single batch update at the end instead of one per channel
      is_batch_processing(TRUE)
      on.exit({
        is_batch_processing(FALSE)
        results_trigger(isolate(results_trigger()) + 1L)
      }, add = TRUE)
      
      withProgress(
        message = paste0("Processing ", n_total, " channel(s)..."),
        value   = 0, {
          
          for (i in seq_along(to_process)) {
            nm  <- to_process[i]
            cfg <- ch[[nm]]   # use snapshot
            
            setProgress((i - 1) / n_total,
                        message = paste0("(", i, "/", n_total, ")  ", nm))
            
            if (is.null(cfg)) { n_skipped <- n_skipped + 1L; next }
            
            model_var   <- cfg$model_variable %||% ""
            skip_reason <- if (!nzchar(model_var))
              "model_variable not configured"
            else if (!model_var %in% names(d$analytical))
              paste0("'", model_var, "' not in Analytical")
            else NULL
            
            if (!is.null(skip_reason)) {
              n_skipped <- n_skipped + 1L
              err_msgs  <- c(err_msgs, paste0(nm, ": ", skip_reason))
              next
            }
            
            # ── OPT: inline pre-filter per channel ────────────────────
            # Avoids holding N filtered subsets simultaneously in RAM
            vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
            rags_nm <- if (!length(vi) || !has_vn_col) {
              d$all_rags
            } else {
              pat <- paste0("^(", paste(vi, collapse = "|"), ")")
              d$all_rags[grepl(pat, d$all_rags$VariableName,
                               ignore.case = TRUE, perl = TRUE), ,
                         drop = FALSE]
            }
            
            res_stored <- NULL; err_msg <- NULL
            tryCatch({
              res_stored <- process_channel(
                all_rags          = rags_nm,
                analytical        = d$analytical,
                dates_df          = d$dates_df,
                cfg               = cfg,
                cross_cols        = cross_cols_val,
                start_report_date = gcfg$start_report_date,
                end_report_date   = gcfg$end_report_date,
                update_label      = gcfg$update_label,
                dimension_breaks  = cfg$dimension_breaks  %||% list(),
                segment_overrides = cfg$segment_overrides %||% list(),
                min_period        = cfg$min_period,
                schema_metadata   = d$schema_metadata, 
                max_period        = cfg$max_period,
                progress_cb       = function(detail, value = NULL) NULL
              )
            }, error = function(e) { err_msg <<- conditionMessage(e) })
            
            # ── OPT: release per-channel filter + fast GC ─────────────
            rm(rags_nm)
            gc(verbose = FALSE, full = FALSE)   # full=FALSE in loop
            
            if (!is.null(err_msg)) {
              n_err    <- n_err + 1L
              err_msgs <- c(err_msgs, paste0(nm, ": ", err_msg))
              next
            }
            
            if (!is.null(res_stored)) {
              active_saved <- Filter(\(m) isTRUE(m$active),
                                     cfg$saved_merges %||% list())
              clean_store[[nm]] <- res_stored
              n_applied <- 0L
              
              if (length(active_saved) > 0) {
                for (m in active_saved) {
                  rag_before <- names(res_stored$rag)
                  res_stored <- tryCatch(
                    apply_single_merge(res_stored, m, cfg),
                    error = function(e) res_stored)
                  if (m$new_name %in% names(res_stored$rag) &&
                      !m$new_name %in% rag_before)
                    n_applied <- n_applied + 1L
                }
                if (n_applied > 0)
                  info_msgs <- c(info_msgs,
                                 paste0(nm, ": ", n_applied, " merge(s)"))
              }
              
              # Direct assign — set_res skips trigger during batch
              results_store[[nm]]   <- res_stored
              original_store[[nm]]  <- res_stored
              merge_log_store[[nm]] <- list()
              history_store[[nm]]   <- list()
              n_ok <- n_ok + 1L
              rm(res_stored)
            }
          }
          
          # ── OPT: full GC once at the end — not once per channel ──────
          gc(verbose = FALSE, full = TRUE)
          setProgress(1.0, message = "Done!")
        })
      
      elapsed  <- round((proc.time() - t_start)[["elapsed"]], 1)
      all_msgs <- c(err_msgs, info_msgs)
      parts    <- c(
        if (n_ok         > 0) paste0(n_ok,         " processed"),
        if (already_done > 0) paste0(already_done, " already done"),
        if (n_skipped    > 0) paste0(n_skipped,    " skipped"),
        if (n_err        > 0) paste0(n_err,        " failed"),
        paste0(elapsed, "s"))
      
      showNotification(
        tagList(
          tags$strong(paste(parts, collapse = " \u2014 ")),
          if (length(all_msgs) > 0) tagList(
            tags$br(),
            tags$div(
              class = if (n_err > 0) "notify-detail-error"
              else "notify-detail-warn",
              tagList(lapply(all_msgs, tags$div))))
        ),
        type     = if (n_err     > 0) "error"
        else if (n_skipped > 0) "warning"
        else "message",
        duration = if (length(all_msgs) > 0) 15 else 5)
    })
    
    # ── Period filter UI ───────────────────────────────────────────────────
    output$period_filter_ui <- renderUI({
      results_trigger()
      nm  <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      res <- results_store[[nm]]
      has_data <- (!is.null(res$act_diagnoses) && nrow(res$act_diagnoses) > 0) ||
        (!is.null(res$rag) && nrow(res$rag) > 0)
      if (is.null(res) || !has_data) return(NULL)
      diag    <- res$act_diagnoses
      n_focus <- sum(diag$period == "focus",    na.rm = TRUE)
      n_nf    <- sum(diag$period == "nonfocus", na.rm = TRUE)
      div(
        class = "filter-bar",
        tags$span(icon("filter", class = "icon-blue-sm"),
                  tags$strong(" View:", class = "filter-bar-label")),
        radioButtons(session$ns("period_filter"), NULL,
                     choices  = c("Focus" = "focus", "Non-Focus" = "nonfocus"),
                     selected = "focus", inline = TRUE),
        div(class = "filter-bar-counts",
            tags$span(paste0("Focus: ",     n_focus), class = "badge-focus"),
            tags$span(paste0("Non-Focus: ", n_nf),    class = "badge-nonfocus"))
      )
    })
    
    # ── current_act_data — cached + data.table aggregation ────────────────
    current_act_data <- reactive({
      results_trigger()
      nm         <- req(input$channel_select)
      res        <- req(results_store[[nm]])
      filter_val <- input$period_filter %||% "focus"
      
      add_pct <- function(df) {
        if (is.null(df) || nrow(df) == 0 || !"total_activity" %in% names(df))
          return(df %||% tibble())
        grand <- sum(df$total_activity, na.rm = TRUE)
        df %>% mutate(pct_total_activity = round(
          total_activity / pmax(grand, 1) * 100, 4))
      }
      
      cfg        <- channels()[[nm]]
      gcfg       <- config()
      spend_kw_f <- cfg$spend_keyword %||% "Spend"
      
      req(!is.null(gcfg$start_report_date), !is.null(gcfg$end_report_date),
          length(gcfg$start_report_date) == 1,
          length(gcfg$end_report_date)   == 1)
      
      rag_df <- as.data.frame(res$rag)
      
      num_cols_r <- names(rag_df)[sapply(rag_df, is.numeric)]
      num_cols_r <- num_cols_r[
        !grepl(spend_kw_f, num_cols_r, ignore.case = TRUE)]
      if (!length(num_cols_r)) return(tibble(VariableSplit = character()))
      
      dt     <- data.table::as.data.table(rag_df)
      agg_dt <- dt[, lapply(.SD, sum, na.rm = TRUE),
                   by = "Period", .SDcols = num_cols_r]
      rag_agg        <- as.data.frame(agg_dt)
      rag_agg$Period <- as.Date(rag_agg$Period, origin = "1970-01-01")
      
      start_d <- as.Date(gcfg$start_report_date)
      end_d   <- as.Date(gcfg$end_report_date)
      req(!is.na(start_d), !is.na(end_d))
      
      rag_agg <- switch(filter_val,
                        "focus"    = rag_agg[rag_agg$Period >= start_d &
                                               rag_agg$Period <= end_d, ],
                        "nonfocus" = rag_agg[rag_agg$Period <  start_d, ],
                        rag_agg)
      if (nrow(rag_agg) == 0) return(tibble(VariableSplit = character()))
      
      act_kw  <- cfg$activity_keyword %||% "Clicks"
      id_cols <- intersect("Period", names(rag_agg))
      
      act_cols_keyword <- grep(act_kw, names(rag_agg),
                               ignore.case = TRUE, value = TRUE)
      act_cols_merged  <- if (
        is.null(res$act_diagnoses) || nrow(res$act_diagnoses) == 0 ||
        !"VariableSplit" %in% names(res$act_diagnoses)
      ) character(0) else {
        intersect(unique(res$act_diagnoses$VariableSplit),
                  setdiff(names(rag_agg), id_cols))
      }
      act_cols <- union(act_cols_keyword, act_cols_merged)
      if (!length(act_cols)) return(tibble(VariableSplit = character()))
      
      df <- splits_summary(rag_agg[, c(id_cols, act_cols), drop = FALSE],
                           "activity")
      if (is.null(df) || nrow(df) == 0)
        return(tibble(VariableSplit = character()))
      
      df %>%
        filter(total_activity > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        add_pct() %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
      
    }) %>% bindCache(
      input$channel_select,
      input$period_filter,
      results_trigger()
    )
    
    # ── current_spend_data — cached ────────────────────────────────────────
    current_spend_data <- reactive({
      results_trigger()
      nm  <- req(input$channel_select)
      res <- req(results_store[[nm]])
      cost_df <- res$cost_diagnoses
      if (is.null(cost_df) || nrow(cost_df) == 0) return(NULL)
      cost_df %>%
        filter(total_spend > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
    }) %>% bindCache(
      input$channel_select,
      results_trigger()
    )
    
    # ── Activity KPIs ──────────────────────────────────────────────────────
    output$activity_kpis <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      df <- current_act_data(); if (nrow(df) == 0) return(NULL)
      threshold     <- input$threshold_pct %||% 1
      total_splits  <- nrow(df)
      above_thresh  <- sum(df$pct_total_activity >= threshold, na.rm = TRUE)
      below_thresh  <- sum(df$pct_total_activity <  threshold, na.rm = TRUE)
      channel_total <- sum(df$total_activity, na.rm = TRUE)
      kpis <- list(
        list(label = "Total splits",    value = total_splits,
             icon = "layer-group", box_class = "kpi-box kpi-box-blue",
             icon_class = "kpi-icon-blue"),
        list(label = paste0("Above ", threshold, "%"), value = above_thresh,
             icon = "arrow-up",   box_class = "kpi-box kpi-box-green",
             icon_class = "kpi-icon-green"),
        list(label = paste0("Below ", threshold, "% (review)"),
             value = below_thresh,
             icon = "arrow-down", box_class = "kpi-box kpi-box-red",
             icon_class = "kpi-icon-red"),
        list(label = "Channel total",   value = fmt_compact(channel_total),
             icon = "chart-bar",  box_class = "kpi-box kpi-box-blue",
             icon_class = "kpi-icon-blue")
      )
      do.call(layout_columns, c(
        list(col_widths = c(3, 3, 3, 3), class = "mb-2"),
        lapply(kpis, function(k)
          div(class = k$box_class,
              icon(k$icon, class = k$icon_class),
              div(tags$strong(k$value, class = "kpi-value"),
                  tags$small(k$label,  class = "kpi-label"))))))
    })
    
    # ── Threshold UI ───────────────────────────────────────────────────────
    output$threshold_ui <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(
        class = "filter-bar",
        style = "gap:12px;",
        tags$span(icon("sliders", class = "icon-blue-sm"),
                  tags$strong(" Small split threshold:",
                              class = "filter-bar-label")),
        div(class = "d-flex align-items-center gap-2",
            div(class = "input-narrow",
                numericInput(session$ns("threshold_pct"), NULL,
                             value = input$threshold_pct %||% 1,
                             min = 0, max = 100, step = 0.5)),
            tags$span("%", class = "pct-symbol")),
        tags$span(class = "hint-text",
                  icon("circle-info", class = "icon-xs"),
                  " Splits below threshold shown in red.",
                  tags$br(),
                  " Select manually based on grouping logic.")
      )
    })
    
    # ── Merge plan toolbar ─────────────────────────────────────────────────
    output$merge_plan_toolbar <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(
        class = "toolbar-row",
        downloadButton(session$ns("dl_merge_plan"),
                       label = tagList(icon("download"), " Download Merge Plan"),
                       class = "btn-outline-secondary btn-sm"),
        div(class = "position-relative",
            tags$label(
              class = "btn-upload-plan",
              icon("upload"), " Apply Merge Plan",
              tags$input(type = "file", accept = ".csv", class = "d-none",
                         onchange = paste0(
                           "var r=new FileReader();",
                           "r.onload=function(e){Shiny.setInputValue('",
                           session$ns("merge_plan_content"), "',",
                           "e.target.result,{priority:'event'});};",
                           "r.readAsText(this.files[0]);"))
            )),
        tags$span(class = "hint-text",
                  icon("circle-info", class = "icon-xs"),
                  " Write the same ", tags$strong("MergeName"),
                  " on splits to merge, then upload.")
      )
    })
    
    output$dl_merge_plan <- downloadHandler(
      filename = function() {
        nm <- input$channel_select %||% "channel"
        paste0("merge_plan_", nm, "_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- tryCatch(isolate(current_act_data()), error = function(e) NULL)
        if (is.null(df) || nrow(df) == 0) {
          write.csv(data.frame(VariableSplit = character(),
                               Split        = character(),
                               MergeName    = character()),
                    file, row.names = FALSE); return()
        }
        stat_cols <- setdiff(names(df), "VariableSplit")
        df %>%
          mutate(Split     = strip_common_prefix(VariableSplit),
                 MergeName = NA_character_) %>%
          select(VariableSplit, Split, MergeName, all_of(stat_cols)) %>%
          write.csv(file, row.names = FALSE, na = "")
      }
    )
    
    # ── Plan merge ─────────────────────────────────────────────────────────
    observeEvent(input$merge_plan_content, {
      req(input$merge_plan_content)
      nm <- req(input$channel_select); req(valid_nm(nm))
      plan <- tryCatch({
        con <- textConnection(input$merge_plan_content)
        on.exit(close(con))
        read.csv(con, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }, error = \(e) {
        showNotification(paste("Error reading file:", conditionMessage(e)),
                         type = "error", duration = 8); NULL
      })
      if (is.null(plan)) return()
      
      missing_cols <- setdiff(c("VariableSplit", "MergeName"), names(plan))
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing columns:",
                               paste(missing_cols, collapse = ", ")),
                         type = "error", duration = 8); return()
      }
      
      plan_active <- plan[!is.na(plan$MergeName) &
                            nzchar(trimws(as.character(plan$MergeName))), ]
      if (nrow(plan_active) == 0) {
        showNotification("No merge names found.", type = "warning"); return()
      }
      
      res         <- results_store[[nm]]; req(res)
      cfg         <- channels()[[nm]]
      view_filter <- input$period_filter %||% "focus"
      groups      <- split(plan_active,
                           trimws(as.character(plan_active$MergeName)))
      set_hist(nm, c(get_hist(nm), list(results_store[[nm]])))
      new_log <- list(); new_saved <- list(); n_skipped <- 0L
      
      withProgress(message = "Applying merge plan...", value = 0, {
        for (i in seq_along(groups)) {
          grp        <- groups[[i]]
          merge_name <- names(groups)[i]
          incProgress(1 / length(groups))
          selected_splits <- grp$VariableSplit
          act_kw   <- cfg$activity_keyword %||% "Impressions"
          spend_kw <- cfg$spend_keyword    %||% "Spend"
          
          if (!length(intersect(selected_splits, names(res$rag)))) {
            n_skipped <- n_skipped + 1L
            showNotification(paste0("'", merge_name, "': not in RAG — skipped."),
                             type = "warning", duration = 5); next
          }
          if (!nrow(filter(res$act_diagnoses,
                           VariableSplit %in% selected_splits,
                           period == view_filter))) {
            n_skipped <- n_skipped + 1L
            showNotification(paste0("'", merge_name, "': no data — skipped."),
                             type = "warning", duration = 5); next
          }
          
          spend_splits   <- str_replace_all(
            selected_splits, regex(act_kw, ignore_case = TRUE), spend_kw)
          new_spend_name <- str_replace_all(
            merge_name, regex(act_kw, ignore_case = TRUE), spend_kw)
          if (new_spend_name == merge_name)
            new_spend_name <- paste0(merge_name, "_", spend_kw)
          matching_cost <- intersect(spend_splits,
                                     res$cost_diagnoses$VariableSplit)
          
          merge_entry <- list(new_name       = merge_name,
                              merged         = as.list(selected_splits),
                              view           = view_filter,
                              spend_merged   = as.list(matching_cost),
                              new_spend_name = new_spend_name)
          res     <- apply_single_merge(res, merge_entry, cfg)
          new_log <- c(new_log, list(list(
            merged = selected_splits, new_name = merge_name,
            view = view_filter, spend_merged = matching_cost,
            new_spend_name = new_spend_name)))
          new_saved <- c(new_saved, list(list(
            merged = as.list(selected_splits), new_name = merge_name,
            view = view_filter, spend_merged = as.list(matching_cost),
            new_spend_name = new_spend_name, active = TRUE,
            saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
        }
      })
      
      set_res(nm, res); set_log(nm, c(get_log(nm), new_log))
      if (!is.null(update_merges) && length(new_saved) > 0) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        max_id   <- if (length(existing))
          max(sapply(existing, \(m) m$id %||% 0L)) else 0L
        for (i in seq_along(new_saved)) new_saved[[i]]$id <- max_id + i
        update_merges(nm, c(existing, new_saved))
      }
      n_ok <- length(new_log)
      showNotification(
        paste0(n_ok, " group(s) merged",
               if (n_skipped > 0) paste0(" (", n_skipped, " skipped)") else "",
               if (!is.null(update_merges) && n_ok > 0)
                 " — saved to config." else "."),
        type = if (n_ok > 0) "message" else "warning")
    })
    
    # ── Merge toolbar ──────────────────────────────────────────────────────
    output$merge_toolbar <- renderUI({
      results_trigger()
      nm  <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      res <- results_store[[nm]]
      if (is.null(res))
        return(tags$p(class = "text-muted small mb-2",
                      icon("info-circle"), " Process a channel first."))
      selected <- input$diag_act_rows_selected %||% integer(0)
      n_sel    <- length(selected)
      if (n_sel == 0)
        return(tags$p(class = "text-muted small mb-2",
                      icon("hand-pointer"),
                      " Select rows in the table to merge splits."))
      
      df             <- current_act_data()
      selected_names <- df$VariableSplit[selected]
      display_names  <- strip_common_prefix(selected_names)
      parts          <- get_merge_name_parts(selected_names)
      
      hint <- if (nchar(parts$prefix) > 0 || nchar(parts$suffix) > 0)
        div(class = "hint-text mt-1",
            icon("circle-info", class = "icon-xs"),
            " Detected pattern: ",
            tags$code(class = "code-tag-blue-sm",
                      paste0(parts$prefix, "...", parts$suffix)))
      else NULL
      
      combined <- df %>%
        filter(VariableSplit %in% selected_names) %>%
        summarise(act   = sum(total_activity,     na.rm = TRUE),
                  pct   = sum(pct_total_activity, na.rm = TRUE),
                  weeks = max(num_weeks_activity, na.rm = TRUE))
      
      div(
        class = "merge-toolbar-box",
        layout_columns(
          col_widths = c(4, 4, 4),
          tagList(
            tags$div(icon("object-group", class = "icon-blue-sm"),
                     tags$strong(paste0(" ", n_sel, " splits selected"),
                                 class = "merge-toolbar-title")),
            tags$div(class = "mt-1",
                     tags$small(paste0("Combined: ",
                                       fmt_compact(combined$act),
                                       " (", round(combined$pct, 2), "%)",
                                       " | Max weeks: ", combined$weeks))),
            tags$div(class = "mt-1",
                     tags$small(class = "text-muted",
                                paste(display_names, collapse = " + ")))
          ),
          div(textInput(session$ns("merge_name"),
                        tags$small("New split name"),
                        placeholder = "e.g. Channel_Small_Other",
                        width = "100%"), hint),
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
    
    observeEvent(input$diag_act_rows_selected, {
      selected <- input$diag_act_rows_selected %||% integer(0)
      if (length(selected) < 2) return()
      df <- tryCatch(current_act_data(), error = \(e) NULL)
      if (is.null(df) || nrow(df) == 0) return()
      selected_names <- df$VariableSplit[selected]
      if (length(selected_names) < 2) return()
      parts <- get_merge_name_parts(selected_names)
      updateTextInput(session, "merge_name",
                      value = paste0(parts$prefix, "Small_Other", parts$suffix))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ── Interactive merge ──────────────────────────────────────────────────
    observeEvent(input$btn_merge, {
      nm       <- req(input$channel_select); req(valid_nm(nm))
      selected <- req(input$diag_act_rows_selected)
      new_name <- trimws(input$merge_name %||% "")
      if (!nchar(new_name)) {
        showNotification("Enter a name for the merged split.",
                         type = "warning"); return()
      }
      res             <- results_store[[nm]]; req(res)
      cfg             <- channels()[[nm]]
      view_filter     <- input$period_filter %||% "focus"
      df              <- current_act_data()
      selected_splits <- df$VariableSplit[selected]
      act_kw          <- cfg$activity_keyword %||% "Impressions"
      spend_kw        <- cfg$spend_keyword    %||% "Spend"
      
      if (!length(intersect(selected_splits, names(res$rag)))) {
        showNotification("Selected splits not found in RAG.",
                         type = "warning"); return()
      }
      if (!nrow(filter(res$act_diagnoses,
                       VariableSplit %in% selected_splits,
                       period == view_filter))) {
        showNotification(paste0("No diagnosis data for '", view_filter, "'."),
                         type = "warning", duration = 10); return()
      }
      
      spend_splits   <- str_replace_all(
        selected_splits, regex(act_kw, ignore_case = TRUE), spend_kw)
      new_spend_name <- str_replace_all(
        new_name, regex(act_kw, ignore_case = TRUE), spend_kw)
      if (new_spend_name == new_name)
        new_spend_name <- paste0(new_name, "_", spend_kw)
      matching_cost <- intersect(spend_splits,
                                 res$cost_diagnoses$VariableSplit)
      
      merge_entry <- list(new_name       = new_name,
                          merged         = as.list(selected_splits),
                          view           = view_filter,
                          spend_merged   = as.list(matching_cost),
                          new_spend_name = new_spend_name)
      
      set_hist(nm, c(get_hist(nm), list(results_store[[nm]])))
      set_res(nm, apply_single_merge(res, merge_entry, cfg))
      set_log(nm, c(get_log(nm), list(list(
        merged = selected_splits, new_name = new_name, view = view_filter,
        spend_merged = matching_cost, new_spend_name = new_spend_name))))
      
      if (!is.null(update_merges)) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        max_id   <- if (length(existing))
          max(sapply(existing, \(m) m$id %||% 0L)) else 0L
        update_merges(nm, c(existing, list(list(
          id = max_id + 1L, new_name = new_name,
          merged = as.list(selected_splits), view = view_filter,
          spend_merged = as.list(matching_cost),
          new_spend_name = new_spend_name, active = TRUE,
          saved_at = format(Sys.time(), "%Y-%m-%d %H:%M")))))
      }
      
      updateTextInput(session, "merge_name", value = "")
      showNotification(
        paste0("Merged ", length(selected_splits),
               " [", toupper(view_filter), "] splits",
               if (length(matching_cost) > 0)
                 paste0(" + ", length(matching_cost), " spend"),
               " \u2192 ", new_name, " \u2014 saved to config"),
        type = "message")
    })
    
    observeEvent(input$btn_clear, {
      DT::dataTableProxy(session$ns("diag_act")) %>% DT::selectRows(NULL)
    })
    
    # ── Undo ───────────────────────────────────────────────────────────────
    observeEvent(input$btn_undo, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      hist <- get_hist(nm)
      if (!length(hist)) {
        showNotification("No merges to undo.", type = "warning"); return()
      }
      set_res(nm, hist[[length(hist)]])
      set_hist(nm, hist[-length(hist)])
      log <- get_log(nm)
      if (length(log) > 0) set_log(nm, log[-length(log)])
      if (!is.null(update_merges)) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        if (length(existing) > 0)
          update_merges(nm, existing[-length(existing)])
      }
      showNotification("Last merge undone \u2014 removed from config.",
                       type = "message")
    })
    
    # ── Reset merges ───────────────────────────────────────────────────────
    observeEvent(input$btn_reset_merges, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation",
                             class = "banner-icon-yellow"),
                        " Reset all merges"),
        tags$p("Reset all merges for ", tags$strong(nm), "?"),
        tags$p(class = "text-muted small",
               "This will also clear them from the saved config."),
        footer = tagList(
          actionButton(session$ns("btn_confirm_reset"), "Reset",
                       class = "btn-danger"),
          modalButton("Cancel")),
        easyClose = TRUE, size = "s"))
    })
    
    observeEvent(input$btn_confirm_reset, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      orig <- get_orig(nm); req(orig)
      set_res(nm, orig); set_log(nm, list()); set_hist(nm, list())
      if (!is.null(update_merges)) update_merges(nm, list())
      removeModal()
      showNotification(paste("All merges reset for", nm,
                             "\u2014 config cleared."),
                       type = "message")
    }, ignoreInit = TRUE)
    
    # ── Merge history card ─────────────────────────────────────────────────
    output$merge_history_card <- renderUI({
      results_trigger()
      nm   <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      log  <- get_log(nm)
      hist <- get_hist(nm)
      if (!length(log)) return(NULL)
      card(
        card_header(
          div(class = "card-header-inner",
              icon("code-merge", class = "icon-blue-sm"),
              "Merge History",
              tags$small(
                paste0(length(log), " merge",
                       if (length(log) != 1) "s" else "",
                       " \u2014 auto-saved to config"),
                class = "merge-history-subtitle"))
        ),
        tagList(
          div(
            class = "mb-3",
            lapply(rev(seq_along(log)), function(i) {
              m       <- log[[i]]
              is_last <- i == length(log)
              vb <- switch(m$view %||% "all",
                           "focus"    = tags$span("FOCUS",
                                                  class = "badge-focus-sm"),
                           "nonfocus" = tags$span("NON-FOCUS",
                                                  class = "badge-nonfocus-sm"),
                           tags$span("ALL", class = "badge-all-sm"))
              div(
                class = "merge-history-row",
                icon("arrow-right",
                     class = if (is_last) "merge-icon-latest"
                     else "merge-icon-old"),
                div(
                  class = "flex-1-mw0",
                  div(class = "d-flex align-items-center gap-2 flex-wrap",
                      tags$strong(m$new_name,
                                  class = if (is_last) "merge-name-latest"
                                  else "merge-name-old"),
                      vb,
                      if (is_last) tags$span("latest",
                                             class = "latest-marker")),
                  tags$div(class = "merge-splits-text",
                           paste(strip_common_prefix(m$merged),
                                 collapse = " + "))
                )
              )
            })
          ),
          div(
            class = "d-flex gap-2",
            if (length(hist) > 0)
              actionButton(session$ns("btn_undo"),
                           tagList(icon("rotate-left"), " Undo Last"),
                           class = "btn-outline-secondary btn-sm flex-fill"),
            actionButton(session$ns("btn_reset_merges"),
                         tagList(icon("trash"), " Reset All"),
                         class = paste("btn-outline-danger btn-sm",
                                       if (length(hist) > 0) "flex-fill"
                                       else "w-100"))
          )
        )
      )
    })
    
    # ── Activity table ─────────────────────────────────────────────────────
    output$diag_act <- DT::renderDT({
      results_trigger()
      nm  <- req(input$channel_select); req(results_store[[nm]])
      df  <- current_act_data(); req(nrow(df) > 0)
      
      threshold   <- input$threshold_pct %||% 1
      finite_pcts <- df$pct_total_activity[is.finite(df$pct_total_activity)]
      max_pct     <- if (length(finite_pcts) > 0 && max(finite_pcts) > 0)
        max(finite_pcts) else 1
      
      df_display <- df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      
      num_fmt  <- intersect(c("sd", "min", "quartile_1", "median",
                              "quartile_3", "max_no_outlier", "max"),
                            names(df_display))
      col_defs <- list(
        list(className = "dt-left", targets = 0),
        list(targets = which(names(df_display) == "total_activity") - 1,
             render = JS("function(d,t){if(t!=='display')return d;",
                         "var n=parseFloat(d);",
                         "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
                         "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
                         "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
                         "return n.toLocaleString();}")))
      
      dt <- df_display %>%
        datatable(
          selection = list(mode = "multiple", target = "row"),
          options   = list(
            scrollX      = TRUE,
            scrollY      = "420px",
            paging       = FALSE,
            dom          = "frt",
            deferRender  = TRUE,
            scroller     = TRUE,
            autoWidth    = FALSE,
            initComplete = dt_blue_callback,
            columnDefs   = col_defs
          ),
          rownames = FALSE
        )
      
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "",
                                    digits = 0, mark = ",")
      dt %>%
        formatStyle("pct_total_activity",
                    background         = styleColorBar(c(0, max_pct), "#EBF3FB"),
                    backgroundSize     = "100% 90%",
                    backgroundRepeat   = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("pct_total_activity",
                    color = styleInterval(threshold, c("#dc3545", "#333")))
    }, server = TRUE)
    
    # ── Spend table ────────────────────────────────────────────────────────
    output$diag_cost <- DT::renderDT({
      results_trigger()
      nm <- req(input$channel_select); req(results_store[[nm]])
      cost_df <- current_spend_data()
      if (is.null(cost_df) || nrow(cost_df) == 0) {
        cfg <- channels()[[nm]]
        return(datatable(
          data.frame(Info = paste0("No spend data. Check keyword: '",
                                   cfg$spend_keyword %||% "Spend", "'")),
          options  = list(initComplete = dt_blue_callback),
          rownames = FALSE))
      }
      df_display <- cost_df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      num_fmt <- intersect(c("sd", "min", "quartile_1", "median",
                             "quartile_3", "max_no_outlier", "max"),
                           names(df_display))
      dt <- df_display %>%
        datatable(
          options = list(
            scrollX      = TRUE, scrollY = "420px",
            paging       = FALSE, dom = "frt",
            deferRender  = TRUE, scroller  = TRUE, autoWidth = FALSE,
            initComplete = dt_blue_callback,
            columnDefs = list(
              list(className = "dt-left", targets = 0),
              list(targets = which(names(df_display) == "total_spend") - 1,
                   render = JS("function(d,t){if(t!=='display')return d;",
                               "var n=parseFloat(d);",
                               "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
                               "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
                               "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
                               "return n.toLocaleString();}")))),
          rownames = FALSE)
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "",
                                    digits = 0, mark = ",")
      dt
    })
    
    # ── Total Check ────────────────────────────────────────────────────────
    total_check_data <- reactive({
      results_trigger()
      nm  <- req(input$channel_select)
      res <- req(results_store[[nm]])
      list(nm   = nm,
           res  = res,
           d    = isolate(data()),
           cfg  = isolate(channels()[[nm]]),
           gcfg = isolate(config()))
    }) %>% bindCache(input$channel_select, results_trigger())
    
    output$diag_check <- DT::renderDT({
      tc   <- total_check_data()
      nm   <- tc$nm;  res  <- tc$res
      d    <- tc$d;   cfg  <- tc$cfg;  gcfg <- tc$gcfg
      req(d$analytical)
      
      cross_cols    <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
      full_cross_id <- c(cross_cols, "Period")
      geo_col       <- cross_cols[1]
      model_var     <- cfg$model_variable %||% ""
      valid_vars    <- intersect(c(model_var), names(d$analytical))
      
      if (!length(valid_vars) || !nzchar(model_var)) {
        avail_num <- names(d$analytical)[sapply(d$analytical, is.numeric)]
        return(datatable(
          data.frame(
            Problem = paste0("model_variable '", model_var, "' not found."),
            Hint    = paste0("Available: ",
                             paste(head(avail_num, 5), collapse = ", "),
                             if (length(avail_num) > 5)
                               paste0(" ... (", length(avail_num) - 5,
                                      " more)") else "")),
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Problem", color = "#721c24",
                        fontWeight = "600", backgroundColor = "#f8d7da"))
      }
      
      rag_df     <- as.data.frame(res$rag)
      an_periods <- sort(unique(d$analytical$Period))
      an_min_p   <- min(an_periods); an_max_p <- max(an_periods)
      
      scope_min_p <- tryCatch({
        if (!is.null(cfg$min_period) && !is.na(as.Date(cfg$min_period)))
          max(an_min_p, as.Date(cfg$min_period)) else an_min_p
      }, error = function(e) an_min_p)
      scope_max_p <- tryCatch({
        if (!is.null(cfg$max_period) && !is.na(as.Date(cfg$max_period)))
          min(an_max_p, as.Date(cfg$max_period)) else an_max_p
      }, error = function(e) an_max_p)
      if (is.na(scope_min_p) || is.na(scope_max_p) ||
          scope_min_p > scope_max_p) {
        scope_min_p <- an_min_p; scope_max_p <- an_max_p
      }
      
      rag_periods_all <- sort(unique(rag_df$Period))
      rag_periods <- {
        in_scope <- rag_periods_all[rag_periods_all >= scope_min_p &
                                      rag_periods_all <= scope_max_p]
        if (length(in_scope) > 0) in_scope else rag_periods_all
      }
      if (length(rag_periods) == 0) {
        return(datatable(
          data.frame(Message = "No RAG data within channel date range."),
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Message", color = "#856404",
                        backgroundColor = "#fff3cd"))
      }
      
      an_periods_scoped <- an_periods[
        an_periods >= scope_min_p & an_periods <= scope_max_p]
      if (length(an_periods_scoped) == 0) {
        return(datatable(
          data.frame(Message = paste0("No analytical periods in scope (",
                                      format(scope_min_p), " \u2192 ",
                                      format(scope_max_p), ").")),
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Message", color = "#856404",
                        backgroundColor = "#fff3cd"))
      }
      
      period_map <- tibble(
        an_period  = an_periods_scoped,
        rag_period = rag_periods[vapply(
          an_periods_scoped,
          function(p) which.min(abs(as.numeric(rag_periods) -
                                      as.numeric(p))),
          integer(1))])
      max_offset_days <- max(
        abs(as.numeric(period_map$an_period) -
              as.numeric(period_map$rag_period)), na.rm = TRUE)
      
      model_at_an_full <- build_model_total(
        d$analytical, full_cross_id, c(model_var), character(0)) %>%
        filter(Period >= scope_min_p & Period <= scope_max_p)
      
      normalize_geo <- function(x)
        trimws(gsub("\\s+", " ", tolower(gsub("[,.]", " ", as.character(x)))))
      rag_geos <- unique(rag_df[[geo_col]])
      an_geos  <- unique(model_at_an_full[[geo_col]])
      geo_map  <- tibble(an_geo = an_geos,
                         norm   = normalize_geo(an_geos)) %>%
        left_join(tibble(rag_geo = rag_geos,
                         norm    = normalize_geo(rag_geos)), by = "norm") %>%
        mutate(rag_geo = if_else(is.na(rag_geo), an_geo, rag_geo)) %>%
        select(an_geo, rag_geo)
      model_remapped <- model_at_an_full %>%
        rename(an_geo = !!sym(geo_col)) %>%
        left_join(geo_map, by = "an_geo") %>%
        mutate(!!geo_col := if_else(!is.na(rag_geo), rag_geo, an_geo)) %>%
        select(-an_geo, -rag_geo)
      
      tc_cross_id   <- full_cross_id
      tc_cross_cols <- cross_cols
      model_at_an   <- model_remapped
      
      seg_overrides     <- cfg$segment_overrides %||% list()
      apply_geo_filters <- function(df, col) {
        if (!col %in% names(df)) return(df)
        for (p in cfg$geography_exclude %||% character(0))
          if (nchar(p %||% "") > 0)
            df <- df[!grepl(p, df[[col]], ignore.case = TRUE), ]
        if (length(seg_overrides) > 0 && "Period" %in% names(df)) {
          for (so in seg_overrides) {
            geo_exc <- so$geography_exclude %||% character(0)
            if (!length(geo_exc)) next
            for (p in geo_exc)
              if (nchar(p %||% "") > 0)
                df <- df[!(rep(TRUE, nrow(df)) &
                             grepl(p, df[[col]], ignore.case = TRUE)), ]
          }
        }
        df
      }
      
      rag_in_scope <- rag_df %>%
        filter(Period >= scope_min_p & Period <= scope_max_p)
      rag_in_scope <- apply_geo_filters(rag_in_scope, geo_col)
      model_at_an  <- apply_geo_filters(model_at_an,  geo_col)
      
      id_in_rag  <- intersect(full_cross_id, names(rag_in_scope))
      spend_kw_f <- cfg$spend_keyword %||% "Spend"
      all_num    <- setdiff(
        names(rag_in_scope)[sapply(rag_in_scope, is.numeric)], id_in_rag)
      split_cols <- all_num[!grepl(spend_kw_f, all_num, ignore.case = TRUE)]
      
      rag_in_scope$row_splits <- if (length(split_cols) > 0)
        rowSums(rag_in_scope[, split_cols, drop = FALSE], na.rm = TRUE) else 0
      
      group_cols  <- intersect(tc_cross_id, names(rag_in_scope))
      splits_side <- rag_in_scope %>%
        select(any_of(c(group_cols, "row_splits"))) %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(SplitsTotal = sum(row_splits, na.rm = TRUE),
                  .groups = "drop")
      
      model_side <- model_at_an %>%
        rename(an_period = Period) %>%
        left_join(period_map, by = "an_period",
                  relationship = "many-to-one") %>%
        mutate(Period = if_else(!is.na(rag_period), rag_period, an_period)) %>%
        select(any_of(c(tc_cross_cols, "Period", "ModelTotal"))) %>%
        filter(ModelTotal > 0)
      
      check_df <- model_side %>%
        left_join(splits_side, by = tc_cross_id) %>%
        mutate(SplitsTotal = replace_na(SplitsTotal, 0),
               Diff   = ModelTotal - SplitsTotal,
               Status = if_else(abs(Diff) < 0.01, "OK", "Mismatch")) %>%
        filter(ModelTotal > 0) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4))) %>%
        arrange(across(any_of(c(tc_cross_cols, "Period"))))
      
      n_total    <- nrow(check_df)
      n_mismatch <- sum(check_df$Status == "Mismatch", na.rm = TRUE)
      cs_label   <- paste(tc_cross_cols, collapse = " \u00d7 ")
      
      scope_note <- if (scope_min_p != an_min_p || scope_max_p != an_max_p)
        htmltools::tags$div(
          style = paste0("background:#f0fdf4; color:#166634;",
                         "padding:4px 10px; border-radius:4px;",
                         "font-size:11.5px; margin-bottom:4px;",
                         "display:inline-block;"),
          paste0(" \u2139 Scoped: ",
                 format(scope_min_p), " \u2192 ", format(scope_max_p)))
      else NULL
      
      offset_note <- if (max_offset_days > 0)
        htmltools::tags$div(
          style = paste0("background:#d1ecf1; color:#0c5460;",
                         "padding:4px 10px; border-radius:4px;",
                         "font-size:11.5px; margin-bottom:4px;",
                         "display:inline-block;"),
          paste0(" \u2139 Date offset (", max_offset_days,
                 " day(s)) \u2014 auto-aligned."))
      else NULL
      
      check_df <- check_df %>% filter(Status == "Mismatch")
      
      if (nrow(check_df) == 0) {
        return(datatable(
          data.frame(Message = paste0(
            " \u2713 All ", format(n_total, big.mark = ","),
            " rows match \u2014 no mismatches found.")),
          caption = if (!is.null(scope_note) || !is.null(offset_note))
            htmltools::tags$caption(
              style = "caption-side:top; text-align:left; padding:4px 0;",
              scope_note, offset_note) else NULL,
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Message", color = "#155724",
                        fontWeight = "600", backgroundColor = "#d4edda"))
      }
      
      check_df %>%
        datatable(
          caption = htmltools::tags$caption(
            style = "caption-side:top; text-align:left; padding:4px 0;",
            scope_note, offset_note,
            htmltools::tags$div(
              style = "font-size:12px; color:#721c24; padding:2px 0;",
              htmltools::tags$span(style = "color:#e74c3c;", " \u2718 "),
              paste0(n_mismatch, " mismatch",
                     if (n_mismatch != 1) "es" else "",
                     " out of ", format(n_total, big.mark = ","), " rows",
                     " \u2014 level: ", cs_label, " \u00d7 Period"))),
          extensions = "Buttons",
          options    = list(
            scrollX      = TRUE, pageLength = 25,
            initComplete = dt_blue_callback, dom = "Bfrtip",
            autoWidth    = FALSE,
            buttons      = make_export_buttons("total_check", nm)),
          rownames = FALSE) %>%
        formatStyle("Status", backgroundColor = "#f8d7da")
    })
    
    # ── Return ─────────────────────────────────────────────────────────────
    list(
      results       = reactive(reactiveValuesToList(results_store)),
      clean_results = reactive(reactiveValuesToList(clean_store))
    )
  })
}