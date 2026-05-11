# ═══════════════════════════════════════════════════════════════════
# R/mod_process.R
# ═══════════════════════════════════════════════════════════════════

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
        hr(),
        uiOutput(ns("status")),
        hr(style = "margin:8px 0;"),
        downloadButton(ns("dl_config_process"),
                       label = tagList(icon("download"), " Download config CSV"),
                       class = "btn-outline-secondary btn-sm w-100")
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
          uiOutput(ns("merge_plan_toolbar")),
          uiOutput(ns("merge_toolbar")),
          DTOutput(ns("diag_act"))
        ),
        nav_panel("Spend",       DTOutput(ns("diag_cost"))),
        nav_panel("Total Check", DTOutput(ns("diag_check")))
      )
    )
  )
}

mod_process_server <- function(id, data, config, channels,
                               update_merges = NULL) {
  moduleServer(id, function(input, output, session) {
    
    results_store   <- reactiveValues()
    original_store  <- reactiveValues()
    merge_log_store <- reactiveValues()
    history_store   <- reactiveValues()
    results_trigger <- reactiveVal(0L)
    pending         <- reactiveValues(nm = NULL, base_res = NULL, merges = NULL)
    cs_selected     <- reactiveVal("ALL")
    
    get_res  <- function(nm) results_store[[nm]]
    set_res  <- function(nm, val) {
      results_store[[nm]] <- val
      results_trigger(isolate(results_trigger()) + 1L)
    }
    get_orig <- function(nm) original_store[[nm]]
    set_orig <- function(nm, val) { original_store[[nm]] <- val }
    get_log  <- function(nm) merge_log_store[[nm]] %||% list()
    set_log  <- function(nm, val) { merge_log_store[[nm]] <- val }
    get_hist <- function(nm) history_store[[nm]] %||% list()
    set_hist <- function(nm, val) { history_store[[nm]] <- val }
    
    valid_nm <- function(nm) {
      !is.null(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)
    }
    
    make_export_buttons <- function(prefix, nm) {
      fname <- paste0(prefix, "_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
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
        TRUE ~ formatC(round(x), format = "f", digits = 0, big.mark = ",")
      )
    }
    
    strip_common_prefix <- function(names_vec) {
      if (length(names_vec) <= 1) return(names_vec)
      parts      <- strsplit(names_vec, "_")
      min_len    <- min(sapply(parts, length))
      if (min_len == 0) return(names_vec)
      common_len <- 0L
      for (i in seq_len(min_len)) {
        if (length(unique(sapply(parts, `[[`, i))) == 1L) common_len <- i else break
      }
      if (common_len == 0L) return(names_vec)
      sapply(parts, function(p) {
        rest <- p[(common_len + 1):length(p)]
        if (!length(rest)) paste(p, collapse = "_") else paste(rest, collapse = "_")
      })
    }
    
    get_merge_name_parts <- function(selected_names) {
      if (length(selected_names) <= 1) return(list(prefix = "", suffix = ""))
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
        revs    <- sapply(strings, \(s) paste(rev(strsplit(s, "")[[1]]), collapse = ""))
        chars   <- lapply(revs, \(s) strsplit(s, "")[[1]])
        min_len <- min(sapply(chars, length)); n <- 0L
        for (i in seq_len(min_len)) {
          if (length(unique(sapply(chars, `[[`, i))) == 1L) n <- i else break
        }
        if (n == 0L) return("")
        raw <- paste(rev(strsplit(substr(revs[1], 1, n), "")[[1]]), collapse = "")
        pos <- min(gregexpr("_", raw)[[1]])
        if (pos < 1L || pos > nchar(raw)) "" else substr(raw, pos, nchar(raw))
      }
      prefix <- common_prefix_chars(selected_names)
      suffix <- common_suffix_chars(selected_names)
      if (nchar(prefix) + nchar(suffix) >= min(nchar(selected_names))) suffix <- ""
      list(prefix = prefix, suffix = suffix)
    }
    
    # ── build_model_total ─────────────────────────────────────────
    build_model_total <- function(analytical, cross_id, model_variables, break_dates) {
      n_vars        <- length(model_variables)
      break_dates_d <- as.Date(break_dates %||% character(0))
      base <- analytical %>% select(all_of(cross_id)) %>% mutate(ModelTotal = 0)
      for (i in seq_len(n_vars)) {
        mv <- model_variables[i]
        if (!mv %in% names(analytical)) {
          warning(sprintf("build_model_total: '%s' not found (segment %d).", mv, i))
          next
        }
        seg_start <- if (i == 1)      as.Date("1900-01-01") else break_dates_d[i - 1] + 1
        seg_end   <- if (i == n_vars) as.Date("2999-12-31") else break_dates_d[i]
        seg_vals  <- analytical %>%
          filter(Period >= seg_start, Period <= seg_end) %>%
          select(all_of(cross_id), model_val = !!sym(mv))
        base <- base %>%
          left_join(seg_vals, by = cross_id) %>%
          mutate(ModelTotal = if_else(!is.na(model_val), model_val, ModelTotal)) %>%
          select(-model_val)
      }
      base
    }
    
    # ── apply_single_merge ────────────────────────────────────────
    apply_single_merge <- function(res, merge_entry, cfg) {
      new_name        <- merge_entry$new_name
      selected_splits <- unlist(merge_entry$merged)
      view_filter     <- merge_entry$view %||% "focus"
      rag_cols        <- intersect(selected_splits, names(res$rag))
      if (!length(rag_cols)) {
        showNotification(paste0("Saved merge '", new_name,
                                "': splits not found in RAG — skipped."),
                         type = "warning", duration = 6)
        return(res)
      }
      new_rag <- res$rag
      new_rag[[new_name]] <- rowSums(new_rag[, rag_cols, drop = FALSE], na.rm = TRUE)
      new_rag <- new_rag[, setdiff(names(new_rag), rag_cols)]
      selected_diag <- res$act_diagnoses %>%
        filter(VariableSplit %in% selected_splits, period == view_filter)
      if (nrow(selected_diag) == 0) {
        showNotification(paste0("Saved merge '", new_name,
                                "': no data for '", view_filter, "' — skipped."),
                         type = "warning", duration = 6)
        return(res)
      }
      merged_act <- tibble(
        VariableSplit         = new_name,
        total_activity        = sum(selected_diag$total_activity,        na.rm = TRUE),
        pct_total_activity    = NA_real_,
        num_weeks_activity    = max(selected_diag$num_weeks_activity,    na.rm = TRUE),
        max_index             = NA_real_,
        min_consecutive_weeks = max(selected_diag$min_consecutive_weeks, na.rm = TRUE),
        sd         = NA_real_,
        min        = min(selected_diag$min,         na.rm = TRUE),
        quartile_1 = mean(selected_diag$quartile_1, na.rm = TRUE),
        median     = mean(selected_diag$median,     na.rm = TRUE),
        quartile_3 = mean(selected_diag$quartile_3, na.rm = TRUE),
        max_no_outlier = max(selected_diag$max_no_outlier, na.rm = TRUE),
        max        = max(selected_diag$max, na.rm = TRUE)
      ) %>% bind_cols(selected_diag %>% slice(1) %>%
                        select(any_of(c("seg", "period", "model_var"))))
      new_act_diag <- res$act_diagnoses %>%
        filter(!VariableSplit %in% selected_splits) %>%
        bind_rows(merged_act) %>% group_by(period) %>%
        mutate(grand_p = sum(total_activity, na.rm = TRUE),
               pct_total_activity = round(total_activity / pmax(grand_p, 1) * 100, 4)) %>%
        ungroup() %>% select(-grand_p)
      merged_as <- res$activity_spend %>%
        filter(VariableSplit %in% selected_splits) %>%
        summarise(VariableSplit = new_name,
                  total_activity = sum(total_activity, na.rm = TRUE),
                  total_spend    = sum(total_spend,    na.rm = TRUE),
                  Channel = first(Channel),
                  MainModelVariableName = first(MainModelVariableName))
      new_act_spend <- res$activity_spend %>%
        filter(!VariableSplit %in% selected_splits) %>% bind_rows(merged_as)
      merged_sm <- res$side_mapping %>%
        filter(VariableSplit %in% selected_splits) %>%
        slice(1) %>% mutate(VariableSplit = new_name)
      new_side_map <- res$side_mapping %>%
        filter(!VariableSplit %in% selected_splits) %>% bind_rows(merged_sm)
      spend_splits   <- unlist(merge_entry$spend_merged %||% character(0))
      new_spend_name <- merge_entry$new_spend_name %||%
        paste0(new_name, "_", cfg$spend_keyword)
      matching_cost  <- intersect(spend_splits, res$cost_diagnoses$VariableSplit)
      new_cost_diag <- tryCatch({
        if (!length(matching_cost)) { res$cost_diagnoses } else {
          sel_cost <- res$cost_diagnoses %>% filter(VariableSplit %in% matching_cost)
          mc_row <- tibble(
            VariableSplit         = new_spend_name,
            total_spend           = sum(sel_cost$total_spend,           na.rm = TRUE),
            pct_total_spend       = NA_real_,
            num_weeks_spend       = max(sel_cost$num_weeks_spend,       na.rm = TRUE),
            min_consecutive_weeks = max(sel_cost$min_consecutive_weeks, na.rm = TRUE),
            sd         = NA_real_,
            min        = min(sel_cost$min,         na.rm = TRUE),
            quartile_1 = mean(sel_cost$quartile_1, na.rm = TRUE),
            median     = mean(sel_cost$median,     na.rm = TRUE),
            quartile_3 = mean(sel_cost$quartile_3, na.rm = TRUE),
            max_no_outlier = max(sel_cost$max_no_outlier, na.rm = TRUE),
            max       = max(sel_cost$max, na.rm = TRUE), max_index = NA_real_,
            period    = sel_cost$period[1]    %||% "focus",
            seg       = sel_cost$seg[1]       %||% NA_integer_,
            model_var = sel_cost$model_var[1] %||% NA_character_
          )
          cc <- res$cost_diagnoses %>%
            filter(!VariableSplit %in% matching_cost) %>% bind_rows(mc_row)
          if ("period" %in% names(cc)) {
            cc %>% group_by(period) %>%
              mutate(gp = sum(total_spend, na.rm = TRUE),
                     pct_total_spend = round(total_spend / pmax(gp, 1) * 100, 4)) %>%
              ungroup() %>% select(-gp)
          } else {
            gt <- sum(cc$total_spend, na.rm = TRUE)
            cc %>% mutate(pct_total_spend = round(total_spend / pmax(gt, 1) * 100, 4))
          }
        }
      }, error = \(e) res$cost_diagnoses)
      list(rag = new_rag, cross_cols = res$cross_cols,
           is_rag = res$is_rag, ref_cross = res$ref_cross,
           activity_spend = new_act_spend, side_mapping = new_side_map,
           act_diagnoses = new_act_diag, cost_diagnoses = new_cost_diag)
    }
    
    # ── Sync channel selector ──────────────────────────────────────
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
        tags$strong(nm, style = "font-size:13px; color:#2c3e50;"),
        tags$span(paste0(" (", idx, " / ", length(nms), ")"),
                  style = "font-size:11px; color:#8a9bb0;")
      )
    })
    
    output$status <- renderUI({
      results_trigger()
      ch_names <- names(channels())
      if (!length(ch_names))
        return(tags$p(class = "text-muted small mt-2", "No channels configured."))
      tagList(lapply(ch_names, function(nm) {
        processed <- !is.null(results_store[[nm]])
        n_merges  <- length(get_log(nm))
        is_sel    <- identical(input$channel_select, nm)
        saved_m   <- channels()[[nm]]$saved_merges %||% list()
        has_saved <- length(saved_m) > 0
        div(
          style = paste0(
            "display:flex; align-items:center; gap:8px;",
            "padding:7px 8px; border-radius:6px; cursor:pointer;",
            "transition:background 0.15s; margin-bottom:2px;",
            "background:", if (is_sel) "#EBF3FB" else "transparent", ";"
          ),
          onclick = paste0("Shiny.setInputValue('", session$ns("ch_click"), "','",
                           nm, "',{priority:'event'});"),
          if (processed)
            icon("circle-check", style = "color:#2ecc71; font-size:13px; flex-shrink:0;")
          else
            icon("circle", style = "color:#dee2e6; font-size:13px; flex-shrink:0;"),
          tags$span(nm, style = paste0(
            "flex:1; font-size:12px; overflow:hidden; text-overflow:ellipsis;",
            "white-space:nowrap; font-weight:", if (is_sel) "700" else "400", ";",
            "color:", if (is_sel) "#5B9BD5" else "#2c3e50", ";"
          )),
          div(
            style = "display:flex; gap:4px; flex-shrink:0;",
            if (processed && n_merges > 0)
              tags$span(paste0(n_merges, "m"),
                        style = paste0("background:#EBF3FB; color:#5B9BD5;",
                                       "font-size:10px; font-weight:600;",
                                       "padding:1px 5px; border-radius:8px;")),
            if (has_saved)
              tags$span(icon("bookmark", style = "font-size:9px;"),
                        style = paste0("background:#fff3cd; color:#856404;",
                                       "font-size:10px; padding:1px 5px;",
                                       "border-radius:8px;"))
          )
        )
      }))
    })
    
    observeEvent(input$ch_click, {
      req(nzchar(input$ch_click %||% ""))
      updateSelectInput(session, "channel_select", selected = input$ch_click)
    }, ignoreInit = TRUE)
    
    output$dl_config_process <- downloadHandler(
      filename = function() {
        paste0("channel_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        cfg_data <- isolate(channels())
        if (!length(cfg_data)) { readr::write_csv(data.frame(), file); return() }
        df <- export_channels_csv(cfg_data)
        readr::write_csv(df, file, na = "")
      }
    )
    
    # ── Run engine ─────────────────────────────────────────────────
    run_one <- function(nm) {
      d    <- data()
      cfg  <- channels()[[nm]]; req(cfg)
      gcfg <- config()
      src  <- gcfg$source_type %||% "all_transformed"
      
      if (src == "all_rags" && is.null(d$all_rags)) {
        showNotification(paste0(nm, ": all_RAGs not uploaded."),
                         type = "error", duration = 8); return()
      }
      if (src == "all_transformed" && is.null(d$all_transformed)) {
        showNotification(paste0(nm, ": all_transformed not uploaded."),
                         type = "error", duration = 8); return()
      }
      if (is.null(d$analytical)) {
        showNotification("AnalyticalDataset not uploaded.",
                         type = "error", duration = 10); return()
      }
      if (is.null(d$dates_df)) {
        showNotification("dates_df is NULL.", type = "error", duration = 10); return()
      }
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        showNotification("Reporting period not configured. Check Setup tab.",
                         type = "error", duration = 10); return()
      }
      cross_cols_val <- gcfg$cross_cols %||% "Geography"
      missing_cc     <- setdiff(cross_cols_val, names(d$analytical))
      if (length(missing_cc) > 0) {
        showNotification(paste0(nm, ": cross-section column(s) '",
                                paste(missing_cc, collapse = "', '"),
                                "' not found in AnalyticalDataset."),
                         type = "error", duration = 12); return()
      }
      n_vars <- length(cfg$model_variables)
      if (length(cfg$break_dates) != n_vars - 1) {
        showNotification(paste0(nm, ": expected ", n_vars - 1,
                                " break date(s), got ", length(cfg$break_dates)),
                         type = "error", duration = 8); return()
      }
      
      res_stored <- NULL
      err_stored <- NULL
      
      withProgress(message = paste0("Processing: ", nm), value = 0, {
        tryCatch({
          res_stored <- process_channel(
            all_transformeds  = d$all_transformed,
            all_rags          = d$all_rags,
            analytical        = d$analytical,
            dates_df          = d$dates_df,
            cfg               = cfg,
            cross_cols        = gcfg$cross_cols %||% "Geography",
            source_type       = src,
            start_report_date = gcfg$start_report_date,
            end_report_date   = gcfg$end_report_date,
            update_label      = gcfg$update_label,
            dimension_breaks  = cfg$dimension_breaks  %||% list(),
            segment_overrides = cfg$segment_overrides %||% list(),
            progress_cb       = function(detail, value = NULL) {
              if (!is.null(value)) setProgress(value,   detail = detail)
              else                 incProgress(0,       detail = detail)
            }
          )
        }, error = function(e) { err_stored <<- conditionMessage(e) })
      })
      
      if (!is.null(err_stored)) {
        showNotification(paste(nm, "error:", err_stored),
                         type = "error", duration = 12)
        gc(verbose = FALSE, full = TRUE)
        return()
      }
      
      if (!is.null(res_stored)) {
        active_saved <- Filter(\(m) isTRUE(m$active), cfg$saved_merges %||% list())
        if (length(active_saved) > 0) {
          pending$nm       <- nm
          pending$base_res <- res_stored
          pending$merges   <- active_saved
          showModal(modalDialog(
            title = tagList(icon("bookmark"), " Saved merges found"),
            div(
              tags$p(style = "font-size:14px; margin-bottom:10px;",
                     tags$strong(length(active_saved)),
                     " saved merge(s) found for ", tags$strong(nm), ":"),
              tags$ul(style = "font-size:13px; color:#4a5568;",
                      lapply(active_saved, function(m) {
                        tags$li(tags$strong(m$new_name),
                                tags$span(paste0(" <- ", paste(
                                  strip_common_prefix(unlist(m$merged)),
                                  collapse = " + ")),
                                  style = "color:#8a9bb0; font-size:12px;"))
                      }))
            ),
            footer = tagList(
              actionButton(session$ns("btn_modal_apply"),
                           tagList(icon("check"), " Yes, apply merges"),
                           class = "btn-primary"),
              actionButton(session$ns("btn_modal_skip"),
                           "Process without merges",
                           class = "btn-outline-secondary"),
              modalButton("Cancel")
            ),
            easyClose = FALSE, size = "m"
          ))
        } else {
          set_res(nm, res_stored)
          set_orig(nm, res_stored)
          set_log(nm, list())
          set_hist(nm, list())
          showNotification(paste(nm, "processed successfully."), type = "message")
        }
        rm(res_stored)
        gc(verbose = FALSE, full = TRUE)
      }
    }
    
    observeEvent(input$btn_one, {
      nm <- req(input$channel_select); req(valid_nm(nm)); run_one(nm)
    })
    observeEvent(input$btn_all, { walk(names(channels()), run_one) })
    
    observeEvent(input$btn_modal_apply, {
      nm  <- pending$nm; req(!is.null(nm))
      res <- pending$base_res; cfg <- channels()[[nm]]
      withProgress(message = paste("Applying", length(pending$merges),
                                   "saved merges..."), value = 0.5, {
                                     for (m in pending$merges) res <- apply_single_merge(res, m, cfg)
                                   })
      set_res(nm, res); set_orig(nm, pending$base_res)
      set_log(nm, list()); set_hist(nm, list())
      n_applied <- length(pending$merges)
      pending$nm <- NULL; pending$base_res <- NULL; pending$merges <- NULL
      removeModal()
      showNotification(paste0(nm, " processed + ", n_applied,
                              " saved merge(s) applied."), type = "message")
      gc(verbose = FALSE, full = TRUE)
    })
    
    observeEvent(input$btn_modal_skip, {
      nm <- pending$nm; req(!is.null(nm))
      set_res(nm, pending$base_res); set_orig(nm, pending$base_res)
      set_log(nm, list()); set_hist(nm, list())
      pending$nm <- NULL; pending$base_res <- NULL; pending$merges <- NULL
      removeModal()
      showNotification(paste(nm, "processed (saved merges skipped)."), type = "message")
      gc(verbose = FALSE, full = TRUE)
    })
    
    # ── Period filter UI ───────────────────────────────────────────
    output$period_filter_ui <- renderUI({
      results_trigger()
      nm <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      res <- results_store[[nm]]
      if (is.null(res) || is.null(res$act_diagnoses) ||
          nrow(res$act_diagnoses) == 0) return(NULL)
      
      diag    <- res$act_diagnoses
      n_focus <- sum(diag$period == "focus",    na.rm = TRUE)
      n_nf    <- sum(diag$period == "nonfocus", na.rm = TRUE)
      
      div(
        style = paste0(
          "background:#f4f6f9; border-radius:8px;",
          "padding:10px 16px; margin-bottom:10px;",
          "display:flex; align-items:center; gap:16px;"
        ),
        tags$span(icon("filter", style = "color:#5B9BD5;"),
                  tags$strong(" View:", style = "font-size:13px; color:#333;")),
        radioButtons(session$ns("period_filter"), NULL,
                     choices  = c("Focus" = "focus", "Non-Focus" = "nonfocus"),
                     selected = "focus", inline = TRUE),
        div(
          style = "display:flex; gap:8px; font-size:12px;",
          tags$span(
            style = "background:#EBF3FB; color:#5B9BD5; padding:2px 8px; border-radius:10px; font-weight:600;",
            paste0("Focus: ", n_focus)),
          tags$span(
            style = "background:#f4f6f9; color:#6c757d; padding:2px 8px; border-radius:10px; font-weight:600; border:1px solid #dee2e6;",
            paste0("Non-Focus: ", n_nf))
        )
      )
    })
    
    # ── Current activity data ──────────────────────────────────────
    current_act_data <- reactive({
      results_trigger()
      nm         <- req(input$channel_select)
      res        <- req(results_store[[nm]])
      filter_val <- input$period_filter %||% "focus"
      
      if (!isTRUE(res$is_rag)) {
        df <- res$act_diagnoses
        if (is.null(df) || nrow(df) == 0)
          return(tibble(VariableSplit = character()))
        if (identical(filter_val, "focus"))
          df <- df[df$period == "focus", ]
        if (identical(filter_val, "nonfocus"))
          df <- df[df$period == "nonfocus", ]
        df %>%
          filter(total_activity > 0) %>%
          select(-any_of(c("seg", "period", "model_var"))) %>%
          mutate(across(where(is.numeric), \(x) round(x, 4)))
      } else {
        cfg  <- channels()[[nm]]
        gcfg <- config()
        req(!is.null(gcfg$start_report_date), !is.null(gcfg$end_report_date),
            length(gcfg$start_report_date) == 1, length(gcfg$end_report_date) == 1)
        
        rag_df <- as.data.frame(res$rag)
        num_cols_r <- names(rag_df)[sapply(rag_df, is.numeric)]
        rag_df <- aggregate(
          rag_df[, num_cols_r, drop = FALSE],
          by  = list(Period = rag_df$Period),
          FUN = function(x) sum(x, na.rm = TRUE)
        )
        rag_df$Period <- as.Date(rag_df$Period, origin = "1970-01-01")
        
        start_d <- as.Date(gcfg$start_report_date)
        end_d   <- as.Date(gcfg$end_report_date)
        req(length(start_d) == 1, length(end_d) == 1, !is.na(start_d), !is.na(end_d))
        
        rag_df <- switch(filter_val,
                         "focus"    = rag_df[rag_df$Period >= start_d & rag_df$Period <= end_d, ],
                         "nonfocus" = rag_df[rag_df$Period < start_d, ],
                         rag_df)
        if (nrow(rag_df) == 0) return(tibble(VariableSplit = character()))
        
        act_kw  <- cfg$activity_keyword %||% "Clicks"
        id_cols <- intersect("Period", names(rag_df))
        act_cols_keyword <- grep(act_kw, names(rag_df), ignore.case = TRUE, value = TRUE)
        act_cols_merged  <- if (
          is.null(res$act_diagnoses) || nrow(res$act_diagnoses) == 0 ||
          !"VariableSplit" %in% names(res$act_diagnoses)
        ) character(0) else {
          intersect(unique(res$act_diagnoses$VariableSplit),
                    setdiff(names(rag_df), id_cols))
        }
        act_cols <- union(act_cols_keyword, act_cols_merged)
        if (!length(act_cols)) return(tibble(VariableSplit = character()))
        
        df <- splits_summary(rag_df[, c(id_cols, act_cols), drop = FALSE], "activity")
        if (is.null(df) || nrow(df) == 0) return(tibble(VariableSplit = character()))
        
        df %>%
          filter(total_activity > 0) %>%
          select(-any_of(c("seg", "period", "model_var"))) %>%
          mutate(across(where(is.numeric), \(x) round(x, 4)))
      }
    })
    
    current_spend_data <- reactive({
      results_trigger()
      nm      <- req(input$channel_select)
      res     <- req(results_store[[nm]])
      cost_df <- res$cost_diagnoses
      if (is.null(cost_df) || nrow(cost_df) == 0) return(NULL)
      cost_df %>% filter(total_spend > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
    })
    
    output$activity_kpis <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      df <- current_act_data(); if (nrow(df) == 0) return(NULL)
      threshold     <- input$threshold_pct %||% 5
      total_splits  <- nrow(df)
      above_thresh  <- sum(df$pct_total_activity >= threshold, na.rm = TRUE)
      below_thresh  <- sum(df$pct_total_activity <  threshold, na.rm = TRUE)
      channel_total <- sum(df$total_activity, na.rm = TRUE)
      kpis <- list(
        list(label = "Total splits",
             value = total_splits,             icon = "layer-group", color = "#5B9BD5", bg = "#EBF3FB"),
        list(label = paste0("Above ", threshold, "%"),
             value = above_thresh,             icon = "arrow-up",    color = "#2ecc71",  bg = "#e8f5e9"),
        list(label = paste0("Below ", threshold, "% (review)"),
             value = below_thresh,             icon = "arrow-down",  color = "#e74c3c",  bg = "#fdecea"),
        list(label = "Channel total",
             value = fmt_compact(channel_total), icon = "chart-bar", color = "#5B9BD5",  bg = "#EBF3FB")
      )
      do.call(layout_columns, c(
        list(col_widths = c(3, 3, 3, 3), style = "margin-bottom:10px;"),
        lapply(kpis, function(k) {
          div(style = paste0("background:", k$bg, "; border-left:3px solid ", k$color, ";",
                             "border-radius:6px; padding:10px 14px;",
                             "display:flex; align-items:center; gap:10px;"),
              icon(k$icon, style = paste0("color:", k$color, "; font-size:20px;")),
              div(tags$strong(k$value,
                              style = "font-size:20px; color:#2c3e50; display:block; line-height:1.2;"),
                  tags$small(k$label, style = "color:#6c757d; font-size:11px;")))
        })
      ))
    })
    
    output$threshold_ui <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(style = paste0("background:#f4f6f9; border-radius:8px;",
                         "padding:10px 16px; margin-bottom:10px;",
                         "display:flex; align-items:center; gap:12px; flex-wrap:wrap;"),
          tags$span(icon("sliders", style = "color:#5B9BD5;"),
                    tags$strong(" Small split threshold:",
                                style = "font-size:13px; color:#333; white-space:nowrap;")),
          div(style = "display:flex; align-items:center; gap:6px;",
              div(style = "width:70px;",
                  numericInput(session$ns("threshold_pct"), NULL,
                               value = input$threshold_pct %||% 5,
                               min = 0, max = 100, step = 0.5)),
              tags$span("%", style = "color:#6c757d; font-size:13px; font-weight:600;")),
          tags$span(style = "font-size:11px; color:#adb5bd;",
                    icon("circle-info"),
                    " Splits below threshold shown in red.",
                    tags$br(),
                    " Select manually based on grouping logic."))
    })
    
    output$merge_plan_toolbar <- renderUI({
      results_trigger()
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(
        style = "display:flex; align-items:center; gap:8px; padding:8px 0; margin-bottom:6px;",
        downloadButton(session$ns("dl_merge_plan"),
                       label = tagList(icon("download"), " Download Merge Plan"),
                       class = "btn-outline-secondary btn-sm"),
        div(style = "position:relative;",
            tags$label(
              style = paste0(
                "display:flex; align-items:center; gap:6px;",
                "border:1px solid #5B9BD5; border-radius:5px;",
                "background:white; color:#5B9BD5; font-size:13px;",
                "font-weight:500; cursor:pointer;",
                "padding:5px 12px; margin:0; transition:all 0.15s; white-space:nowrap;"
              ),
              icon("upload"), " Apply Merge Plan",
              tags$input(type = "file", accept = ".csv", style = "display:none;",
                         onchange = paste0(
                           "var r=new FileReader();",
                           "r.onload=function(e){",
                           "Shiny.setInputValue('", session$ns("merge_plan_content"), "',",
                           "e.target.result,{priority:'event'});};",
                           "r.readAsText(this.files[0]);"))
            )),
        tags$span(style = "font-size:11px; color:#adb5bd;",
                  icon("circle-info", style = "font-size:10px;"),
                  " Write the same ", tags$strong("MergeName"),
                  " on splits you want to merge, then upload.")
      )
    })
    
    output$dl_merge_plan <- downloadHandler(
      filename = function() {
        nm <- input$channel_select %||% "channel"
        paste0("merge_plan_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- tryCatch(isolate(current_act_data()), error = function(e) NULL)
        if (is.null(df) || nrow(df) == 0) {
          write.csv(data.frame(VariableSplit = character(), Split = character(),
                               MergeName = character()),
                    file, row.names = FALSE); return()
        }
        stat_cols   <- setdiff(names(df), "VariableSplit")
        df_download <- df %>%
          mutate(Split = strip_common_prefix(VariableSplit),
                 MergeName = NA_character_) %>%
          select(VariableSplit, Split, MergeName, all_of(stat_cols))
        write.csv(df_download, file, row.names = FALSE, na = "")
      }
    )
    
    # ── Plan merge ─────────────────────────────────────────────────
    observeEvent(input$merge_plan_content, {
      req(input$merge_plan_content)
      nm <- req(input$channel_select); req(valid_nm(nm))
      plan <- tryCatch({
        con <- textConnection(input$merge_plan_content); on.exit(close(con))
        read.csv(con, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }, error = \(e) {
        showNotification(paste("Error reading file:", conditionMessage(e)),
                         type = "error", duration = 8); NULL
      })
      if (is.null(plan)) return()
      missing_cols <- setdiff(c("VariableSplit", "MergeName"), names(plan))
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")),
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
      groups      <- split(plan_active, trimws(as.character(plan_active$MergeName)))
      set_hist(nm, c(get_hist(nm), list(results_store[[nm]])))
      new_log <- list(); new_saved <- list(); n_skipped <- 0L
      withProgress(message = "Applying merge plan...", value = 0, {
        for (i in seq_along(groups)) {
          grp        <- groups[[i]]
          merge_name <- names(groups)[i]
          incProgress(1 / length(groups))
          selected_splits <- grp$VariableSplit
          if (!length(intersect(selected_splits, names(res$rag)))) {
            n_skipped <- n_skipped + 1L
            showNotification(paste0("'", merge_name, "': splits not in RAG — skipped."),
                             type = "warning", duration = 5); next
          }
          if (!nrow(filter(res$act_diagnoses,
                           VariableSplit %in% selected_splits,
                           period == view_filter))) {
            n_skipped <- n_skipped + 1L
            showNotification(paste0("'", merge_name, "': no data for '",
                                    view_filter, "' — skipped."),
                             type = "warning", duration = 5); next
          }
          spend_splits   <- str_replace_all(selected_splits,
                                            regex(cfg$activity_keyword, ignore_case = TRUE), cfg$spend_keyword)
          new_spend_name <- str_replace_all(merge_name,
                                            regex(cfg$activity_keyword, ignore_case = TRUE), cfg$spend_keyword)
          if (new_spend_name == merge_name)
            new_spend_name <- paste0(merge_name, "_", cfg$spend_keyword)
          matching_cost <- intersect(spend_splits, res$cost_diagnoses$VariableSplit)
          merge_entry <- list(new_name = merge_name, merged = as.list(selected_splits),
                              view = view_filter, spend_merged = as.list(matching_cost),
                              new_spend_name = new_spend_name)
          res <- apply_single_merge(res, merge_entry, cfg)
          new_log   <- c(new_log, list(list(
            merged = selected_splits, new_name = merge_name,
            view   = view_filter, spend_merged = matching_cost,
            new_spend_name = new_spend_name)))
          new_saved <- c(new_saved, list(list(
            merged = as.list(selected_splits), new_name = merge_name,
            view   = view_filter, spend_merged = as.list(matching_cost),
            new_spend_name = new_spend_name, active = TRUE,
            saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
        }
      })
      set_res(nm, res); set_log(nm, c(get_log(nm), new_log))
      if (!is.null(update_merges) && length(new_saved) > 0) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        max_id   <- if (length(existing)) max(sapply(existing, \(m) m$id %||% 0L)) else 0L
        for (i in seq_along(new_saved)) new_saved[[i]]$id <- max_id + i
        update_merges(nm, c(existing, new_saved))
      }
      n_ok <- length(new_log)
      showNotification(
        paste0(n_ok, " group(s) merged",
               if (n_skipped > 0) paste0(" (", n_skipped, " skipped)") else "",
               if (!is.null(update_merges) && n_ok > 0) " — saved to config." else "."),
        type = if (n_ok > 0) "message" else "warning")
    })
    
    output$merge_toolbar <- renderUI({
      results_trigger()
      nm <- input$channel_select; if (!valid_nm(nm)) return(NULL)
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
        div(style = "font-size:11px; color:#8a9bb0; margin-top:4px;",
            icon("circle-info", style = "font-size:10px;"),
            " Detected pattern: ",
            tags$code(
              style = "background:#EBF3FB; color:#5B9BD5; padding:1px 5px; border-radius:3px; font-size:11px;",
              paste0(parts$prefix, "...", parts$suffix)))
      else NULL
      combined <- df %>%
        filter(VariableSplit %in% selected_names) %>%
        summarise(act   = sum(total_activity,     na.rm = TRUE),
                  pct   = sum(pct_total_activity, na.rm = TRUE),
                  weeks = max(num_weeks_activity, na.rm = TRUE))
      div(
        style = paste0("background:#EBF3FB; border:1px solid #5B9BD5;",
                       "border-radius:8px; padding:12px 16px; margin-bottom:12px;"),
        layout_columns(
          col_widths = c(4, 4, 4),
          tagList(
            tags$div(icon("object-group", style = "color:#5B9BD5;"),
                     tags$strong(paste0(" ", n_sel, " splits selected"),
                                 style = "color:#5B9BD5; font-size:13px;")),
            tags$div(class = "mt-1",
                     tags$small(paste0("Combined: ", fmt_compact(combined$act),
                                       " (", round(combined$pct, 2), "%)",
                                       " | Max weeks: ", combined$weeks))),
            tags$div(class = "mt-1",
                     tags$small(style = "color:#6c757d; font-size:11px;",
                                paste(display_names, collapse = " + ")))
          ),
          div(textInput(session$ns("merge_name"), tags$small("New split name"),
                        placeholder = "e.g. Channel_Small_Other", width = "100%"), hint),
          tagList(
            actionButton(session$ns("btn_merge"), tagList(icon("link"), " Merge"),
                         class = "btn-primary btn-sm w-100 mb-1"),
            actionButton(session$ns("btn_clear"), tagList(icon("xmark"), " Clear"),
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
      parts      <- get_merge_name_parts(selected_names)
      suggestion <- paste0(parts$prefix, "Small_Other", parts$suffix)
      updateTextInput(session, "merge_name", value = suggestion)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ── Interactive merge ──────────────────────────────────────────
    observeEvent(input$btn_merge, {
      nm       <- req(input$channel_select); req(valid_nm(nm))
      selected <- req(input$diag_act_rows_selected)
      new_name <- trimws(input$merge_name %||% "")
      if (!nchar(new_name)) {
        showNotification("Enter a name for the merged split.", type = "warning"); return()
      }
      res <- results_store[[nm]]; req(res)
      cfg         <- channels()[[nm]]
      view_filter <- input$period_filter %||% "focus"
      df          <- current_act_data()
      selected_splits <- df$VariableSplit[selected]
      if (!length(intersect(selected_splits, names(res$rag)))) {
        showNotification("Selected splits not found in RAG.", type = "warning"); return()
      }
      if (!nrow(filter(res$act_diagnoses,
                       VariableSplit %in% selected_splits, period == view_filter))) {
        showNotification(paste0("No diagnosis data for '", view_filter,
                                "'. Try switching the period filter."),
                         type = "warning", duration = 10); return()
      }
      spend_splits   <- str_replace_all(selected_splits,
                                        regex(cfg$activity_keyword, ignore_case = TRUE), cfg$spend_keyword)
      new_spend_name <- str_replace_all(new_name,
                                        regex(cfg$activity_keyword, ignore_case = TRUE), cfg$spend_keyword)
      if (new_spend_name == new_name)
        new_spend_name <- paste0(new_name, "_", cfg$spend_keyword)
      matching_cost <- intersect(spend_splits, res$cost_diagnoses$VariableSplit)
      merge_entry <- list(new_name = new_name, merged = as.list(selected_splits),
                          view = view_filter, spend_merged = as.list(matching_cost),
                          new_spend_name = new_spend_name)
      set_hist(nm, c(get_hist(nm), list(results_store[[nm]])))
      set_res(nm, apply_single_merge(res, merge_entry, cfg))
      set_log(nm, c(get_log(nm), list(list(
        merged = selected_splits, new_name = new_name,
        view   = view_filter, spend_merged = matching_cost,
        new_spend_name = new_spend_name))))
      updateTextInput(session, "merge_name", value = "")
      showNotification(
        paste0("Merged ", length(selected_splits), " [", toupper(view_filter), "] splits",
               if (length(matching_cost) > 0) paste0(" + ", length(matching_cost), " spend"),
               " -> ", new_name),
        type = "message")
    })
    
    observeEvent(input$btn_clear, {
      DT::dataTableProxy(session$ns("diag_act")) %>% DT::selectRows(NULL)
    })
    
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
      showNotification("Last merge undone.", type = "message")
    })
    
    observeEvent(input$btn_save_merges, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      if (is.null(update_merges)) {
        showNotification("Save merges not available.", type = "warning"); return()
      }
      current_log <- get_log(nm)
      if (!length(current_log)) {
        showNotification("No session merges to save.", type = "warning"); return()
      }
      existing <- channels()[[nm]]$saved_merges %||% list()
      max_id   <- if (length(existing)) max(sapply(existing, \(m) m$id %||% 0L)) else 0L
      new_saved <- lapply(seq_along(current_log), function(i) {
        c(current_log[[i]], list(id = max_id + i, active = TRUE,
                                 saved_at = format(Sys.time(), "%Y-%m-%d %H:%M")))
      })
      update_merges(nm, c(existing, new_saved))
      showNotification(paste0(length(new_saved), " merge(s) saved to config."),
                       type = "message")
    })
    
    observe({
      results_trigger()
      nm <- input$channel_select; if (!valid_nm(nm)) return()
      saved <- channels()[[nm]]$saved_merges %||% list()
      if (!is.null(session$userData$toggle_obs))
        lapply(session$userData$toggle_obs,
               \(obs) tryCatch(obs$destroy(), error = \(e) NULL))
      obs_list <- lapply(seq_along(saved), function(i) {
        local({
          local_i <- i; local_nm <- nm
          observeEvent(input[[paste0("toggle_saved_", local_i)]], {
            if (is.null(update_merges)) return()
            curr <- channels()[[local_nm]]$saved_merges %||% list()
            if (local_i <= length(curr)) {
              curr[[local_i]]$active <- !isTRUE(curr[[local_i]]$active)
              update_merges(local_nm, curr)
            }
          }, ignoreInit = TRUE)
        })
      })
      session$userData$toggle_obs <- obs_list
    })
    
    output$merge_history_card <- renderUI({
      results_trigger()
      nm <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      log       <- get_log(nm); hist <- get_hist(nm)
      saved     <- channels()[[nm]]$saved_merges %||% list()
      has_session <- length(log) > 0; has_saved <- length(saved) > 0
      if (!has_session && !has_saved) return(NULL)
      card(
        card_header("Merge History"),
        tagList(
          if (has_saved) tagList(
            div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                icon("bookmark", style = "color:#5B9BD5; font-size:12px;"),
                tags$strong("Saved in config",
                            style = "font-size:11.5px; color:#5B9BD5;")),
            tagList(lapply(seq_along(saved), function(i) {
              m <- saved[[i]]; is_act <- isTRUE(m$active)
              div(class = "mb-2 p-2",
                  style = paste0(
                    "background:", if (is_act) "#EBF3FB" else "#f8f9fa", ";",
                    "border-radius:6px; border-left:3px solid ",
                    if (is_act) "#5B9BD5" else "#dee2e6", ";", "font-size:12px;"),
                  div(class = "d-flex align-items-center gap-2",
                      actionButton(session$ns(paste0("toggle_saved_", i)),
                                   label = if (is_act)
                                     tags$span(style = "background:#5B9BD5; color:white; padding:2px 8px; border-radius:10px; font-size:10px; font-weight:600;", "Active")
                                   else
                                     tags$span(style = "background:#dee2e6; color:#6c757d; padding:2px 8px; border-radius:10px; font-size:10px; font-weight:600;", "Inactive"),
                                   class = "btn btn-link p-0",
                                   style = "border:none; background:transparent;"),
                      tags$strong(m$new_name,
                                  style = paste0("color:", if (is_act) "#2c3e50" else "#adb5bd", ";")),
                      tags$small(m$saved_at %||% "",
                                 style = "color:#adb5bd; font-size:10px; margin-left:auto;")),
                  tags$div(class = "text-muted mt-1",
                           style = paste0("font-size:11px;", if (!is_act) "opacity:0.5;" else ""),
                           paste(strip_common_prefix(unlist(m$merged)), collapse = " + ")))
            })),
            hr(style = "margin:10px 0;")
          ),
          if (has_session) tagList(
            div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                icon("clock", style = "color:#6c757d; font-size:12px;"),
                tags$strong("This session",
                            style = "font-size:11.5px; color:#6c757d;")),
            if (length(hist) > 0)
              actionButton(session$ns("btn_undo"),
                           tagList(icon("rotate-left"), " Undo Last Merge"),
                           class = "btn-outline-secondary btn-sm w-100 mb-2"),
            lapply(rev(seq_along(log)), function(i) {
              m  <- log[[i]]
              vb <- switch(m$view %||% "all",
                           "focus"    = tags$span(style = "background:#EBF3FB; color:#5B9BD5; padding:1px 6px; border-radius:8px; font-size:10px;", "FOCUS"),
                           "nonfocus" = tags$span(style = "background:#f4f6f9; color:#6c757d; padding:1px 6px; border-radius:8px; font-size:10px; border:1px solid #dee2e6;", "NON-FOCUS"),
                           tags$span(style = "background:#fff3cd; color:#856404; padding:1px 6px; border-radius:8px; font-size:10px;", "ALL"))
              div(class = "mb-2 p-2",
                  style = paste0(
                    "background:#f4f6f9; border-radius:6px;",
                    "border-left:3px solid #5B9BD5; font-size:12px;",
                    if (i == length(log)) "border-left-width:4px;" else ""),
                  div(class = "d-flex align-items-center gap-2 mb-1",
                      icon("arrow-right", style = "color:#5B9BD5;"),
                      tags$strong(m$new_name), vb,
                      if (i == length(log))
                        tags$span("latest", style = "font-size:9px; color:#8a9bb0; margin-left:auto;")),
                  tags$div(class = "text-muted",
                           paste(strip_common_prefix(m$merged), collapse = " + ")),
                  if (length(m$spend_merged) > 0)
                    tags$div(class = "text-muted", style = "font-size:11px;",
                             paste0("Spend -> ", m$new_spend_name)))
            }),
            hr(style = "margin:10px 0;")
          ),
          div(style = "display:flex; flex-direction:column; gap:6px;",
              if (has_session && !is.null(update_merges))
                actionButton(session$ns("btn_save_merges"),
                             tagList(icon("bookmark"), " Save merges to config"),
                             class = "btn-primary btn-sm w-100"),
              downloadButton(session$ns("dl_config_process"),
                             label = tagList(icon("download"), " Download config CSV"),
                             class = "btn-outline-secondary btn-sm w-100"),
              actionButton(session$ns("btn_reset_merges"),
                           tagList(icon("trash"), " Reset All Merges"),
                           class = "btn-outline-danger btn-sm w-100"))
        )
      )
    })
    
    observeEvent(input$btn_reset_merges, {
      nm       <- req(input$channel_select); req(valid_nm(nm))
      n_merges <- length(get_log(nm))
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", style = "color:#f39c12;"),
                        " Reset all merges"),
        tags$p("This will reset all merges for channel ", tags$strong(nm), "."),
        if (n_merges > 0)
          tags$p(class = "text-muted small",
                 paste0(n_merges, " session merge(s) will be lost.")),
        footer = tagList(
          actionButton(session$ns("btn_confirm_reset"), "Reset all merges",
                       class = "btn-danger"),
          modalButton("Cancel")),
        easyClose = TRUE, size = "s"))
    })
    
    observeEvent(input$btn_confirm_reset, {
      nm   <- req(input$channel_select); req(valid_nm(nm))
      orig <- get_orig(nm); req(orig)
      set_res(nm, orig); set_log(nm, list()); set_hist(nm, list())
      removeModal()
      showNotification(paste("All merges reset for", nm), type = "message")
    }, ignoreInit = TRUE)
    
    # ── Activity table ─────────────────────────────────────────────
    output$diag_act <- DT::renderDT({
      results_trigger()
      nm        <- req(input$channel_select)
      req(results_store[[nm]])
      df        <- current_act_data(); req(nrow(df) > 0)
      threshold <- input$threshold_pct %||% 5
      max_pct   <- max(df$pct_total_activity, na.rm = TRUE)
      if (!is.finite(max_pct) || max_pct == 0) max_pct <- 1
      df_display <- df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      num_fmt <- intersect(
        c("sd", "min", "quartile_1", "median", "quartile_3", "max_no_outlier", "max"),
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
        datatable(selection = list(mode = "multiple", target = "row"),
                  options   = list(scrollX = TRUE, scrollY = "460px",
                                   paging = FALSE, dom = "frt",
                                   initComplete = dt_blue_callback,
                                   columnDefs = col_defs),
                  rownames  = FALSE)
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "", digits = 0, mark = ",")
      dt %>%
        formatStyle("pct_total_activity",
                    background = styleColorBar(c(0, max_pct), "#EBF3FB"),
                    backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("pct_total_activity",
                    color = styleInterval(threshold, c("#dc3545", "#333")))
    }, server = TRUE)
    
    # ── Spend table ────────────────────────────────────────────────
    output$diag_cost <- DT::renderDT({
      results_trigger()
      nm      <- req(input$channel_select)
      req(results_store[[nm]])
      cost_df <- current_spend_data()
      if (is.null(cost_df) || nrow(cost_df) == 0) {
        cfg <- channels()[[nm]]
        return(datatable(
          data.frame(Info = paste0("No spend data. Check keyword: '",
                                   cfg$spend_keyword %||% "Spend", "'")),
          options = list(initComplete = dt_blue_callback), rownames = FALSE))
      }
      df_display <- cost_df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      num_fmt <- intersect(
        c("sd", "min", "quartile_1", "median", "quartile_3", "max_no_outlier", "max"),
        names(df_display))
      dt <- df_display %>%
        datatable(extensions = "Buttons",
                  options = list(scrollX = TRUE, pageLength = 15,
                                 initComplete = dt_blue_callback, dom = "Bfrtip",
                                 buttons = make_export_buttons("spend", nm),
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
        dt <- dt %>% formatCurrency(num_fmt, currency = "", digits = 0, mark = ",")
      dt
    })
    
    # ── Total Check ────────────────────────────────────────────────
    output$diag_check <- DT::renderDT({
      results_trigger()
      nm  <- req(input$channel_select)
      res <- req(results_store[[nm]])
      d    <- isolate(data())
      cfg  <- isolate(channels()[[nm]])
      gcfg <- isolate(config())
      req(d$analytical)
      
      cross_cols    <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
      full_cross_id <- c(cross_cols, "Period")
      geo_col       <- cross_cols[1]
      
      # ── Missing variable check ─────────────────────────────────
      missing_vars <- setdiff(cfg$model_variables, names(d$analytical))
      if (length(missing_vars) > 0) {
        avail_num <- names(d$analytical)[sapply(d$analytical, is.numeric)]
        return(datatable(
          data.frame(
            Problem = paste0("Variable not found: '",
                             paste(missing_vars, collapse = "', '"), "'"),
            Hint    = paste0("Available: ",
                             paste(head(avail_num, 5), collapse = ", "),
                             if (length(avail_num) > 5)
                               paste0(" ... (", length(avail_num) - 5, " more)") else "")),
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Problem", color = "#721c24", fontWeight = "600",
                        backgroundColor = "#f8d7da"))
      }
      
      rag_df <- as.data.frame(res$rag)
      
      # ── Period scope ───────────────────────────────────────────
      an_periods      <- sort(unique(d$analytical$Period))
      an_min_p        <- min(an_periods)
      an_max_p        <- max(an_periods)
      rag_periods_all <- sort(unique(rag_df$Period))
      rag_periods <- {
        in_scope <- rag_periods_all[rag_periods_all >= an_min_p & rag_periods_all <= an_max_p]
        if (length(in_scope) > 0) in_scope else rag_periods_all
      }
      if (length(rag_periods) == 0) {
        return(datatable(
          data.frame(Message = "No RAG data within the analytical date range. Re-process the channel."),
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Message", color = "#856404", backgroundColor = "#fff3cd"))
      }
      
      # ── Period alignment ────────────────────────────────────────
      period_map <- tibble(
        an_period  = an_periods,
        rag_period = rag_periods[vapply(
          an_periods,
          function(p) which.min(abs(as.numeric(rag_periods) - as.numeric(p))),
          integer(1))])
      max_offset_days <- max(
        abs(as.numeric(period_map$an_period) - as.numeric(period_map$rag_period)),
        na.rm = TRUE)
      
      # ── Build model at full cross_id ────────────────────────────
      model_at_an_full <- build_model_total(
        d$analytical, full_cross_id, cfg$model_variables, cfg$break_dates)
      
      # ── Geography name normalization ────────────────────────────
      normalize_geo <- function(x)
        trimws(gsub("\\s+", " ", tolower(gsub("[,.]", " ", as.character(x)))))
      
      rag_geos <- unique(rag_df[[geo_col]])
      an_geos  <- unique(model_at_an_full[[geo_col]])
      geo_map  <- tibble(an_geo = an_geos, norm = normalize_geo(an_geos)) %>%
        left_join(tibble(rag_geo = rag_geos, norm = normalize_geo(rag_geos)), by = "norm") %>%
        mutate(rag_geo = if_else(is.na(rag_geo), an_geo, rag_geo)) %>%
        select(an_geo, rag_geo)
      
      model_remapped <- model_at_an_full %>%
        rename(an_geo = !!sym(geo_col)) %>%
        left_join(geo_map, by = "an_geo") %>%
        mutate(!!geo_col := if_else(!is.na(rag_geo), rag_geo, an_geo)) %>%
        select(-an_geo, -rag_geo)
      
      # ── Comparison level ────────────────────────────────────────
      # Two cases only — no auto-detection by product variation:
      # - National (all_transformed): Period only
      # - Geographic (all_rags):      full cross_id always
      tc_cross_id     <- full_cross_id
      tc_cross_cols   <- cross_cols
      comparison_note <- NULL
      
      if (!isTRUE(res$is_rag)) {
        tc_cross_id   <- "Period"
        tc_cross_cols <- character(0)
        comparison_note <- htmltools::tags$div(
          style = paste0(
            "background:#EBF3FB; color:#1e40af;",
            "padding:4px 10px; border-radius:4px;",
            "font-size:11.5px; margin-bottom:4px; display:inline-block;"),
          paste0("\u2139 National channel (all_transformed). ",
                 "Comparison at Period level \u2014 ",
                 "splits replicated across all analytical entities."))
      }
      
      # ── Aggregate model to comparison level ─────────────────────
      model_at_an <- if (identical(tc_cross_id, "Period")) {
        model_at_an_full %>%
          group_by(Period) %>%
          summarise(ModelTotal = dplyr::first(ModelTotal), .groups = "drop")
      } else {
        model_remapped
      }
      
      # ── Geo filter helper ───────────────────────────────────────
      seg_overrides <- cfg$segment_overrides %||% list()
      break_dates_d <- as.Date(cfg$break_dates %||% character(0))
      n_segs        <- length(cfg$model_variables)
      
      apply_geo_filters <- function(df, col) {
        if (!col %in% names(df)) return(df)
        for (p in cfg$geography_exclude %||% character(0))
          if (nchar(p %||% "") > 0)
            df <- df[!grepl(p, df[[col]], ignore.case = TRUE), ]
        if (length(seg_overrides) > 0 && "Period" %in% names(df)) {
          for (so in seg_overrides) {
            geo_exc   <- so$geography_exclude %||% character(0)
            if (!length(geo_exc)) next
            seg_start <- if (so$seg == 1) as.Date("1900-01-01")
            else break_dates_d[so$seg - 1] + 1
            seg_end   <- if (so$seg == n_segs) as.Date("2999-12-31")
            else break_dates_d[so$seg]
            for (p in geo_exc)
              if (nchar(p %||% "") > 0) {
                in_seg    <- df$Period >= seg_start & df$Period <= seg_end
                geo_match <- grepl(p, df[[col]], ignore.case = TRUE)
                df        <- df[!(in_seg & geo_match), ]
              }
          }
        }
        df
      }
      
      # Apply geo filters BEFORE computing splits (performance + correctness)
      rag_in_scope <- rag_df %>% filter(Period >= an_min_p & Period <= an_max_p)
      if (isTRUE(res$is_rag)) {
        rag_in_scope <- apply_geo_filters(rag_in_scope, geo_col)
        model_at_an  <- apply_geo_filters(model_at_an, geo_col)
      }
      
      # ── Splits side ─────────────────────────────────────────────
      id_in_rag  <- intersect(full_cross_id, names(rag_in_scope))
      split_cols <- setdiff(names(rag_in_scope)[sapply(rag_in_scope, is.numeric)],
                            id_in_rag)
      rag_in_scope$row_splits <- if (length(split_cols) > 0)
        rowSums(rag_in_scope[, split_cols, drop = FALSE], na.rm = TRUE) else 0
      
      group_cols  <- intersect(
        if (identical(tc_cross_id, "Period")) "Period" else tc_cross_id,
        names(rag_in_scope))
      splits_side <- rag_in_scope %>%
        select(any_of(c(group_cols, "row_splits"))) %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(SplitsTotal = sum(row_splits, na.rm = TRUE), .groups = "drop")
      
      # ── Model side ──────────────────────────────────────────────
      model_side <- model_at_an %>%
        rename(an_period = Period) %>%
        left_join(period_map, by = "an_period", relationship = "many-to-one") %>%
        mutate(Period = if_else(!is.na(rag_period), rag_period, an_period)) %>%
        select(any_of(c(tc_cross_cols, "Period", "ModelTotal"))) %>%
        filter(ModelTotal > 0)
      
      # ── Join ────────────────────────────────────────────────────
      join_by_cols <- if (identical(tc_cross_id, "Period")) "Period" else tc_cross_id
      check_df <- model_side %>%
        left_join(splits_side, by = join_by_cols) %>%
        mutate(SplitsTotal = replace_na(SplitsTotal, 0),
               Diff        = ModelTotal - SplitsTotal,
               Status      = if_else(abs(Diff) < 0.01, "OK", "Mismatch")) %>%
        filter(ModelTotal > 0) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4))) %>%
        arrange(across(any_of(c(tc_cross_cols, "Period"))))
      
      n_total    <- nrow(check_df)
      n_mismatch <- sum(check_df$Status == "Mismatch", na.rm = TRUE)
      cs_label   <- if (identical(tc_cross_id, "Period")) "Period"
      else paste(tc_cross_cols, collapse = " \u00d7 ")
      
      offset_note <- if (max_offset_days > 0)
        htmltools::tags$div(
          style = paste0("background:#d1ecf1; color:#0c5460;",
                         "padding:4px 10px; border-radius:4px;",
                         "font-size:11.5px; margin-bottom:4px; display:inline-block;"),
          paste0("\u2139 Date offset (", max_offset_days,
                 " day(s)) \u2014 model values auto-aligned to Data File periods."))
      else NULL
      
      check_df <- check_df %>% filter(Status == "Mismatch")
      
      # ── All OK ──────────────────────────────────────────────────
      if (nrow(check_df) == 0) {
        return(datatable(
          data.frame(Message = paste0(
            "\u2713 All ", format(n_total, big.mark = ","),
            " rows match \u2014 no mismatches found.")),
          caption = if (!is.null(comparison_note) || !is.null(offset_note))
            htmltools::tags$caption(
              style = "caption-side:top; text-align:left; padding:4px 0;",
              comparison_note, offset_note) else NULL,
          options  = list(initComplete = dt_blue_callback, dom = "t"),
          rownames = FALSE) %>%
            formatStyle("Message", color = "#155724", fontWeight = "600",
                        backgroundColor = "#d4edda"))
      }
      
      # ── Mismatch table ──────────────────────────────────────────
      check_df %>%
        datatable(
          caption = htmltools::tags$caption(
            style = "caption-side:top; text-align:left; padding:4px 0;",
            comparison_note, offset_note,
            htmltools::tags$div(
              style = "font-size:12px; color:#721c24; padding:2px 0;",
              htmltools::tags$span(style = "color:#e74c3c;", "\u2718 "),
              paste0(n_mismatch, " mismatch", if (n_mismatch != 1) "es" else "",
                     " out of ", format(n_total, big.mark = ","), " rows",
                     " \u2014 level: ", cs_label,
                     if (!identical(tc_cross_id, "Period")) " \u00d7 Period" else ""))),
          extensions = "Buttons",
          options    = list(scrollX = TRUE, pageLength = 25,
                            initComplete = dt_blue_callback, dom = "Bfrtip",
                            buttons = make_export_buttons("total_check", nm)),
          rownames = FALSE) %>%
        formatStyle("Status", backgroundColor = "#f8d7da")
    })
    
    reactive(reactiveValuesToList(results_store))
  })
}