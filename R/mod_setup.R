# ════════════════════════════════════════════════════════════════
# R/mod_setup.R
# ════════════════════════════════════════════════════════════════

mod_setup_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    card(
      class = "setup-files-card", fill = FALSE,
      card_header("Data Files"),
      fileInput(ns("file_main"),
                tags$span("Main Data File ",
                          tags$small(".csv / .parquet / .zip / .gz", class = "text-muted")),
                accept = c(".csv", ".parquet", ".zip", ".gz")),
      fileInput(ns("file_analytical"),
                tags$span("AnalyticalDataset ",
                          tags$small(".RData / .csv / .xlsx", class = "text-muted")),
                accept = c(".RData", ".csv", ".xlsx", ".xls")),
      fileInput(ns("file_details"),
                tags$span("ModelDetails ", tags$small(".csv", class = "text-muted")),
                accept = ".csv"),
      fileInput(ns("file_rois"),
                tags$span("ROIs by Channel ",
                          tags$small(".csv / .xlsx", class = "text-muted")),
                accept = c(".csv", ".xlsx"))
    ),
    
    div(
      class = "setup-right-col",
      
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          card_header("Global Parameters"),
          uiOutput(ns("update_label_ui")),
          div(
            style = "margin-bottom:6px;",
            tags$label("Reporting Period",
                       style = paste0("font-size:13px; font-weight:500;",
                                      "color:#4a5568; display:block; margin-bottom:6px;")),
            div(class = "ds-pill-group",
                radioButtons(ns("period_preset"), NULL,
                             choices  = c("Last 52w" = "last52", "Last 13w" = "last13",
                                          "All Period" = "all", "Custom" = "custom"),
                             selected = "last52", inline = TRUE))
          ),
          uiOutput(ns("custom_dates_ui")),
          hr(),
          uiOutput(ns("cross_section_info")),
          hr(),
          uiOutput(ns("validation_alerts"))
        ),
        
        card(card_header("Column Suffix Preview"), uiOutput(ns("suffix_preview")))
      ),
      
      card(
        class = "setup-comparison-card", full_screen = TRUE,
        card_header("File Comparison — Analytical vs Data File"),
        uiOutput(ns("file_comparison"))
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────
mod_setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      main_data         = NULL,
      source_type       = NULL,
      analytical        = NULL,
      analytical_rag    = NULL,
      dates_df          = NULL,
      details           = NULL,
      channels_rois     = NULL,
      cross_cols        = NULL,
      validation_status = "pending"
    )
    
    # ── Shared geography normalizer ────────────────────────────────
    # Used in comparison_result and exposed for mod_process via data()
    normalize_geo <- function(x)
      trimws(gsub("\\s+", " ", tolower(gsub("[,.]", " ", as.character(x)))))
    
    # ── Validate required columns ─────────────────────────────────
    validate_required_cols <- function(df, label) {
      missing <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing) > 0) {
        showNotification(
          paste0(label, " is missing required columns: ",
                 paste(missing, collapse = ", ")),
          type = "error", duration = 15)
        return(FALSE)
      }
      TRUE
    }
    
    # ── Load Main Data File ────────────────────────────────────────
    observeEvent(input$file_main, {
      req(input$file_main)
      ext <- tools::file_ext(input$file_main$name)
      withProgress(message = "Loading data file...", {
        tryCatch({
          df <- read_all_transformed(input$file_main$datapath, ext)
          if (!validate_required_cols(df, "Main data file")) return()
          rv$main_data <- df
          gc()
          showNotification("Data file loaded.", type = "message")
        }, error = \(e) showNotification(e$message, type = "error", duration = 10))
      })
    })
    
    # ── Load AnalyticalDataset ─────────────────────────────────────
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      ext <- tools::file_ext(input$file_analytical$name)
      tryCatch({
        df <- if (tolower(ext) == "rdata") {
          e <- new.env()
          load(input$file_analytical$datapath, envir = e)
          get(ls(e)[1], envir = e)
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
          read_excel(input$file_analytical$datapath)
        } else {
          data.table::fread(input$file_analytical$datapath,
                            data.table = FALSE, colClasses = "character", showProgress = FALSE)
        }
        
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        
        if (tolower(ext) %in% c("csv", "tsv", "txt")) {
          id_cols <- c("Geography", "Product", "BP_Year", "Period")
          df <- df %>% mutate(across(-any_of(id_cols), ~ {
            converted <- suppressWarnings(as.numeric(.))
            non_na    <- !is.na(.)
            if (sum(non_na) == 0) return(.)
            if (sum(!is.na(converted) & non_na) / sum(non_na) >= 0.8) converted else .
          }))
        }
        
        if ("Period" %in% names(df))
          df <- df %>% mutate(Period = parse_period_robust(Period))
        
        rv$analytical <- as_tibble(df) %>% ungroup()
        rv$dates_df   <- rv$analytical %>% distinct(Period) %>% arrange(Period)
        
        tryCatch({
          detected          <- auto_detect_cross_cols(rv$analytical)
          rv$cross_cols     <- detected
          rv$analytical_rag <- rv$analytical %>%
            distinct(across(any_of(c(detected, "Period"))))
        }, error = \(e) showNotification(
          paste("Cross-section detection failed:", e$message),
          type = "warning", duration = 10))
        
        rm(df); gc()
        showNotification(
          paste0("AnalyticalDataset loaded — ",
                 format(nrow(rv$analytical), big.mark = ","), " rows"),
          type = "message")
      }, error = \(e) showNotification(
        paste("AnalyticalDataset error:", e$message), type = "error", duration = 15))
    })
    
    # ── Load ModelDetails ──────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      tryCatch({
        rv$details <- data.table::fread(input$file_details$datapath,
                                        data.table = FALSE, showProgress = FALSE) %>%
          filter(!str_detect(str_to_lower(Type), "none")) %>%
          unite("Analytical_varname", VariableName, Campaign, Outlet, Creative,
                sep = "_", remove = FALSE) %>%
          mutate(Analytical_varname = str_replace_all(Analytical_varname, "_NA", ""))
        gc()
        showNotification("ModelDetails loaded.", type = "message")
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Load ROIs by Channel ───────────────────────────────────────
    observeEvent(input$file_rois, {
      req(input$file_rois)
      ext <- tools::file_ext(input$file_rois$name)
      tryCatch({
        rv$channels_rois <- if (tolower(ext) %in% c("xlsx", "xls"))
          read_excel(input$file_rois$datapath)
        else
          data.table::fread(input$file_rois$datapath,
                            data.table = FALSE, showProgress = FALSE)
        gc()
        showNotification("ROIs loaded.", type = "message")
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Auto-detect source type ────────────────────────────────────
    observe({
      if (!is.null(rv$main_data) && !is.null(rv$cross_cols))
        rv$source_type <- auto_detect_source_type(rv$main_data, rv$cross_cols)
      else
        rv$source_type <- NULL
    })
    
    # ── Update label UI ────────────────────────────────────────────
    output$update_label_ui <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all") return(NULL)
      val <- switch(preset,
                    last52 = "Last52w", last13 = "Last13w",
                    custom = isolate(input$update_label %||% "Last52w"))
      textInput(ns("update_label"), "Update Label", value = val)
    })
    
    # ── Custom date pickers ────────────────────────────────────────
    output$custom_dates_ui <- renderUI({
      req(input$period_preset == "custom")
      default_start <- if (!is.null(rv$dates_df)) {
        s <- sort(rv$dates_df$Period)
        if (length(s) >= 52) s[length(s) - 51] else s[1]
      } else Sys.Date() - 365
      default_end <- if (!is.null(rv$dates_df)) max(rv$dates_df$Period) else Sys.Date()
      div(style = "margin-top:8px;",
          layout_columns(col_widths = c(6, 6),
                         div(dateInput(ns("start_report_date"), "Start Date", value = default_start)),
                         div(dateInput(ns("end_report_date"),   "End Date",   value = default_end))))
    })
    
    # ── Period dates reactive ──────────────────────────────────────
    period_dates <- reactive({
      preset <- input$period_preset %||% "last52"
      if (preset == "custom") {
        req(input$start_report_date, input$end_report_date)
        return(list(start = as.Date(input$start_report_date),
                    end   = as.Date(input$end_report_date)))
      }
      req(rv$dates_df)
      s <- sort(rv$dates_df$Period); n <- length(s); mx <- s[n]
      switch(preset,
             last52 = list(start = if (n >= 52) s[n - 51] else s[1], end = mx),
             last13 = list(start = if (n >= 13) s[n - 12] else s[1], end = mx),
             all    = list(start = s[1], end = mx))
    })
    
    # ── Comparison reactive ────────────────────────────────────────
    comparison_result <- reactive({
      req(rv$main_data, rv$analytical, rv$cross_cols, rv$source_type)
      
      df_main     <- rv$main_data
      df_an       <- rv$analytical
      cross_cols  <- rv$cross_cols
      source_type <- rv$source_type
      checks      <- list()
      
      make_check <- function(label, n_an, n_main) {
        if (!is.numeric(n_an) || !is.numeric(n_main))
          return(list(label = label, n_an = n_an, n_main = n_main, status = "na"))
        status <- if (n_an > n_main) "red" else if (n_main > n_an) "yellow" else "green"
        list(label = label, n_an = n_an, n_main = n_main, status = status)
      }
      
      # ── Geography ───────────────────────────────────────────────
      checks$geography <- if (source_type == "all_rags" && "Geography" %in% cross_cols)
        make_check("Geography", n_distinct(df_an$Geography), n_distinct(df_main$Geography))
      else
        list(label = "Geography", n_an = "N/A", n_main = "N/A", status = "na")
      
      # ── Products ─────────────────────────────────────────────────
      checks$product <- if (source_type == "all_rags" && "Product" %in% cross_cols)
        make_check("Products", n_distinct(df_an$Product), n_distinct(df_main$Product))
      else
        list(label = "Products", n_an = "N/A", n_main = "N/A", status = "na")
      
      # ── Time scope ───────────────────────────────────────────────
      an_min <- min(df_an$Period,   na.rm = TRUE)
      an_max <- max(df_an$Period,   na.rm = TRUE)
      mn_min <- min(df_main$Period, na.rm = TRUE)
      mn_max <- max(df_main$Period, na.rm = TRUE)
      time_st <- if (mn_min > an_min || mn_max < an_max) "red"
      else if (mn_min < an_min || mn_max > an_max) "yellow"
      else "green"
      checks$time_scope <- list(
        label      = "Time Scope",
        an_range   = paste0(format(an_min), " \u2192 ", format(an_max)),
        main_range = paste0(format(mn_min), " \u2192 ", format(mn_max)),
        status     = time_st)
      
      # ── Period alignment ──────────────────────────────────────────
      an_periods  <- sort(unique(df_an$Period))
      mn_periods  <- sort(unique(df_main$Period))
      an_min_p    <- min(an_periods)
      an_max_p    <- max(an_periods)
      mn_in_range <- mn_periods[mn_periods >= an_min_p & mn_periods <= an_max_p]
      n_common    <- length(intersect(an_periods, mn_periods))
      n_an_total  <- length(an_periods)
      n_mn_extra  <- sum(mn_periods < an_min_p) + sum(mn_periods > an_max_p)
      
      max_offset <- if (length(mn_in_range) > 0) {
        sample_mn <- head(mn_in_range, min(20, length(mn_in_range)))
        max(vapply(sample_mn, function(p)
          min(abs(as.numeric(an_periods) - as.numeric(p))),
          numeric(1)), na.rm = TRUE)
      } else if (length(mn_periods) > 0) 999L else 0L
      
      an_sample <- paste(format(head(an_periods, 3)), collapse = ", ")
      mn_sample <- if (length(mn_in_range) > 0)
        paste(format(head(mn_in_range, 3)), collapse = ", ")
      else
        paste(format(head(mn_periods,  3)), collapse = ", ")
      
      period_align_st <- if (n_common >= n_an_total * 0.95 && max_offset == 0) "green"
      else if (max_offset <= 7) "yellow"
      else "red"
      
      checks$period_align <- list(
        label      = "Period Values",
        n_common   = n_common,
        n_an       = n_an_total,
        n_mn_extra = n_mn_extra,
        max_offset = max_offset,
        an_sample  = an_sample,
        mn_sample  = mn_sample,
        an_min     = format(an_min_p),
        an_max     = format(an_max_p),
        status     = period_align_st)
      
      # ── Geography name consistency ────────────────────────────────
      # Detects format differences like "Alexandria, LA" vs "Alexandria LA"
      # that would cause Total Check join failures
      checks$geo_names <- if (source_type == "all_rags" &&
                              "Geography" %in% names(df_an) &&
                              "Geography" %in% names(df_main)) {
        an_geos    <- unique(as.character(df_an$Geography))
        src_geos   <- unique(as.character(df_main$Geography))
        not_in_src <- setdiff(an_geos, src_geos)
        n_fuzzy    <- length(intersect(normalize_geo(an_geos), normalize_geo(src_geos)))
        n_total    <- length(an_geos)
        geo_st     <- if (length(not_in_src) == 0)       "green"
        else if (n_fuzzy >= n_total * 0.95) "yellow"
        else                                "red"
        list(
          n_exact        = length(intersect(an_geos, src_geos)),
          n_total        = n_total,
          n_fuzzy        = n_fuzzy,
          n_missing      = length(not_in_src),
          sample_missing = head(not_in_src, 3),
          status         = geo_st)
      } else {
        list(n_exact = NA, n_total = NA, n_fuzzy = NA,
             n_missing = 0, sample_missing = character(0), status = "na")
      }
      
      # ── VariableValue completeness ────────────────────────────────
      vv    <- df_main$VariableValue
      na_vv <- sum(is.na(vv))
      checks$variable_value <- list(
        label   = "VariableValue",
        n_na    = na_vv,
        n_total = length(vv),
        status  = if (na_vv > 0) "red" else "green")
      
      # ── Dimension NA checks (main data only) ──────────────────────
      check_dim_na <- function(col_name) {
        if (!col_name %in% names(df_main))
          return(list(label = col_name, n_na = 0, n_total = 0, status = "na"))
        vals  <- df_main[[col_name]]
        n_tot <- length(vals)
        n_na  <- sum(is.na(vals) | (is.character(vals) & !nzchar(trimws(vals))),
                     na.rm = TRUE)
        list(label = col_name, n_na = n_na, n_total = n_tot,
             status = if (n_na > 0) "yellow" else "green")
      }
      checks$campaign <- check_dim_na("Campaign")
      checks$outlet   <- check_dim_na("Outlet")
      checks$creative <- check_dim_na("Creative")
      
      # ── Overall status ────────────────────────────────────────────
      all_s   <- sapply(checks, \(c) c$status)
      overall <- if (any(all_s == "red"))          "red"
      else if (any(all_s == "yellow"))  "yellow"
      else if (all(all_s %in% c("green", "na"))) "green"
      else "pending"
      
      list(checks = checks, overall = overall)
    })
    
    observe({
      result               <- tryCatch(comparison_result(), error = \(e) NULL)
      rv$validation_status <- if (is.null(result)) "pending" else result$overall
    })
    
    # ── output: cross_section_info ─────────────────────────────────
    output$cross_section_info <- renderUI({
      if (is.null(rv$cross_cols))
        return(tags$p(class = "text-muted small mb-0",
                      "Cross-sections will be detected after uploading the AnalyticalDataset."))
      tagList(
        tags$strong("Cross-sections detected from AnalyticalDataset:",
                    style = "font-size:12px; color:#2c3e50; display:block; margin-bottom:6px;"),
        div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:6px;",
            lapply(rv$cross_cols, function(col) {
              tags$span(col, style = paste0(
                "background:#EBF3FB; color:#5B9BD5;",
                "padding:2px 10px; border-radius:10px;",
                "font-size:12px; font-weight:600;"))
            })),
        if (!is.null(rv$source_type))
          tags$p(style = "font-size:12px; color:#6c757d; margin-bottom:0;",
                 "File type detected: ",
                 tags$strong(if (rv$source_type == "all_rags")
                   "Geographic (all_RAGs)" else "National (all_transformed)"))
      )
    })
    
    # ── output: suffix_preview ─────────────────────────────────────
    output$suffix_preview <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all")
        return(div(class = "text-muted small p-2",
                   "No column suffix applied — All Period selected."))
      lbl <- input$update_label %||% "Last52w"
      tagList(
        tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
        div(style = "background:#f8f9fa; border-radius:6px; padding:12px; font-size:12px;",
            div(style = "margin-bottom:10px;",
                tags$span("Non-Focus",
                          style = "color:#6c757d; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
                tags$code(style = "background:#e9ecef; padding:3px 6px; border-radius:4px;",
                          paste0("_Before ", lbl))),
            div(style = "margin-bottom:10px;",
                tags$span("Focus",
                          style = "color:#5B9BD5; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
                tags$code(style = "background:#EBF3FB; color:#5B9BD5; padding:3px 6px; border-radius:4px;",
                          paste0("_", lbl))),
            hr(style = "margin:8px 0;"),
            div(
              tags$span("Multi-break:",
                        style = "color:#6c757d; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
              tags$code(
                style = "background:#e9ecef; padding:3px 6px; border-radius:4px; display:block; margin-bottom:4px;",
                paste0("_Before ", lbl, "|FirstTimeBreak")),
              tags$code(
                style = "background:#e9ecef; padding:3px 6px; border-radius:4px; display:block;",
                paste0("_Before ", lbl, "|SecondTimeBreak")))
        )
      )
    })
    
    # ── output: validation_alerts ──────────────────────────────────
    output$validation_alerts <- renderUI({
      if (is.null(rv$dates_df)) return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates)) return(NULL)
      start  <- dates$start; end <- dates$end
      d      <- rv$dates_df
      min_d  <- min(d$Period); max_d <- max(d$Period)
      alerts <- list()
      
      if (start <= min_d)
        alerts <- c(alerts, list(div(
          class = "alert alert-warning p-2 mb-1", style = "font-size:12px;",
          icon("triangle-exclamation"),
          " Start Date is at or before the model scope start.")))
      if (start > max_d)
        alerts <- c(alerts, list(div(
          class = "alert alert-danger p-2 mb-1", style = "font-size:12px;",
          icon("circle-xmark"), " Start Date is outside the date spine.")))
      if (end > max_d)
        alerts <- c(alerts, list(div(
          class = "alert alert-warning p-2 mb-1", style = "font-size:12px;",
          icon("triangle-exclamation"),
          paste0(" End Date (", end, ") exceeds last available period (", max_d, ")."))))
      if (end < start)
        alerts <- c(alerts, list(div(
          class = "alert alert-danger p-2 mb-1", style = "font-size:12px;",
          icon("circle-xmark"), " End Date is before Start Date.")))
      
      focus_n <- d %>% filter(between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0)
        alerts <- c(alerts, list(div(
          class = "alert alert-warning p-2 mb-1", style = "font-size:12px;",
          icon("triangle-exclamation"),
          paste0(" Focus period has only ", focus_n, " week(s). Minimum recommended: 4."))))
      
      if (!length(alerts))
        div(class = "alert alert-success p-2", style = "font-size:12px;",
            icon("circle-check"), " Date parameters look good.")
      else
        tagList(alerts)
    })
    
    # ── output: file_comparison ────────────────────────────────────
    output$file_comparison <- renderUI({
      
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload both the AnalyticalDataset and the main data file."
        else if (is.null(rv$analytical))
          "Upload the AnalyticalDataset to complete the comparison."
        else
          "Upload the main data file to complete the comparison."
        return(div(class = "text-center py-4", style = "color:#adb5bd;",
                   icon("table-columns",
                        style = "font-size:32px; display:block; margin-bottom:10px; color:#dee2e6;"),
                   tags$p(msg)))
      }
      
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      if (is.null(result))
        return(div(class = "text-muted small p-2", "Computing comparison..."))
      
      checks  <- result$checks
      overall <- result$overall
      
      # ── Banner ───────────────────────────────────────────────────
      banner_conf <- switch(overall,
                            green = list(
                              style = "background:#e8f5e9; border:1px solid #a5d6a7; color:#155724;",
                              icon  = icon("circle-check", style = "font-size:18px; color:#2ecc71;"),
                              text  = "All checks passed — you can proceed to Channels."),
                            yellow = list(
                              style = "background:#fff8e1; border:1px solid #ffe082; color:#856404;",
                              icon  = icon("triangle-exclamation", style = "font-size:18px; color:#f39c12;"),
                              text  = "Warnings found — review before processing."),
                            red = list(
                              style = "background:#fdecea; border:1px solid #f5c6cb; color:#721c24;",
                              icon  = icon("circle-xmark", style = "font-size:18px; color:#e74c3c;"),
                              text  = "Critical issues found — processing is blocked."),
                            list(
                              style = "background:#f8f9fa; border:1px solid #dee2e6; color:#6c757d;",
                              icon  = icon("circle-info", style = "font-size:18px;"),
                              text  = "Comparing files..."))
      
      banner <- div(
        style = paste0("display:flex; align-items:center; gap:10px;",
                       "padding:10px 16px; border-radius:8px; margin-bottom:16px; ",
                       banner_conf$style),
        banner_conf$icon,
        tags$strong(banner_conf$text, style = "font-size:13px;"))
      
      # ── Shared styles ────────────────────────────────────────────
      th_style <- paste0("padding:7px 12px; color:#2c3e50;",
                         "font-size:11.5px; font-weight:600; background:#fafcff;")
      td_style <- "padding:8px 12px; font-size:13px;"
      
      mk_status_badge <- function(status, text = NULL) {
        conf <- switch(status,
                       green  = list(bg = "#d4edda", col = "#155724", text = text %||% "OK"),
                       yellow = list(bg = "#fff3cd", col = "#856404", text = text %||% "Warning"),
                       red    = list(bg = "#f8d7da", col = "#721c24", text = text %||% "Blocked"),
                       na     = list(bg = "#f8f9fa", col = "#adb5bd", text = "N/A"))
        tags$td(conf$text, style = paste0(
          "padding:8px 12px; font-size:12.5px; font-weight:600;",
          "color:", conf$col, "; background:", conf$bg, ";"))
      }
      
      # ════════════════════════════════════════════════════════════
      # SECTION 1: File Alignment
      # ════════════════════════════════════════════════════════════
      section1 <- div(
        style = "margin-bottom:20px;",
        div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
            icon("arrows-left-right", style = "color:#5B9BD5; font-size:13px;"),
            tags$strong("File Alignment", style = "font-size:13px; color:#2c3e50;"),
            tags$small("Analytical vs Data File", style = "color:#8a9bb0; font-size:11px;")),
        div(style = "overflow-x:auto;",
            tags$table(class = "table table-sm",
                       style = "font-size:13px; margin-bottom:0;",
                       tags$thead(tags$tr(style = "border-bottom:2px solid #5B9BD5;",
                                          tags$th("Dimension",  style = th_style),
                                          tags$th("Analytical", style = th_style),
                                          tags$th("Data File",  style = th_style),
                                          tags$th("Status",     style = th_style))),
                       tags$tbody(
                         
                         # Geography
                         local({
                           chk <- checks$geography
                           txt <- switch(chk$status,
                                         green  = "Match",
                                         yellow = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                           paste0("+", chk$n_main - chk$n_an, " extra in data") else "Data file has more",
                                         red    = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                           paste0("-", chk$n_an - chk$n_main, " missing") else "Data file has fewer",
                                         na = "N/A")
                           tags$tr(
                             tags$td("Geography", style = paste0("font-weight:500; ", td_style)),
                             tags$td(as.character(chk$n_an),   style = td_style),
                             tags$td(as.character(chk$n_main), style = td_style),
                             mk_status_badge(chk$status, txt))
                         }),
                         
                         # Product
                         local({
                           chk <- checks$product
                           txt <- switch(chk$status,
                                         green  = "Match",
                                         yellow = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                           paste0("+", chk$n_main - chk$n_an, " extra in data") else "Data file has more",
                                         red    = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                           paste0("-", chk$n_an - chk$n_main, " missing") else "Data file has fewer",
                                         na = "N/A")
                           tags$tr(
                             tags$td("Product", style = paste0("font-weight:500; ", td_style)),
                             tags$td(as.character(chk$n_an),   style = td_style),
                             tags$td(as.character(chk$n_main), style = td_style),
                             mk_status_badge(chk$status, txt))
                         }),
                         
                         # Time Scope
                         local({
                           chk <- checks$time_scope
                           tags$tr(
                             tags$td("Time Scope", style = paste0("font-weight:500; ", td_style)),
                             tags$td(chk$an_range,   style = paste0(td_style, "white-space:nowrap;")),
                             tags$td(chk$main_range, style = paste0(td_style, "white-space:nowrap;")),
                             mk_status_badge(chk$status))
                         }),
                         
                         # Period Values
                         local({
                           chk <- checks$period_align
                           data_cell <- if (chk$status == "green")
                             tags$td(tagList(
                               icon("circle-check", style = "color:#2ecc71; font-size:12px;"),
                               paste0(" All ", format(chk$n_an, big.mark = ","), " analytical periods found")),
                               style = paste0(td_style, "color:#155724;"))
                           else if (chk$status == "yellow")
                             tags$td(tagList(
                               icon("triangle-exclamation", style = "color:#f39c12; font-size:12px;"),
                               tags$span(paste0(" \u00b1", chk$max_offset, " day(s) offset"),
                                         style = "color:#856404; font-weight:600;")),
                               style = td_style)
                           else
                             tags$td(tagList(
                               icon("circle-xmark", style = "color:#e74c3c; font-size:12px;"),
                               tags$span(paste0(" Only ", format(chk$n_common, big.mark = ","),
                                                " / ", format(chk$n_an, big.mark = ","),
                                                " analytical periods found"),
                                         style = "color:#721c24; font-weight:700;")),
                               style = td_style)
                           detail_text <- if (chk$status == "green") {
                             extra_note <- if (chk$n_mn_extra > 0)
                               paste0(" (Data File has ", format(chk$n_mn_extra, big.mark = ","),
                                      " additional historical periods \u2014 normal.)") else ""
                             paste0("Analytical: ", chk$an_sample,
                                    " | Data File: ", chk$mn_sample, extra_note)
                           } else if (chk$status == "yellow") {
                             paste0(format(chk$n_common, big.mark = ","), " exact matches. ",
                                    "Analytical sample: ", chk$an_sample,
                                    " | Data File sample: ", chk$mn_sample,
                                    ". Small offset \u2014 Total Check will auto-align.")
                           } else {
                             paste0("Analytical range: ", chk$an_min, " \u2192 ", chk$an_max, ". ",
                                    "Analytical: ", chk$an_sample,
                                    " | Data File: ", chk$mn_sample,
                                    ". Data File does not cover analytical range. Verify files.")
                           }
                           tags$tr(
                             tags$td("Period Values", style = paste0("font-weight:500; ", td_style)),
                             data_cell,
                             tags$td(detail_text,
                                     style = paste0(td_style, "color:#6c757d; font-size:12px;")),
                             mk_status_badge(chk$status,
                                             switch(chk$status, green = "Exact match",
                                                    yellow = paste0("\u00b1", chk$max_offset, "d"),
                                                    red    = "Missing periods")))
                         }),
                         
                         # Geography Names — NEW
                         local({
                           chk <- checks$geo_names
                           if (identical(chk$status, "na")) return(NULL)
                           
                           data_cell <- if (chk$status == "green") {
                             tags$td(tagList(
                               icon("circle-check", style = "color:#2ecc71; font-size:12px;"),
                               paste0(" All ", chk$n_total, " names match exactly")),
                               style = paste0(td_style, "color:#155724;"))
                           } else if (chk$status == "yellow") {
                             tags$td(tagList(
                               icon("triangle-exclamation", style = "color:#f39c12; font-size:12px;"),
                               tags$span(paste0(" ", chk$n_missing, " name(s) differ in punctuation"),
                                         style = "color:#856404; font-weight:600;")),
                               style = td_style)
                           } else {
                             tags$td(tagList(
                               icon("circle-xmark", style = "color:#e74c3c; font-size:12px;"),
                               tags$span(paste0(" ", chk$n_missing, " unmatched"),
                                         style = "color:#721c24; font-weight:700;")),
                               style = td_style)
                           }
                           
                           detail_text <- if (chk$status == "green") {
                             "Geography names are identical in both files \u2014 Total Check joins will work correctly."
                           } else if (chk$status == "yellow") {
                             paste0(chk$n_fuzzy, " / ", chk$n_total,
                                    " match after normalization (punctuation differences). ",
                                    "Total Check will auto-correct. ",
                                    if (length(chk$sample_missing) > 0)
                                      paste0("Sample: ", paste(chk$sample_missing, collapse = " | "))
                                    else "")
                           } else {
                             paste0(chk$n_missing,
                                    " Analytical geographies not found in Data File. ",
                                    "These will show SplitsTotal = 0 in Total Check. ",
                                    if (length(chk$sample_missing) > 0)
                                      paste0("Sample: ", paste(chk$sample_missing, collapse = " | "))
                                    else "")
                           }
                           
                           tags$tr(
                             tags$td("Geography Names",
                                     style = paste0("font-weight:500; ", td_style)),
                             data_cell,
                             tags$td(detail_text,
                                     style = paste0(td_style, "color:#6c757d; font-size:12px;")),
                             mk_status_badge(chk$status,
                                             switch(chk$status,
                                                    green  = "Exact match",
                                                    yellow = "Auto-corrected",
                                                    red    = "Mismatch")))
                         })
                         
                       ) # end tbody
            ) # end table
        ) # end overflow div
      ) # end section1
      
      # ════════════════════════════════════════════════════════════
      # SECTION 2: Data Quality — Main Data File only
      # ════════════════════════════════════════════════════════════
      vv_chk <- checks$variable_value
      
      vv_row <- tags$tr(
        tags$td("VariableValue", style = paste0("font-weight:500; ", td_style)),
        tags$td(
          if (vv_chk$n_na == 0)
            tagList(icon("circle-check", style = "color:#2ecc71; font-size:12px;"), " No NAs")
          else
            tagList(icon("circle-xmark", style = "color:#e74c3c; font-size:12px;"),
                    tags$span(paste0(" ", format(vv_chk$n_na, big.mark = ","), " NAs"),
                              style = "color:#721c24; font-weight:700;")),
          style = td_style),
        tags$td(
          if (vv_chk$n_na == 0)
            paste0("All ", format(vv_chk$n_total, big.mark = ","), " rows have a value")
          else
            paste0(round(vv_chk$n_na / max(vv_chk$n_total, 1) * 100, 1),
                   "% missing \u2014 splits will have 0 activity"),
          style = paste0(td_style, "color:#6c757d; font-size:12.5px;")),
        mk_status_badge(vv_chk$status, if (vv_chk$n_na == 0) "OK" else "BLOCKED"))
      
      mk_dim_row <- function(chk) {
        tags$tr(
          tags$td(chk$label, style = paste0("font-weight:500; ", td_style)),
          tags$td(
            if (chk$n_na == 0)
              tagList(icon("circle-check", style = "color:#2ecc71; font-size:12px;"),
                      " No empty values")
            else
              tagList(icon("triangle-exclamation", style = "color:#f39c12; font-size:12px;"),
                      tags$span(paste0(" ", format(chk$n_na, big.mark = ","), " empty values"),
                                style = "color:#856404; font-weight:600;")),
            style = td_style),
          tags$td(
            if (chk$n_na == 0)
              paste0("All ", format(chk$n_total, big.mark = ","), " rows filled")
            else
              paste0(round(chk$n_na / max(chk$n_total, 1) * 100, 1),
                     "% empty \u2014 may create unwanted splits"),
            style = paste0(td_style, "color:#6c757d; font-size:12.5px;")),
          mk_status_badge(chk$status, if (chk$n_na == 0) "OK" else "Review"))
      }
      
      section2 <- div(
        div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
            icon("magnifying-glass", style = "color:#5B9BD5; font-size:13px;"),
            tags$strong("Data Quality", style = "font-size:13px; color:#2c3e50;"),
            tags$small("Main Data File only", style = "color:#8a9bb0; font-size:11px;")),
        div(style = "overflow-x:auto;",
            tags$table(class = "table table-sm",
                       style = "font-size:13px; margin-bottom:0;",
                       tags$thead(tags$tr(style = "border-bottom:2px solid #5B9BD5;",
                                          tags$th("Column", style = th_style),
                                          tags$th("Result", style = th_style),
                                          tags$th("Detail", style = th_style),
                                          tags$th("Status", style = th_style))),
                       tags$tbody(
                         vv_row,
                         mk_dim_row(checks$campaign),
                         mk_dim_row(checks$outlet),
                         mk_dim_row(checks$creative))))
      )
      
      tagList(banner, section1, section2)
    })
    
    # ── Return ─────────────────────────────────────────────────────
    list(
      data = reactive(list(
        all_transformed = if (identical(rv$source_type, "all_transformed"))
          rv$main_data else NULL,
        all_rags        = if (identical(rv$source_type, "all_rags"))
          rv$main_data else NULL,
        analytical      = rv$analytical,
        analytical_rag  = rv$analytical_rag,
        dates_df        = rv$dates_df,
        details         = rv$details,
        channels_rois   = rv$channels_rois
      )),
      
      config = reactive({
        preset <- input$period_preset %||% "last52"
        dates  <- tryCatch(period_dates(), error = \(e) NULL)
        list(
          update_label      = if (preset == "all") ""
          else (input$update_label %||% "Last52w"),
          start_report_date = if (!is.null(dates)) dates$start else NULL,
          end_report_date   = if (!is.null(dates)) dates$end   else NULL,
          cross_cols        = rv$cross_cols,
          source_type       = rv$source_type,
          period_preset     = preset)
      }),
      
      validation_status = reactive(rv$validation_status)
    )
    
  })
}