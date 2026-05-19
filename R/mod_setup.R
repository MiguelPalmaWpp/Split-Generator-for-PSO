# ═══════════════════════════════════════════════════════════════════════
# R/mod_setup.R
# ═══════════════════════════════════════════════════════════════════════

mod_setup_ui <- function(id) {
  ns <- NS(id)
  
  mk_file_card <- function(num, title, formats, input_ui) {
    div(
      class = "file-card",
      div(
        class = "file-card-header",
        tags$span(num,     class = "file-card-num"),
        tags$span(title,   class = "file-card-title")
      ),
      tags$span(formats, class = "file-card-formats"),
      div(class = "mt-auto", input_ui)
    )
  }
  
  tagList(
    
    # ── Row 1: Data Files ──────────────────────────────────────────────
    card(
      class = "setup-files-card", fill = FALSE,
      card_header(div(
        class = "card-header-inner",
        icon("folder-open", class = "icon-blue-sm"),
        "Data Files"
      )),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        class = "mb-3",
        mk_file_card("1", "Main Data File", ".csv  .parquet  .zip  .gz",
                     fileInput(ns("file_main"), NULL,
                               accept = c(".csv", ".parquet", ".zip", ".gz"))),
        mk_file_card("2", "Analytical Dataset", ".RData  .csv  .xlsx",
                     fileInput(ns("file_analytical"), NULL,
                               accept = c(".RData", ".csv", ".xlsx", ".xls"))),
        mk_file_card("3", "VOF Metadata", ".csv  .xlsx",
                     fileInput(ns("file_vof"), NULL,
                               accept = c(".csv", ".xlsx", ".xls")))
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        mk_file_card("4", "ModelDetails", ".csv",
                     fileInput(ns("file_details"), NULL, accept = ".csv")),
        mk_file_card("5", "ROIs by Channel", ".csv  .xlsx",
                     fileInput(ns("file_rois"), NULL,
                               accept = c(".csv", ".xlsx"))),
        div()
      )
    ),
    
    # ── Row 2: Global Parameters + Column Suffix Preview ───────────────
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        card_header("Global Parameters"),
        uiOutput(ns("update_label_ui")),
        div(
          class = "mb-2",
          tags$label("Reporting Period", class = "form-label"),
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
      
      card(
        card_header("Column Suffix Preview"),
        uiOutput(ns("suffix_preview"))
      )
    ),
    
    # ── Row 3: File Validation ─────────────────────────────────────────
    card(
      class    = "setup-comparison-card",
      full_screen = TRUE,
      card_header("File Validation"),
      uiOutput(ns("file_comparison"))
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      main_data         = NULL,
      analytical        = NULL,
      analytical_rag    = NULL,
      dates_df          = NULL,
      details           = NULL,
      channels_rois     = NULL,
      vof_data          = NULL,
      cross_cols        = NULL,
      validation_status = "pending",
      media_index       = NULL
    )
    
    normalize_geo <- function(x)
      trimws(gsub("\\s+", " ", tolower(gsub("[,.]", " ", as.character(x)))))
    
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
    
    # ── Load Main Data File ────────────────────────────────────────────
    observeEvent(input$file_main, {
      req(input$file_main)
      ext <- tools::file_ext(input$file_main$name)
      withProgress(message = "Loading Main Data File...", {
        tryCatch({
          df <- read_main_data(input$file_main$datapath, ext)
          if (!validate_required_cols(df, "Main data file")) return()
          rv$main_data <- df
          gc()
          showNotification(
            paste0("Main Data File loaded — ",
                   format(nrow(df), big.mark = ","), " rows"),
            type = "message")
        }, error = \(e) showNotification(e$message, type = "error", duration = 10))
      })
    })
    
    # ── Load AnalyticalDataset ─────────────────────────────────────────
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      ext <- tools::file_ext(input$file_analytical$name)
      tryCatch({
        df <- if (tolower(ext) == "rdata") {
          e <- new.env(); load(input$file_analytical$datapath, envir = e)
          get(ls(e)[1], envir = e)
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
          read_excel(input$file_analytical$datapath)
        } else {
          data.table::fread(input$file_analytical$datapath,
                            data.table = FALSE, colClasses = "character",
                            showProgress = FALSE)
        }
        
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        
        if (tolower(ext) %in% c("csv", "tsv", "txt")) {
          id_cols <- c("Geography", "Product", "BP_Year", "Period")
          df <- df %>% mutate(across(-any_of(id_cols), ~ {
            converted <- suppressWarnings(as.numeric(.))
            non_na    <- !is.na(.)
            if (sum(non_na) == 0) return(.)
            if (sum(!is.na(converted) & non_na) / sum(non_na) >= 0.8)
              converted else .
          }))
        }
        
        if ("Period" %in% names(df))
          df <- df %>% mutate(Period = parse_period_robust(Period))
        
        rv$analytical <- as_tibble(df) %>% ungroup()
        rv$dates_df   <- rv$analytical %>% distinct(Period) %>% arrange(Period)
        
        tryCatch({
          detected      <- auto_detect_cross_cols(rv$analytical)
          rv$cross_cols <- detected
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
        paste("AnalyticalDataset error:", e$message),
        type = "error", duration = 15))
    })
    
    # ── Load VOF Metadata ──────────────────────────────────────────────
    observeEvent(input$file_vof, {
      req(input$file_vof)
      ext <- tools::file_ext(input$file_vof$name)
      tryCatch({
        vof_raw <- if (tolower(ext) %in% c("xlsx", "xls"))
          read_excel(input$file_vof$datapath)
        else
          data.table::fread(input$file_vof$datapath,
                            data.table = FALSE, stringsAsFactors = FALSE,
                            showProgress = FALSE)
        
        vof_raw  <- as.data.frame(vof_raw)
        req_vof  <- c("AnalyticalVariableName", "MainModelVariableName",
                      "MinPeriod", "MaxPeriod", "Geographies")
        miss_vof <- setdiff(req_vof, names(vof_raw))
        
        if (length(miss_vof) > 0) {
          showNotification(
            paste0("VOF missing required columns: ",
                   paste(miss_vof, collapse = ", ")),
            type = "error", duration = 15)
          return()
        }
        
        rv$vof_data <- vof_raw
        showNotification(
          paste0("VOF loaded — ", format(nrow(vof_raw), big.mark = ","),
                 " rows, ", n_distinct(vof_raw$MainModelVariableName),
                 " model variables"),
          type = "message")
      }, error = \(e) showNotification(
        paste("VOF error:", e$message), type = "error", duration = 10))
    })
    
    # ── Load ModelDetails ──────────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      tryCatch({
        rv$details <- data.table::fread(
          input$file_details$datapath, data.table = FALSE,
          showProgress = FALSE) %>%
          filter(!str_detect(str_to_lower(Type), "none"))
        gc()
        showNotification(
          paste0("ModelDetails loaded — ",
                 sum(str_detect(str_to_lower(rv$details$Type),
                                "\\b(in|fixed)\\b"), na.rm = TRUE),
                 " Type='IN'/'FIXED' variables"),
          type = "message")
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Load ROIs by Channel ───────────────────────────────────────────
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
        showNotification(
          paste0("ROIs loaded — ", nrow(rv$channels_rois), " rows"),
          type = "message")
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Build Media Variable Index ─────────────────────────────────────
    observe({
      req(rv$main_data, rv$analytical, rv$vof_data, rv$details, rv$cross_cols)
      tryCatch({
        mi <- build_media_index(
          main_data     = rv$main_data,
          analytical    = rv$analytical,
          vof_df        = rv$vof_data,
          model_details = rv$details,
          channels_rois = rv$channels_rois,
          cross_cols    = rv$cross_cols %||% "Geography"
        )
        rv$media_index <- mi
        showNotification(
          tagList(
            icon("circle-check", class = "icon-success-sm"),
            paste0(" Media Index built — ", mi$summary$total_channels,
                   " channels (", mi$summary$from_vof, " from VOF",
                   if (mi$summary$from_fallback > 0)
                     paste0(", ", mi$summary$from_fallback, " keyword fallback")
                   else "", ")")
          ),
          type = "message", duration = 5)
      }, error = function(e) {
        rv$media_index <- NULL
        showNotification(paste("Media Index error:", e$message),
                         type = "error", duration = 10)
      })
    })
    
    # ── Update label UI ────────────────────────────────────────────────
    output$update_label_ui <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all") return(NULL)
      val <- switch(preset,
                    last52 = "Last52w", last13 = "Last13w",
                    custom = isolate(input$update_label %||% "Last52w"))
      textInput(ns("update_label"), "Update Label", value = val)
    })
    
    # ── Custom date pickers ────────────────────────────────────────────
    output$custom_dates_ui <- renderUI({
      req(input$period_preset == "custom")
      default_start <- if (!is.null(rv$dates_df)) {
        s <- sort(rv$dates_df$Period)
        if (length(s) >= 52) s[length(s) - 51] else s[1]
      } else Sys.Date() - 365
      default_end <- if (!is.null(rv$dates_df))
        max(rv$dates_df$Period) else Sys.Date()
      div(class = "mt-2",
          layout_columns(col_widths = c(6, 6),
                         dateInput(ns("start_report_date"), "Start Date",
                                   value = default_start),
                         dateInput(ns("end_report_date"),   "End Date",
                                   value = default_end)))
    })
    
    # ── Period dates reactive ──────────────────────────────────────────
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
    
    # ── Comparison reactive ────────────────────────────────────────────
    comparison_result <- reactive({
      req(rv$main_data, rv$analytical, rv$cross_cols)
      
      df_main    <- rv$main_data
      df_an      <- rv$analytical
      cross_cols <- rv$cross_cols
      checks     <- list()
      
      make_check <- function(label, n_an, n_main) {
        if (!is.numeric(n_an) || !is.numeric(n_main))
          return(list(label = label, n_an = n_an, n_main = n_main, status = "na"))
        status <- if (n_an > n_main) "red" else if (n_main > n_an) "yellow" else "green"
        list(label = label, n_an = n_an, n_main = n_main, status = status)
      }
      
      checks$geography <- if ("Geography" %in% cross_cols)
        make_check("Geography", n_distinct(df_an$Geography),
                   n_distinct(df_main$Geography))
      else list(label = "Geography", n_an = "N/A", n_main = "N/A", status = "na")
      
      checks$product <- if ("Product" %in% cross_cols)
        make_check("Products", n_distinct(df_an$Product),
                   n_distinct(df_main$Product))
      else list(label = "Products", n_an = "N/A", n_main = "N/A", status = "na")
      
      an_min <- min(df_an$Period, na.rm = TRUE)
      an_max <- max(df_an$Period, na.rm = TRUE)
      mn_min <- min(df_main$Period, na.rm = TRUE)
      mn_max <- max(df_main$Period, na.rm = TRUE)
      time_st <- if (mn_min > an_min || mn_max < an_max) "red"
      else if (mn_min < an_min || mn_max > an_max) "yellow" else "green"
      checks$time_scope <- list(
        label      = "Time Scope",
        an_range   = paste0(format(an_min), " \u2192 ", format(an_max)),
        main_range = paste0(format(mn_min), " \u2192 ", format(mn_max)),
        status     = time_st)
      
      an_periods  <- sort(unique(df_an$Period))
      mn_periods  <- sort(unique(df_main$Period))
      an_min_p    <- min(an_periods); an_max_p <- max(an_periods)
      mn_in_range <- mn_periods[mn_periods >= an_min_p & mn_periods <= an_max_p]
      n_common    <- length(intersect(an_periods, mn_periods))
      n_an_total  <- length(an_periods)
      n_mn_extra  <- sum(mn_periods < an_min_p) + sum(mn_periods > an_max_p)
      
      max_offset <- if (length(mn_in_range) > 0) {
        sample_mn <- head(mn_in_range, min(20, length(mn_in_range)))
        max(vapply(sample_mn, function(p)
          min(abs(as.numeric(an_periods) - as.numeric(p))), numeric(1)),
          na.rm = TRUE)
      } else if (length(mn_periods) > 0) 999L else 0L
      
      period_align_st <- if (n_common >= n_an_total * 0.95 && max_offset == 0) "green"
      else if (max_offset <= 7) "yellow" else "red"
      
      checks$period_align <- list(
        n_common   = n_common, n_an = n_an_total, n_mn_extra = n_mn_extra,
        max_offset = max_offset,
        an_sample  = paste(format(head(an_periods, 3)), collapse = ", "),
        mn_sample  = if (length(mn_in_range) > 0)
          paste(format(head(mn_in_range, 3)), collapse = ", ")
        else paste(format(head(mn_periods, 3)), collapse = ", "),
        an_min = format(an_min_p), an_max = format(an_max_p),
        status = period_align_st)
      
      vv    <- df_main$VariableValue; na_vv <- sum(is.na(vv))
      checks$variable_value <- list(
        n_na = na_vv, n_total = length(vv),
        status = if (na_vv > 0) "red" else "green")
      
      check_dim_na <- function(col_name) {
        if (!col_name %in% names(df_main))
          return(list(label = col_name, n_na = 0, n_total = 0, status = "na"))
        vals <- df_main[[col_name]]; n_tot <- length(vals)
        n_na <- sum(is.na(vals) | (is.character(vals) & !nzchar(trimws(vals))),
                    na.rm = TRUE)
        list(label = col_name, n_na = n_na, n_total = n_tot,
             status = if (n_na > 0) "yellow" else "green")
      }
      checks$campaign <- check_dim_na("Campaign")
      checks$outlet   <- check_dim_na("Outlet")
      checks$creative <- check_dim_na("Creative")
      
      checks$geo_names <- if ("Geography" %in% names(df_an) &&
                              "Geography" %in% names(df_main)) {
        an_geos    <- unique(as.character(df_an$Geography))
        src_geos   <- unique(as.character(df_main$Geography))
        not_in_src <- setdiff(an_geos, src_geos)
        n_fuzzy    <- length(intersect(normalize_geo(an_geos), normalize_geo(src_geos)))
        n_total_g  <- length(an_geos)
        geo_st     <- if (length(not_in_src) == 0) "green"
        else if (n_fuzzy >= n_total_g * 0.95) "yellow" else "red"
        list(n_exact = length(intersect(an_geos, src_geos)),
             n_total = n_total_g, n_fuzzy = n_fuzzy,
             n_missing = length(not_in_src),
             sample_missing = head(not_in_src, 3), status = geo_st)
      } else {
        list(n_exact = NA, n_total = NA, n_fuzzy = NA,
             n_missing = 0, sample_missing = character(0), status = "na")
      }
      
      checks$media_index <- if (!is.null(rv$media_index)) {
        mi      <- rv$media_index
        vof_cov <- mi$summary$vof_coverage
        list(
          n_channels   = mi$summary$total_channels,
          from_vof     = mi$summary$from_vof,
          from_fb      = mi$summary$from_fallback,
          with_roi     = mi$summary$with_roi,
          vof_cov      = vof_cov,
          var_key_type = mi$summary$var_key_type,
          status       = if (mi$summary$total_channels == 0) "red" else "green"
        )
      } else {
        list(n_channels = 0,
             status = if (!is.null(rv$vof_data) && !is.null(rv$details))
               "pending" else "na")
      }
      
      all_s   <- sapply(checks, \(c) c$status)
      overall <- if (any(all_s == "red")) "red"
      else if (any(all_s == "yellow")) "yellow"
      else if (all(all_s %in% c("green", "na", "pending"))) "green"
      else "pending"
      
      list(checks = checks, overall = overall)
    })
    
    observe({
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      rv$validation_status <- if (is.null(result)) "pending" else result$overall
    })
    
    # ── Cross-section info ─────────────────────────────────────────────
    output$cross_section_info <- renderUI({
      if (is.null(rv$cross_cols))
        return(tags$p(class = "text-muted small mb-0",
                      "Cross-sections detected after uploading the AnalyticalDataset."))
      tagList(
        tags$strong("Cross-sections detected:", class = "section-strong"),
        div(class = "tag-group",
            lapply(rv$cross_cols, function(col)
              tags$span(col, class = "badge-blue")
            ))
      )
    })
    
    # ── Suffix preview ─────────────────────────────────────────────────
    output$suffix_preview <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all")
        return(div(class = "text-muted small p-2",
                   "No column suffix — All Period selected."))
      lbl <- input$update_label %||% "Last52w"
      tagList(
        tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
        div(class = "suffix-box",
            
            div(class = "mb-10",
                tags$span("Non-Focus", class = "preview-label"),
                tags$code(class = "code-tag", paste0("_Before ", lbl))),
            
            div(class = "mb-10",
                tags$span("Focus", class = "preview-label-focus"),
                tags$code(class = "code-tag-blue", paste0("_", lbl))),
            
            hr(class = "hr-sm"),
            
            div(
              tags$span("Time-break (auto-detected from VOF):", class = "preview-label"),
              tags$code(class = "code-tag d-block mb-1",
                        paste0("_Before ", lbl, "|FirstTimeBreak")),
              tags$code(class = "code-tag d-block",
                        paste0("_Before ", lbl, "|SecondTimeBreak")),
              tags$p(class = "hint-text",
                     icon("circle-info", class = "icon-xs"),
                     " Applied automatically when VOF has multiple entries for the same variable.")
            )
        )
      )
    })
    
    # ── Validation alerts ──────────────────────────────────────────────
    output$validation_alerts <- renderUI({
      if (is.null(rv$dates_df)) return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates)) return(NULL)
      start <- dates$start; end <- dates$end
      d     <- rv$dates_df; min_d <- min(d$Period); max_d <- max(d$Period)
      alerts <- list()
      
      if (start <= min_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        icon("triangle-exclamation"), " Start Date is at or before model scope start.")))
      if (start > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        icon("circle-xmark"), " Start Date is outside the date spine.")))
      if (end > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        icon("triangle-exclamation"),
        paste0(" End Date (", end, ") exceeds last available period (", max_d, ")."))))
      if (end < start) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        icon("circle-xmark"), " End Date is before Start Date.")))
      
      focus_n <- d %>% filter(between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        icon("triangle-exclamation"),
        paste0(" Focus period has only ", focus_n, " week(s). Minimum: 4."))))
      
      if (!length(alerts))
        div(class = "alert alert-success alert-sm p-2",
            icon("circle-check"), " Date parameters look good.")
      else tagList(alerts)
    })
    
    # ── File comparison ────────────────────────────────────────────────
    output$file_comparison <- renderUI({
      
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload Main Data File and Analytical Dataset to begin validation."
        else if (is.null(rv$analytical))
          "Upload the Analytical Dataset to complete validation."
        else
          "Upload the Main Data File to complete validation."
        return(div(class = "text-center py-4 text-muted",
                   icon("table-columns", class = "icon-empty"),
                   tags$p(msg)))
      }
      
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      if (is.null(result))
        return(div(class = "text-muted small p-2", "Computing validation..."))
      
      checks  <- result$checks
      overall <- result$overall
      
      # ── Banner ────────────────────────────────────────────────────────
      banner_conf <- switch(overall,
                            green = list(
                              cls  = "banner-validation banner-green",
                              icon = icon("circle-check",          class = "banner-icon-green"),
                              text = "All checks passed — ready to proceed to Channels."),
                            yellow = list(
                              cls  = "banner-validation banner-yellow",
                              icon = icon("triangle-exclamation",  class = "banner-icon-yellow"),
                              text = "Warnings found — review before processing."),
                            red = list(
                              cls  = "banner-validation banner-red",
                              icon = icon("circle-xmark",          class = "banner-icon-red"),
                              text = "Critical issues found — fix before processing."),
                            list(
                              cls  = "banner-validation banner-neutral",
                              icon = icon("circle-info"),
                              text = "Validating files..."))
      
      banner <- div(
        class = banner_conf$cls,
        banner_conf$icon,
        tags$strong(banner_conf$text, class = "banner-text"))
      
      # ── mk_badge ──────────────────────────────────────────────────────
      mk_badge <- function(status, text = NULL) {
        label <- switch(status,
                        green   = text %||% "OK",
                        yellow  = text %||% "Warning",
                        red     = text %||% "Blocked",
                        pending = text %||% "Pending",
                        "N/A")
        tags$td(label, class = paste("badge-td", paste0("badge-td-", status)))
      }
      
      # ── Section 1: File Alignment ──────────────────────────────────────
      section1 <- div(
        class = "section-block",
        div(class = "section-title-row",
            icon("arrows-left-right", class = "icon-blue-sm"),
            tags$strong("File Alignment"),
            tags$small("Analytical vs Main Data File", class = "section-subtitle")),
        div(class = "table-responsive",
            tags$table(
              class = "table table-sm mb-0",
              tags$thead(tags$tr(
                style = "border-bottom:2px solid #5B9BD5;",
                tags$th("Dimension", class = "th-val"),
                tags$th("Analytical", class = "th-val"),
                tags$th("Data File",  class = "th-val"),
                tags$th("Status",     class = "th-val"))),
              tags$tbody(
                
                local({ chk <- checks$geography
                txt <- switch(chk$status,
                              green  = "Match",
                              yellow = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                paste0("+", chk$n_main - chk$n_an, " extra") else "Data has more",
                              red    = if (is.numeric(chk$n_main) && is.numeric(chk$n_an))
                                paste0("-", chk$n_an - chk$n_main, " missing") else "Data has fewer",
                              "N/A")
                tags$tr(
                  tags$td("Geography",              class = "td-val-bold"),
                  tags$td(as.character(chk$n_an),   class = "td-val"),
                  tags$td(as.character(chk$n_main), class = "td-val"),
                  mk_badge(chk$status, txt)) }),
                
                local({ chk <- checks$product
                txt <- switch(chk$status,
                              green = "Match", yellow = "Data has more",
                              red   = "Data has fewer", "N/A")
                tags$tr(
                  tags$td("Product",                class = "td-val-bold"),
                  tags$td(as.character(chk$n_an),   class = "td-val"),
                  tags$td(as.character(chk$n_main), class = "td-val"),
                  mk_badge(chk$status, txt)) }),
                
                local({ chk <- checks$time_scope
                tags$tr(
                  tags$td("Time Scope",   class = "td-val-bold"),
                  tags$td(chk$an_range,   class = "td-val td-nowrap"),
                  tags$td(chk$main_range, class = "td-val td-nowrap"),
                  mk_badge(chk$status)) }),
                
                local({ chk <- checks$period_align
                data_cell <- if (chk$status == "green")
                  tags$td(tagList(
                    icon("circle-check", class = "icon-success-sm"),
                    paste0(" All ", format(chk$n_an, big.mark = ","), " periods found")),
                    class = "td-val text-success")
                else if (chk$status == "yellow")
                  tags$td(tagList(
                    icon("triangle-exclamation", class = "icon-warning-sm"),
                    tags$span(paste0(" \u00b1", chk$max_offset, "d offset"),
                              class = "fw-semibold text-warning")),
                    class = "td-val")
                else
                  tags$td(tagList(
                    icon("circle-xmark", class = "icon-danger-sm"),
                    tags$span(paste0(" Only ", chk$n_common, "/", chk$n_an, " found"),
                              class = "fw-bold text-danger")),
                    class = "td-val")
                detail <- if (chk$status == "green") {
                  extra <- if (chk$n_mn_extra > 0)
                    paste0(" (Data has ", chk$n_mn_extra, " extra historical — normal.)") else ""
                  paste0("Analytical: ", chk$an_sample, " | Data: ", chk$mn_sample, extra)
                } else if (chk$status == "yellow")
                  paste0("Analytical: ", chk$an_sample, " | Data: ", chk$mn_sample,
                         ". Small offset — Total Check will auto-align.")
                else
                  paste0("Analytical: ", chk$an_sample, " | Data: ", chk$mn_sample)
                tags$tr(
                  tags$td("Period Values", class = "td-val-bold"),
                  data_cell,
                  tags$td(detail, class = "td-val text-muted small"),
                  mk_badge(chk$status, switch(chk$status,
                                              green  = "Exact match",
                                              yellow = paste0("\u00b1", chk$max_offset, "d"),
                                              "Missing periods"))) }),
                
                local({ chk <- checks$geo_names
                if (identical(chk$status, "na")) return(NULL)
                data_cell <- if (chk$status == "green")
                  tags$td(tagList(
                    icon("circle-check", class = "icon-success-sm"),
                    paste0(" All ", chk$n_total, " names match")),
                    class = "td-val text-success")
                else if (chk$status == "yellow")
                  tags$td(tagList(
                    icon("triangle-exclamation", class = "icon-warning-sm"),
                    tags$span(paste0(" ", chk$n_missing, " differ"),
                              class = "fw-semibold text-warning")),
                    class = "td-val")
                else
                  tags$td(tagList(
                    icon("circle-xmark", class = "icon-danger-sm"),
                    tags$span(paste0(" ", chk$n_missing, " unmatched"),
                              class = "fw-bold text-danger")),
                    class = "td-val")
                detail <- if (chk$status == "green")
                  "Geography names identical — Total Check joins will work correctly."
                else if (chk$status == "yellow")
                  paste0(chk$n_fuzzy, "/", chk$n_total,
                         " match after normalization. Total Check will auto-correct.",
                         if (length(chk$sample_missing) > 0)
                           paste0(" Sample: ", paste(chk$sample_missing, collapse = " | ")) else "")
                else
                  paste0(chk$n_missing, " not found in Data File. ",
                         if (length(chk$sample_missing) > 0)
                           paste0("Sample: ", paste(chk$sample_missing, collapse = " | ")) else "")
                tags$tr(
                  tags$td("Geography Names", class = "td-val-bold"),
                  data_cell,
                  tags$td(detail, class = "td-val text-muted small"),
                  mk_badge(chk$status, switch(chk$status,
                                              green  = "Exact match",
                                              yellow = "Auto-corrected",
                                              "Mismatch"))) })
              )
            )
        )
      )
      
      # ── Section 2: Data Quality ────────────────────────────────────────
      vv_chk <- checks$variable_value
      
      vv_row <- tags$tr(
        tags$td("VariableValue", class = "td-val-bold"),
        tags$td(if (vv_chk$n_na == 0)
          tagList(icon("circle-check", class = "icon-success-sm"), " No NAs")
          else tagList(icon("circle-xmark", class = "icon-danger-sm"),
                       tags$span(paste0(" ", format(vv_chk$n_na, big.mark = ","), " NAs"),
                                 class = "fw-bold text-danger")),
          class = "td-val"),
        tags$td(if (vv_chk$n_na == 0)
          paste0("All ", format(vv_chk$n_total, big.mark = ","), " rows have a value")
          else paste0(round(vv_chk$n_na / max(vv_chk$n_total, 1) * 100, 1),
                      "% missing — splits will have 0 activity"),
          class = "td-val text-muted small"),
        mk_badge(vv_chk$status, if (vv_chk$n_na == 0) "OK" else "BLOCKED"))
      
      mk_dim_row <- function(chk) {
        tags$tr(
          tags$td(chk$label, class = "td-val-bold"),
          tags$td(if (chk$n_na == 0)
            tagList(icon("circle-check",          class = "icon-success-sm"),
                    " No empty values")
            else tagList(icon("triangle-exclamation", class = "icon-warning-sm"),
                         tags$span(paste0(" ", format(chk$n_na, big.mark = ","), " empty"),
                                   class = "fw-semibold text-warning")),
            class = "td-val"),
          tags$td(if (chk$n_na == 0)
            paste0("All ", format(chk$n_total, big.mark = ","), " rows filled")
            else paste0(round(chk$n_na / max(chk$n_total, 1) * 100, 1),
                        "% empty — may create unwanted splits"),
            class = "td-val text-muted small"),
          mk_badge(chk$status, if (chk$n_na == 0) "OK" else "Review"))
      }
      
      section2 <- div(
        class = "section-block",
        div(class = "section-title-row",
            icon("magnifying-glass", class = "icon-blue-sm"),
            tags$strong("Data Quality"),
            tags$small("Main Data File only", class = "section-subtitle")),
        div(class = "table-responsive",
            tags$table(
              class = "table table-sm mb-0",
              tags$thead(tags$tr(
                style = "border-bottom:2px solid #5B9BD5;",
                tags$th("Column", class = "th-val"),
                tags$th("Result", class = "th-val"),
                tags$th("Detail", class = "th-val"),
                tags$th("Status", class = "th-val"))),
              tags$tbody(
                vv_row,
                mk_dim_row(checks$campaign),
                mk_dim_row(checks$outlet),
                mk_dim_row(checks$creative))))
      )
      
      # ── Section 3: Media Variable Index ───────────────────────────────
      mi_chk <- checks$media_index
      
      section3 <- div(
        div(class = "section-title-row",
            icon("diagram-project", class = "icon-blue-sm"),
            tags$strong("Media Variable Index"),
            tags$small("VOF + ModelDetails + ROIs", class = "section-subtitle")),
        
        if (mi_chk$status == "na") {
          div(class = "mi-box-na",
              div(class = "mi-box-na-inner",
                  icon("clock", class = "icon-blue-sm"),
                  tags$span("Load VOF, ModelDetails and ROIs to build the Media Index.")))
          
        } else if (mi_chk$status == "pending") {
          div(class = "mi-box-pending",
              div(class = "mi-box-pending-inner",
                  icon("hourglass-half", class = "icon-blue-sm"),
                  tags$span("Building Media Index...")))
          
        } else {
          div(class = "mi-box",
              div(class = "mi-header",
                  div(class = "mi-header-left",
                      if (mi_chk$status == "red")
                        icon("circle-xmark",  class = "icon-danger-sm")
                      else
                        icon("circle-check",  class = "icon-success-sm"),
                      tags$strong(
                        paste0(mi_chk$n_channels, " channel",
                               if (mi_chk$n_channels != 1) "s" else "",
                               " auto-generated"),
                        class = "mi-title")),
                  tags$span(paste0("VOF coverage: ", mi_chk$vof_cov, "%"),
                            class = "mi-coverage")),
              div(class = "mi-stats",
                  div(tags$span(mi_chk$from_vof, class = "stat-number"),
                      tags$span("from VOF",      class = "stat-label")),
                  if (mi_chk$from_fb > 0)
                    div(tags$span(mi_chk$from_fb,    class = "stat-number"),
                        tags$span("keyword fallback", class = "stat-label")),
                  div(tags$span(mi_chk$with_roi, class = "stat-number"),
                      tags$span("with ROI",      class = "stat-label")),
                  div(tags$span(
                    if (mi_chk$var_key_type == "with_product")
                      "Geography \u00d7 Product" else "Geography",
                    class = "stat-type"),
                    tags$span("var_key type", class = "stat-label"))
              )
          )
        }
      )
      
      tagList(banner, section1, section2, section3)
    })
    
    # ── Return ─────────────────────────────────────────────────────────
    list(
      
      data = reactive(list(
        all_rags       = rv$main_data,
        analytical     = rv$analytical,
        analytical_rag = rv$analytical_rag,
        dates_df       = rv$dates_df,
        details        = rv$details,
        channels_rois  = rv$channels_rois,
        vof_data       = rv$vof_data
      )),
      
      config = reactive({
        preset <- input$period_preset %||% "last52"
        dates  <- tryCatch(period_dates(), error = \(e) NULL)
        list(
          update_label      = if (preset == "all") "" else (input$update_label %||% "Last52w"),
          start_report_date = if (!is.null(dates)) dates$start else NULL,
          end_report_date   = if (!is.null(dates)) dates$end   else NULL,
          cross_cols        = rv$cross_cols,
          period_preset     = preset
        )
      }),
      
      media_index       = reactive(rv$media_index),
      validation_status = reactive(rv$validation_status)
    )
  })
}