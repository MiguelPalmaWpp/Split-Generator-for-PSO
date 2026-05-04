# ═══════════════════════════════════════════════════════════════════
# R/mod_setup.R
# ═══════════════════════════════════════════════════════════════════

mod_setup_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: File inputs ─────────────────────────────────────────
    card(
      class = "setup-files-card",
      fill  = FALSE,
      card_header("Data Files"),
      
      fileInput(
        ns("file_main"),
        tags$span("Main Data File ",
                  tags$small(".csv / .parquet / .zip / .gz",
                             class = "text-muted")),
        accept = c(".csv", ".parquet", ".zip", ".gz")
      ),
      fileInput(
        ns("file_analytical"),
        tags$span("AnalyticalDataset ",
                  tags$small(".RData / .csv / .xlsx", class = "text-muted")),
        accept = c(".RData", ".csv", ".xlsx", ".xls")
      ),
      fileInput(
        ns("file_details"),
        tags$span("ModelDetails ",
                  tags$small(".csv", class = "text-muted")),
        accept = ".csv"
      ),
      fileInput(
        ns("file_rois"),
        tags$span("ROIs by Channel ",
                  tags$small(".csv / .xlsx", class = "text-muted")),
        accept = c(".csv", ".xlsx")
      )
    ),
    
    # ── Right ─────────────────────────────────────────────────────
    div(
      class = "setup-right-col",
      
      # Row 1: Global Parameters (7) + Column Suffix Preview (5)
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          card_header("Global Parameters"),
          
          uiOutput(ns("update_label_ui")),
          
          div(
            style = "margin-bottom:6px;",
            tags$label(
              "Reporting Period",
              style = "font-size:13px; font-weight:500; color:#4a5568; display:block; margin-bottom:6px;"
            ),
            div(
              class = "ds-pill-group",
              radioButtons(
                ns("period_preset"), NULL,
                choices  = c("Last 52w"   = "last52",
                             "Last 13w"   = "last13",
                             "All Period" = "all",
                             "Custom"     = "custom"),
                selected = "last52",
                inline   = TRUE
              )
            )
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
      
      # Row 2: Data Overview
      navset_card_underline(
        id          = ns("overview_tabs"),
        full_screen = TRUE,
        
        nav_panel(
          title = tagList(icon("arrows-left-right"), " File Comparison"),
          value = "comparison",
          uiOutput(ns("file_comparison"))
        ),
        
        nav_panel(
          title = tagList(icon("table"), " Dimension Summary"),
          value = "dimensions",
          
          tags$p(
            class = "text-muted small mb-2",
            "Distinct values per dimension for each Variable Name — from uploaded data file."
          ),
          DTOutput(ns("dimension_table")),
          
          # ── Dimension Breaks ──────────────────────────────────────
          hr(style = "margin:16px 0 10px;"),
          div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
            div(
              tags$strong("Dimension Breaks",
                          style = "font-size:13px; color:#2c3e50; display:block;"),
              tags$small(
                "Broken dimensions become available as split options in Channels.",
                style = "color:#8a9bb0; font-size:11px;"
              )
            ),
            actionButton(
              ns("btn_add_break"),
              tagList(icon("scissors"), " Add Break"),
              class = "btn-outline-secondary btn-sm"
            )
          ),
          uiOutput(ns("breaks_list"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────
mod_setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── State ─────────────────────────────────────────────────────
    rv <- reactiveValues(
      main_data         = NULL,
      source_type       = NULL,
      analytical        = NULL,
      analytical_rag    = NULL,
      dates_df          = NULL,
      details           = NULL,
      channels_rois     = NULL,
      cross_cols        = NULL,
      validation_status = "pending",
      dimension_breaks  = list()     # global breaks config
    )
    
    # ── Helper: validate required columns ─────────────────────────
    validate_required_cols <- function(df, label) {
      missing <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing) > 0) {
        showNotification(
          paste0(label, " is missing required columns: ",
                 paste(missing, collapse = ", ")),
          type = "error", duration = 15
        )
        return(FALSE)
      }
      TRUE
    }
    
    # ── Helper: effective split choices considering breaks ─────────
    effective_split_choices <- function(breaks = rv$dimension_breaks) {
      choices <- SPLIT_CHOICES
      for (brk in breaks) {
        choices <- setdiff(choices, brk$column)
        choices <- c(choices, brk$names)
      }
      choices
    }
    
    # ── Load Main Data File ───────────────────────────────────────
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
        }, error = \(e) showNotification(e$message,
                                         type = "error", duration = 10))
      })
    })
    
    # ── Load AnalyticalDataset ────────────────────────────────────
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
                            data.table = FALSE, colClasses = "character",
                            showProgress = FALSE)
        }
        
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        
        if (tolower(ext) %in% c("csv", "tsv", "txt")) {
          id_cols <- c("Geography", "Product", "BP_Year", "Period")
          df <- df %>%
            mutate(across(-any_of(id_cols), ~ {
              converted <- suppressWarnings(as.numeric(.))
              non_na    <- !is.na(.)
              if (sum(non_na) == 0) return(.)
              if (sum(!is.na(converted) & non_na) / sum(non_na) >= 0.8) converted else .
            }))
        }
        
        if ("Period" %in% names(df))
          df <- df %>% mutate(Period = parse_period_robust(Period))
        
        rv$analytical     <- as_tibble(df) %>% ungroup()
        rv$dates_df       <- rv$analytical %>% distinct(Period) %>% arrange(Period)
        
        tryCatch({
          detected          <- auto_detect_cross_cols(rv$analytical)
          rv$cross_cols     <- detected
          rv$analytical_rag <- rv$analytical %>%
            distinct(across(any_of(c(detected, "Period"))))
        }, error = \(e) showNotification(
          paste("Cross-section detection failed:", e$message),
          type = "warning", duration = 10
        ))
        
        rm(df); gc()
        showNotification(
          paste0("AnalyticalDataset loaded — ",
                 format(nrow(rv$analytical), big.mark = ","), " rows"),
          type = "message"
        )
      }, error = \(e) showNotification(
        paste("AnalyticalDataset error:", e$message),
        type = "error", duration = 15
      ))
    })
    
    # ── Load ModelDetails ─────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      tryCatch({
        rv$details <- data.table::fread(
          input$file_details$datapath, data.table = FALSE, showProgress = FALSE
        ) %>%
          filter(!str_detect(str_to_lower(Type), "none")) %>%
          unite("Analytical_varname", VariableName, Campaign, Outlet, Creative,
                sep = "_", remove = FALSE) %>%
          mutate(Analytical_varname = str_replace_all(Analytical_varname, "_NA", ""))
        gc()
        showNotification("ModelDetails loaded.", type = "message")
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Load ROIs by Channel ──────────────────────────────────────
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
    
    # ── Auto-detect source type ───────────────────────────────────
    observe({
      if (!is.null(rv$main_data) && !is.null(rv$cross_cols))
        rv$source_type <- auto_detect_source_type(rv$main_data, rv$cross_cols)
      else
        rv$source_type <- NULL
    })
    
    # ── Period: custom date pickers ───────────────────────────────
    output$custom_dates_ui <- renderUI({
      req(input$period_preset == "custom")
      default_start <- if (!is.null(rv$dates_df)) {
        s <- sort(rv$dates_df$Period)
        if (length(s) >= 52) s[length(s) - 51] else s[1]
      } else Sys.Date() - 365
      default_end <- if (!is.null(rv$dates_df)) max(rv$dates_df$Period) else Sys.Date()
      div(
        style = "margin-top:8px;",
        layout_columns(
          col_widths = c(6, 6),
          div(dateInput(ns("start_report_date"), "Start Date", value = default_start)),
          div(dateInput(ns("end_report_date"),   "End Date",   value = default_end))
        )
      )
    })
    
    # ── Period: update label auto-fill ────────────────────────────
    output$update_label_ui <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all") return(NULL)
      val <- switch(preset,
                    last52 = "Last52w",
                    last13 = "Last13w",
                    custom = isolate(input$update_label %||% "Last52w"))
      textInput(ns("update_label"), "Update Label", value = val)
    })
    
    # ── Period dates reactive ─────────────────────────────────────
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
             last52 = list(start = if (n >= 52) s[n-51] else s[1], end = mx),
             last13 = list(start = if (n >= 13) s[n-12] else s[1], end = mx),
             all    = list(start = s[1], end = mx))
    })
    
    # ── Comparison reactive ───────────────────────────────────────
    comparison_result <- reactive({
      req(rv$main_data, rv$analytical, rv$cross_cols, rv$source_type)
      df_main <- rv$main_data; df_an <- rv$analytical
      cross_cols <- rv$cross_cols; source_type <- rv$source_type
      checks <- list()
      
      make_check <- function(label, n_an, n_main) {
        if (!is.numeric(n_an) || !is.numeric(n_main))
          return(list(label=label, n_an=n_an, n_main=n_main, status="na"))
        status <- if (n_an > n_main) "red" else if (n_main > n_an) "yellow" else "green"
        list(label=label, n_an=n_an, n_main=n_main, status=status)
      }
      
      checks$geography <- if (source_type == "all_rags" && "Geography" %in% cross_cols)
        make_check("Geography (Entities)", n_distinct(df_an$Geography), n_distinct(df_main$Geography))
      else list(label="Geography (Entities)", n_an="N/A", n_main="N/A", status="na")
      
      checks$product <- if (source_type == "all_rags" && "Product" %in% cross_cols)
        make_check("Products", n_distinct(df_an$Product), n_distinct(df_main$Product))
      else list(label="Products", n_an="N/A", n_main="N/A", status="na")
      
      an_min <- min(df_an$Period, na.rm=TRUE); an_max <- max(df_an$Period, na.rm=TRUE)
      mn_min <- min(df_main$Period, na.rm=TRUE); mn_max <- max(df_main$Period, na.rm=TRUE)
      time_status <- if (mn_min > an_min || mn_max < an_max) "red"
      else if (mn_min < an_min || mn_max > an_max) "yellow"
      else "green"
      checks$time_scope <- list(
        label      = "Time Scope",
        an_range   = paste0(format(an_min), " \u2192 ", format(an_max)),
        main_range = paste0(format(mn_min), " \u2192 ", format(mn_max)),
        status     = time_status
      )
      
      all_s <- sapply(checks, \(c) c$status)
      overall <- if (any(all_s == "red")) "red"
      else if (any(all_s == "yellow")) "yellow"
      else if (all(all_s %in% c("green","na"))) "green"
      else "pending"
      list(checks = checks, overall = overall)
    })
    
    observe({
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      rv$validation_status <- if (is.null(result)) "pending" else result$overall
    })
    
    # ── output: cross_section_info ────────────────────────────────
    output$cross_section_info <- renderUI({
      if (is.null(rv$cross_cols))
        return(tags$p(class="text-muted small mb-0",
                      "Cross-sections will be detected after uploading the AnalyticalDataset."))
      tagList(
        tags$strong("Cross-sections detected from AnalyticalDataset:",
                    style="font-size:12px; color:#2c3e50; display:block; margin-bottom:6px;"),
        div(style="display:flex; gap:6px; flex-wrap:wrap; margin-bottom:6px;",
            lapply(rv$cross_cols, function(col) {
              tags$span(col, style=paste0("background:#EBF3FB; color:#5B9BD5;",
                                          "padding:2px 10px; border-radius:10px;",
                                          "font-size:12px; font-weight:600;"))
            })),
        if (!is.null(rv$source_type))
          tags$p(style="font-size:12px; color:#6c757d; margin-bottom:0;",
                 "File type detected: ",
                 tags$strong(if (rv$source_type=="all_rags") "Geographic (all_RAGs)"
                             else "National (all_transformed)"))
      )
    })
    
    # ── output: suffix_preview ────────────────────────────────────
    output$suffix_preview <- renderUI({
      preset <- input$period_preset %||% "last52"
      if (preset == "all")
        return(div(class="text-muted small p-2",
                   "No column suffix applied — All Period selected."))
      lbl <- input$update_label %||% "Last52w"
      tagList(
        tags$p(class="text-muted small mb-2", "Column names based on Update Label:"),
        div(
          style="background:#f8f9fa; border-radius:6px; padding:12px; font-size:12px;",
          div(style="margin-bottom:10px;",
              tags$span("Non-Focus",
                        style="color:#6c757d; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
              tags$code(style="background:#e9ecef; padding:3px 6px; border-radius:4px;",
                        paste0("_Before_", lbl))),
          div(style="margin-bottom:10px;",
              tags$span("Focus",
                        style="color:#5B9BD5; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
              tags$code(style="background:#EBF3FB; color:#5B9BD5; padding:3px 6px; border-radius:4px;",
                        paste0("_", lbl))),
          hr(style="margin:8px 0;"),
          div(
            tags$span("Multi-break:",
                      style="color:#6c757d; font-weight:600; font-size:11px; display:block; margin-bottom:4px;"),
            tags$code(style="background:#e9ecef; padding:3px 6px; border-radius:4px; display:block; margin-bottom:4px;",
                      paste0("_Before_", lbl, "|FirstTimeBreak")),
            tags$code(style="background:#e9ecef; padding:3px 6px; border-radius:4px; display:block;",
                      paste0("_Before_", lbl, "|SecondTimeBreak"))
          )
        )
      )
    })
    
    # ── output: validation_alerts ─────────────────────────────────
    output$validation_alerts <- renderUI({
      if (is.null(rv$dates_df)) return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates)) return(NULL)
      start <- dates$start; end <- dates$end
      d <- rv$dates_df; min_d <- min(d$Period); max_d <- max(d$Period)
      alerts <- list()
      if (start <= min_d)
        alerts <- c(alerts, list(div(class="alert alert-warning p-2 mb-1", style="font-size:12px;",
                                     icon("triangle-exclamation"),
                                     " Start Date is at or before the model scope start.")))
      if (start > max_d)
        alerts <- c(alerts, list(div(class="alert alert-danger p-2 mb-1", style="font-size:12px;",
                                     icon("circle-xmark"), " Start Date is outside the date spine.")))
      if (end > max_d)
        alerts <- c(alerts, list(div(class="alert alert-warning p-2 mb-1", style="font-size:12px;",
                                     icon("triangle-exclamation"),
                                     paste0(" End Date (", end, ") exceeds last available period (", max_d, ")."))))
      if (end < start)
        alerts <- c(alerts, list(div(class="alert alert-danger p-2 mb-1", style="font-size:12px;",
                                     icon("circle-xmark"), " End Date is before Start Date.")))
      focus_n <- d %>% filter(between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0)
        alerts <- c(alerts, list(div(class="alert alert-warning p-2 mb-1", style="font-size:12px;",
                                     icon("triangle-exclamation"),
                                     paste0(" Focus period has only ", focus_n, " week(s). Minimum recommended: 4."))))
      if (!length(alerts))
        div(class="alert alert-success p-2", style="font-size:12px;",
            icon("circle-check"), " Date parameters look good.")
      else tagList(alerts)
    })
    
    # ── output: file_comparison ───────────────────────────────────
    output$file_comparison <- renderUI({
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload both the AnalyticalDataset and the main data file to see the comparison."
        else if (is.null(rv$analytical)) "Upload the AnalyticalDataset to complete the comparison."
        else "Upload the main data file to complete the comparison."
        return(div(class="text-center py-4", style="color:#adb5bd;",
                   icon("table-columns", style="font-size:32px; display:block; margin-bottom:10px; color:#dee2e6;"),
                   tags$p(msg)))
      }
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      if (is.null(result))
        return(div(class="text-muted small p-2", "Computing comparison..."))
      checks <- result$checks; overall <- result$overall
      
      banner_conf <- switch(overall,
                            green  = list(style="background:#e8f5e9; border:1px solid #a5d6a7; color:#155724;",
                                          icon=icon("circle-check", style="font-size:18px; color:#2ecc71;"),
                                          text="Files are consistent — you can proceed."),
                            yellow = list(style="background:#fff8e1; border:1px solid #ffe082; color:#856404;",
                                          icon=icon("triangle-exclamation", style="font-size:18px; color:#f39c12;"),
                                          text="Warning: data file has more entities than the Analytical."),
                            red    = list(style="background:#fdecea; border:1px solid #f5c6cb; color:#721c24;",
                                          icon=icon("circle-xmark", style="font-size:18px; color:#e74c3c;"),
                                          text="Critical: Analytical has more entities than the data file. Processing is blocked."),
                            list(style="background:#f8f9fa; border:1px solid #dee2e6; color:#6c757d;",
                                 icon=icon("circle-info", style="font-size:18px;"), text="Comparing files...")
      )
      
      banner <- div(
        style=paste0("display:flex; align-items:center; gap:10px;",
                     "padding:10px 16px; border-radius:8px; margin-bottom:14px; ",
                     banner_conf$style),
        banner_conf$icon,
        tags$strong(banner_conf$text, style="font-size:13px;")
      )
      
      mk_status_cell <- function(status, n_an=NULL, n_main=NULL) {
        conf <- switch(status,
                       green  = list(bg="#d4edda", col="#155724", text="Match"),
                       yellow = list(bg="#fff3cd", col="#856404",
                                     text=if (is.numeric(n_main)&&is.numeric(n_an))
                                       paste0("Data file has ", n_main-n_an, " more") else "Data file has more"),
                       red    = list(bg="#f8d7da", col="#721c24",
                                     text=if (is.numeric(n_main)&&is.numeric(n_an))
                                       paste0("Analytical has ", n_an-n_main, " more") else "Analytical has more"),
                       na     = list(bg="#f8f9fa", col="#adb5bd", text="N/A")
        )
        tags$td(conf$text, style=paste0("padding:8px 12px; font-size:12.5px; font-weight:600;",
                                        "color:",conf$col,"; background:",conf$bg,";"))
      }
      
      mk_count_row <- function(chk) {
        tags$tr(
          tags$td(chk$label, style="font-weight:500; padding:8px 12px; font-size:13px;"),
          tags$td(as.character(chk$n_an),   style="padding:8px 12px; font-size:13px;"),
          tags$td(as.character(chk$n_main), style="padding:8px 12px; font-size:13px;"),
          mk_status_cell(chk$status, chk$n_an, chk$n_main)
        )
      }
      mk_time_row <- function(chk) {
        tags$tr(
          tags$td(chk$label,      style="font-weight:500; padding:8px 12px; font-size:13px;"),
          tags$td(chk$an_range,   style="padding:8px 12px; font-size:13px; white-space:nowrap;"),
          tags$td(chk$main_range, style="padding:8px 12px; font-size:13px; white-space:nowrap;"),
          mk_status_cell(chk$status)
        )
      }
      
      tbl <- div(style="overflow-x:auto; margin-bottom:14px;",
                 tags$table(class="table table-sm", style="font-size:13px; margin-bottom:0;",
                            tags$thead(tags$tr(style="border-bottom:2px solid #5B9BD5;",
                                               tags$th("Dimension",  style="padding:8px 12px; color:#2c3e50; font-size:12px;"),
                                               tags$th("Analytical", style="padding:8px 12px; color:#2c3e50; font-size:12px;"),
                                               tags$th("Data File",  style="padding:8px 12px; color:#2c3e50; font-size:12px;"),
                                               tags$th("Status",     style="padding:8px 12px; color:#2c3e50; font-size:12px;")
                            )),
                            tags$tbody(mk_count_row(checks$geography), mk_count_row(checks$product), mk_time_row(checks$time_scope))
                 )
      )
      
      detection_strip <- div(
        style=paste0("background:#f4f6f9; border-radius:6px; padding:8px 14px;",
                     "font-size:12px; color:#6c757d; display:flex; gap:16px; flex-wrap:wrap;"),
        tags$span(tags$strong("Detected type: "),
                  if(rv$source_type=="all_rags") "Geographic (all_RAGs)" else "National (all_transformed)"),
        tags$span(tags$strong("Cross-sections: "), paste(rv$cross_cols, collapse=", "))
      )
      
      tagList(banner, tbl, detection_strip)
    })
    
    # ── output: dimension_table ───────────────────────────────────
    output$dimension_table <- DT::renderDT({
      if (is.null(rv$main_data)) {
        return(datatable(
          data.frame(Message = "Upload the main data file to see the dimension summary."),
          options = list(dom = "t", initComplete = dt_blue_callback),
          rownames = FALSE
        ))
      }
      summary_df <- rv$main_data %>%
        group_by(VariableName) %>%
        summarise(Campaign = n_distinct(Campaign),
                  Outlet   = n_distinct(Outlet),
                  Creative = n_distinct(Creative),
                  .groups  = "drop") %>%
        arrange(desc(Campaign), VariableName) %>%
        rename(`Variable Name` = VariableName)
      
      summary_df %>%
        datatable(
          options = list(
            scrollX      = TRUE,
            scrollY      = "300px",
            paging       = FALSE,
            dom          = "ft",
            initComplete = dt_blue_callback,
            columnDefs   = list(
              list(className = "dt-left",   targets = 0),
              list(className = "dt-center", targets = c(1, 2, 3))
            )
          ),
          rownames = FALSE
        ) %>%
        formatStyle("Variable Name", fontWeight = "600", color = "#2c3e50")
    }, server = FALSE)
    
    # ══════════════════════════════════════════════════════════════
    # DIMENSION BREAKS
    # ══════════════════════════════════════════════════════════════
    
    # ── output: breaks_list ───────────────────────────────────────
    output$breaks_list <- renderUI({
      breaks <- rv$dimension_breaks
      
      if (!length(breaks)) {
        return(tags$p(class = "text-muted small mb-0",
                      "No breaks configured. Click \"Add Break\" to create one."))
      }
      
      tagList(lapply(seq_along(breaks), function(i) {
        brk <- breaks[[i]]
        div(
          style = paste0(
            "display:flex; align-items:center; gap:8px;",
            "padding:7px 10px; border-radius:6px; margin-bottom:5px;",
            "background:#f4f6f9; border-left:3px solid #5B9BD5;"
          ),
          icon("scissors", style = "color:#5B9BD5; font-size:12px; flex-shrink:0;"),
          div(
            style = "flex:1; font-size:12.5px;",
            tags$strong(brk$column, style = "color:#2c3e50;"),
            tags$span(paste0(" split by \"", brk$separator, "\" into ",
                             brk$n_parts, " parts:"),
                      style = "color:#6c757d;"),
            tags$span(paste(brk$names, collapse = "  |  "),
                      style = "color:#5B9BD5; font-weight:600; margin-left:4px;")
          ),
          actionButton(
            ns(paste0("remove_break_", i)),
            icon("xmark"),
            class = "btn btn-link p-0",
            style = "color:#adb5bd; min-height:0; min-width:0; font-size:12px;"
          )
        )
      }))
    })
    
    # ── Dynamic observers for remove_break_i ─────────────────────
    observe({
      breaks <- rv$dimension_breaks
      if (!is.null(session$userData$remove_break_obs)) {
        lapply(session$userData$remove_break_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      }
      session$userData$remove_break_obs <- lapply(seq_along(breaks), function(i) {
        local({
          local_i <- i
          observeEvent(input[[paste0("remove_break_", local_i)]], {
            brk    <- rv$dimension_breaks[[local_i]]
            rv$dimension_breaks <- rv$dimension_breaks[-local_i]
            showNotification(
              paste0("Break on '", brk$column, "' removed."),
              type = "message"
            )
          }, ignoreInit = TRUE)
        })
      })
    })
    
    # ── Modal: Add Break ──────────────────────────────────────────
    observeEvent(input$btn_add_break, {
      if (is.null(rv$main_data)) {
        showNotification("Upload the main data file first.", type = "warning")
        return()
      }
      
      already_broken <- sapply(rv$dimension_breaks, \(b) b$column)
      available      <- setdiff(c("Campaign", "Outlet", "Creative"), already_broken)
      
      if (!length(available)) {
        showNotification("All breakable dimensions already have a break defined.",
                         type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = tagList(icon("scissors"), " Configure Dimension Break"),
        
        selectInput(ns("break_col"), "Column to break", choices = available),
        
        layout_columns(
          col_widths = c(8, 4),
          textInput(ns("break_sep"), "Separator", value = "_",
                    placeholder = "e.g. _"),
          div(numericInput(ns("break_n"), "Parts", value = 2,
                           min = 2, max = 5, step = 1))
        ),
        
        uiOutput(ns("break_preview_ui")),
        uiOutput(ns("break_names_ui")),
        
        footer = tagList(
          actionButton(ns("btn_confirm_break"),
                       tagList(icon("check"), " Add Break"),
                       class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = FALSE, size = "m"
      ))
    })
    
    # ── Break preview (inside modal) ──────────────────────────────
    output$break_preview_ui <- renderUI({
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% "_"
      n   <- as.integer(input$break_n %||% 2)
      md  <- rv$main_data
      
      if (is.null(md) || !col %in% names(md))
        return(tags$p(class = "text-muted small mt-2",
                      "Upload the data file to see a preview."))
      
      vals  <- head(sort(unique(as.character(md[[col]]))), 8)
      parts <- lapply(vals, function(v) strsplit(v, sep, fixed = TRUE)[[1]])
      
      n_short <- sum(sapply(parts, length) < n)
      
      rows <- lapply(seq_along(vals), function(i) {
        p       <- parts[[i]]
        is_warn <- length(p) < n
        cells <- c(
          list(tags$td(vals[i],
                       style = "font-size:11px; padding:3px 8px; color:#6c757d; border-right:1px solid #e3e8ef;")),
          lapply(seq_len(n), function(j) {
            val <- if (length(p) < j)  p[length(p)]
            else if (j == n)    paste(p[j:length(p)], collapse = sep)
            else                p[j]
            tags$td(val, style = paste0(
              "font-size:11px; padding:3px 8px;",
              if (is_warn) " color:#856404;" else " color:#2c3e50;"
            ))
          })
        )
        do.call(tags$tr, cells)
      })
      
      header <- c(
        list(tags$th("Original", style = "padding:3px 8px; font-size:11px; color:#6c757d;")),
        lapply(seq_len(n), function(j) {
          tags$th(paste0("Part ", j),
                  style = "padding:3px 8px; font-size:11px; color:#2c3e50;")
        })
      )
      
      tagList(
        tags$strong("Preview:", style = "font-size:12px; display:block; margin:10px 0 4px;"),
        div(
          style = "overflow-x:auto; border:1px solid #e3e8ef; border-radius:5px; margin-bottom:6px;",
          tags$table(
            class = "table table-sm", style = "margin-bottom:0;",
            tags$thead(tags$tr(style = "border-bottom:1px solid #5B9BD5;",
                               do.call(tagList, header))),
            tags$tbody(do.call(tagList, rows))
          )
        ),
        if (n_short > 0)
          div(class = "small", style = "color:#856404;",
              icon("triangle-exclamation"),
              paste0(" ", n_short, " value(s) have fewer parts than expected.",
                     " The last available part will be used as fallback."))
      )
    })
    
    # ── Break part names (inside modal) ───────────────────────────
    output$break_names_ui <- renderUI({
      col <- input$break_col %||% "Campaign"
      n   <- as.integer(input$break_n %||% 2)
      
      tagList(
        tags$strong("Name each part:",
                    style = "font-size:12px; display:block; margin:10px 0 6px;"),
        lapply(seq_len(n), function(i) {
          div(
            style = "display:flex; align-items:center; gap:8px; margin-bottom:6px;",
            tags$span(paste0("Part ", i, ":"),
                      style = paste0("font-size:12px; font-weight:600;",
                                     "width:55px; flex-shrink:0; color:#4a5568;")),
            textInput(ns(paste0("break_part_", i)), NULL,
                      value = paste0(col, "_", LETTERS[i]),
                      width = "100%") %>%
              tagAppendAttributes(style = "margin-bottom:0;")
          )
        })
      )
    })
    
    # ── Confirm break ─────────────────────────────────────────────
    observeEvent(input$btn_confirm_break, {
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% "_"
      n   <- as.integer(input$break_n %||% 2)
      
      part_names <- sapply(seq_len(n), function(i) {
        trimws(input[[paste0("break_part_", i)]] %||% paste0(col, "_", LETTERS[i]))
      })
      
      # Validations
      if (any(!nzchar(part_names))) {
        showNotification("All part names must be non-empty.", type = "warning")
        return()
      }
      if (length(unique(part_names)) < n) {
        showNotification("Part names must be unique.", type = "warning")
        return()
      }
      conflicts <- intersect(part_names, SPLIT_CHOICES)
      if (length(conflicts) > 0) {
        showNotification(
          paste0("Names conflict with existing split choices: ",
                 paste(conflicts, collapse = ", ")),
          type = "warning"
        )
        return()
      }
      
      rv$dimension_breaks <- c(rv$dimension_breaks, list(list(
        column    = col,
        separator = sep,
        n_parts   = n,
        names     = part_names
      )))
      
      removeModal()
      showNotification(
        paste0("Break added: ", col, " \u2192 ",
               paste(part_names, collapse = " | ")),
        type = "message"
      )
    })
    
    # ── Return ────────────────────────────────────────────────────
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
          update_label       = if (preset == "all") ""
          else (input$update_label %||% "Last52w"),
          start_report_date  = if (!is.null(dates)) dates$start else NULL,
          end_report_date    = if (!is.null(dates)) dates$end   else NULL,
          cross_cols         = rv$cross_cols,
          source_type        = rv$source_type,
          period_preset      = preset,
          dimension_breaks   = rv$dimension_breaks,        # ← breaks available globally
          effective_choices  = effective_split_choices()   # ← ready for Channels to use
        )
      }),
      
      validation_status = reactive(rv$validation_status)
    )
    
  })
}