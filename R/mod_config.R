# ── Config Module ─────────────────────────────────────────────

mod_config_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: Parameters ──────────────────────────────────────
    card(
      card_header(" Global Parameters"),
      textInput(ns("update_label"),      "Update Label",      value = "Last52w"),
      dateInput(ns("start_report_date"), "Start Report Date", value = "2024-09-02"),
      dateInput(ns("end_report_date"),   "End Report Date",   value = "2025-08-25"),
      hr(),
      uiOutput(ns("date_info")),
      hr(),
      
      # ── Cross-sectional structure ────────────────────────
      tags$strong(" Cross-Sectional Structure",
                  style = "font-size:13px; color:#333;"),
      tags$p(class = "text-muted small mb-2",
             "Dimensions that define each observation in your model"),
      radioButtons(
        ns("n_cross"), NULL,
        choices  = c("1 cross-section" = 1, "2 cross-sections" = 2),
        selected = 1,
        inline   = TRUE
      ),
      uiOutput(ns("cross_cols_ui")),
      hr(),
      uiOutput(ns("validation_alerts"))
    ),
    
    # ── Right: KPIs + timeline + suffix + date spine ──────────
    tagList(
      uiOutput(ns("kpi_cards")),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Period Timeline"),        uiOutput(ns("timeline"))),
        card(card_header("Column Suffix Preview"), uiOutput(ns("suffix_preview")))
      ),
      card(
        card_header(" Date Spine — with type classification"),
        DTOutput(ns("date_table"))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────

mod_config_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns    #  required for renderUI id namespacing
    
    # ── Available columns from analytical ────────────────────
    avail_cols <- reactive({
      a <- data()$analytical
      if (is.null(a)) return(c("Geography", "Product"))
      cols <- names(a)[!sapply(a, is.numeric) & names(a) != "Period"]
      if (!length(cols)) c("Geography", "Product") else cols
    })
    
    # ── Cross-section selector UI ────────────────────────────
    output$cross_cols_ui <- renderUI({
      cols <- avail_cols()
      n    <- as.integer(input$n_cross %||% 1)
      
      if (n == 1) {
        selectizeInput(
          ns("cross_col_1"),
          "Cross-section column",
          choices  = cols,
          selected = if ("Geography" %in% cols) "Geography" else cols[1]
        )
      } else {
        tagList(
          selectizeInput(
            ns("cross_col_1"),
            "Cross-section 1",
            choices  = cols,
            selected = if ("Geography" %in% cols) "Geography" else cols[1]
          ),
          selectizeInput(
            ns("cross_col_2"),
            "Cross-section 2",
            choices  = cols,
            selected = if ("Product" %in% cols) "Product" else cols[2]
          )
        )
      }
    })
    
    # ── Date helpers ──────────────────────────────────────────
    dates_df <- reactive(data()$dates_df)
    
    period_stats <- reactive({
      req(dates_df())
      d     <- dates_df()
      start <- as.Date(input$start_report_date)
      end   <- as.Date(input$end_report_date)
      
      total   <- nrow(d)
      nf      <- d %>% filter(Period < start) %>% nrow()
      focus   <- d %>% filter(between(Period, start, end)) %>% nrow()
      outside <- total - nf - focus
      
      list(
        total       = total,
        nonfocus    = nf,
        focus       = focus,
        outside     = outside,
        pct_nf      = round(nf      / max(total, 1) * 100, 1),
        pct_focus   = round(focus   / max(total, 1) * 100, 1),
        pct_outside = round(outside / max(total, 1) * 100, 1),
        model_start = min(d$Period),
        model_end   = max(d$Period)
      )
    })
    
    # ── Date info ─────────────────────────────────────────────
    output$date_info <- renderUI({
      req(dates_df())
      d <- dates_df()
      tags$p(class = "text-info small", icon("calendar"),
             sprintf(" %d periods  |  %s → %s",
                     nrow(d),
                     format(min(d$Period)),
                     format(max(d$Period))))
    })
    
    # ── Validation alerts ─────────────────────────────────────
    output$validation_alerts <- renderUI({
      req(dates_df())
      d     <- dates_df()
      start <- as.Date(input$start_report_date)
      end   <- as.Date(input$end_report_date)
      min_d <- min(d$Period)
      max_d <- max(d$Period)
      
      alerts <- list()
      
      if (start <= min_d)
        alerts <- c(alerts, list(
          div(class = "alert alert-warning p-2 mb-1",
              style = "font-size:12px;",
              icon("triangle-exclamation"),
              " Start Date is at or before the model scope start.")
        ))
      
      if (start > max_d)
        alerts <- c(alerts, list(
          div(class = "alert alert-danger p-2 mb-1",
              style = "font-size:12px;",
              icon("circle-xmark"),
              " Start Date is outside the date spine.")
        ))
      
      if (end > max_d)
        alerts <- c(alerts, list(
          div(class = "alert alert-warning p-2 mb-1",
              style = "font-size:12px;",
              icon("triangle-exclamation"),
              paste0(" End Date (", end,
                     ") exceeds last available period (", max_d, ")."))
        ))
      
      if (end < start)
        alerts <- c(alerts, list(
          div(class = "alert alert-danger p-2 mb-1",
              style = "font-size:12px;",
              icon("circle-xmark"),
              " End Date is before Start Date.")
        ))
      
      focus_n <- d %>% filter(between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0)
        alerts <- c(alerts, list(
          div(class = "alert alert-warning p-2 mb-1",
              style = "font-size:12px;",
              icon("triangle-exclamation"),
              paste0(" Focus period has only ", focus_n,
                     " week(s). Minimum recommended: 4."))
        ))
      
      if (!length(alerts))
        div(class = "alert alert-success p-2",
            style = "font-size:12px;",
            icon("circle-check"),
            " Date parameters look good.")
      else
        tagList(alerts)
    })
    
    # ── KPI cards ─────────────────────────────────────────────
    output$kpi_cards <- renderUI({
      if (is.null(dates_df())) return(NULL)
      ps <- period_stats()
      
      kpis <- list(
        list(icon  = "clock",
             label = "Total Weeks",
             value = ps$total,
             color = "#5B9BD5", bg = "#EBF3FB"),
        list(icon  = "circle",
             label = "Non-Focus Weeks",
             value = paste0(ps$nonfocus, "  (", ps$pct_nf, "%)"),
             color = "#6c757d", bg = "#f8f9fa"),
        list(icon  = "circle-dot",
             label = "Focus Weeks",
             value = paste0(ps$focus, "  (", ps$pct_focus, "%)"),
             color = "#5B9BD5", bg = "#EBF3FB"),
        list(icon  = "calendar-day",
             label = "Model Start",
             value = format(ps$model_start),
             color = "#5B9BD5", bg = "#EBF3FB")
      )
      
      do.call(layout_columns,
              c(list(col_widths = c(3, 3, 3, 3),
                     style      = "margin-bottom:12px;"),
                lapply(kpis, function(k) {
                  div(
                    style = paste0(
                      "background:", k$bg, ";",
                      "border-left:3px solid ", k$color, ";",
                      "border-radius:8px; padding:14px 10px; text-align:center;"
                    ),
                    icon(k$icon,
                         style = paste0("color:", k$color, "; font-size:18px;")),
                    tags$div(
                      tags$strong(k$value,
                                  style = "font-size:15px; color:#2c3e50;
                                         display:block; margin-top:6px;"),
                      tags$small(k$label,
                                 style = "color:#6c757d; font-size:11px;")
                    )
                  )
                })))
    })
    
    # ── Visual timeline ───────────────────────────────────────
    output$timeline <- renderUI({
      req(dates_df())
      ps    <- period_stats()
      start <- as.Date(input$start_report_date)
      
      tagList(
        # Bar
        div(
          style = paste0(
            "width:100%; display:flex; border-radius:6px;",
            "overflow:hidden; height:38px; margin-bottom:10px;"
          ),
          if (ps$pct_nf > 0)
            div(
              style = paste0(
                "width:", ps$pct_nf, "%; background:#adb5bd;",
                "display:flex; align-items:center; justify-content:center;",
                "color:white; font-size:11px; font-weight:600;"
              ),
              if (ps$pct_nf > 10) paste0("Non-Focus (", ps$nonfocus, "w)")
            ),
          if (ps$pct_focus > 0)
            div(
              style = paste0(
                "width:", ps$pct_focus, "%; background:#5B9BD5;",
                "display:flex; align-items:center; justify-content:center;",
                "color:white; font-size:11px; font-weight:600;"
              ),
              if (ps$pct_focus > 8) paste0("Focus (", ps$focus, "w)")
            ),
          if (ps$pct_outside > 0)
            div(
              style = paste0(
                "width:", ps$pct_outside, "%; background:#dee2e6;",
                "display:flex; align-items:center; justify-content:center;",
                "color:#6c757d; font-size:11px;"
              ),
              if (ps$pct_outside > 8) paste0("Outside (", ps$outside, "w)")
            )
        ),
        
        # Date markers
        div(
          style = paste0(
            "display:flex; justify-content:space-between;",
            "font-size:11px; color:#6c757d; margin-top:4px;"
          ),
          tags$span(format(ps$model_start)),
          tags$span(icon("flag", style = "color:#5B9BD5; margin-right:3px;"),
                    format(start)),
          tags$span(format(ps$model_end))
        ),
        
        # Legend
        div(
          style = "display:flex; gap:16px; margin-top:12px; font-size:12px;",
          div(
            div(style = paste0("width:12px; height:12px; background:#adb5bd;",
                               "border-radius:2px; display:inline-block; margin-right:4px;")),
            tags$span("Non-Focus", style = "color:#6c757d;")
          ),
          div(
            div(style = paste0("width:12px; height:12px; background:#5B9BD5;",
                               "border-radius:2px; display:inline-block; margin-right:4px;")),
            tags$span("Focus", style = "color:#5B9BD5;")
          ),
          if (ps$outside > 0)
            div(
              div(style = paste0("width:12px; height:12px; background:#dee2e6;",
                                 "border-radius:2px; display:inline-block; margin-right:4px;")),
              tags$span("Outside Scope", style = "color:#adb5bd;")
            )
        )
      )
    })
    
    # ── Suffix preview ────────────────────────────────────────
    output$suffix_preview <- renderUI({
      lbl <- input$update_label %||% "Last52w"
      
      tagList(
        tags$p(class = "text-muted small mb-2",
               "Column names based on Update Label:"),
        div(
          style = "background:#f8f9fa; border-radius:6px;
                 padding:12px; font-size:12px;",
          
          div(
            style = "margin-bottom:10px;",
            tags$span("⚪ Non-Focus",
                      style = "color:#6c757d; font-weight:600; font-size:11px;
                             display:block; margin-bottom:4px;"),
            tags$code(
              style = "background:#e9ecef; padding:3px 6px; border-radius:4px;",
              paste0("_Before_", lbl)
            )
          ),
          
          div(
            style = "margin-bottom:10px;",
            tags$span(" Focus",
                      style = "color:#5B9BD5; font-weight:600; font-size:11px;
                             display:block; margin-bottom:4px;"),
            tags$code(
              style = "background:#EBF3FB; padding:3px 6px;
                     border-radius:4px; color:#5B9BD5;",
              paste0("_", lbl)
            )
          ),
          
          hr(style = "margin:8px 0;"),
          
          div(
            tags$span("Multi-break:",
                      style = "color:#6c757d; font-weight:600; font-size:11px;
                             display:block; margin-bottom:4px;"),
            tags$code(
              style = "background:#e9ecef; padding:3px 6px; border-radius:4px;
                     display:block; margin-bottom:4px;",
              paste0("_Before_", lbl, "|FirstTimeBreak")
            ),
            tags$code(
              style = "background:#e9ecef; padding:3px 6px; border-radius:4px;
                     display:block;",
              paste0("_Before_", lbl, "|SecondTimeBreak")
            )
          )
        )
      )
    })
    
    # ── Enriched date spine ───────────────────────────────────
    output$date_table <- renderDT({
      req(dates_df())
      start <- as.Date(input$start_report_date)
      end   <- as.Date(input$end_report_date)
      lbl   <- input$update_label %||% "Last52w"
      
      dates_df() %>%
        mutate(
          Type = case_when(
            between(Period, start, end) ~ "Focus",
            Period < start             ~ "Non-Focus",
            TRUE                       ~ "Outside Scope"
          ),
          Suffix = case_when(
            between(Period, start, end) ~ paste0("_", lbl),
            Period < start             ~ paste0("_Before_", lbl),
            TRUE                       ~ "—"
          )
        ) %>%
        datatable(
          options  = list(
            scrollY      = "320px",
            paging       = FALSE,
            scrollX      = TRUE,
            dom          = "ft",
            initComplete = dt_blue_callback
          ),
          rownames = FALSE
        ) %>%
        formatStyle(
          "Type",
          backgroundColor = styleEqual(
            c(" Focus", "Non-Focus", " Outside Scope"),
            c("#EBF3FB",   "#f8f9fa",     "#fff3cd")
          ),
          color = styleEqual(
            c(" Focus", "Non-Focus", "Outside Scope"),
            c("#5B9BD5",   "#6c757d",     "#856404")
          ),
          fontWeight = styleEqual(
            c(" Focus", "Non-Focus", " Outside Scope"),
            c("600",       "400",         "400")
          )
        ) %>%
        formatStyle(
          "Suffix",
          fontFamily = "monospace",
          fontSize   = "11px",
          color      = styleEqual(
            c(paste0("_", lbl), paste0("_Before_", lbl), "—"),
            c("#5B9BD5",         "#6c757d",                "#adb5bd")
          )
        )
    })
    
    # ── Return config ─────────────────────────────────────────
    reactive(list(
      update_label      = input$update_label,
      start_report_date = input$start_report_date,
      end_report_date   = input$end_report_date,
      cross_cols        = {
        n <- as.integer(input$n_cross %||% 1)
        if (n == 1)
          input$cross_col_1 %||% "Geography"
        else
          c(input$cross_col_1 %||% "Geography",
            input$cross_col_2 %||% "Product")
      }
    ))
  })
}