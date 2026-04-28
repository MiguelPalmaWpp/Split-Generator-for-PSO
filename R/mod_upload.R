# ═══════════════════════════════════════════════════════════════════
# R/mod_upload.R
# ═══════════════════════════════════════════════════════════════════

mod_upload_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: File inputs ──────────────────────────────────────
    card(
      class       = "upload-files-card",
      fill        = FALSE,
      card_header("Data Files"),
      
      fileInput(
        ns("file_at"),
        tags$span("all_transformed ",
                  tags$small(".csv / .parquet / .zip / .gz — national",
                             class = "text-muted")),
        accept = c(".csv", ".parquet", ".zip", ".gz")
      ),
      
      fileInput(
        ns("file_all_rags"),
        tags$span("all_RAGs ",
                  tags$small(".csv / .parquet / .zip / .gz — geographic",
                             class = "text-muted")),
        accept = c(".csv", ".parquet", ".zip", ".gz")
      ),
      
      fileInput(
        ns("file_analytical"),
        tags$span("AnalyticalDataset ",
                  tags$small(".RData / .csv / .xlsx",
                             class = "text-muted")),
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
    
    # ── Right: Status + KPIs + Previews ───────────────────────
    div(
      class = "upload-right-col",
      
      uiOutput(ns("status_strip")),
      uiOutput(ns("kpi_cards")),
      
      card(
        class       = "upload-preview-card",
        full_screen = TRUE,
        card_header("Preview"),
        
        navset_card_underline(
          id = ns("preview_tabs"),
          
          # ── Comparison tab (NEW) ─────────────────────────────
          nav_panel(
            title = tagList(icon("table-columns"), " Comparison"),
            value = "tab_comparison",
            uiOutput(ns("file_comparison"))
          ),
          
          nav_panel(
            title = tagList(icon("table"), " all_transformed"),
            value = "tab_at",
            uiOutput(ns("at_summary")),
            DTOutput(ns("preview_at"))
          ),
          nav_panel(
            title = tagList(icon("map"), " all_RAGs"),
            value = "tab_rags",
            uiOutput(ns("rags_summary")),
            DTOutput(ns("preview_rags"))
          ),
          nav_panel(
            title = tagList(icon("list-check"), " ModelDetails"),
            value = "tab_details",
            DTOutput(ns("preview_details"))
          ),
          nav_panel(
            title = tagList(icon("chart-bar"), " ROIs by Channel"),
            value = "tab_rois",
            DTOutput(ns("preview_rois"))
          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      all_transformed = NULL,
      all_rags        = NULL,
      analytical      = NULL,
      analytical_rag  = NULL,
      dates_df        = NULL,
      details         = NULL,
      channels_rois   = NULL
    )
    
    # ── Validate required columns ──────────────────────────────
    validate_required_cols <- function(df, file_label) {
      missing_cols <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing_cols) > 0) {
        showNotification(
          paste0(file_label, " is missing required columns: ",
                 paste(missing_cols, collapse = ", ")),
          type = "error", duration = 15
        )
        return(FALSE)
      }
      TRUE
    }
    
    # ── all_transformed ────────────────────────────────────────
    observeEvent(input$file_at, {
      req(input$file_at)
      ext <- tools::file_ext(input$file_at$name)
      withProgress(message = "Loading all_transformed...", {
        tryCatch({
          df <- read_all_transformed(input$file_at$datapath, ext)
          if (!validate_required_cols(df, "all_transformed")) return()
          rv$all_transformed <- df
          gc()
          showNotification("all_transformed loaded.", type = "message")
          nav_select(id = "preview_tabs", selected = "tab_at",
                     session = session)
        }, error = \(e) showNotification(e$message,
                                         type = "error", duration = 10))
      })
    })
    
    # ── all_RAGs ───────────────────────────────────────────────
    observeEvent(input$file_all_rags, {
      req(input$file_all_rags)
      ext <- tools::file_ext(input$file_all_rags$name)
      withProgress(message = "Loading all_RAGs...", {
        tryCatch({
          df <- read_all_transformed(input$file_all_rags$datapath, ext)
          if (!validate_required_cols(df, "all_RAGs")) return()
          rv$all_rags <- df
          gc()
          showNotification("all_RAGs loaded.", type = "message")
          nav_select(id = "preview_tabs", selected = "tab_rags",
                     session = session)
        }, error = \(e) showNotification(e$message,
                                         type = "error", duration = 10))
      })
    })
    
    # ── AnalyticalDataset ──────────────────────────────────────
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
          data.table::fread(
            input$file_analytical$datapath,
            data.table   = FALSE,
            colClasses   = "character",
            showProgress = FALSE
          )
        }
        
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        
        # Convert numeric columns for CSV/TSV
        if (tolower(ext) %in% c("csv", "tsv", "txt")) {
          id_cols <- c("Geography", "Product", "BP_Year", "Period")
          df <- df %>%
            mutate(across(
              -any_of(id_cols),
              ~ {
                converted  <- suppressWarnings(as.numeric(.))
                non_na     <- !is.na(.)
                if (sum(non_na) == 0) return(.)
                pct_num    <- sum(!is.na(converted) & non_na) / sum(non_na)
                if (pct_num >= 0.8) converted else .
              }
            ))
        }
        
        if ("Period" %in% names(df))
          df <- df %>% mutate(Period = parse_period_robust(Period))
        
        rv$analytical     <- as_tibble(df) %>% ungroup()
        rv$analytical_rag <- rv$analytical %>%
          ungroup() %>% distinct(Geography, Period)
        rv$dates_df       <- rv$analytical %>%
          ungroup() %>% distinct(Period) %>% arrange(Period)
        
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
    
    # ── ModelDetails ───────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      tryCatch({
        rv$details <- data.table::fread(
          input$file_details$datapath,
          data.table   = FALSE,
          showProgress = FALSE
        ) %>%
          filter(!str_detect(str_to_lower(Type), "none")) %>%
          unite("Analytical_varname",
                VariableName, Campaign, Outlet, Creative,
                sep = "_", remove = FALSE) %>%
          mutate(Analytical_varname =
                   str_replace_all(Analytical_varname, "_NA", ""))
        gc()
        showNotification("ModelDetails loaded.", type = "message")
        nav_select(id = "preview_tabs", selected = "tab_details",
                   session = session)
      }, error = \(e) showNotification(e$message,
                                       type = "error", duration = 10))
    })
    
    # ── ROIs by Channel ────────────────────────────────────────
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
        nav_select(id = "preview_tabs", selected = "tab_rois",
                   session = session)
      }, error = \(e) showNotification(e$message,
                                       type = "error", duration = 10))
    })
    
    # ── Upload status strip ────────────────────────────────────
    output$status_strip <- renderUI({
      files <- list(
        list(label = "all_transformed", icon = "table",
             data  = rv$all_transformed, note = "national",
             rows  = if (!is.null(rv$all_transformed))
               format(nrow(rv$all_transformed), big.mark = ",") else NULL),
        list(label = "all_RAGs",  icon = "map",
             data  = rv$all_rags, note = "geographic",
             rows  = if (!is.null(rv$all_rags))
               format(nrow(rv$all_rags), big.mark = ",") else NULL),
        list(label = "AnalyticalDataset", icon = "database",
             data  = rv$analytical, note = NULL,
             rows  = if (!is.null(rv$analytical))
               format(nrow(rv$analytical), big.mark = ",") else NULL),
        list(label = "ModelDetails", icon = "list",
             data  = rv$details, note = NULL,
             rows  = if (!is.null(rv$details))
               format(nrow(rv$details), big.mark = ",") else NULL),
        list(label = "ROIs by Channel", icon = "chart-bar",
             data  = rv$channels_rois, note = NULL,
             rows  = if (!is.null(rv$channels_rois))
               format(nrow(rv$channels_rois), big.mark = ",") else NULL)
      )
      
      cards <- lapply(files, function(f) {
        loaded <- !is.null(f$data)
        div(
          style = paste0(
            "border-left:4px solid ", if (loaded) "#5B9BD5" else "#dee2e6", ";",
            "background:", if (loaded) "#EBF3FB" else "#f8f9fa", ";",
            "border-radius:6px; padding:8px 12px;",
            "display:flex; align-items:center; gap:8px;"
          ),
          div(icon(f$icon,
                   style = paste0("font-size:18px; color:",
                                  if (loaded) "#5B9BD5" else "#adb5bd", ";"))),
          div(
            div(
              tags$strong(f$label, style = "font-size:11px; color:#333;"),
              if (!is.null(f$note))
                tags$span(paste0(" (", f$note, ")"),
                          style = "font-size:10px; color:#adb5bd;")
            ),
            if (loaded)
              tags$small(paste0(" ✓ ", f$rows, " rows"),
                         style = "color:#5B9BD5; font-weight:500;")
            else
              tags$small("Not uploaded", style = "color:#adb5bd;")
          )
        )
      })
      
      do.call(layout_columns,
              c(list(col_widths = c(3, 3, 2, 2, 2),
                     style = "margin-bottom:12px;"),
                cards))
    })
    
    # ── KPI summary cards ──────────────────────────────────────
    output$kpi_cards <- renderUI({
      d_nat  <- rv$all_transformed
      d_rags <- rv$all_rags
      if (is.null(d_nat) && is.null(d_rags)) return(NULL)
      
      d_ref <- d_nat %||% d_rags
      
      n_geo  <- if (!is.null(rv$analytical)) {
        n_distinct(rv$analytical$Geography)
      } else if (!is.null(rv$all_rags)) {
        n_distinct(rv$all_rags$Geography)
      } else "—"
      
      n_prod <- if (!is.null(rv$analytical)) {
        n_distinct(rv$analytical$Product)
      } else if (!is.null(rv$all_rags)) {
        n_distinct(rv$all_rags$Product)
      } else "—"
      
      kpis <- list(
        list(icon = "location-dot", label = "Geographies", value = n_geo),
        list(icon = "box",          label = "Products",    value = n_prod),
        list(icon = "tv",           label = "Variable Names",
             value = if (!is.null(d_ref))
               format(n_distinct(d_ref$VariableName), big.mark = ",") else "—"),
        list(icon = "calendar-days", label = "Date Range",
             value = if (!is.null(d_ref))
               paste0(format(min(d_ref$Period)), " → ", format(max(d_ref$Period)))
             else "—"),
        list(icon = "clock", label = "Total Weeks",
             value = if (!is.null(d_ref)) n_distinct(d_ref$Period) else "—")
      )
      
      kpi_cards <- lapply(kpis, function(k) {
        div(
          style = paste0(
            "background:#EBF3FB; border-radius:8px;",
            "padding:14px 10px; text-align:center;"
          ),
          icon(k$icon, style = "color:#5B9BD5; font-size:20px;"),
          div(
            tags$strong(k$value,
                        style = "font-size:17px; color:#2c3e50; display:block; margin-top:6px;"),
            tags$small(k$label, style = "color:#6c757d; font-size:11px;")
          )
        )
      })
      
      do.call(layout_columns,
              c(list(col_widths = c(2, 2, 2, 4, 2),
                     style = "margin-bottom:12px;"),
                kpi_cards))
    })
    
    # ── Inline summaries ───────────────────────────────────────
    output$at_summary <- renderUI({
      if (is.null(rv$all_transformed))
        return(tags$p(class = "text-muted small",
                      "Upload all_transformed to preview."))
      tags$p(
        style = "color:#5B9BD5; font-weight:500; margin-bottom:8px;",
        icon("circle-check"),
        sprintf(" %s rows — national (single geography)",
                format(nrow(rv$all_transformed), big.mark = ","))
      )
    })
    
    output$rags_summary <- renderUI({
      if (is.null(rv$all_rags))
        return(tags$p(class = "text-muted small",
                      "Upload all_RAGs to preview."))
      n_geos <- n_distinct(rv$all_rags$Geography)
      tags$p(
        style = "color:#5B9BD5; font-weight:500; margin-bottom:8px;",
        icon("circle-check"),
        sprintf(" %s rows — %d geographies",
                format(nrow(rv$all_rags), big.mark = ","), n_geos)
      )
    })
    
    # ── Preview tables ─────────────────────────────────────────
    make_preview_dt <- function(data_fn, empty_msg) {
      renderDT({
        d <- data_fn()
        if (is.null(d))
          return(datatable(
            data.frame(Message = empty_msg),
            options  = list(initComplete = dt_blue_callback),
            rownames = FALSE
          ))
        datatable(
          d,
          options = list(
            scrollX         = TRUE,
            pageLength      = 25,
            lengthMenu      = c(25, 50, 100, 200, 500),
            searchHighlight = TRUE,
            initComplete    = dt_blue_callback,
            language        = list(
              info = "Showing _START_ to _END_ of _TOTAL_ rows")
          ),
          rownames = FALSE,
          filter   = "top"
        )
      }, server = TRUE)
    }
    
    output$preview_at <- make_preview_dt(
      \() rv$all_transformed, "Upload all_transformed to preview")
    output$preview_rags <- make_preview_dt(
      \() rv$all_rags, "Upload all_RAGs to preview")
    
    output$preview_details <- renderDT({
      if (is.null(rv$details))
        return(datatable(
          data.frame(Message = "Upload ModelDetails to preview"),
          options  = list(initComplete = dt_blue_callback),
          rownames = FALSE
        ))
      rv$details %>%
        datatable(options = list(scrollX = TRUE, pageLength = 25,
                                 initComplete = dt_blue_callback),
                  rownames = FALSE)
    })
    
    output$preview_rois <- renderDT({
      if (is.null(rv$channels_rois))
        return(datatable(
          data.frame(Message = "Upload ROIs by Channel to preview"),
          options  = list(initComplete = dt_blue_callback),
          rownames = FALSE
        ))
      rv$channels_rois %>%
        datatable(options = list(scrollX = TRUE, pageLength = 25,
                                 initComplete = dt_blue_callback),
                  rownames = FALSE)
    })
    
    # ── File Comparison Panel ──────────────────────────────────
    output$file_comparison <- renderUI({
      
      at   <- rv$all_transformed
      rags <- rv$all_rags
      an   <- rv$analytical
      det  <- rv$details
      roi  <- rv$channels_rois
      
      n_main <- sum(!sapply(list(at, rags, an), is.null))
      
      if (n_main < 2) {
        return(div(
          class = "text-center py-4",
          style = "color:#adb5bd;",
          icon("table-columns",
               style = paste0("font-size:36px; display:block;",
                              "margin-bottom:12px; color:#dee2e6;")),
          tags$p("Upload at least 2 main files to compare consistency."),
          tags$p(class = "small",
                 "(all_transformed, all_RAGs or AnalyticalDataset)")
        ))
      }
      
      # ── Extract metadata ──────────────────────────────────────
      get_meta <- function(df) {
        if (is.null(df)) return(NULL)
        list(
          rows  = nrow(df),
          d_min = if ("Period"       %in% names(df)) min(df$Period,       na.rm = TRUE) else NA,
          d_max = if ("Period"       %in% names(df)) max(df$Period,       na.rm = TRUE) else NA,
          n_wk  = if ("Period"       %in% names(df)) n_distinct(df$Period) else NA_integer_,
          n_geo = if ("Geography"    %in% names(df)) n_distinct(df$Geography) else NA_integer_,
          n_prd = if ("Product"      %in% names(df)) n_distinct(df$Product) else NA_integer_,
          n_var = if ("VariableName" %in% names(df)) n_distinct(df$VariableName) else NA_integer_
        )
      }
      
      m_at   <- get_meta(at)
      m_rags <- get_meta(rags)
      m_an   <- get_meta(an)
      
      # ── Summary table ─────────────────────────────────────────
      mk_cell <- function(x) {
        if (is.null(x) || (length(x) == 1 && is.na(x)))
          tags$td("—", style = "color:#dee2e6;")
        else
          tags$td(format(x, big.mark = ","))
      }
      
      mk_row <- function(label, icon_nm, meta) {
        if (is.null(meta))
          return(tags$tr(
            style = "color:#dee2e6;",
            tags$td(tagList(icon(icon_nm), " ", label)),
            tags$td(colspan = "6", tags$em("not uploaded"))
          ))
        
        date_str <- if (!is.na(meta$d_min))
          paste0(format(meta$d_min), " → ", format(meta$d_max))
        else "—"
        
        tags$tr(
          tags$td(tagList(
            icon(icon_nm, style = "color:#5B9BD5; font-size:11px;"), " ",
            tags$strong(label, style = "font-size:12px;")
          )),
          mk_cell(meta$rows),
          tags$td(date_str, style = "white-space:nowrap; font-size:12px;"),
          mk_cell(meta$n_wk),
          mk_cell(meta$n_geo),
          mk_cell(meta$n_prd),
          mk_cell(meta$n_var)
        )
      }
      
      mk_row_simple <- function(label, icon_nm, df) {
        if (is.null(df))
          return(tags$tr(
            style = "color:#dee2e6;",
            tags$td(tagList(icon(icon_nm), " ", label)),
            tags$td(colspan = "6", tags$em("not uploaded"))
          ))
        tags$tr(
          tags$td(tagList(
            icon(icon_nm, style = "color:#5B9BD5; font-size:11px;"), " ",
            tags$strong(label, style = "font-size:12px;")
          )),
          tags$td(format(nrow(df), big.mark = ",")),
          tags$td("—"), tags$td("—"),
          tags$td("—"), tags$td("—"), tags$td("—")
        )
      }
      
      summary_tbl <- div(
        style = "overflow-x:auto; margin-bottom:16px;",
        tags$table(
          class = "table table-sm table-hover",
          style = "font-size:12.5px; margin-bottom:0;",
          tags$thead(tags$tr(
            style = "border-bottom:2px solid #5B9BD5;",
            lapply(
              c("File", "Rows", "Date Range",
                "Weeks", "Geos", "Products", "Variables"),
              \(h) tags$th(
                h, style = "font-size:12px; color:#2c3e50; padding:6px 8px;")
            )
          )),
          tags$tbody(
            mk_row("all_transformed", "table",      m_at),
            mk_row("all_RAGs",        "map",         m_rags),
            mk_row("AnalyticalData",  "database",    m_an),
            mk_row_simple("ModelDetails",    "list-check", det),
            mk_row_simple("ROIs by Channel", "chart-bar",  roi)
          )
        )
      )
      
      # ── Consistency checks ────────────────────────────────────
      check_results <- list()
      n_fail <- 0L
      n_warn <- 0L
      
      mk_check <- function(status, title, detail = NULL) {
        bg  <- switch(status,
                      pass    = "#e8f5e9",
                      warning = "#fff8e1",
                      fail    = "#fdecea")
        col <- switch(status,
                      pass    = "#155724",
                      warning = "#856404",
                      fail    = "#721c24")
        ico <- switch(status,
                      pass    = icon("circle-check",
                                     style = "color:#2ecc71; font-size:14px;"),
                      warning = icon("triangle-exclamation",
                                     style = "color:#f39c12; font-size:14px;"),
                      fail    = icon("circle-xmark",
                                     style = "color:#e74c3c; font-size:14px;")
        )
        div(
          style = paste0(
            "display:flex; align-items:flex-start; gap:10px;",
            "padding:8px 12px; border-radius:6px; margin-bottom:6px;",
            "background:", bg, ";"
          ),
          div(style = "flex-shrink:0; margin-top:1px;", ico),
          div(
            tags$strong(title,
                        style = paste0("font-size:13px; color:", col, ";")),
            if (!is.null(detail))
              div(detail,
                  style = "font-size:11.5px; color:#6c757d; margin-top:2px;")
          )
        )
      }
      
      add_check <- function(status, title, detail = NULL) {
        check_results[[length(check_results) + 1]] <<-
          mk_check(status, title, detail)
        if (status == "fail")    n_fail <<- n_fail + 1L
        if (status == "warning") n_warn <<- n_warn + 1L
      }
      
      fmt_vals <- function(vals) {
        paste(
          sapply(names(vals), \(n) paste0(n, ": ", vals[[n]])),
          collapse = "  |  "
        )
      }
      
      # Check 1: Start date
      d_mins <- Filter(
        \(x) !is.null(x) && !is.na(x),
        list(`all_transformed` = m_at$d_min,
             `all_RAGs`        = m_rags$d_min,
             `Analytical`      = m_an$d_min)
      )
      if (length(d_mins) >= 2) {
        uniq <- unique(as.character(unlist(lapply(d_mins, as.character))))
        if (length(uniq) == 1)
          add_check("pass",
                    paste0("Start date consistent — ", uniq))
        else
          add_check("fail", "Start date MISMATCH",
                    paste0(fmt_vals(lapply(d_mins, format)),
                           " — data ranges don't align"))
      }
      
      # Check 2: End date
      d_maxs <- Filter(
        \(x) !is.null(x) && !is.na(x),
        list(`all_transformed` = m_at$d_max,
             `all_RAGs`        = m_rags$d_max,
             `Analytical`      = m_an$d_max)
      )
      if (length(d_maxs) >= 2) {
        uniq <- unique(as.character(unlist(lapply(d_maxs, as.character))))
        if (length(uniq) == 1)
          add_check("pass",
                    paste0("End date consistent — ", uniq))
        else
          add_check("fail", "End date MISMATCH",
                    fmt_vals(lapply(d_maxs, format)))
      }
      
      # Check 3: Week count
      wks <- Filter(
        \(x) !is.null(x) && !is.na(x),
        list(`all_transformed` = m_at$n_wk,
             `all_RAGs`        = m_rags$n_wk,
             `Analytical`      = m_an$n_wk)
      )
      if (length(wks) >= 2) {
        uniq <- unique(unlist(wks))
        if (length(uniq) == 1)
          add_check("pass",
                    paste0("Week count consistent — ", uniq, " weeks"))
        else
          add_check("warning", "Week count differs between files",
                    fmt_vals(lapply(wks, \(x) paste0(x, " wks"))))
      }
      
      # Check 4: Geography count (rags vs analytical)
      if (!is.null(m_rags) && !is.na(m_rags$n_geo) &&
          !is.null(m_an)   && !is.na(m_an$n_geo)) {
        if (m_rags$n_geo == m_an$n_geo)
          add_check("pass",
                    paste0("Geography count consistent — ",
                           m_rags$n_geo, " geographies"))
        else
          add_check("fail", "Geography count MISMATCH",
                    paste0("all_RAGs: ", m_rags$n_geo,
                           " vs Analytical: ", m_an$n_geo,
                           " — this will cause processing errors"))
      }
      
      # Check 5: Product count
      prds <- Filter(
        \(x) !is.null(x) && !is.na(x),
        list(`all_transformed` = m_at$n_prd,
             `all_RAGs`        = m_rags$n_prd,
             `Analytical`      = m_an$n_prd)
      )
      if (length(prds) >= 2) {
        uniq <- unique(unlist(prds))
        if (length(uniq) == 1)
          add_check("pass",
                    paste0("Product count consistent — ",
                           uniq, " product(s)"))
        else
          add_check("warning", "Product count differs between files",
                    fmt_vals(prds))
      }
      
      # Check 6: Variable name count (at vs rags)
      if (!is.null(m_at)   && !is.na(m_at$n_var) &&
          !is.null(m_rags) && !is.na(m_rags$n_var)) {
        if (m_at$n_var == m_rags$n_var)
          add_check("pass",
                    paste0("Variable name count consistent — ",
                           m_at$n_var, " variables"))
        else
          add_check("warning", "Variable name count differs",
                    paste0("all_transformed: ", m_at$n_var,
                           " vs all_RAGs: ", m_rags$n_var))
      }
      
      # ── Overall banner ────────────────────────────────────────
      if (n_fail > 0) {
        b_style <- paste0("background:#fdecea;",
                          "border:1px solid #f5c6cb; color:#721c24;")
        b_icon  <- icon("circle-xmark",        style = "font-size:18px;")
        b_text  <- paste0(n_fail,
                          " critical issue(s) found — fix before processing")
      } else if (n_warn > 0) {
        b_style <- paste0("background:#fff8e1;",
                          "border:1px solid #ffe082; color:#856404;")
        b_icon  <- icon("triangle-exclamation", style = "font-size:18px;")
        b_text  <- paste0(n_warn, " warning(s) — review before processing")
      } else {
        b_style <- paste0("background:#e8f5e9;",
                          "border:1px solid #a5d6a7; color:#155724;")
        b_icon  <- icon("circle-check",         style = "font-size:18px;")
        b_text  <- "All checks passed — files are consistent"
      }
      
      banner <- div(
        style = paste0(
          "display:flex; align-items:center; gap:10px;",
          "padding:10px 16px; border-radius:8px; margin-bottom:14px; ",
          b_style
        ),
        b_icon,
        tags$strong(b_text, style = "font-size:13px;")
      )
      
      # ── Assemble ──────────────────────────────────────────────
      tagList(
        banner,
        
        div(
          style = "margin-bottom:16px;",
          tags$strong("File Summary",
                      style = paste0("font-size:13px; color:#2c3e50;",
                                     "display:block; margin-bottom:8px;")),
          summary_tbl
        ),
        
        div(
          tags$strong("Consistency Checks",
                      style = paste0("font-size:13px; color:#2c3e50;",
                                     "display:block; margin-bottom:8px;")),
          if (length(check_results) == 0)
            tags$p(class = "text-muted small",
                   "Upload more files to run consistency checks.")
          else
            tagList(check_results)
        )
      )
    })
    
    # ── Return reactive data bundle ────────────────────────────
    reactive(list(
      all_transformed = rv$all_transformed,
      all_rags        = rv$all_rags,
      analytical      = rv$analytical,
      analytical_rag  = rv$analytical_rag,
      dates_df        = rv$dates_df,
      details         = rv$details,
      channels_rois   = rv$channels_rois
    ))
    
  })
}