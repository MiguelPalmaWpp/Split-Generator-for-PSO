# ── Upload Module ─────────────────────────────────────────────

mod_upload_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: File inputs ──────────────────────────────────────
    card(
      card_header("Data Files"),
      
      fileInput(
        ns("file_at"),
        tags$span("all_transformed ",
                  tags$small(".csv / .parquet — national",
                             class = "text-muted")),
        accept = c(".csv", ".parquet")
      ),
      
      fileInput(
        ns("file_all_rags"),
        tags$span("all_RAGs ",
                  tags$small(".csv / .parquet — geographic",
                             class = "text-muted")),
        accept = c(".csv", ".parquet")
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
    tagList(
      uiOutput(ns("status_strip")),
      uiOutput(ns("kpi_cards")),
      
      card(
        full_screen = TRUE,
        card_header("Preview"),
        navset_card_underline(
          id = ns("preview_tabs"),      #  ID for programmatic switching
          
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

# ── Server ────────────────────────────────────────────────────

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
    
    # ── all_transformed (national — single geography) ─────────
    observeEvent(input$file_at, {
      req(input$file_at)
      ext <- tools::file_ext(input$file_at$name)
      withProgress(message = "Loading all_transformed...", {
        tryCatch({
          rv$all_transformed <- read_all_transformed(
            input$file_at$datapath, ext
          )
          gc()
          showNotification("✅ all_transformed loaded.",
                           type = "message")
        }, error = \(e) showNotification(
          e$message, type = "error", duration = 10)
        )
      })
    })
    
    # Auto-switch tab after all_transformed loads
    observeEvent(rv$all_transformed, {
      req(rv$all_transformed)
      nav_select(id      = "preview_tabs",
                 selected = "tab_at",
                 session  = session)
    }, ignoreNULL = TRUE)
    
    # ── all_RAGs (geographic — multiple geographies) ──────────
    observeEvent(input$file_all_rags, {
      req(input$file_all_rags)
      ext <- tools::file_ext(input$file_all_rags$name)
      withProgress(message = "Loading all_RAGs...", {
        tryCatch({
          rv$all_rags <- read_all_transformed(
            input$file_all_rags$datapath, ext
          )
          gc()
          showNotification("✅ all_RAGs loaded.", type = "message")
        }, error = \(e) showNotification(
          e$message, type = "error", duration = 10)
        )
      })
    })
    
    # Auto-switch tab after all_RAGs loads
    observeEvent(rv$all_rags, {
      req(rv$all_rags)
      nav_select(id      = "preview_tabs",
                 selected = "tab_rags",
                 session  = session)
    }, ignoreNULL = TRUE)
    
    # ── AnalyticalDataset ─────────────────────────────────────
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      ext <- tools::file_ext(input$file_analytical$name)
      
      tryCatch({
        
        df <- if (tolower(ext) == "rdata") {
          e <- new.env()
          load(input$file_analytical$datapath, envir = e)
          get(ls(e)[1], envir = e)
          
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
          # Excel: read_excel already detects types correctly
          read_excel(input$file_analytical$datapath)
          
        } else {
          # CSV: fread reads everything as character
          # We fix types below
          data.table::fread(
            input$file_analytical$datapath,
            data.table   = FALSE,
            colClasses   = "character",
            showProgress = FALSE
          )
        }
        
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        if (tolower(ext) %in% c("csv", "tsv", "txt")) {
          id_cols <- c("Geography", "Product", "BP_Year", "Period")
          df <- df %>%
            mutate(across(
              -any_of(id_cols),
              ~ {
                converted <- suppressWarnings(as.numeric(.))
                # Convert only if at least 80% of non-NA values parse as numeric
                non_na    <- !is.na(.)
                if (sum(non_na) == 0) return(.)
                pct_numeric <- sum(!is.na(converted) & non_na) / sum(non_na)
                if (pct_numeric >= 0.8) converted else .
              }
            ))
        }
        
        # ── Parse Period to Date ──────────────────────────────────
        if ("Period" %in% names(df)) {
          df <- df %>%
            mutate(Period = parse_period_robust(Period))
        }
        
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
    
    # ── ModelDetails ──────────────────────────────────────────
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
        showNotification("✅ ModelDetails loaded.",
                         type = "message")
        
        nav_select(id      = "preview_tabs",
                   selected = "tab_details",
                   session  = session)
        
      }, error = \(e) showNotification(
        e$message, type = "error", duration = 10)
      )
    })
    
    # ── ROIs by Channel ───────────────────────────────────────
    observeEvent(input$file_rois, {
      req(input$file_rois)
      ext <- tools::file_ext(input$file_rois$name)
      tryCatch({
        rv$channels_rois <- if (tolower(ext) %in% c("xlsx", "xls"))
          read_excel(input$file_rois$datapath)
        else
          data.table::fread(
            input$file_rois$datapath,
            data.table   = FALSE,
            showProgress = FALSE
          )
        gc()
        showNotification("✅ ROIs loaded.", type = "message")
        
        nav_select(id      = "preview_tabs",
                   selected = "tab_rois",
                   session  = session)
        
      }, error = \(e) showNotification(
        e$message, type = "error", duration = 10)
      )
    })
    
    # ── Upload status strip ───────────────────────────────────
    output$status_strip <- renderUI({
      
      files <- list(
        list(label = "all_transformed", icon = "table",
             data  = rv$all_transformed, note = "national",
             rows  = if (!is.null(rv$all_transformed))
               format(nrow(rv$all_transformed), big.mark = ",")
             else NULL),
        list(label = "all_RAGs", icon = "map",
             data  = rv$all_rags, note = "geographic",
             rows  = if (!is.null(rv$all_rags))
               format(nrow(rv$all_rags), big.mark = ",")
             else NULL),
        list(label = "AnalyticalDataset", icon = "database",
             data  = rv$analytical, note = NULL,
             rows  = if (!is.null(rv$analytical))
               format(nrow(rv$analytical), big.mark = ",")
             else NULL),
        list(label = "ModelDetails", icon = "list",
             data  = rv$details, note = NULL,
             rows  = if (!is.null(rv$details))
               format(nrow(rv$details), big.mark = ",")
             else NULL),
        list(label = "ROIs by Channel", icon = "chart-bar",
             data  = rv$channels_rois, note = NULL,
             rows  = if (!is.null(rv$channels_rois))
               format(nrow(rv$channels_rois), big.mark = ",")
             else NULL)
      )
      
      cards <- lapply(files, function(f) {
        loaded <- !is.null(f$data)
        div(
          style = paste0(
            "border-left:4px solid ",
            if (loaded) "#5B9BD5" else "#dee2e6", ";",
            "background:", if (loaded) "#EBF3FB" else "#f8f9fa", ";",
            "border-radius:6px; padding:8px 12px;",
            "display:flex; align-items:center; gap:8px;"
          ),
          tags$div(icon(f$icon, style = paste0(
            "font-size:18px; color:",
            if (loaded) "#5B9BD5" else "#adb5bd", ";"))),
          tags$div(
            tags$div(
              tags$strong(f$label,
                          style = "font-size:11px; color:#333;"),
              if (!is.null(f$note))
                tags$span(paste0(" (", f$note, ")"),
                          style = "font-size:10px; color:#adb5bd;")
            ),
            if (loaded)
              tags$small(paste0("✓ ", f$rows, " rows"),
                         style = "color:#5B9BD5; font-weight:500;")
            else
              tags$small("Not uploaded", style = "color:#adb5bd;")
          )
        )
      })
      
      do.call(layout_columns,
              c(list(col_widths = c(3, 3, 2, 2, 2),
                     style      = "margin-bottom:12px;"),
                cards))
    })
    
    # ── KPI summary cards ─────────────────────────────────────
    output$kpi_cards <- renderUI({
      d_nat  <- rv$all_transformed
      d_rags <- rv$all_rags
      if (is.null(d_nat) && is.null(d_rags)) return(NULL)
      
      d_ref <- d_nat %||% d_rags  # reference for dates + variable names
      
      # ── Geography: prefer analytical, fall back to all_rags ──────
      n_geo <- if (!is.null(rv$analytical)) {
        n_distinct(rv$analytical$Geography)
      } else if (!is.null(rv$all_rags)) {
        n_distinct(rv$all_rags$Geography)
      } else "—"
      
      # ── Product: prefer analytical, fall back to all_rags ────────
      n_prod <- if (!is.null(rv$analytical)) {
        n_distinct(rv$analytical$Product)
      } else if (!is.null(rv$all_rags)) {
        n_distinct(rv$all_rags$Product)
      } else "—"
      
      kpis <- list(
        list(icon = "location-dot",  label = "Geographies",   value = n_geo),
        list(icon = "box",           label = "Products",       value = n_prod),
        list(icon = "tv",            label = "Variable Names",
             value = if (!is.null(d_ref))
               format(n_distinct(d_ref$VariableName), big.mark = ",") else "—"),
        list(icon = "calendar-days", label = "Date Range",
             value = if (!is.null(d_ref))
               paste0(format(min(d_ref$Period)), " → ", format(max(d_ref$Period))) else "—"),
        list(icon = "clock",         label = "Total Weeks",
             value = if (!is.null(d_ref)) n_distinct(d_ref$Period) else "—")
      )
      
      kpi_cards <- lapply(kpis, function(k) {
        div(
          style = paste0("background:#EBF3FB; border-radius:8px;",
                         "padding:14px 10px; text-align:center;"),
          icon(k$icon, style = "color:#5B9BD5; font-size:20px;"),
          tags$div(
            tags$strong(k$value,
                        style = "font-size:17px; color:#2c3e50; display:block; margin-top:6px;"),
            tags$small(k$label,
                       style = "color:#6c757d; font-size:11px;")
          )
        )
      })
      
      do.call(layout_columns,
              c(list(col_widths = c(2, 2, 2, 4, 2),
                     style = "margin-bottom:12px;"),
                kpi_cards))
    })
    
    # ── Inline summaries ──────────────────────────────────────
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
    
    # ── Preview tables ────────────────────────────────────────
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
            # Muestra info de filas totales
            language        = list(
              info = "Mostrando _START_ a _END_ de _TOTAL_ filas"
            )
          ),
          rownames = FALSE,
          filter   = "top"   
        )
        
      }, server = TRUE)  
    }
    
    output$preview_at   <- make_preview_dt(
      \() rv$all_transformed,
      "Upload all_transformed to preview"
    )
    output$preview_rags <- make_preview_dt(
      \() rv$all_rags,
      "Upload all_RAGs to preview"
    )
    
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
    
    # ── Return reactive data bundle ───────────────────────────
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