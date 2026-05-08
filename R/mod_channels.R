# ═══════════════════════════════════════════════════════════════════
# R/mod_channels.R
# ═══════════════════════════════════════════════════════════════════

mod_channels_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: Channel list ──────────────────────────────────────────
    card(
      card_header("Channel List"),
      
      div(
        style = "display:flex; gap:8px; margin-bottom:10px;",
        textInput(ns("new_name"), NULL, placeholder = "Channel name...") %>%
          tagAppendAttributes(style = "margin-bottom:0;"),
        actionButton(ns("btn_add"), "Add",
                     class = "btn-primary btn-sm",
                     style = "white-space:nowrap; align-self:flex-end; margin-bottom:1px;")
      ),
      
      # Save / Load / VOF
      div(
        style = paste0(
          "display:flex; flex-direction:column; gap:6px;",
          "margin-bottom:10px; padding:8px 0;",
          "border-top:1px solid #e3e8ef;",
          "border-bottom:1px solid #e3e8ef;"
        ),
        
        # Row 1: Save Config + Load Config
        div(
          style = "display:flex; gap:6px;",
          downloadButton(ns("dl_config_csv"),
                         label = tagList(icon("download"), " Save Config"),
                         class = "btn-outline-secondary btn-sm",
                         style = "flex:1; font-size:12px;"),
          div(
            style = "flex:1; position:relative;",
            tags$label(
              style = paste0(
                "display:flex; align-items:center; justify-content:center; gap:5px;",
                "width:100%; height:100%;",
                "border:1px solid #c8d6e5; border-radius:5px; background:white;",
                "color:#5B9BD5; font-size:12px; font-weight:500; cursor:pointer;",
                "padding:5px 10px; margin:0; transition:all 0.15s; white-space:nowrap;"
              ),
              icon("upload"), " Load Config",
              tags$input(
                type = "file", accept = ".csv", style = "display:none;",
                onchange = paste0(
                  "var r=new FileReader();",
                  "r.onload=function(e){",
                  " Shiny.setInputValue('", ns("config_csv_content"), "',",
                  " e.target.result,{priority:'event'});};",
                  "r.readAsText(this.files[0]);"
                )
              )
            )
          )
        ),
        
        # Row 2: Import from VOF
        div(
          style = "position:relative;",
          tags$label(
            style = paste0(
              "display:flex; align-items:center; justify-content:center; gap:6px;",
              "width:100%; border:1px solid #5B9BD5; border-radius:5px;",
              "background:white; color:#5B9BD5; font-size:12px; font-weight:500;",
              "cursor:pointer; padding:6px 10px; margin:0;",
              "transition:all 0.15s; white-space:nowrap;"
            ),
            icon("file-import"), " Import from VOF",
            tags$input(
              type = "file", accept = ".csv,.xlsx,.xls", style = "display:none;",
              onchange = paste0(
                "var f=this.files[0];",
                "var r=new FileReader();",
                "r.onload=function(e){",
                " Shiny.setInputValue('", ns("vof_name"), "',f.name,{priority:'event'});",
                " Shiny.setInputValue('", ns("vof_content"), "',",
                " e.target.result,{priority:'event'});};",
                "r.readAsDataURL(f);"
              )
            )
          )
        )
      ),
      
      # Channel cards (scrollable)
      div(
        style = "max-height:calc(100vh - 320px); overflow-y:auto; padding-right:4px;",
        uiOutput(ns("ch_list"))
      )
    ),
    
    # ── Right: tabs ─────────────────────────────────────────────────
    card(
      full_screen = TRUE,
      card_header(uiOutput(ns("editor_title"))),
      
      navset_card_underline(
        id = ns("editor_tabs"),
        
        # ── Tab 1: Channel Editor (DEFAULT) ─────────────────────────
        nav_panel(
          title = tagList(icon("gear"), " Channel Editor"),
          value = "editor",
          uiOutput(ns("editor_form"))
        ),
        
        # ── Tab 2: Dimension Summary (ACTIONABLE) ────────────────────
        nav_panel(
          title = tagList(icon("table"), " Dimension Summary"),
          value = "dimensions",
          
          # Header: hint + coverage badge
          div(
            style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
            tags$p(
              class = "text-muted small mb-0",
              icon("hand-pointer", style = "font-size:11px; color:#5B9BD5;"),
              " Click a row to add it to the active channel's ",
              tags$strong("VarName filter."),
              " Covered rows are highlighted in green."
            ),
            uiOutput(ns("dim_coverage_badge"))
          ),
          
          # Keyword strip
          div(
            style = paste0(
              "background:#f4f6f9; border-radius:6px;",
              "padding:10px 14px; margin-bottom:12px;"
            ),
            tags$label("Metric keywords to strip",
                       style = "font-size:12px; font-weight:600; color:#4a5568; display:block; margin-bottom:6px;"),
            div(
              style = "display:flex; gap:8px; align-items:flex-end;",
              div(style = "flex:1;",
                  textInput(ns("var_keywords"), NULL,
                            placeholder = "e.g. Impressions, Spend, Clicks, Cost")),
              actionButton(ns("btn_detect_keywords"),
                           tagList(icon("wand-magic-sparkles"), " Auto-detect"),
                           class = "btn-outline-secondary btn-sm",
                           style = "margin-bottom:1px;")
            ),
            tags$small(
              "Variables sharing the same base name will be grouped into one row.",
              class = "text-muted")
          ),
          
          div(class = "dim-table-wrapper", DTOutput(ns("dimension_table"))),
          
          # Dimension Breaks
          hr(style = "margin:16px 0 10px;"),
          div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
            uiOutput(ns("breaks_header")),
            actionButton(ns("btn_add_break"),
                         tagList(icon("scissors"), " Add Break"),
                         class = "btn-outline-secondary btn-sm")
          ),
          uiOutput(ns("breaks_list"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────
mod_channels_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    rv <- reactiveValues(channels = list(), selected = NULL)
    
    # Per-channel dirty tracking
    dirty_channels  <- reactiveValues()
    prevent_dirty   <- reactiveVal(FALSE)
    
    # Modal variable picker state
    mv_selections   <- reactiveValues()
    pending_mv_slot <- reactiveVal(NULL)
    
    # Dimension table data (stored for row-click access)
    dim_table_data  <- reactiveVal(NULL)
    
    pending_delete   <- reactiveVal(NULL)
    pending_load_cfg <- reactiveVal(NULL)
    pending_vof      <- reactiveVal(NULL)
    
    # ── Helpers ────────────────────────────────────────────────────
    analytical_vars <- reactive({
      a <- data()$analytical
      if (is.null(a)) return(character(0))
      names(a)[sapply(a, is.numeric)] %>% sort()
    })
    
    effective_split_choices <- function(breaks = list()) {
      choices <- SPLIT_CHOICES
      for (brk in breaks) {
        choices <- setdiff(choices, brk$column)
        choices <- c(choices, brk$names)
      }
      choices
    }
    
    main_data <- reactive({
      d <- data()
      d$all_rags %||% d$all_transformed
    })
    
    # ── Channel card renderer ──────────────────────────────────────
    render_channel_card <- function(nm, cfg, is_selected) {
      n_var  <- length(cfg$model_variables[nzchar(cfg$model_variables %||% "")])
      act_kw <- cfg$activity_keyword %||% "Clicks"
      n_brk  <- length(cfg$dimension_breaks  %||% list())
      n_seg  <- length(cfg$segment_overrides  %||% list())
      is_dirty <- isTRUE(dirty_channels[[nm]])
      
      border_color <- if (is_selected) "#5B9BD5"
      else if (n_var > 0) "#90caf9"
      else "#dee2e6"
      bg_color <- if (is_selected) "#EBF3FB" else "white"
      
      div(
        style = paste0(
          "border:1px solid ", border_color, ";",
          "border-left:4px solid ", border_color, ";",
          "border-radius:6px; background:", bg_color, ";",
          "padding:10px 12px; margin-bottom:8px;",
          "cursor:pointer; transition:all 0.15s; position:relative;"
        ),
        
        tags$span(icon("xmark"),
                  style = "position:absolute; top:6px; right:8px; color:#adb5bd; font-size:11px; cursor:pointer; padding:2px 4px;",
                  title = "Delete channel",
                  onclick = paste0("Shiny.setInputValue('", ns("delete_nm"), "','", nm,
                                   "',{priority:'event'});")),
        
        tags$span(icon("copy"),
                  style = "position:absolute; top:6px; right:30px; color:#adb5bd; font-size:11px; cursor:pointer; padding:2px 4px;",
                  title = "Duplicate channel",
                  onclick = paste0("Shiny.setInputValue('", ns("duplicate_nm"), "','", nm,
                                   "',{priority:'event'});")),
        
        tags$div(
          style = "cursor:pointer; padding-right:52px;",
          onclick = paste0("Shiny.setInputValue('", ns("select_nm"), "','", nm,
                           "',{priority:'event'});"),
          
          div(
            style = "display:flex; align-items:center; gap:5px; margin-bottom:5px;",
            tags$span(nm, style = paste0(
              "font-weight:600; font-size:13px; color:#2c3e50;",
              "overflow:hidden; text-overflow:ellipsis; white-space:nowrap; max-width:130px;")),
            if (is_dirty)
              tags$span("\u25CF",
                        style = "color:#f39c12; font-size:10px;",
                        title = "Unsaved changes")
          ),
          
          div(
            style = "display:flex; gap:10px; flex-wrap:wrap;",
            tags$span(
              icon("layer-group", style = "font-size:10px; color:#8a9bb0;"),
              tags$span(
                if (n_var == 0) "No variables" else paste0(n_var, " var", if (n_var > 1) "s" else ""),
                style = "font-size:11px; color:#6c757d;")),
            tags$span(
              icon("arrow-pointer", style = "font-size:10px; color:#8a9bb0;"),
              tags$span(act_kw, style = "font-size:11px; color:#6c757d;")),
            if (n_brk > 0)
              tags$span(
                icon("scissors", style = "font-size:10px; color:#8a9bb0;"),
                tags$span(paste0(n_brk, " break", if (n_brk > 1) "s" else ""),
                          style = "font-size:11px; color:#6c757d;")),
            if (n_seg > 0)
              tags$span(
                icon("location-dot", style = "font-size:10px; color:#8a9bb0;"),
                tags$span(paste0(n_seg, " seg geo"),
                          style = "font-size:11px; color:#6c757d;"))
          )
        )
      )
    }
    
    # ── Channel list ───────────────────────────────────────────────
    output$ch_list <- renderUI({
      nms <- names(rv$channels)
      if (!length(nms))
        return(div(
          style = "text-align:center; padding:24px 12px; color:#adb5bd; font-size:12px;",
          icon("layer-group",
               style = "font-size:28px; display:block; margin-bottom:8px; color:#dee2e6;"),
          "No channels yet.", tags$br(),
          "Type a name above and click Add,", tags$br(),
          "or load a saved config."))
      tagList(lapply(nms, function(nm) {
        render_channel_card(nm, rv$channels[[nm]], identical(rv$selected, nm))
      }))
    })
    
    # ── Add channel ────────────────────────────────────────────────
    observeEvent(input$btn_add, {
      nm <- trimws(input$new_name %||% "")
      if (!nchar(nm))
        return(showNotification("Enter a channel name.", type = "warning"))
      if (nm %in% names(rv$channels))
        return(showNotification("Channel already exists.", type = "warning"))
      rv$channels[[nm]] <- default_channel_config(nm)
      dirty_channels[[nm]] <- FALSE
      rv$selected <- nm
      updateTextInput(session, "new_name", value = "")
    })
    
    # ── Select channel ─────────────────────────────────────────────
    observeEvent(input$select_nm, {
      req(nzchar(input$select_nm %||% ""))
      if (input$select_nm %in% names(rv$channels))
        rv$selected <- input$select_nm
    }, ignoreInit = TRUE)
    
    # ── Delete with confirmation ───────────────────────────────────
    observeEvent(input$delete_nm, {
      req(nzchar(input$delete_nm %||% ""))
      pending_delete(input$delete_nm)
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", style = "color:#f39c12;"),
                        " Delete channel"),
        tags$p("Are you sure you want to delete channel ",
               tags$strong(input$delete_nm), "?"),
        tags$p(class = "text-muted small", "This action cannot be undone."),
        footer = tagList(
          actionButton(ns("btn_confirm_delete"), "Delete", class = "btn-danger"),
          modalButton("Cancel")),
        easyClose = TRUE, size = "s"
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_confirm_delete, {
      nm <- pending_delete(); req(!is.null(nm))
      rv$channels[[nm]]  <- NULL
      dirty_channels[[nm]] <- NULL
      if (identical(rv$selected, nm))
        rv$selected <- names(rv$channels)[1]
      pending_delete(NULL)
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── Duplicate channel ──────────────────────────────────────────
    observeEvent(input$duplicate_nm, {
      req(nzchar(input$duplicate_nm %||% ""))
      nm  <- input$duplicate_nm
      cfg <- rv$channels[[nm]]; req(cfg)
      new_nm <- paste0(nm, " (copy)")
      i <- 1L
      while (new_nm %in% names(rv$channels)) { i <- i + 1L; new_nm <- paste0(nm, " (copy ", i, ")") }
      cfg_copy <- cfg
      cfg_copy$channel_name       <- new_nm
      cfg_copy$saved_merges       <- list()
      cfg_copy$dimension_breaks   <- list()
      cfg_copy$segment_overrides  <- list()
      rv$channels[[new_nm]] <- cfg_copy
      dirty_channels[[new_nm]] <- FALSE
      rv$selected <- new_nm
      showNotification(paste0("'", nm, "' duplicated as '", new_nm, "'"), type = "message")
    }, ignoreInit = TRUE)
    
    # ── n_vars sync + mv_selections load ──────────────────────────
    observeEvent(rv$selected, {
      cfg <- rv$channels[[rv$selected]]; req(cfg)
      for (i in seq_len(5)) {
        mv_selections[[paste0("mv_", i)]] <-
          if (i <= length(cfg$model_variables) && nzchar(cfg$model_variables[i] %||% ""))
            cfg$model_variables[i] else ""
      }
      updateNumericInput(session, "ch_n_vars",
                         value = max(1L, sum(nzchar(cfg$model_variables %||% ""))))
      prevent_dirty(TRUE)
      session$onFlushed(function() {
        session$onFlushed(function() { prevent_dirty(FALSE) }, once = TRUE)
      }, once = TRUE)
    }, ignoreNULL = TRUE)
    
    # ── Dirty tracking ─────────────────────────────────────────────
    observeEvent(
      list(
        input$activity_radio, input$activity_other,
        input$spend_radio,    input$spend_other,
        input$ch_n_vars,      input$splits_selected,
        input$varname_include, input$varname_exclude,
        input$geography_exclude, input$campaign_exclude,
        input$outlet_exclude,    input$creative_exclude,
        input$seg_geo_exclude_1, input$seg_geo_exclude_2,
        input$seg_geo_exclude_3, input$seg_geo_exclude_4,
        input$seg_geo_exclude_5
      ),
      handlerExpr = {
        if (!is.null(rv$selected) && !isolate(prevent_dirty()))
          dirty_channels[[rv$selected]] <- TRUE
      },
      ignoreInit = TRUE
    )
    
    # ── CSV: Download config ───────────────────────────────────────
    output$dl_config_csv <- downloadHandler(
      filename = \() paste0("channel_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        df <- export_channels_csv(rv$channels)
        readr::write_csv(df, file, na = "")
      }
    )
    
    # ── CSV: Load config ───────────────────────────────────────────
    do_load_config_csv <- function(content) {
      tryCatch({
        con <- textConnection(content); on.exit(close(con), add = TRUE)
        df  <- read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
        if (!nrow(df) || !"Channel" %in% names(df) || !"Type" %in% names(df)) {
          showNotification("Invalid config CSV.", type = "error"); return()
        }
        loaded <- import_channels_csv(df)
        if (!length(loaded)) {
          showNotification("No Config rows found.", type = "warning"); return()
        }
        rv$channels <- loaded
        rv$selected <- names(loaded)[1]
        for (nm in names(loaded)) dirty_channels[[nm]] <- FALSE
        n_config <- sum(trimws(df$Type) == "Config")
        n_merge  <- sum(trimws(df$Type) == "Merge")
        n_break  <- sum(trimws(df$Type) == "Break")
        n_seg    <- sum(trimws(df$Type) == "Segment")
        showNotification(
          paste0("Loaded: ", n_config, " channel(s), ", n_merge, " merge(s), ",
                 n_break, " break(s), ", n_seg, " segment override(s)."),
          type = "message", duration = 5)
      }, error = \(e) showNotification(paste("Error loading CSV:", e$message),
                                       type = "error", duration = 10))
    }
    
    observeEvent(input$config_csv_content, {
      req(input$config_csv_content)
      if (length(rv$channels) > 0) {
        pending_load_cfg(input$config_csv_content)
        showModal(modalDialog(
          title = tagList(icon("triangle-exclamation", style = "color:#f39c12;"),
                          " Overwrite config"),
          tags$p("You have ", tags$strong(length(rv$channels)), " channel(s) configured."),
          tags$p("Loading a new config will ",
                 tags$strong("replace all current channels and merges"), "."),
          footer = tagList(
            actionButton(ns("btn_confirm_load_csv"), "Yes, overwrite", class = "btn-warning"),
            modalButton("Cancel")),
          easyClose = TRUE, size = "s"
        ))
      } else {
        do_load_config_csv(input$config_csv_content)
      }
    })
    
    observeEvent(input$btn_confirm_load_csv, {
      content <- pending_load_cfg(); req(!is.null(content))
      do_load_config_csv(content)
      pending_load_cfg(NULL)
      removeModal()
    }, ignoreInit = TRUE)
    
    # ── VOF Import ─────────────────────────────────────────────────
    observeEvent(input$vof_content, {
      req(input$vof_content, input$vof_name)
      ext <- tools::file_ext(input$vof_name)
      tryCatch({
        raw_data <- sub("^data:.*?;base64,", "", input$vof_content)
        tmp_file <- tempfile(fileext = paste0(".", ext))
        on.exit(unlink(tmp_file), add = TRUE)
        writeBin(jsonlite::base64_dec(raw_data), tmp_file)
        vof_df <- if (tolower(ext) %in% c("xlsx", "xls"))
          read_excel(tmp_file)
        else
          data.table::fread(tmp_file, data.table = FALSE, fill = TRUE)
        vof_df <- as.data.frame(vof_df)
        an_geos <- if (!is.null(data()$analytical) && "Geography" %in% names(data()$analytical))
          unique(data()$analytical$Geography) else character(0)
        parsed <- parse_vof_to_channels(vof_df, an_geos)
        if (!length(parsed)) {
          showNotification("No channels found in VOF.", type = "warning"); return()
        }
        pending_vof(parsed)
        showModal(modalDialog(
          title = tagList(icon("file-import"), " Import from VOF"),
          tags$p(style = "font-size:13px; margin-bottom:12px;",
                 tags$strong(length(parsed)), " channel(s) detected. Select which to import:"),
          div(style = "max-height:350px; overflow-y:auto;",
              tagList(lapply(names(parsed), function(nm) {
                cfg     <- parsed[[nm]]
                n_mv    <- length(cfg$model_variables)
                n_bk    <- length(cfg$break_dates)
                n_seg   <- length(cfg$segment_overrides)
                has_geo <- length(cfg$geography_exclude) > 0
                div(style = "display:flex; align-items:flex-start; gap:10px; padding:8px 0; border-bottom:1px solid #f0f0f0;",
                    checkboxInput(ns(paste0("vof_sel_", make.names(nm))), label = NULL, value = TRUE),
                    div(
                      tags$strong(nm, style = "font-size:13px; color:#2c3e50;"),
                      tags$div(style = "font-size:11.5px; color:#6c757d; margin-top:2px;",
                               paste0(n_mv, " model var", if (n_mv != 1) "s" else ""),
                               if (n_bk > 0) paste0(", ", n_bk, " break", if (n_bk != 1) "s" else "") else "",
                               if (n_seg > 0) paste0(", ", n_seg, " segment geo override", if (n_seg != 1) "s" else "") else "",
                               if (has_geo) paste0(", geographic (", length(cfg$geography_exclude), " geo excluded)") else ", national",
                               tags$br(),
                               tags$span(paste("include:", paste(cfg$varname_include, collapse = ", ")),
                                         style = "color:#5B9BD5;"))
                    )
                )
              }))
          ),
          if (length(rv$channels) > 0)
            div(class = "alert alert-warning mt-2 mb-0", style = "font-size:12px;",
                icon("triangle-exclamation"),
                " Channels with the same name will be overwritten."),
          footer = tagList(
            actionButton(ns("btn_confirm_vof"),
                         tagList(icon("file-import"), " Import Selected"), class = "btn-primary"),
            modalButton("Cancel")),
          easyClose = FALSE, size = "m"
        ))
      }, error = \(e) showNotification(paste("VOF parse error:", e$message),
                                       type = "error", duration = 10))
    })
    
    observeEvent(input$btn_confirm_vof, {
      parsed <- pending_vof(); req(!is.null(parsed))
      n_imported <- 0L
      for (nm in names(parsed)) {
        key <- paste0("vof_sel_", make.names(nm))
        if (isTRUE(input[[key]])) {
          rv$channels[[nm]] <- parsed[[nm]]
          dirty_channels[[nm]] <- FALSE
          n_imported <- n_imported + 1L
        }
      }
      if (n_imported > 0 && is.null(rv$selected))
        rv$selected <- names(rv$channels)[1]
      pending_vof(NULL)
      removeModal()
      showNotification(paste0(n_imported, " channel(s) imported from VOF."), type = "message")
    }, ignoreInit = TRUE)
    
    # ── Editor title ───────────────────────────────────────────────
    output$editor_title <- renderUI({
      if (is.null(rv$selected)) return("Select or add a channel")
      dirty <- isTRUE(dirty_channels[[rv$selected]])
      tagList(
        "Editing: ", tags$strong(rv$selected),
        if (dirty) tags$span("\u25CF", style = "color:#f39c12; font-size:13px; margin-left:3px;"),
        tags$span(
          if (dirty) "Unsaved changes" else "(changes saved only on click Save)",
          style = paste0(
            "font-size:11px; font-weight:400; margin-left:8px;",
            if (dirty) "color:#f39c12;" else "color:#8a9bb0;"
          )
        )
      )
    })
    
    # ── Editor form ────────────────────────────────────────────────
    output$editor_form <- renderUI({
      req(rv$selected, rv$channels[[rv$selected]])
      cfg  <- rv$channels[[rv$selected]]
      n    <- max(1L, length(cfg$model_variables))
      avail_choices <- effective_split_choices(cfg$dimension_breaks %||% list())
      sel_act   <- if (cfg$activity_keyword %in% c("Clicks", "Impressions"))
        cfg$activity_keyword else "Other"
      sel_spend <- if (cfg$spend_keyword %in% c("Cost", "Spend"))
        cfg$spend_keyword else "Other"
      global_geo_hint <- paste(cfg$geography_exclude %||% character(0), collapse = ", ")
      
      tagList(
        
        # Row 1: Activity + Spend
        div(
          style = paste0("background:#f4f6f9; border-radius:8px;",
                         "padding:10px 18px; margin-bottom:14px;",
                         "display:flex; align-items:center; gap:20px; flex-wrap:wrap;"),
          div(style = "display:flex; align-items:center; gap:8px;",
              tags$span(icon("arrow-pointer", style = "color:#5B9BD5;"),
                        tags$strong("Activity:", style = "font-size:12.5px; color:#333; white-space:nowrap;")),
              div(style = "width:130px;",
                  selectInput(ns("activity_radio"), NULL,
                              choices = c("Clicks", "Impressions", "Other"), selected = sel_act, width = "100%")),
              uiOutput(ns("activity_other_ui"))
          ),
          div(style = "width:1px; height:28px; background:#dee2e6; flex-shrink:0;"),
          div(style = "display:flex; align-items:center; gap:8px;",
              tags$span(icon("dollar-sign", style = "color:#5B9BD5;"),
                        tags$strong("Spend:", style = "font-size:12.5px; color:#333; white-space:nowrap;")),
              div(style = "width:130px;",
                  selectInput(ns("spend_radio"), NULL,
                              choices = c("Cost", "Spend", "Other"), selected = sel_spend, width = "100%")),
              uiOutput(ns("spend_other_ui"))
          )
        ),
        
        # Row 2: Split Dimensions + Filters
        layout_columns(
          col_widths = c(5, 7),
          
          card(
            card_header(div(
              style = "display:flex; align-items:center; gap:10px;",
              tags$span(icon("arrows-up-down"), " Split Dimensions"),
              tags$small("Drag to set order — left = excluded",
                         style = "color:#8a9bb0; font-size:11px;")
            )),
            bucket_list(
              header     = NULL,
              group_name = ns("split_bucket"),
              orientation = "horizontal",
              add_rank_list(text = "Available",
                            labels   = setdiff(avail_choices, cfg$split_columns),
                            input_id = ns("splits_available")),
              add_rank_list(text = "Split Order",
                            labels   = cfg$split_columns,
                            input_id = ns("splits_selected"))
            )
          ),
          
          card(
            card_header(tags$span(icon("filter"), " Filters")),
            div(style = "display:grid; grid-template-columns:1fr 1fr; gap:8px;",
                div(tags$label("VarName — contains", class = "filter-label"),
                    textAreaInput(ns("varname_include"), NULL,
                                  value = paste(cfg$varname_include %||% character(0), collapse = "\n"),
                                  rows = 2, placeholder = "e.g. Audio\nOLV")),
                div(tags$label("VarName — exclude", class = "filter-label"),
                    textAreaInput(ns("varname_exclude"), NULL,
                                  value = paste(cfg$varname_exclude, collapse = "\n"),
                                  rows = 2, placeholder = "e.g. Impressions\nLocal")),
                div(tags$label("Geography — exclude", class = "filter-label"),
                    textAreaInput(ns("geography_exclude"), NULL,
                                  value = paste(cfg$geography_exclude %||% character(0), collapse = "\n"),
                                  rows = 2, placeholder = "Global — applies to all segments")),
                div(tags$label("Campaign — exclude", class = "filter-label"),
                    textAreaInput(ns("campaign_exclude"), NULL,
                                  value = paste(cfg$campaign_exclude, collapse = "\n"),
                                  rows = 2, placeholder = "e.g. Pmax")),
                div(tags$label("Outlet — exclude", class = "filter-label"),
                    textAreaInput(ns("outlet_exclude"), NULL,
                                  value = paste(cfg$outlet_exclude %||% character(0), collapse = "\n"),
                                  rows = 2, placeholder = "e.g. TV")),
                div(tags$label("Creative — exclude", class = "filter-label"),
                    textAreaInput(ns("creative_exclude"), NULL,
                                  value = paste(cfg$creative_exclude %||% character(0), collapse = "\n"),
                                  rows = 2, placeholder = "e.g. 15s"))
            )
          )
        ),
        
        # Row 3: Model Variables
        card(
          card_header(div(
            style = "display:flex; align-items:center; justify-content:space-between;",
            tags$span(icon("layer-group"), " Model Variables ",
                      tags$strong("& Time Breaks")),
            div(style = "display:flex; align-items:center; gap:8px;",
                tags$small("Number of variables:",
                           style = "color:#6c757d; font-size:11px; white-space:nowrap;"),
                div(style = "width:68px;",
                    numericInput(ns("ch_n_vars"), NULL, value = n,
                                 min = 1, max = 5, step = 1)))
          )),
          uiOutput(ns("mv_inputs"))
        ),
        
        # Save button
        actionButton(ns("btn_save"),
                     tagList(icon("floppy-disk"), " Save Channel Config"),
                     class = "btn-success w-100 mt-2")
      )
    })
    
    # ── Model variable inputs ──────────────────────────────────────
    output$mv_inputs <- renderUI({
      req(input$ch_n_vars, rv$selected)
      cfg  <- rv$channels[[rv$selected]]
      n    <- input$ch_n_vars
      global_geo_hint <- paste(cfg$geography_exclude %||% character(0), collapse = ", ")
      
      tagList(lapply(seq_len(n), function(i) {
        current_val <- mv_selections[[paste0("mv_", i)]] %||% ""
        seg_ovr_val <- {
          matches <- Filter(\(o) isTRUE(o$seg == i), cfg$segment_overrides %||% list())
          if (length(matches) > 0)
            paste(matches[[1]]$geography_exclude %||% character(0), collapse = "\n")
          else ""
        }
        
        div(
          style = paste0(
            "background:#fafcff; border-radius:6px;",
            "padding:10px 12px; margin-bottom:8px; border:1px solid #e3e8ef;"),
          
          tags$label(paste("Model Variable", i),
                     style = paste0(
                       "font-size:12px; font-weight:600;",
                       "color:", if (i == 1) "#5B9BD5;" else "#2c3e50;",
                       " display:block; margin-bottom:8px;")),
          
          # Variable picker row
          div(
            style = "display:flex; align-items:stretch; gap:8px; margin-bottom:10px;",
            div(
              style = paste0(
                "flex:1; background:white;",
                "border:1px solid ", if (nzchar(current_val)) "#5B9BD5" else "#dde5ef", ";",
                "border-radius:5px; padding:8px 12px;",
                "font-size:12.5px; line-height:1.5;",
                "word-break:break-all; min-height:38px;"),
              if (nzchar(current_val))
                tags$span(current_val, style = "color:#2c3e50;")
              else
                tags$span("Click Browse to select a variable...",
                          style = "color:#adb5bd; font-style:italic;")
            ),
            tags$button(
              tagList(icon("magnifying-glass"), " Browse"),
              class = "btn btn-outline-secondary btn-sm",
              style = "white-space:nowrap; flex-shrink:0;",
              onclick = paste0(
                "Shiny.setInputValue('", ns("browse_mv_slot"), "',",
                "'mv_", i, "',{priority:'event'});")
            )
          ),
          
          # Segment geography exclude
          div(
            style = paste0(
              "background:#f9f4ff; border:1px solid #e0d4f7;",
              "border-radius:5px; padding:8px 10px;"),
            div(
              style = "display:flex; align-items:center; gap:6px; margin-bottom:5px;",
              icon("location-dot", style = "color:#8a9bb0; font-size:11px;"),
              tags$span(paste0("Geography exclude — Segment ", i, " only"),
                        style = "font-size:11.5px; font-weight:600; color:#5b4b7a;"),
              if (nzchar(global_geo_hint))
                tags$span(paste0("(global: ", global_geo_hint, ")"),
                          style = "font-size:10.5px; color:#adb5bd; margin-left:4px;")
            ),
            textAreaInput(ns(paste0("seg_geo_exclude_", i)), NULL,
                          value = seg_ovr_val, rows = 2, width = "100%",
                          placeholder = if (nzchar(global_geo_hint))
                            paste0("Leave empty to use global (", global_geo_hint, ")")
                          else "Optional: one geo per line — overrides global for this segment only"
            ) %>% tagAppendAttributes(style = "margin-bottom:0; font-size:12.5px;")
          ),
          
          # Break date
          if (i < n)
            div(
              style = "margin-top:10px;",
              tags$label(paste0("Break after Var ", i),
                         style = "font-size:11px; font-weight:600; color:#6c757d; display:block; margin-bottom:4px;"),
              dateInput(ns(paste0("bd_", i)), NULL,
                        value = if (i <= length(cfg$break_dates))
                          as.Date(cfg$break_dates[i]) else Sys.Date())
            )
        )
      }))
    })
    
    # ── Open variable picker modal ─────────────────────────────────
    observeEvent(input$browse_mv_slot, {
      req(input$browse_mv_slot)
      pending_mv_slot(input$browse_mv_slot)
      updateTextInput(session, "mv_search", value = "")
      slot_num <- sub("mv_", "", input$browse_mv_slot)
      showModal(modalDialog(
        title = tagList(icon("magnifying-glass"), paste0(" Select Model Variable ", slot_num)),
        size  = "l",
        div(style = "margin-bottom:10px;",
            textInput(ns("mv_search"), NULL,
                      placeholder = "Search variables...", width = "100%")),
        uiOutput(ns("mv_current_banner")),
        div(style = "height:420px; overflow-y:auto; border:1px solid #e3e8ef; border-radius:6px;",
            uiOutput(ns("mv_picker_list"))),
        hr(style = "margin:12px 0 8px;"),
        div(style = "display:flex; gap:8px; align-items:flex-end;",
            div(style = "flex:1;",
                tags$label("Or enter manually:",
                           style = "font-size:12px; color:#6c757d; display:block; margin-bottom:4px;"),
                textInput(ns("mv_custom_input"), NULL, value = "",
                          placeholder = "Paste or type a variable name...", width = "100%")),
            actionButton(ns("btn_mv_use_custom"), tagList(icon("check"), " Use"),
                         class = "btn-outline-secondary btn-sm", style = "margin-bottom:1px;")
        ),
        footer = modalButton("Cancel"),
        easyClose = TRUE
      ))
    })
    
    output$mv_current_banner <- renderUI({
      slot    <- pending_mv_slot(); req(slot)
      current <- mv_selections[[slot]] %||% ""
      if (!nzchar(current)) return(NULL)
      div(class = "alert p-2 mb-2",
          style = "background:#EBF3FB; border:1px solid #90caf9; font-size:12px; border-radius:5px;",
          icon("circle-check", style = "color:#5B9BD5;"),
          " Selected: ",
          tags$strong(current, style = "color:#2c3e50; word-break:break-all;"))
    })
    
    output$mv_picker_list <- renderUI({
      vars    <- analytical_vars()
      slot    <- pending_mv_slot(); req(slot)
      current <- mv_selections[[slot]] %||% ""
      search  <- tolower(trimws(input$mv_search %||% ""))
      if (!length(vars))
        return(div(style = "text-align:center; padding:30px; color:#adb5bd;",
                   icon("database",
                        style = "font-size:28px; display:block; margin-bottom:8px; color:#dee2e6;"),
                   "Upload AnalyticalDataset to see available variables."))
      if (nzchar(search)) vars <- vars[grepl(search, tolower(vars), fixed = TRUE)]
      if (!length(vars))
        return(div(style = "text-align:center; padding:24px; color:#adb5bd; font-size:13px;",
                   icon("magnifying-glass"),
                   paste0(' No variables match "', input$mv_search, '"')))
      tagList(lapply(vars, function(v) {
        is_sel <- identical(v, current)
        div(
          style = paste0(
            "padding:10px 16px; cursor:pointer; font-size:13px;",
            "border-bottom:1px solid #f4f6f9;",
            "word-break:break-all; line-height:1.5;",
            "transition:background 0.1s;",
            if (is_sel) "background:#EBF3FB; font-weight:600; border-left:3px solid #5B9BD5;"
            else "color:#2c3e50;"
          ),
          onmouseover = if (!is_sel) "this.style.background='#f8f9fa';" else NULL,
          onmouseout  = if (!is_sel) "this.style.background='';" else NULL,
          onclick = paste0(
            "Shiny.setInputValue('", ns("mv_picker_clicked"), "','",
            gsub("'", "\\\\'", v), "',{priority:'event'});"),
          div(style = "display:flex; align-items:flex-start; gap:10px;",
              if (is_sel)
                icon("circle-check",
                     style = "color:#5B9BD5; font-size:14px; margin-top:2px; flex-shrink:0;")
              else
                div(style = "width:14px; flex-shrink:0;"),
              tags$span(v, style = if (is_sel) "color:#2c3e50;" else ""))
        )
      }))
    })
    
    observeEvent(input$mv_picker_clicked, {
      req(input$mv_picker_clicked, pending_mv_slot())
      slot <- pending_mv_slot()
      mv_selections[[slot]] <- input$mv_picker_clicked
      if (!is.null(rv$selected)) dirty_channels[[rv$selected]] <- TRUE
      removeModal(); pending_mv_slot(NULL)
      updateTextInput(session, "mv_search", value = "")
    })
    
    observeEvent(input$btn_mv_use_custom, {
      req(pending_mv_slot())
      val <- trimws(input$mv_custom_input %||% "")
      if (!nzchar(val)) { showNotification("Enter a variable name.", type = "warning"); return() }
      slot <- pending_mv_slot()
      mv_selections[[slot]] <- val
      if (!is.null(rv$selected)) dirty_channels[[rv$selected]] <- TRUE
      removeModal(); pending_mv_slot(NULL)
    })
    
    # ── Keyword other inputs ───────────────────────────────────────
    output$activity_other_ui <- renderUI({
      req(input$activity_radio == "Other")
      cfg <- rv$channels[[rv$selected]]
      ov  <- if (!is.null(cfg) && !cfg$activity_keyword %in% c("Clicks", "Impressions"))
        cfg$activity_keyword else ""
      div(style = "width:110px;",
          textInput(ns("activity_other"), NULL, value = ov, placeholder = "Custom..."))
    })
    
    output$spend_other_ui <- renderUI({
      req(input$spend_radio == "Other")
      cfg <- rv$channels[[rv$selected]]
      ov  <- if (!is.null(cfg) && !cfg$spend_keyword %in% c("Cost", "Spend"))
        cfg$spend_keyword else ""
      div(style = "width:110px;",
          textInput(ns("spend_other"), NULL, value = ov, placeholder = "Custom..."))
    })
    
    # ── Save channel config ────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(rv$selected, input$ch_n_vars)
      n <- input$ch_n_vars
      
      mvars   <- vapply(seq_len(n), \(i) mv_selections[[paste0("mv_", i)]] %||% "", character(1))
      brdates <- if (n > 1)
        vapply(seq_len(n - 1), \(i) as.character(input[[paste0("bd_", i)]]), character(1))
      else character(0)
      
      parse_lines <- \(x) Filter(\(l) nchar(trimws(l)) > 0, strsplit(x %||% "", "\n")[[1]])
      
      activity_kw <- if (isTRUE(input$activity_radio == "Other"))
        trimws(input$activity_other %||% "Clicks")
      else input$activity_radio %||% "Clicks"
      
      spend_kw <- if (isTRUE(input$spend_radio == "Other"))
        trimws(input$spend_other %||% "Spend")
      else input$spend_radio %||% "Spend"
      
      existing_merges <- rv$channels[[rv$selected]]$saved_merges   %||% list()
      existing_breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      
      segment_overrides <- Filter(\(o) length(o$geography_exclude) > 0,
                                  lapply(seq_len(n), function(i) {
                                    geo <- parse_lines(input[[paste0("seg_geo_exclude_", i)]])
                                    list(seg = i, geography_exclude = geo)
                                  })
      )
      
      rv$channels[[rv$selected]] <- list(
        channel_name      = rv$selected,
        model_variables   = mvars,
        break_dates       = brdates,
        varname_include   = parse_lines(input$varname_include),
        varname_exclude   = parse_lines(input$varname_exclude),
        geography_exclude = parse_lines(input$geography_exclude),
        campaign_exclude  = parse_lines(input$campaign_exclude),
        outlet_exclude    = parse_lines(input$outlet_exclude),
        creative_exclude  = parse_lines(input$creative_exclude),
        split_columns     = input$splits_selected %||% c("VariableName", "Campaign"),
        activity_keyword  = if (nchar(activity_kw)) activity_kw else "Clicks",
        spend_keyword     = if (nchar(spend_kw))    spend_kw    else "Spend",
        saved_merges      = existing_merges,
        dimension_breaks  = existing_breaks,
        segment_overrides = segment_overrides
      )
      
      dirty_channels[[rv$selected]] <- FALSE
      showNotification(paste("Saved:", rv$selected), type = "message")
    })
    
    # ══ DIMENSION SUMMARY ══════════════════════════════════════════
    
    strip_keywords <- reactive({
      kw <- trimws(input$var_keywords %||% "")
      if (!nzchar(kw)) return(character(0))
      Filter(nzchar, trimws(strsplit(kw, ",")[[1]]))
    })
    
    observeEvent(input$btn_detect_keywords, {
      md <- main_data(); req(md)
      var_names  <- unique(md$VariableName)
      last_words <- str_extract(var_names, "\\S+$")
      counts     <- table(last_words)
      keywords   <- sort(names(counts[counts > 1]))
      if (!length(keywords)) {
        showNotification("No repeating last-words found.", type = "warning"); return()
      }
      updateTextInput(session, "var_keywords", value = paste(keywords, collapse = ", "))
      showNotification(paste0("Detected: ", paste(keywords, collapse = ", ")),
                       type = "message", duration = 4)
    })
    
    # ── Dimension table with coverage highlighting ─────────────────
    output$dimension_table <- DT::renderDT({
      md <- main_data()
      if (is.null(md)) {
        return(datatable(
          data.frame(Message = "Upload the main data file to see the dimension summary."),
          options  = list(dom = "t", initComplete = dt_blue_callback),
          rownames = FALSE
        ))
      }
      
      kws <- strip_keywords()
      df  <- md
      
      if (length(kws) > 0) {
        pattern <- paste0("\\s+(", paste(kws, collapse = "|"), ")$")
        df <- df %>% mutate(BaseVar = str_remove(VariableName, regex(pattern, ignore_case = TRUE)))
      } else {
        df <- df %>% mutate(BaseVar = VariableName)
      }
      
      summary_df <- df %>%
        group_by(BaseVar) %>%
        summarise(
          Campaign = n_distinct(Campaign),
          Outlet   = n_distinct(Outlet),
          Creative = n_distinct(Creative),
          .groups  = "drop"
        ) %>%
        arrange(desc(Campaign), BaseVar) %>%
        rename(`Variable Name` = BaseVar)
      
      # ── Coverage: which rows match current channel's varname_include ──
      cfg <- rv$channels[[rv$selected]]
      vi  <- if (!is.null(cfg))
        cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      else character(0)
      
      summary_df <- summary_df %>%
        mutate(
          covered = if (length(vi) > 0)
            sapply(`Variable Name`, function(vn)
              any(sapply(vi, function(p) grepl(p, vn, ignore.case = TRUE))))
          else FALSE,
          covered_int = as.integer(covered)
        )
      
      # Store for row-click observer
      dim_table_data(summary_df)
      
      # Display: hide the helper column
      display_df <- summary_df %>%
        select(`Variable Name`, Campaign, Outlet, Creative, covered_int)
      
      datatable(
        display_df,
        selection = list(mode = "single", target = "row"),
        options   = list(
          scrollX    = TRUE,
          scrollY    = "320px",
          paging     = FALSE,
          dom        = "ft",
          initComplete = dt_blue_callback,
          columnDefs = list(
            list(className = "dt-left",   targets = 0),
            list(className = "dt-center", targets = c(1, 2, 3)),
            list(visible = FALSE,         targets = 4)   # hide covered_int
          )
        ),
        rownames = FALSE
      ) %>%
        formatStyle("Variable Name", fontWeight = "600") %>%
        # Green row = covered, white = not covered
        formatStyle(
          "covered_int",
          target          = "row",
          backgroundColor = styleEqual(c(0, 1), c("white",   "#f0fdf4")),
          color           = styleEqual(c(0, 1), c("#2c3e50", "#166534"))
        )
    }, server = FALSE)
    
    # ── Coverage badge ─────────────────────────────────────────────
    output$dim_coverage_badge <- renderUI({
      tbl <- dim_table_data()
      if (is.null(tbl) || !nrow(tbl)) return(NULL)
      req(rv$selected)
      
      n_covered   <- sum(tbl$covered, na.rm = TRUE)
      n_total     <- nrow(tbl)
      n_uncovered <- n_total - n_covered
      
      div(
        style = "display:flex; gap:6px; align-items:center; flex-shrink:0;",
        tags$span(
          style = paste0(
            "background:#dcfce7; color:#166534;",
            "padding:3px 10px; border-radius:10px;",
            "font-size:11.5px; font-weight:600;"),
          icon("circle-check", style = "font-size:10px;"),
          paste0(" ", n_covered, " covered")
        ),
        if (n_uncovered > 0)
          tags$span(
            style = paste0(
              "background:#f8f9fa; color:#6c757d;",
              "padding:3px 10px; border-radius:10px;",
              "font-size:11.5px; border:1px solid #e2e8f0;"),
            paste0(n_uncovered, " uncovered")
          )
      )
    })
    
    # ── Dimension table row click → add to varname_include ─────────
    observeEvent(input$dimension_table_rows_selected, {
      req(rv$selected, dim_table_data())
      
      sel_idx <- input$dimension_table_rows_selected
      if (!length(sel_idx)) return()
      
      tbl      <- dim_table_data()
      var_name <- tbl$`Variable Name`[sel_idx]
      
      cfg <- rv$channels[[rv$selected]]
      vi  <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      
      # Already exactly in the list
      if (var_name %in% vi) {
        showNotification(
          paste0("'", var_name, "' is already in VarName filter."),
          type = "warning", duration = 3
        )
        DT::dataTableProxy(session$ns("dimension_table")) %>% DT::selectRows(NULL)
        return()
      }
      
      # Add to varname_include
      new_vi <- c(vi, var_name)
      rv$channels[[rv$selected]]$varname_include <- new_vi
      dirty_channels[[rv$selected]] <- TRUE
      
      # Sync the textarea in Channel Editor if it's currently rendered
      tryCatch(
        updateTextAreaInput(session, "varname_include",
                            value = paste(new_vi, collapse = "\n")),
        error = function(e) NULL
      )
      
      showNotification(
        tagList(
          icon("circle-check", style = "color:#16a34a;"),
          paste0(" '", var_name, "' added to VarName filter")
        ),
        type = "message", duration = 3
      )
      
      # Deselect to allow re-clicking
      DT::dataTableProxy(session$ns("dimension_table")) %>% DT::selectRows(NULL)
      
    }, ignoreInit = TRUE)
    
    # ── Breaks header ──────────────────────────────────────────────
    output$breaks_header <- renderUI({
      channel_label <- if (!is.null(rv$selected))
        tags$span(rv$selected, style = "color:#5B9BD5; font-weight:700;")
      else
        tags$span("no channel selected", style = "color:#adb5bd; font-style:italic;")
      div(
        tags$strong("Dimension Breaks",
                    style = "font-size:13px; color:#2c3e50; display:block;"),
        tags$small(tagList("Breaks for channel: ", channel_label),
                   style = "color:#6c757d; font-size:11px;")
      )
    })
    
    # ── Breaks list ────────────────────────────────────────────────
    output$breaks_list <- renderUI({
      req(rv$selected)
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!length(breaks))
        return(tags$p(class = "text-muted small mb-0",
                      'No breaks configured. Click "Add Break" to create one.'))
      tagList(lapply(seq_along(breaks), function(i) {
        brk <- breaks[[i]]
        div(
          style = paste0(
            "display:flex; align-items:center; gap:8px;",
            "padding:7px 10px; border-radius:6px; margin-bottom:5px;",
            "background:#f4f6f9; border-left:3px solid #5B9BD5;"),
          icon("scissors", style = "color:#5B9BD5; font-size:12px; flex-shrink:0;"),
          div(style = "flex:1; font-size:12.5px;",
              tags$strong(brk$column, style = "color:#2c3e50;"),
              tags$span(paste0(" by \"", brk$separator, "\" \u2192 "),
                        style = "color:#6c757d;"),
              tags$span(paste(brk$names, collapse = " | "),
                        style = "color:#5B9BD5; font-weight:600;")),
          actionButton(ns(paste0("remove_break_", i)), icon("xmark"),
                       class = "btn btn-link p-0",
                       style = "color:#adb5bd; min-height:0; min-width:0; font-size:12px;")
        )
      }))
    })
    
    # ── Dynamic remove-break observers ────────────────────────────
    observe({
      req(rv$selected)
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!is.null(session$userData$remove_break_obs))
        lapply(session$userData$remove_break_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      session$userData$remove_break_obs <- lapply(seq_along(breaks), function(i) {
        local({
          local_i  <- i
          local_nm <- rv$selected
          observeEvent(input[[paste0("remove_break_", local_i)]], {
            curr <- rv$channels[[local_nm]]$dimension_breaks %||% list()
            if (local_i <= length(curr)) {
              brk <- curr[[local_i]]
              rv$channels[[local_nm]]$dimension_breaks <- curr[-local_i]
              rv$channels[[local_nm]]$split_columns <- setdiff(
                rv$channels[[local_nm]]$split_columns %||% character(0), brk$names)
              showNotification(paste0("Break on '", brk$column, "' removed."),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
    })
    
    # ── Modal: Add Break ───────────────────────────────────────────
    observeEvent(input$btn_add_break, {
      req(rv$selected)
      md <- main_data()
      if (is.null(md)) {
        showNotification("Upload the main data file first.", type = "warning"); return()
      }
      already_broken <- sapply(rv$channels[[rv$selected]]$dimension_breaks %||% list(),
                               \(b) b$column)
      available <- setdiff(c("Campaign", "Outlet", "Creative"), already_broken)
      if (!length(available)) {
        showNotification("All breakable dimensions already have a break.", type = "warning"); return()
      }
      kws <- strip_keywords()
      df  <- md
      if (length(kws) > 0) {
        pattern <- paste0("\\s+(", paste(kws, collapse = "|"), ")$")
        df <- df %>% mutate(BaseVar = str_remove(VariableName, regex(pattern, ignore_case = TRUE)))
      } else {
        df <- df %>% mutate(BaseVar = VariableName)
      }
      showModal(modalDialog(
        title = tagList(icon("scissors"), " Configure Dimension Break"),
        selectInput(ns("break_col"), "Column to break", choices = available),
        layout_columns(col_widths = c(8, 4),
                       textInput(ns("break_sep"), "Separator", value = "_", placeholder = "e.g. _"),
                       div(numericInput(ns("break_n"), "Parts", value = 2, min = 2, max = 5, step = 1))),
        uiOutput(ns("break_preview_ui")),
        uiOutput(ns("break_names_ui")),
        footer = tagList(
          actionButton(ns("btn_confirm_break"), tagList(icon("check"), " Add Break"),
                       class = "btn-primary"),
          modalButton("Cancel")),
        easyClose = FALSE, size = "m"
      ))
    })
    
    output$break_preview_ui <- renderUI({
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% "_"
      n   <- as.integer(input$break_n %||% 2)
      md  <- main_data()
      if (is.null(md) || !col %in% names(md))
        return(tags$p(class = "text-muted small mt-2",
                      "Upload the data file to see a preview."))
      df <- md
      if (!is.null(rv$selected)) {
        cfg <- rv$channels[[rv$selected]]
        vi  <- cfg$varname_include[nzchar(cfg$varname_include %||% "") > 0]
        if (length(vi) > 0) {
          keep <- Reduce("|", lapply(vi, function(p)
            grepl(p, df$VariableName, ignore.case = TRUE)))
          df <- df[keep, ]
        }
        for (p in cfg$varname_exclude)
          if (nchar(p %||% "") > 0)
            df <- df[!grepl(p, df$VariableName, ignore.case = TRUE), ]
      }
      if (nrow(df) == 0)
        return(tags$p(class = "text-muted small mt-1",
                      "No data matches this channel's filters."))
      vals    <- head(sort(unique(as.character(df[[col]]))), 8)
      parts   <- lapply(vals, function(v) strsplit(v, sep, fixed = TRUE)[[1]])
      n_short <- sum(sapply(parts, length) < n)
      rows <- lapply(seq_along(vals), function(i) {
        p       <- parts[[i]]; is_warn <- length(p) < n
        cells <- c(
          list(tags$td(vals[i],
                       style = "font-size:11px; padding:3px 8px; color:#6c757d; border-right:1px solid #e3e8ef;")),
          lapply(seq_len(n), function(j) {
            val <- if (length(p) < j) p[length(p)]
            else if (j == n) paste(p[j:length(p)], collapse = sep)
            else p[j]
            tags$td(val, style = paste0(
              "font-size:11px; padding:3px 8px;",
              if (is_warn) " color:#856404;" else " color:#2c3e50;"))
          })
        )
        do.call(tags$tr, cells)
      })
      header <- c(
        list(tags$th("Original",
                     style = "padding:3px 8px; font-size:11px; color:#6c757d;")),
        lapply(seq_len(n), function(j) {
          tags$th(paste0("Part ", j),
                  style = "padding:3px 8px; font-size:11px; color:#2c3e50;")
        })
      )
      tagList(
        tags$strong("Preview:", style = "font-size:12px; display:block; margin:6px 0 4px;"),
        div(style = "overflow-x:auto; border:1px solid #e3e8ef; border-radius:5px; margin-bottom:6px;",
            tags$table(class = "table table-sm", style = "margin-bottom:0;",
                       tags$thead(tags$tr(style = "border-bottom:1px solid #5B9BD5;",
                                          do.call(tagList, header))),
                       tags$tbody(do.call(tagList, rows))
            )
        ),
        if (n_short > 0)
          div(class = "small", style = "color:#856404;",
              icon("triangle-exclamation"),
              paste0(" ", n_short, " value(s) have fewer parts. Last available part used as fallback."))
      )
    })
    
    output$break_names_ui <- renderUI({
      col <- input$break_col %||% "Campaign"
      n   <- as.integer(input$break_n %||% 2)
      tagList(
        tags$strong("Name each part:",
                    style = "font-size:12px; display:block; margin:10px 0 6px;"),
        lapply(seq_len(n), function(i) {
          div(style = "display:flex; align-items:center; gap:8px; margin-bottom:6px;",
              tags$span(paste0("Part ", i, ":"),
                        style = "font-size:12px; font-weight:600; width:55px; flex-shrink:0; color:#4a5568;"),
              textInput(ns(paste0("break_part_", i)), NULL,
                        value = paste0(col, "_", LETTERS[i]), width = "100%") %>%
                tagAppendAttributes(style = "margin-bottom:0;"))
        })
      )
    })
    
    observeEvent(input$btn_confirm_break, {
      req(rv$selected)
      col  <- input$break_col %||% "Campaign"
      sep  <- input$break_sep %||% "_"
      n    <- as.integer(input$break_n %||% 2)
      part_names <- sapply(seq_len(n), function(i) {
        trimws(input[[paste0("break_part_", i)]] %||% paste0(col, "_", LETTERS[i]))
      })
      if (any(!nzchar(part_names))) {
        showNotification("All part names must be non-empty.", type = "warning"); return()
      }
      if (length(unique(part_names)) < n) {
        showNotification("Part names must be unique.", type = "warning"); return()
      }
      conflicts <- intersect(part_names, SPLIT_CHOICES)
      if (length(conflicts) > 0) {
        showNotification(paste0("Names conflict with existing choices: ",
                                paste(conflicts, collapse = ", ")), type = "warning"); return()
      }
      nm          <- rv$selected
      existing    <- rv$channels[[nm]]$dimension_breaks %||% list()
      rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
        column    = col,
        separator = sep,
        n_parts   = n,
        names     = part_names
      )))
      removeModal()
      showNotification(paste0("Break added: ", col, " \u2192 ",
                              paste(part_names, collapse = " | ")),
                       type = "message")
    })
    
    # ── Return ─────────────────────────────────────────────────────
    list(
      channels      = reactive(rv$channels),
      update_merges = function(nm, merges) {
        rv$channels[[nm]]$saved_merges <- merges
      }
    )
  })
}

# ── Default config ──────────────────────────────────────────────────
default_channel_config <- function(nm) {
  list(
    channel_name      = nm,
    model_variables   = "",
    break_dates       = character(0),
    varname_include   = character(0),
    varname_exclude   = character(0),
    geography_exclude = character(0),
    campaign_exclude  = character(0),
    outlet_exclude    = character(0),
    creative_exclude  = character(0),
    split_columns     = c("VariableName", "Campaign"),
    activity_keyword  = "Clicks",
    spend_keyword     = "Spend",
    saved_merges      = list(),
    dimension_breaks  = list(),
    segment_overrides = list()
  )
}