# ══════════════════════════════════════════════════════════════════
# R/mod_channels.R
# ══════════════════════════════════════════════════════════════════

mod_channels_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    
    # ── Left: Channel list ───────────────────────────────────────
    card(
      card_header("Channel List"),
      
      # Add new channel
      div(
        style = "display:flex; gap:8px; margin-bottom:10px;",
        textInput(ns("new_name"), NULL,
                  placeholder = "Channel name...") %>%
          tagAppendAttributes(style = "margin-bottom:0;"),
        actionButton(ns("btn_add"), "Add",
                     class = "btn-primary btn-sm",
                     style = "white-space:nowrap; align-self:flex-end;
                            margin-bottom:1px;")
      ),
      
      # Save / Load Config
      div(
        style = paste0("display:flex; gap:6px; margin-bottom:10px;",
                       "padding:8px 0;",
                       "border-top:1px solid #e3e8ef;",
                       "border-bottom:1px solid #e3e8ef;"),
        downloadButton(
          ns("dl_config"),
          label = tagList(icon("download"), " Save Config"),
          class = "btn-outline-secondary btn-sm",
          style = "flex:1; font-size:12px;"
        ),
        div(
          style = "flex:1; position:relative;",
          tags$label(
            style = paste0(
              "display:flex; align-items:center;",
              "justify-content:center; gap:5px;",
              "width:100%; height:100%;",
              "border:1px solid #c8d6e5;",
              "border-radius:5px; background:white;",
              "color:#5B9BD5; font-size:12px;",
              "font-weight:500; cursor:pointer;",
              "padding:5px 10px; margin:0;",
              "transition:all 0.15s; white-space:nowrap;"
            ),
            icon("upload"), " Load Config",
            tags$input(
              type     = "file",
              accept   = ".json",
              style    = "display:none;",
              onchange = paste0(
                "var r=new FileReader();",
                "r.onload=function(e){",
                " Shiny.setInputValue('", ns("config_content"), "',",
                " e.target.result, {priority:'event'});};",
                "r.readAsText(this.files[0]);"
              )
            )
          )
        )
      ),
      
      # Channel cards (scrollable)
      div(
        style = "max-height:calc(100vh - 290px);
               overflow-y:auto; padding-right:4px;",
        uiOutput(ns("ch_list"))
      )
    ),
    
    # ── Right: Editor ────────────────────────────────────────────
    card(
      full_screen = TRUE,
      card_header(uiOutput(ns("editor_title"))),
      uiOutput(ns("editor_form"))
    )
  )
}

# ── Server ───────────────────────────────────────────────────────
mod_channels_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    rv <- reactiveValues(channels = list(), selected = NULL)
    
    # ── Analytical variable names ─────────────────────────────
    analytical_vars <- reactive({
      a <- data()$analytical
      if (is.null(a)) return(character(0))
      names(a)[sapply(a, is.numeric)] %>% sort()
    })
    
    # ── Channel card renderer ─────────────────────────────────
    render_channel_card <- function(nm, cfg, is_selected) {
      src    <- cfg$data_source %||% "all_transformed"
      n_var  <- length(cfg$model_variables[
        nzchar(cfg$model_variables %||% "")])
      act_kw  <- cfg$activity_keyword %||% "Clicks"
      is_conf <- n_var > 0
      
      src_badge <- if (src == "all_rags")
        tags$span("Geographic",
                  style = paste0(
                    "background:#e8f5e9;color:#1b5e20;",
                    "font-size:10px;font-weight:600;",
                    "padding:1px 6px;border-radius:8px;",
                    "border:1px solid #a5d6a7;"))
      else
        tags$span("National",
                  style = paste0(
                    "background:#EBF3FB;color:#1565c0;",
                    "font-size:10px;font-weight:600;",
                    "padding:1px 6px;border-radius:8px;",
                    "border:1px solid #90caf9;"))
      
      border_color <- if (is_selected) "#5B9BD5"
      else if (is_conf) "#90caf9"
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
        actionButton(
          ns(paste0("del_", make.names(nm))),
          icon("xmark"),
          class = "btn btn-link btn-sm p-0",
          style = paste0(
            "position:absolute;top:6px;right:8px;",
            "color:#adb5bd;font-size:11px;",
            "min-height:0;min-width:0;")
        ),
        actionButton(
          ns(paste0("sel_", make.names(nm))),
          label = tagList(
            div(
              style = "display:flex;align-items:center;
                     gap:6px;margin-bottom:5px;padding-right:20px;",
              tags$span(nm,
                        style = paste0(
                          "font-weight:600;font-size:13px;",
                          "color:#2c3e50;overflow:hidden;",
                          "text-overflow:ellipsis;",
                          "white-space:nowrap;max-width:140px;")),
              src_badge
            ),
            div(
              style = "display:flex;gap:10px;flex-wrap:wrap;",
              tags$span(
                icon("layer-group",
                     style = "font-size:10px;color:#8a9bb0;"),
                tags$span(
                  if (n_var == 0) "No variables"
                  else paste0(n_var, " var", if (n_var > 1) "s" else ""),
                  style = "font-size:11px;color:#6c757d;")
              ),
              tags$span(
                icon("arrow-pointer",
                     style = "font-size:10px;color:#8a9bb0;"),
                tags$span(act_kw,
                          style = "font-size:11px;color:#6c757d;")
              )
            )
          ),
          class = "btn btn-link p-0 text-start w-100",
          style = "text-decoration:none;border:none;background:transparent;"
        )
      )
    }
    
    # ── Channel list ──────────────────────────────────────────
    output$ch_list <- renderUI({
      nms <- names(rv$channels)
      if (!length(nms))
        return(div(
          style = "text-align:center;padding:24px 12px;
                 color:#adb5bd;font-size:12px;",
          icon("layer-group",
               style = "font-size:28px;display:block;
                      margin-bottom:8px;color:#dee2e6;"),
          "No channels yet.", tags$br(),
          "Type a name above and click Add,", tags$br(),
          "or load a saved config."
        ))
      tagList(lapply(nms, function(nm) {
        render_channel_card(nm, rv$channels[[nm]],
                            identical(rv$selected, nm))
      }))
    })
    
    # ── Add channel ───────────────────────────────────────────
    observeEvent(input$btn_add, {
      nm <- trimws(input$new_name %||% "")
      if (!nchar(nm))
        return(showNotification("Enter a channel name.",
                                type = "warning"))
      if (nm %in% names(rv$channels))
        return(showNotification("Channel already exists.",
                                type = "warning"))
      rv$channels[[nm]] <- default_channel_config(nm)
      rv$selected       <- nm
      updateTextInput(session, "new_name", value = "")
    })
    
    # ── Dynamic select / delete ───────────────────────────────
    observe({
      walk(names(rv$channels), function(nm) {
        observeEvent(
          input[[paste0("sel_", make.names(nm))]],
          { rv$selected <- nm },
          ignoreInit = TRUE
        )
        observeEvent(
          input[[paste0("del_", make.names(nm))]],
          {
            rv$channels[[nm]] <- NULL
            if (identical(rv$selected, nm))
              rv$selected <- names(rv$channels)[1]
          },
          ignoreInit = TRUE
        )
      })
    })
    
    observeEvent(rv$selected, {
      cfg <- rv$channels[[rv$selected]]
      req(cfg)
      updateNumericInput(session, "ch_n_vars",
                         value = max(1L, length(cfg$model_variables)))
    }, ignoreNULL = TRUE)
    
    # ── Save / Load config (JSON) ─────────────────────────────
    output$dl_config <- downloadHandler(
      filename = function() {
        paste0("split_config_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      },
      content = function(file) {
        if (!length(rv$channels)) {
          writeLines("{}", file); return()
        }
        jsonlite::write_json(rv$channels, path = file,
                             pretty = TRUE, auto_unbox = TRUE,
                             null = "null")
      }
    )
    
    observeEvent(input$config_content, {
      req(input$config_content)
      tryCatch({
        loaded <- jsonlite::fromJSON(input$config_content,
                                     simplifyVector = FALSE)
        if (!is.list(loaded) || !length(loaded)) {
          showNotification("Empty or invalid config file.",
                           type = "error"); return()
        }
        normalized <- lapply(names(loaded), function(nm) {
          cfg <- loaded[[nm]]
          cfg$model_variables  <- unlist(cfg$model_variables  %||% "")
          cfg$break_dates      <- unlist(cfg$break_dates      %||% character(0))
          for (f in c("varname_include", "varname_exclude",
                      "campaign_exclude", "outlet_exclude",
                      "creative_exclude", "split_columns"))
            cfg[[f]] <- unlist(cfg[[f]] %||% character(0))
          cfg$channel_name     <- cfg$channel_name     %||% nm
          cfg$data_source      <- cfg$data_source      %||% "all_transformed"
          cfg$activity_keyword <- cfg$activity_keyword %||% "Clicks"
          cfg$spend_keyword    <- cfg$spend_keyword    %||% "Spend"
          cfg$saved_merges     <- cfg$saved_merges     %||% list()
          cfg
        })
        names(normalized) <- names(loaded)
        rv$channels <- normalized
        rv$selected <- names(normalized)[1]
        showNotification(
          paste0("Config loaded: ", length(normalized), " channel(s) restored."),
          type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Error loading config:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
    
    # ── Editor title ──────────────────────────────────────────
    output$editor_title <- renderUI({
      if (is.null(rv$selected)) "Select or add a channel"
      else
        tagList(
          "Editing: ", tags$strong(rv$selected),
          tags$span(
            "(changes saved only on click Save)",
            style = "font-size:11px;font-weight:400;
                   color:#8a9bb0;margin-left:8px;")
        )
    })
    
    # ── Editor form ───────────────────────────────────────────
    output$editor_form <- renderUI({
      req(rv$selected, rv$channels[[rv$selected]])
      cfg <- rv$channels[[rv$selected]]
      n   <- max(1L, length(cfg$model_variables))
      
      sel_act   <- if (cfg$activity_keyword %in% c("Clicks", "Impressions"))
        cfg$activity_keyword else "Other"
      sel_spend <- if (cfg$spend_keyword %in% c("Cost", "Spend"))
        cfg$spend_keyword else "Other"
      
      tagList(
        
        # ══ Row 1: Compact bar ════════════════════════════════
        div(
          style = paste0(
            "background:#f4f6f9; border-radius:8px;",
            "padding:10px 18px; margin-bottom:14px;",
            "display:flex; align-items:center;",
            "gap:20px; flex-wrap:wrap;"),
          # Data Source
          div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span(
              icon("database", style = "color:#5B9BD5;"),
              tags$strong("Data Source:",
                          style = "font-size:12.5px; color:#333;
                                 white-space:nowrap;")),
            div(class = "ds-pill-group",
                radioButtons(ns("data_source"), NULL,
                             choices  = c("National"   = "all_transformed",
                                          "Geographic" = "all_rags"),
                             selected = cfg$data_source %||% "all_transformed",
                             inline   = TRUE))
          ),
          div(style = "width:1px; height:28px; background:#dee2e6;
                     flex-shrink:0;"),
          # Activity
          div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span(
              icon("arrow-pointer", style = "color:#5B9BD5;"),
              tags$strong("Activity:",
                          style = "font-size:12.5px; color:#333;
                                 white-space:nowrap;")),
            div(style = "width:130px;",
                selectInput(ns("activity_radio"), NULL,
                            choices  = c("Clicks", "Impressions", "Other"),
                            selected = sel_act, width = "100%")),
            uiOutput(ns("activity_other_ui"))
          ),
          div(style = "width:1px; height:28px; background:#dee2e6;
                     flex-shrink:0;"),
          # Spend
          div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span(
              icon("dollar-sign", style = "color:#5B9BD5;"),
              tags$strong("Spend:",
                          style = "font-size:12.5px; color:#333;
                                 white-space:nowrap;")),
            div(style = "width:130px;",
                selectInput(ns("spend_radio"), NULL,
                            choices  = c("Cost", "Spend", "Other"),
                            selected = sel_spend, width = "100%")),
            uiOutput(ns("spend_other_ui"))
          )
        ),
        
        # ══ Row 2: Split Dimensions (arriba) ══════════════════
        card(
          card_header(
            div(
              style = "display:flex; align-items:center; gap:10px;",
              tags$span(icon("arrows-up-down"), " Split Dimensions"),
              tags$small("Drag to set order — left = excluded",
                         style = "color:#8a9bb0; font-size:11px;")
            )
          ),
          bucket_list(
            header      = NULL,
            group_name  = ns("split_bucket"),
            orientation = "horizontal",
            add_rank_list(
              text     = "Available",
              labels   = setdiff(SPLIT_CHOICES, cfg$split_columns),
              input_id = ns("splits_available")
            ),
            add_rank_list(
              text     = "Split Order",
              labels   = cfg$split_columns,
              input_id = ns("splits_selected")
            )
          )
        ),
        
        # ══ Row 3: Model Variables + Filters ══════════════════
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            style = "min-height:380px;",
            card_header(
              div(
                style = "display:flex; align-items:center;
                       justify-content:space-between;",
                tags$span(icon("layer-group"),
                          " Model Variables",
                          tags$strong("& Time Breaks")),
                div(
                  style = "display:flex; align-items:center; gap:8px;",
                  tags$small("Number of variables:",
                             style = "color:#6c757d; font-size:11px;
                                    white-space:nowrap;"),
                  div(style = "width:68px;",
                      numericInput(ns("ch_n_vars"), NULL,
                                   value = n, min = 1, max = 5, step = 1))
                )
              )
            ),
            uiOutput(ns("mv_inputs"))
          ),
          
          card(
            style = "min-height:380px;",
            card_header(tags$span(icon("filter"), " Filters")),
            div(
              style = "display:grid; grid-template-columns:1fr 1fr; gap:8px;",
              div(
                tags$label("VarName — contains", class = "filter-label"),
                textAreaInput(ns("varname_include"), NULL,
                              value = paste(cfg$varname_include %||%
                                              character(0), collapse = "\n"),
                              rows = 4, placeholder = "e.g. Audio\nOLV")
              ),
              div(
                tags$label("VarName — exclude", class = "filter-label"),
                textAreaInput(ns("varname_exclude"), NULL,
                              value = paste(cfg$varname_exclude,
                                            collapse = "\n"),
                              rows = 4, placeholder = "e.g. Impressions\nLocal")
              ),
              div(
                tags$label("Campaign — exclude", class = "filter-label"),
                textAreaInput(ns("campaign_exclude"), NULL,
                              value = paste(cfg$campaign_exclude,
                                            collapse = "\n"),
                              rows = 3, placeholder = "e.g. Pmax")
              ),
              div(
                tags$label("Outlet — exclude", class = "filter-label"),
                textAreaInput(ns("outlet_exclude"), NULL,
                              value = paste(cfg$outlet_exclude %||%
                                              character(0), collapse = "\n"),
                              rows = 3, placeholder = "e.g. TV")
              ),
              div(
                style = "grid-column: 1 / -1;",
                tags$label("Creative — exclude", class = "filter-label"),
                textAreaInput(ns("creative_exclude"), NULL,
                              value = paste(cfg$creative_exclude %||%
                                              character(0), collapse = "\n"),
                              rows = 3, placeholder = "e.g. 15s")
              )
            )
          )
        ),
        
        # ══ Save button ════════════════════════════════════════
        actionButton(
          ns("btn_save"),
          tagList(icon("floppy-disk"), " Save Channel Config"),
          class = "btn-success w-100 mt-2"
        )
      )
    })
    
    # ── Model variable inputs (inline break dates) ────────────
    output$mv_inputs <- renderUI({
      req(input$ch_n_vars, rv$selected)
      cfg  <- rv$channels[[rv$selected]]
      vars <- analytical_vars()
      n    <- input$ch_n_vars
      
      tagList(lapply(seq_len(n), function(i) {
        div(
          style = paste0(
            "background:#fafcff; border-radius:6px;",
            "padding:10px 12px; margin-bottom:8px;",
            "border:1px solid #e3e8ef;"),
          tags$label(
            paste("Model Variable", i),
            style = paste0(
              "font-size:12px; font-weight:600;",
              "color:", if (i == 1) "#5B9BD5;" else "#2c3e50;",
              " display:block; margin-bottom:4px;")
          ),
          div(
            style = "display:flex; align-items:flex-start;
                   gap:10px; flex-wrap:wrap;",
            div(
              style = "flex:1; min-width:200px;",
              selectizeInput(
                ns(paste0("mv_", i)), NULL,
                choices  = vars,
                selected = cfg$model_variables[i] %||% NULL,
                options  = list(
                  placeholder = if (!length(vars))
                    "Upload AnalyticalDataset first..."
                  else "Type to search...",
                  create           = TRUE,
                  maxOptions       = 500,
                  closeAfterSelect = TRUE
                )
              )
            ),
            if (i < n) {
              div(
                style = "flex-shrink:0;",
                tags$label(
                  paste0("Break after Var ", i),
                  style = "font-size:11px; font-weight:600; color:#6c757d;
                         display:block; margin-bottom:4px;"),
                dateInput(
                  ns(paste0("bd_", i)), NULL,
                  value = if (i <= length(cfg$break_dates))
                    as.Date(cfg$break_dates[i]) else Sys.Date()
                )
              )
            }
          )
        )
      }))
    })
    
    # ── Conditional keyword other inputs ──────────────────────
    output$activity_other_ui <- renderUI({
      req(input$activity_radio == "Other")
      cfg <- rv$channels[[rv$selected]]
      ov  <- if (!is.null(cfg) &&
                 !cfg$activity_keyword %in% c("Clicks", "Impressions"))
        cfg$activity_keyword else ""
      div(style = "width:110px;",
          textInput(ns("activity_other"), NULL,
                    value = ov, placeholder = "Custom..."))
    })
    
    output$spend_other_ui <- renderUI({
      req(input$spend_radio == "Other")
      cfg <- rv$channels[[rv$selected]]
      ov  <- if (!is.null(cfg) &&
                 !cfg$spend_keyword %in% c("Cost", "Spend"))
        cfg$spend_keyword else ""
      div(style = "width:110px;",
          textInput(ns("spend_other"), NULL,
                    value = ov, placeholder = "Custom..."))
    })
    
    # ── Save ──────────────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(rv$selected, input$ch_n_vars)
      n <- input$ch_n_vars
      
      mvars <- vapply(seq_len(n),
                      \(i) input[[paste0("mv_", i)]] %||% "",
                      character(1))
      
      brdates <- if (n > 1)
        vapply(seq_len(n - 1),
               \(i) as.character(input[[paste0("bd_", i)]]),
               character(1))
      else character(0)
      
      parse_lines <- \(x) Filter(
        \(l) nchar(trimws(l)) > 0,
        strsplit(x %||% "", "\n")[[1]]
      )
      
      activity_kw <- if (isTRUE(input$activity_radio == "Other"))
        trimws(input$activity_other %||% "Clicks")
      else input$activity_radio %||% "Clicks"
      
      spend_kw <- if (isTRUE(input$spend_radio == "Other"))
        trimws(input$spend_other %||% "Spend")
      else input$spend_radio %||% "Spend"
      
      # ── FIX: preservar saved_merges al guardar ────────────
      existing_merges <- rv$channels[[rv$selected]]$saved_merges %||% list()
      
      rv$channels[[rv$selected]] <- list(
        channel_name     = rv$selected,
        data_source      = input$data_source %||% "all_transformed",
        model_variables  = mvars,
        break_dates      = brdates,
        varname_include  = parse_lines(input$varname_include),
        varname_exclude  = parse_lines(input$varname_exclude),
        campaign_exclude = parse_lines(input$campaign_exclude),
        outlet_exclude   = parse_lines(input$outlet_exclude),
        creative_exclude = parse_lines(input$creative_exclude),
        split_columns    = input$splits_selected %||%
          c("VariableName", "Campaign"),
        activity_keyword = if (nchar(activity_kw)) activity_kw else "Clicks",
        spend_keyword    = if (nchar(spend_kw)) spend_kw else "Spend",
        saved_merges     = existing_merges   # ← preservar siempre
      )
      
      showNotification(paste("Saved:", rv$selected), type = "message")
    })
    
    # ── Return ────────────────────────────────────────────────
    reactive(rv$channels)
  })
}

# ── Default config ────────────────────────────────────────────
default_channel_config <- function(nm) {
  list(
    channel_name     = nm,
    data_source      = "all_transformed",
    model_variables  = "",
    break_dates      = character(0),
    varname_include  = character(0),
    varname_exclude  = character(0),
    campaign_exclude = character(0),
    outlet_exclude   = character(0),
    creative_exclude = character(0),
    split_columns    = c("VariableName", "Campaign"),
    activity_keyword = "Clicks",
    spend_keyword    = "Spend",
    saved_merges     = list()
  )
}