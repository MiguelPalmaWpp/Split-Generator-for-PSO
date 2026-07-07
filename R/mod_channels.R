
# ═══════════════════════════════════════════════════════════════════════
# R/mod_channels.R
# ═══════════════════════════════════════════════════════════════════════

mod_channels_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    card(
      card_header("Channels"),
      div(
        class = "ch-btn-row",
        downloadButton(ns("dl_config_csv"),
                       label = "Save Config",
                       class = "btn-outline-secondary btn-sm flex-fill"),
        div(
          class = "ch-load-wrap",
          tags$label(
            class = "ch-load-label",
            icon("upload"), " Load Config",
            tags$input(type = "file", accept = ".csv",
                       class = "d-none",
                       onchange = paste0(
                         "var r=new FileReader();",
                         "r.onload=function(e){Shiny.setInputValue('",
                         ns("config_csv_content"),
                         "',e.target.result,{priority:'event'});};",
                         "r.readAsText(this.files[0]);")))
        )
      ),
      actionButton(ns("btn_manage_channels"),
                   tagList(icon("file-import"), " Import / Manage Channels"),
                   class = "btn-primary btn-sm w-100 mb-2"),
      actionButton(ns("btn_save_all"),
                   tagList(icon("floppy-disk"), " Save All Split Orders"),
                   class = "btn-outline-secondary btn-sm w-100 mb-10"),
      textInput(ns("ch_search"), NULL, placeholder = "Search channels...", width = "100%"),
      div(class = "ch-list-scroll", uiOutput(ns("ch_list")))
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev_ch"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "ch-editor-title-wrap", uiOutput(ns("editor_title"))),
          actionButton(ns("btn_next_ch"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      uiOutput(ns("editor_body"))
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_channels_server <- function(id, data, media_index) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      channels           = list(),
      available_channels = list(),
      selected           = NULL
    )
    
    dirty_channels <- reactiveValues()
    pending_delete <- reactiveVal(NULL)
    breaks_enabled <- reactiveVal(FALSE)
    config_import_pending <- reactiveVal(NULL)
    copy_config_source <- reactiveVal(NULL)
    preview_row_limit <- 1000L
    split_preview_cache <- reactiveValues(key = NULL, ui = NULL)
    break_preview_cache <- reactiveValues(key = NULL, data = NULL)
    break_n_parts_cache <- reactiveVal(2L)
    
    effective_split_choices <- function(breaks = list(), cross_cols = character(0)) {
      base_choices <- setdiff(SPLIT_CHOICES, cross_cols)
      choices      <- base_choices
      for (brk in breaks) {
        choices <- setdiff(choices, brk$column)
        choices <- c(choices, brk$names)
      }
      choices
    }

    clone_dimension_breaks <- function(breaks = list()) {
      lapply(breaks %||% list(), function(brk) {
        part_names <- as.character(brk$names %||% character(0))
        n_parts <- brk$n_parts %||% length(part_names)
        n_parts <- if (length(n_parts) && !is.na(n_parts)) as.integer(n_parts) else length(part_names)
        list(
          column = brk$column %||% "",
          separator = brk$separator %||% "_",
          n_parts = n_parts,
          names = part_names
        )
      })
    }

    limit_preview_rows <- function(df, limit = preview_row_limit) {
      if (is.null(df) || nrow(df) <= limit) return(df)
      df[seq_len(limit), , drop = FALSE]
    }

    breaks_signature <- function(breaks = list()) {
      if (!length(breaks)) return("none")
      paste(vapply(breaks, function(brk) {
        paste(
          brk$column %||% "",
          brk$separator %||% "",
          brk$n_parts %||% "",
          paste(brk$names %||% character(0), collapse = ","),
          sep = ":"
        )
      }, character(1)), collapse = "|")
    }
    
    get_cross_cols <- function() {
      tryCatch({
        an <- data()$analytical
        if (!is.null(an)) auto_detect_cross_cols(an) else c("Geography", "Product")
      }, error = \(e) c("Geography", "Product"))
    }
    
    main_data <- reactive({ data()$all_rags })
    
    mk_info_row <- function(label, value) {
      div(class = "info-row",
          tags$span(label, class = "info-row-label"),
          div(class = "info-row-value", value))
    }

    normalize_model_var <- function(x) {
      trimws(stringr::str_remove(as.character(x),
                                 stringr::regex("(_Total)+$", ignore_case = TRUE)))
    }

    lookup_roi <- function(model_var, fallback = NULL) {
      rois <- tryCatch(data()$channels_rois, error = \(e) NULL)
      if (is.null(rois) || !"MainModelVariableName" %in% names(rois)) {
        return(list(value = NA_real_, channel = NA_character_))
      }

      candidates <- unique(normalize_model_var(c(model_var, fallback)))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (!length(candidates)) return(list(value = NA_real_, channel = NA_character_))

      roi_norm <- rois
      roi_norm$.mv_norm <- normalize_model_var(roi_norm$MainModelVariableName)
      rows <- roi_norm[roi_norm$.mv_norm %in% candidates, , drop = FALSE]
      if (!nrow(rows)) return(list(value = NA_real_, channel = NA_character_))

      roi_num <- setdiff(names(rows)[sapply(rows, is.numeric)], "MainModelVariableName")
      roi_val <- if (length(roi_num)) {
        vals <- rows[[roi_num[1]]]
        vals <- vals[!is.na(vals)]
        if (length(vals)) mean(vals) else NA_real_
      } else NA_real_

      roi_channel <- if ("Channel" %in% names(rows)) {
        vals <- trimws(as.character(rows$Channel))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals)) vals[1] else NA_character_
      } else NA_character_

      list(value = roi_val, channel = roi_channel)
    }

    effective_roi <- function(cfg, fallback = NULL) {
      stored <- cfg$roi %||% NA_real_
      if (!is.na(stored)) return(stored)
      lookup_roi(cfg$model_variable %||% fallback, fallback)$value
    }
    
    get_varname_include_fallback <- function(nm) {
      vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
      if (!is.null(vof_d) &&
          all(c("MainModelVariableName", "AnalyticalVariableName") %in% names(vof_d))) {
        vof_rows <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
        if (nrow(vof_rows) > 0) {
          an_vn <- unique(vof_rows$AnalyticalVariableName[
            !is.na(vof_rows$AnalyticalVariableName) &
              nzchar(vof_rows$AnalyticalVariableName)])
          if (length(an_vn) > 0) {
            an_vn_clean <- unique(stringr::str_remove(an_vn, "_Total(_Total)*$"))
            return(unique(c(an_vn_clean, nm)))
          }
        }
      }
      base_vn <- trimws(stringr::str_remove(
        stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$"))
      if (!nzchar(base_vn)) base_vn <- nm
      act_kw   <- detect_activity_keyword(base_vn)
      broad_vn <- trimws(stringr::str_remove(
        base_vn,
        stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)))
      if (nzchar(broad_vn) && broad_vn != base_vn) c(base_vn, broad_vn) else base_vn
    }
    
    observeEvent(media_index(), {
      mi <- media_index()
      req(!is.null(mi), length(mi$channels) > 0)
      rv$available_channels <- mi$channels
      for (nm in names(mi$channels)) {
        if (nm %in% names(rv$channels)) {
          new_cfg <- mi$channels[[nm]]
          rv$channels[[nm]]$varname_include   <- new_cfg$varname_include
          rv$channels[[nm]]$activity_keyword  <- new_cfg$activity_keyword
          rv$channels[[nm]]$spend_keyword     <- new_cfg$spend_keyword
          rv$channels[[nm]]$segment_overrides <- new_cfg$segment_overrides
          rv$channels[[nm]]$vof_time_breaks   <- new_cfg$vof_time_breaks %||% list()
          rv$channels[[nm]]$media_channel     <- new_cfg$media_channel %||% ""
          rv$channels[[nm]]$sub_channel       <- new_cfg$sub_channel %||% ""
          rv$channels[[nm]]$effect            <- new_cfg$effect %||% ""
          rv$channels[[nm]]$min_period        <- new_cfg$min_period
          rv$channels[[nm]]$max_period        <- new_cfg$max_period
          rv$channels[[nm]]$time_break_label  <- new_cfg$time_break_label %||% ""
        }
      }
    }, ignoreNULL = TRUE)
    
    observeEvent(input$btn_prev_ch, {
      nms <- names(rv$channels); if (!length(nms)) return()
      cur <- which(nms == rv$selected)
      if (length(cur) > 0 && cur > 1) rv$selected <- nms[cur - 1]
    })
    observeEvent(input$btn_next_ch, {
      nms <- names(rv$channels); if (!length(nms)) return()
      cur <- which(nms == rv$selected)
      if (length(cur) > 0 && cur < length(nms)) rv$selected <- nms[cur + 1]
    })
    
    observeEvent(rv$selected, {
      split_preview_cache$key <- NULL
      split_preview_cache$ui <- NULL
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        breaks_enabled(
          length(rv$channels[[rv$selected]]$dimension_breaks %||% list()) > 0)
      else breaks_enabled(FALSE)
    }, ignoreNULL = TRUE)

    observeEvent(main_data(), {
      split_preview_cache$key <- NULL
      split_preview_cache$ui <- NULL
      break_preview_cache$key <- NULL
      break_preview_cache$data <- NULL
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_enable_breaks, { breaks_enabled(TRUE) })
    
    observeEvent(input$btn_save_all, {
      if (!length(rv$channels)) {
        showNotification("No channels to save.", type = "warning"); return()
      }
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        rv$channels[[rv$selected]]$split_columns <-
          isolate(input$splits_selected) %||% c("VariableName")
      }
      for (nm in names(rv$channels)) dirty_channels[[nm]] <- FALSE
      showNotification(paste0("Split order saved for ", length(rv$channels), " channel(s)."),
                       type = "message", duration = 3)
    })
    
    render_channel_card <- function(nm, cfg, is_selected) {
      is_dirty <- isTRUE(dirty_channels[[nm]])
      is_vof   <- identical(cfg$source %||% "vof", "vof")
      is_kw    <- identical(cfg$source %||% "", "keyword_fallback")
      roi_val  <- effective_roi(cfg, nm)
      has_roi  <- !is.na(roi_val)
      n_brk    <- length(cfg$dimension_breaks %||% list())
      n_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1)))
      has_dates <- !is.na(cfg$min_period %||% NA) && !is.na(cfg$max_period %||% NA)
      configured <- length(cfg$split_columns %||% character(0)) > 0 &&
        nzchar(cfg$model_variable %||% "")
      
      src_label <- if (is_vof) "VOF" else if (is_kw) "KW" else "MFF"
      src_class <- if (is_vof) "ch-badge-vof" else if (is_kw) "ch-badge-kw" else "ch-badge-mff"
      
      div(
        class = paste("ch-card", if (is_selected) "selected" else ""),
        tags$span(icon("xmark"), class = "ch-card-remove", title = "Remove channel",
                  onclick = paste0("Shiny.setInputValue('", ns("delete_nm"), "','",
                                   nm, "',{priority:'event'});")),
        tags$div(
          class = "ch-card-body",
          onclick = paste0("Shiny.setInputValue('", ns("select_nm"), "','",
                           nm, "',{priority:'event'});"),
          div(class = "ch-card-header-row",
              tags$span(nm, class = "ch-card-name"),
              if (is_dirty) tags$span("\u25CF", class = "ch-card-dirty",
                                      title = "Unsaved changes"),
              if (has_roi) tags$span(paste0("ROI ", round(roi_val, 1)),
                                     class = "ch-card-roi"),
              if (!has_roi) tags$span("No ROI", class = "ch-badge-warn"),
              if (configured) tags$span("Configured", class = "ch-badge-ok"),
              if (n_merges > 0) tags$span(paste0(n_merges, " merges"), class = "ch-badge-info"),
              if (has_dates) tags$span("Date limited", class = "ch-badge-muted"),
              tags$span(src_label, class = src_class)),
          div(class = "ch-card-meta",
              if (is_vof) {
                mc <- cfg$media_channel %||% ""
                mt <- cfg$activity_keyword %||% ""
                ef <- cfg$effect %||% ""
                parts <- c(if (nzchar(mc)) mc else NULL,
                           if (nzchar(mt)) mt else NULL,
                           if (nzchar(ef) && ef != "In") ef else NULL)
                if (length(parts) > 0)
                  tags$span(paste(parts, collapse = " \u00b7 "),
                            class = "ch-card-meta-vars")
              },
              if (!is.na(cfg$min_period %||% NA) && !is.na(cfg$max_period %||% NA))
                tags$span(paste0(format(cfg$min_period, "%Y-%m-%d"), " \u2192 ",
                                 format(cfg$max_period, "%Y-%m-%d")),
                          class = "ch-card-meta-dates"),
              if (n_brk > 0)
                tags$span(icon("scissors", class = "icon-scissors-sm"),
                          tags$span(paste0(n_brk, " break", if (n_brk > 1) "s" else ""),
                                    class = "ch-card-meta-vars")))
        )
      )
    }
    
    output$ch_list <- renderUI({
      nms <- names(rv$channels)
      if (!length(nms))
        return(div(class = "ch-empty-state",
                   icon("file-import", class = "icon-empty-lg"),
                   tags$p("No channels loaded.", class = "ch-empty-msg"),
                   tags$p(tagList("Click ", tags$strong("Import / Manage Channels"),
                                  " to get started."), class = "ch-empty-hint")))
      search_term <- trimws(input$ch_search %||% "")
      if (nzchar(search_term)) {
        nms <- nms[stringr::str_detect(nms, stringr::regex(search_term, ignore_case = TRUE))]
      }
      if (!length(nms))
        return(div(class = "ch-empty-state",
                   icon("magnifying-glass", class = "icon-empty-lg"),
                   tags$p("No channels match your search.", class = "ch-empty-msg")))
      tagList(lapply(nms, function(nm)
        render_channel_card(nm, rv$channels[[nm]], identical(rv$selected, nm))))
    })
    
    observeEvent(input$select_nm, {
      req(nzchar(input$select_nm %||% ""))
      if (input$select_nm %in% names(rv$channels)) rv$selected <- input$select_nm
    }, ignoreInit = TRUE)
    
    observeEvent(input$delete_nm, {
      req(nzchar(input$delete_nm %||% ""))
      pending_delete(input$delete_nm)
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", class = "banner-icon-yellow"),
                        " Remove channel"),
        tags$p("Remove channel ", tags$strong(input$delete_nm), "?"),
        tags$p(class = "text-muted small", "This action cannot be undone."),
        footer = tagList(actionButton(ns("btn_confirm_delete"), "Remove",
                                      class = "btn-danger"),
                         modalButton("Cancel")),
        easyClose = TRUE, size = "s"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_confirm_delete, {
      nm <- pending_delete(); req(!is.null(nm))
      rv$channels[[nm]] <- NULL; dirty_channels[[nm]] <- NULL
      if (identical(rv$selected, nm))
        rv$selected <- names(rv$channels)[1] %||% NULL
      pending_delete(NULL); removeModal()
    }, ignoreInit = TRUE)
    
    output$editor_title <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(tags$span("Select a channel", class = "text-muted"))
      dirty <- isTRUE(dirty_channels[[rv$selected]])
      nms   <- names(rv$channels)
      idx   <- which(nms == rv$selected)
      tagList(
        tags$span(rv$selected, class = "ch-editor-name"),
        if (dirty) tags$span(" \u25CF Unsaved", class = "ch-dirty-indicator"),
        tags$span(paste0(" (", if (length(idx)) idx else "?", " / ", length(nms), ")"),
                  class = "ch-editor-counter"))
    })
    
    output$editor_body <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(div(class = "ch-editor-empty", icon("hand-pointer", class = "icon-empty"),
                   tags$p("Select a channel from the list to configure it.",
                          class = "ch-editor-empty-msg")))
      
      cfg        <- rv$channels[[rv$selected]]
      cross_cols <- get_cross_cols()
      avail_choices <- effective_split_choices(cfg$dimension_breaks %||% list(), cross_cols)
      
      excluded_geos <- if (length(cfg$segment_overrides) > 0)
        cfg$segment_overrides[[1]]$geography_exclude %||% character(0)
      else character(0)
      
      geo_display <- {
        n <- length(excluded_geos)
        if (n == 0) "None"
        else tagList(
          tags$span(paste0(n, " excluded: "), class = "text-muted small me-1"),
          if (n <= 5) tags$span(paste(excluded_geos, collapse = ", "), class = "info-row-value")
          else tagList(
            tags$span(paste(head(excluded_geos, 5), collapse = ", "), class = "info-row-value"),
            tags$span(paste0(" +", n - 5, " more"), class = "text-muted small")))
      }
      
      roi_info <- lookup_roi(cfg$model_variable %||% rv$selected, rv$selected)
      roi_ch <- roi_info$channel
      roi_val <- effective_roi(cfg, rv$selected)
      
      info_title <- switch(cfg$source %||% "vof",
                           vof              = "Auto-configured from VOF",
                           keyword_fallback = "From MFF (keyword match)",
                           "From MFF")
      info_class <- switch(cfg$source %||% "vof",
                           vof = "info-box-vof", keyword_fallback = "info-box-kw", "info-box-mff")
      info_icon  <- switch(cfg$source %||% "vof",
                           vof = "icon-blue-sm", keyword_fallback = "icon-kw-sm", "icon-mff-sm")
      
      tagList(
        div(class = info_class,
            div(class = "card-header-inner mb-2",
                icon("circle-info", class = info_icon),
                tags$strong(info_title, class = "info-box-title"),
                tags$span("(read-only)", class = "section-subtitle")),
            if (!is.na(roi_ch)) mk_info_row("Channel", roi_ch),
            if (nzchar(cfg$sub_channel %||% "")) mk_info_row("Sub Channel", cfg$sub_channel),
            if (nzchar(cfg$effect %||% ""))      mk_info_row("Effect", cfg$effect),
            mk_info_row("VarName filter",
                        if (length(cfg$varname_include) > 0)
                          paste(cfg$varname_include, collapse = ", ") else "\u2014"),
            mk_info_row("Activity keyword", cfg$activity_keyword %||% "\u2014"),
            mk_info_row("Spend keyword",    cfg$spend_keyword    %||% "\u2014"),
            mk_info_row("Data range",
                        if (!is.na(cfg$min_period %||% NA_real_) &&
                            !is.na(cfg$max_period %||% NA_real_))
                          paste0(format(cfg$min_period, "%Y-%m-%d"), " \u2192 ",
                                 format(cfg$max_period, "%Y-%m-%d"))
                        else "Full range"),
            mk_info_row("Geo overrides", geo_display),
            if (!is.na(roi_val))
              mk_info_row("ROI", format(round(roi_val, 2), big.mark = ",")),
            if (!is.null(cfg$time_break_label) && nzchar(cfg$time_break_label %||% ""))
              mk_info_row("Time segment",
                          tags$span(cfg$time_break_label, class = "badge-blue"))),
        
        div(class = "section-block",
            div(class = "section-title-row",
                icon("scissors", class = "icon-blue-sm"),
                tags$strong("Dimension Breaks"),
                tags$small("Applied to both activity and spend. Filtered to channel variables.",
                           class = "section-subtitle")),
            if (!breaks_enabled()) {
              actionButton(ns("btn_enable_breaks"),
                           tagList(icon("scissors"),
                                   " Do you need to break a dimension for this channel?"),
                           class = "btn-enable-breaks btn-sm")
            } else {
              tagList(div(class = "d-flex justify-content-end mb-2",
                          actionButton(ns("btn_add_break"), tagList(icon("plus"), " Add Break"),
                                       class = "btn-outline-secondary btn-sm")),
                      uiOutput(ns("breaks_list")))
            }),
        
        hr(class = "mb-4"),
        
        div(class = "section-title-row mb-10",
            icon("arrows-up-down", class = "icon-blue-sm"),
            tags$strong("Split Dimensions"),
            tags$small("Drag to set order \u2014 left = excluded", class = "section-subtitle")),
        
        layout_columns(
          col_widths = c(6, 6),
          bucket_list(
            header = NULL, group_name = ns("split_bucket"), orientation = "horizontal",
            add_rank_list(text = "Available",
                          labels = setdiff(avail_choices, cfg$split_columns),
                          input_id = ns("splits_available")),
            add_rank_list(text = "Split Order",
                          labels = cfg$split_columns,
                          input_id = ns("splits_selected"))),
          uiOutput(ns("split_preview"))),
        
        div(class = "mt-3 ch-copy-actions",
            actionButton(ns("btn_copy_config"),
                         tagList(icon("copy"), " Copy Config"),
                         class = "btn-outline-secondary btn-sm"),
            actionButton(ns("btn_save"), tagList(icon("floppy-disk"), " Save Split Order"),
                         class = "btn-success btn-sm flex-fill"))
      )
    })
    
    observeEvent(input$btn_copy_config, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      target_choices <- setdiff(names(rv$channels), rv$selected)
      if (!length(target_choices)) {
        showNotification("No other channels available to copy into.", type = "warning")
        return()
      }

      source_nm <- rv$selected
      copy_config_source(source_nm)
      source_splits <- isolate(input$splits_selected) %||%
        rv$channels[[source_nm]]$split_columns %||% c("VariableName")
      source_splits <- source_splits[!is.na(source_splits) & nzchar(source_splits)]
      if (!length(source_splits)) source_splits <- c("VariableName")
      source_breaks <- rv$channels[[source_nm]]$dimension_breaks %||% list()

      showModal(modalDialog(
        title = tagList(icon("copy"), " Copy Split/Break Config"),
        div(class = "break-info-box",
            icon("circle-info", class = "icon-blue-sm"),
            " Copy split order and dimension breaks from the current channel. Target merges will be cleared."),
        tags$div(class = "import-preview-box mb-3",
                 tags$p(class = "mb-1",
                        tags$strong("Source: "), source_nm),
                 tags$p(class = "text-muted small mb-0",
                        paste0(length(source_splits), " split dimension",
                               if (length(source_splits) != 1) "s" else "",
                               " and ", length(source_breaks), " break",
                               if (length(source_breaks) != 1) "s" else "",
                               " will be copied."))),
        selectizeInput(
          ns("copy_config_targets"),
          "Copy into channel(s)",
          choices = target_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(placeholder = "Select target channels...")
        ),
        footer = tagList(
          actionButton(ns("btn_apply_copy_config"),
                       tagList(icon("check"), " Apply Copy"),
                       class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_apply_copy_config, {
      source_nm <- copy_config_source()
      req(!is.null(source_nm), source_nm %in% names(rv$channels))

      targets <- input$copy_config_targets %||% character(0)
      targets <- intersect(targets, setdiff(names(rv$channels), source_nm))
      if (!length(targets)) {
        showNotification("Select at least one target channel.", type = "warning")
        return()
      }

      source_cfg <- rv$channels[[source_nm]]
      source_splits <- if (identical(source_nm, rv$selected)) {
        isolate(input$splits_selected) %||% source_cfg$split_columns %||% c("VariableName")
      } else {
        source_cfg$split_columns %||% c("VariableName")
      }
      source_splits <- source_splits[!is.na(source_splits) & nzchar(source_splits)]
      if (!length(source_splits)) source_splits <- c("VariableName")
      source_breaks <- clone_dimension_breaks(source_cfg$dimension_breaks %||% list())

      for (target_nm in targets) {
        rv$channels[[target_nm]]$split_columns <- source_splits
        rv$channels[[target_nm]]$dimension_breaks <- clone_dimension_breaks(source_breaks)
        rv$channels[[target_nm]]$saved_merges <- list()
        dirty_channels[[target_nm]] <- TRUE
      }

      split_preview_cache$key <- NULL
      split_preview_cache$ui <- NULL
      copy_config_source(NULL)
      removeModal()
      showNotification(
        paste0("Copied split/break config to ", length(targets),
               " channel(s). Merges cleared for review."),
        type = "message",
        duration = 5
      )
    }, ignoreInit = TRUE)

    observeEvent(input$btn_save, {
      req(rv$selected)
      splits <- isolate(input$splits_selected) %||% c("VariableName")
      rv$channels[[rv$selected]]$split_columns <-
        splits
      dirty_channels[[rv$selected]] <- FALSE
      showNotification(paste0("Split order saved: ", rv$selected),
                       type = "message", duration = 2)
    })
    
    observeEvent(input$splits_selected, {
      req(!is.null(rv$selected), rv$selected %in% names(rv$channels))
      stored   <- rv$channels[[rv$selected]]$split_columns %||% c("VariableName")
      incoming <- input$splits_selected %||% c("VariableName")
      if (!identical(stored, incoming)) dirty_channels[[rv$selected]] <- TRUE
    }, ignoreInit = TRUE)
    
    output$split_preview <- renderUI({
      splits <- input$splits_selected %||% c("VariableName")
      if (!length(splits))
        return(div(class = "preview-empty", icon("eye-slash", class = "icon-preview-empty"),
                   tags$p("Add columns to Split Order to see preview.",
                          class = "preview-empty-msg")))
      md <- main_data()
      if (is.null(md))
        return(div(class = "preview-empty", icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Upload RAE Datafile to see preview.", class = "preview-empty-msg")))
      
      cfg_p <- if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        rv$channels[[rv$selected]] else NULL

      cache_key <- paste(
        rv$selected %||% "",
        paste(splits, collapse = "|"),
        if (!is.null(cfg_p)) breaks_signature(cfg_p$dimension_breaks %||% list()) else "none",
        if (!is.null(cfg_p)) paste(cfg_p$varname_include %||% character(0), collapse = "|") else "",
        nrow(md),
        sep = "::"
      )
      if (identical(split_preview_cache$key, cache_key) &&
          !is.null(split_preview_cache$ui)) {
        return(split_preview_cache$ui)
      }
      
      if (!is.null(cfg_p)) {
        vi <- cfg_p$varname_include[nzchar(cfg_p$varname_include %||% "")]
        if (length(vi) > 0 && "VariableName" %in% names(md)) {
          vn_vec <- as.character(md$VariableName)
          keep   <- Reduce("|", lapply(vi, function(p)
            grepl(paste0("^", p), vn_vec, ignore.case = TRUE, perl = TRUE)))
          md <- md[keep, , drop = FALSE]
        }
        is_limited <- nrow(md) > preview_row_limit
        md <- limit_preview_rows(md)
        if (length(cfg_p$dimension_breaks %||% list()) > 0)
          md <- tryCatch(apply_dimension_breaks(md, cfg_p$dimension_breaks), error = \(e) md)
      } else {
        is_limited <- nrow(md) > preview_row_limit
        md <- limit_preview_rows(md)
      }
      
      if (nrow(md) == 0)
        return(div(class = "preview-warn",
                   icon("triangle-exclamation", class = "icon-warning-sm d-block mb-1"),
                   tags$p("No data matches this channel's filter.", class = "preview-warn-msg")))
      
      valid_cols <- intersect(splits, names(md))
      if (!length(valid_cols)) return(NULL)
      
      all_combos <- md %>% dplyr::select(dplyr::all_of(valid_cols)) %>% dplyr::distinct()
      n_total    <- nrow(all_combos)
      count_label <- paste0(if (is_limited) "~" else "",
                            format(n_total, big.mark = ","))
      non_total  <- all_combos %>%
        dplyr::filter(dplyr::if_all(dplyr::everything(),
                                    ~ trimws(as.character(.)) != "Total"))
      sample_row <- if (nrow(non_total) > 0) head(non_total, 1) else head(all_combos, 1)
      parts_vals <- trimws(as.character(sample_row[1, ]))
      parts_vals <- parts_vals[!is.na(parts_vals) & parts_vals != "NA"]
      example    <- paste(parts_vals, collapse = "_")
      if (nchar(example) > 55) example <- paste0(substr(example, 1, 55), "...")
      
      ui <- div(class = "split-preview-box",
                div(class = "split-preview-header",
                    div(class = "split-preview-left",
                        icon("eye", class = "icon-blue-sm"),
                        tags$span("Split name preview", class = "split-preview-title")),
                    tags$span(paste0(count_label, " unique splits"),
                              class = "badge-blue")),
                div(class = "split-preview-formula",
                    tags$code(paste(valid_cols, collapse = " + "), class = "split-formula-code")),
                div(class = "split-preview-example", div(class = "split-example-text", example)),
                tags$p(paste0(count_label, " unique splits in this channel preview. ",
                              if (is_limited) "Preview based on sample." else ""),
                       class = "hint-text"))
      split_preview_cache$key <- cache_key
      split_preview_cache$ui <- ui
      ui
    })
    
    output$breaks_list <- renderUI({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return(NULL)
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!length(breaks))
        return(tags$p(class = "text-muted small mb-0",
                      icon("circle-info", class = "icon-xs"),
                      " No breaks configured. Click \"Add Break\" to split a dimension."))
      tagList(lapply(seq_along(breaks), function(i) {
        brk <- breaks[[i]]
        div(class = "break-item", icon("scissors", class = "icon-blue-sm"),
            div(class = "break-item-text",
                tags$strong(brk$column),
                tags$span(paste0(" by \"", brk$separator, "\" \u2192 "), class = "text-muted"),
                tags$span(paste(brk$names, collapse = " | "), class = "text-blue fw-semibold")),
            actionButton(ns(paste0("remove_break_", i)), icon("xmark"),
                         class = "btn btn-link p-0 btn-break-remove"))
      }))
    })
    
    observe({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return()
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!is.null(session$userData$remove_break_obs))
        lapply(session$userData$remove_break_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      session$userData$remove_break_obs <- lapply(seq_along(breaks), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("remove_break_", local_i)]], {
            curr <- rv$channels[[local_nm]]$dimension_breaks %||% list()
            if (local_i <= length(curr)) {
              brk <- curr[[local_i]]
              rv$channels[[local_nm]]$dimension_breaks <- curr[-local_i]
              rv$channels[[local_nm]]$split_columns    <- setdiff(
                rv$channels[[local_nm]]$split_columns %||% character(0), brk$names)
              if (!length(rv$channels[[local_nm]]$dimension_breaks)) breaks_enabled(FALSE)
              showNotification(paste0("Break on '", brk$column, "' removed."),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
    })
    
    auto_n_parts <- reactive({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% "_"
      md  <- main_data()
      if (is.null(md) || !col %in% names(md)) return(2L)
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        cfg_p <- rv$channels[[rv$selected]]
        vi    <- cfg_p$varname_include[nzchar(cfg_p$varname_include %||% "")]
        if (length(vi) > 0 && "VariableName" %in% names(md)) {
          vn_vec <- as.character(md$VariableName)
          keep   <- Reduce("|", lapply(vi, function(p)
            grepl(paste0("^", p), vn_vec, ignore.case = TRUE, perl = TRUE)))
          md <- md[keep, , drop = FALSE]
        }
      }
      if (nrow(md) == 0 || !col %in% names(md)) return(2L)
      md <- limit_preview_rows(md)
      vals <- unique(clean_split_part(md[[col]]))
      vals <- vals[!is.na(vals)]
      if (!length(vals)) {
        break_n_parts_cache(2L)
        return(2L)
      }
      max_parts <- max(vapply(vals, function(v)
        length(strsplit(v, sep, fixed = TRUE)[[1]]), integer(1)))
      n_parts <- as.integer(max(2L, min(max_parts, 8L)))
      break_n_parts_cache(n_parts)
      n_parts
    })
    
    observeEvent(input$btn_add_break, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      md <- main_data()
      if (is.null(md)) {
        showNotification("Upload RAE Datafile first.", type = "warning"); return()
      }
      already_broken <- sapply(rv$channels[[rv$selected]]$dimension_breaks %||% list(),
                               \(b) b$column)
      available <- setdiff(c("Campaign", "Outlet", "Creative"), already_broken)
      if (!length(available)) {
        showNotification("All dimensions already have a break.", type = "warning"); return()
      }
      break_n_parts_cache(2L)
      showModal(modalDialog(
        title = tagList(icon("scissors"), " Configure Dimension Break"),
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            " This break will be applied to both activity and spend columns automatically."),
        selectInput(ns("break_col"), "Column to break", choices = available),
        textInput(ns("break_sep"), "Separator", value = "_"),
        uiOutput(ns("break_preview_ui")), uiOutput(ns("break_names_ui")),
        footer = tagList(actionButton(ns("btn_confirm_break"),
                                      tagList(icon("check"), " Add Break"),
                                      class = "btn-primary") %>%
                         tagAppendAttributes(
                           onclick = "this.disabled=true; this.classList.add('disabled');"
                         ),
                         modalButton("Cancel")),
        easyClose = FALSE, size = "m"))
    })

    break_preview_unique_values <- reactive({
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% "_"
      n   <- auto_n_parts()
      md  <- main_data()

      empty_preview <- list(
        filtered_rows = 0L,
        raw_vals = character(0),
        unique_vals = character(0),
        parts = list()
      )
      if (is.null(md) || !col %in% names(md)) return(empty_preview)

      cfg_p <- if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        rv$channels[[rv$selected]] else NULL
      vi <- if (!is.null(cfg_p))
        cfg_p$varname_include[nzchar(cfg_p$varname_include %||% "")]
      else character(0)

      cache_key <- paste(
        rv$selected %||% "",
        col,
        sep,
        n,
        paste(vi, collapse = "|"),
        nrow(md),
        paste(names(md), collapse = "|"),
        sep = "::"
      )
      if (identical(break_preview_cache$key, cache_key) &&
          !is.null(break_preview_cache$data)) {
        return(break_preview_cache$data)
      }

      cols_needed <- intersect(c("VariableName", col), names(md))
      md_small <- md[, cols_needed, drop = FALSE]
      if (length(vi) > 0 && "VariableName" %in% names(md_small)) {
        vn_vec <- as.character(md_small$VariableName)
        keep <- Reduce("|", lapply(vi, function(p)
          grepl(paste0("^", p), vn_vec, ignore.case = TRUE, perl = TRUE)))
        md_small <- md_small[keep, , drop = FALSE]
      }
      if (nrow(md_small) == 0 || !col %in% names(md_small)) {
        break_preview_cache$key <- cache_key
        break_preview_cache$data <- empty_preview
        return(empty_preview)
      }

      raw_vals <- clean_split_part(md_small[[col]])
      unique_vals <- unique(raw_vals)
      if (!length(unique_vals)) unique_vals <- NA_character_
      parts <- lapply(unique_vals, function(v) {
        if (is.na(v)) character(0) else strsplit(v, sep, fixed = TRUE)[[1]]
      })

      out <- list(
        filtered_rows = nrow(md_small),
        raw_vals = raw_vals,
        unique_vals = unique_vals,
        parts = parts
      )
      break_preview_cache$key <- cache_key
      break_preview_cache$data <- out
      out
    })
    
    output$break_preview_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% "_"
      n   <- auto_n_parts(); md <- main_data()
      if (is.null(md) || !col %in% names(md))
        return(tags$p(class = "text-muted small mt-2", "Upload data to see preview."))

      preview_data <- break_preview_unique_values()
      if (preview_data$filtered_rows == 0)
        return(tags$p(class = "text-muted small mt-2",
                      "No data matches this channel's VarName filter."))

      raw_vals <- preview_data$raw_vals
      unique_vals_all <- preview_data$unique_vals
      unique_vals <- unique_vals_all[!is.na(unique_vals_all)]
      unique_part_counts <- if (length(unique_vals)) lengths(strsplit(unique_vals, sep, fixed = TRUE))
      else integer(0)
      part_counts <- unique_part_counts
      vals <- unique_vals_all
      parts <- preview_data$parts
      missing_parts_n <- sum(vapply(parts, function(p) {
        sum(vapply(seq_len(n), function(j) {
          value <- if (!length(p) || length(p) < j) {
            NA_character_
          } else if (j == n) {
            paste(p[j:length(p)], collapse = sep)
          } else {
            p[j]
          }
          is.na(clean_split_part(value))
        }, logical(1)))
      }, integer(1)))
      dist_tbl <- table(part_counts)
      dist_text <- if (length(dist_tbl)) {
        paste(paste0(names(dist_tbl), " part", ifelse(names(dist_tbl) == "1", "", "s"),
                     ": ", as.integer(dist_tbl)), collapse = " | ")
      } else "No values"
      separators <- c("_", " - ", "--", "|", "/")
      sep_hits <- vapply(separators, function(s) {
        if (!length(unique_vals)) 0L else sum(lengths(strsplit(unique_vals, s, fixed = TRUE)) > 1)
      }, integer(1))
      suggestion <- if (sum(part_counts > 1) == 0 && any(sep_hits > 0)) {
        best_sep <- separators[[which.max(sep_hits)]]
        paste0("Try separator \"", best_sep, "\"")
      } else ""
      split_examples <- head(vapply(parts[lengths(parts) > 0], function(p) {
        values <- vapply(seq_len(n), function(j) {
          value <- if (length(p) < j) {
            NA_character_
          } else if (j == n) {
            paste(p[j:length(p)], collapse = sep)
          } else {
            p[j]
          }
          value <- clean_split_part(value)
          if (is.na(value)) "Total" else value
        }, character(1))
        values <- values[nzchar(values)]
        if (length(values)) paste(values, collapse = "_") else "Total"
      }, character(1)), 3)
      rows <- lapply(seq_along(vals), function(i) {
        p <- parts[[i]]
        original <- if (is.na(vals[i])) "Total" else vals[i]
        cells <- c(list(tags$td(original, class = "break-preview-td-orig")),
                   lapply(seq_len(n), function(j) {
                     if (length(p) < j)
                       tags$td("Total", class = "break-preview-td-warn",
                               style = "color:#adb5bd; font-style:italic;")
                     else if (j == n) {
                       value <- clean_split_part(paste(p[j:length(p)], collapse = sep))
                       tags$td(if (is.na(value)) "Total" else value,
                               class = "break-preview-td")
                     } else {
                       value <- clean_split_part(p[j])
                       tags$td(if (is.na(value)) "Total" else value,
                               class = "break-preview-td")
                     }
                   }))
        do.call(tags$tr, cells)
      })
      header <- c(list(tags$th("Original", class = "break-preview-th-orig")),
                  lapply(seq_len(n), function(j)
                    tags$th(paste0("Part ", j), class = "break-preview-th")))
      unique_label <- paste0(
        "Showing all ", format(length(vals), big.mark = ","),
        " unique value", if (length(vals) != 1) "s" else "",
        ". Scroll to inspect."
      )
      tagList(
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            paste0(" Auto-detected ", n, " part", if (n != 1) "s" else "",
                   " using separator \"", sep, "\".")),
        div(class = "break-preview-stats",
            tags$span(class = "break-preview-chip",
                      paste0("Filtered rows ", format(preview_data$filtered_rows, big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Unique values ", format(length(vals), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-ok",
                      paste0("Multi-part ", format(sum(part_counts > 1), big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Single-part ", format(sum(part_counts <= 1), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-warn",
                      paste0("Total-filled ", format(missing_parts_n, big.mark = ",")))),
        div(class = "break-preview-dist", dist_text),
        if (!nzchar(suggestion) && sum(part_counts > 1) == 0)
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              " No multi-part values found with this separator in the filtered values."),
        if (nzchar(suggestion))
          div(class = "break-preview-note break-preview-note-suggest",
              icon("wand-magic-sparkles", class = "icon-xs"),
              paste0(" ", suggestion, " for more useful splits.")),
        tags$strong("Preview (filtered to channel variables):",
                    class = "section-strong mt-2 mb-1"),
        if (length(vals) > 25)
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", unique_label)),
        div(class = "table-responsive break-preview-scroll",
            tags$table(class = "table table-sm mb-1",
                       tags$thead(tags$tr(style = "border-bottom:1px solid #5B9BD5;",
                                          do.call(tagList, header))),
                       tags$tbody(do.call(tagList, rows)))),
        if (length(split_examples))
          div(class = "break-preview-final",
              tags$span(class = "break-preview-final-label", "Split examples"),
              tagList(lapply(split_examples, function(ex)
                tags$code(class = "break-preview-final-code", ex))),
              tags$p(class = "break-preview-final-hint",
                     "VariableName is preserved internally for Process; missing parts are filled with Total.")),
        if (missing_parts_n > 0)
          div(class = "small text-muted mt-1", icon("circle-info", class = "icon-xs"),
              paste0(" ", missing_parts_n, " missing part(s) will be filled with Total.")))
    })
    
    output$break_names_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; n <- auto_n_parts()
      tagList(tags$strong("Name each part:", class = "break-names-label"),
              lapply(seq_len(n), function(i) {
                div(class = "break-part-row",
                    tags$span(paste0("Part ", i, ":"), class = "break-part-label"),
                    textInput(ns(paste0("break_part_", i)), NULL,
                              value = paste0(col, "_", LETTERS[i]),
                              width = "100%") %>%
                      tagAppendAttributes(style = "margin-bottom:0;"))
              }))
    })
    
    observeEvent(input$btn_confirm_break, {
      req(rv$selected)
      enable_confirm_break <- function() {
        session$sendCustomMessage(
          "setActionButtonDisabled",
          list(id = ns("btn_confirm_break"), disabled = FALSE)
        )
      }
      col        <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% "_"
      n          <- isolate(break_n_parts_cache() %||% 2L)
      part_names <- sapply(seq_len(n), function(i)
        trimws(input[[paste0("break_part_", i)]] %||% paste0(col, "_", LETTERS[i])))
      if (any(!nzchar(part_names))) {
        enable_confirm_break()
        showNotification("All part names must be non-empty.", type = "warning"); return()
      }
      if (length(unique(part_names)) < n) {
        enable_confirm_break()
        showNotification("Part names must be unique.", type = "warning"); return()
      }
      conflicts <- intersect(part_names, SPLIT_CHOICES)
      if (length(conflicts) > 0) {
        enable_confirm_break()
        showNotification(paste0("Names conflict with existing choices: ",
                                paste(conflicts, collapse = ", ")), type = "warning"); return()
      }
      nm       <- rv$selected
      existing <- rv$channels[[nm]]$dimension_breaks %||% list()
      removeModal()
      split_preview_cache$key <- NULL
      split_preview_cache$ui <- NULL
      rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
        column = col, separator = sep, n_parts = n, names = part_names)))
      dirty_channels[[nm]] <- TRUE
      showNotification(paste0("Break added: ", col, " \u2192 ",
                              paste(part_names, collapse = " | ")), type = "message")
    })
    
    # ═══════════════════════════════════════════════════════════════════
    # IMPORT / MANAGE CHANNELS MODAL
    # ═══════════════════════════════════════════════════════════════════
    observeEvent(input$btn_manage_channels, {
      showModal(modalDialog(
        title = tagList(icon("file-import"), " Import / Manage Channels"),
        size = "l", easyClose = TRUE,
        layout_columns(col_widths = c(8, 4), class = "mb-3",
                       textInput(ns("ch_mgr_search"), NULL,
                                 placeholder = "Search variables...", width = "100%"),
                       uiOutput(ns("ch_mgr_stats"))),
        div(style = "height:460px; overflow-y:auto; border:1px solid #e3e8ef; border-radius:8px;",
            uiOutput(ns("ch_mgr_list"))),
        footer = modalButton("Close")))
    })

    manager_model_vars <- function() {
      an <- data()$analytical
      md <- data()$details
      if (!is.null(md) && all(c("Type", "VariableName") %in% names(md))) {
        return(md %>%
                 dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "\\bnone\\b")) %>%
                 dplyr::pull(VariableName) %>%
                 unique())
      }
      if (!is.null(an)) {
        exclude_cols <- c("Geography", "Product", "BP_Year", "Period")
        return(setdiff(names(an)[sapply(an, is.numeric)], exclude_cols))
      }
      character(0)
    }

    manager_channel_vars <- function() {
      model_vars <- manager_model_vars()
      candidates <- unique(c(names(rv$channels), names(rv$available_channels)))
      candidates <- candidates[nzchar(candidates)]
      if (!length(model_vars)) return(sort(candidates))

      norm <- function(x) stringr::str_to_lower(normalize_model_var(x))
      allowed_norm <- norm(model_vars)
      candidate_matches <- vapply(candidates, function(nm) {
        cfg <- rv$channels[[nm]] %||% rv$available_channels[[nm]]
        cfg_names <- c(
          nm,
          cfg$channel_name %||% "",
          cfg$model_variable %||% "",
          cfg$analytical_varkeys %||% character(0)
        )
        any(norm(cfg_names) %in% allowed_norm)
      }, logical(1))
      matched_candidates <- candidates[candidate_matches]
      represented_norm <- unique(norm(c(matched_candidates, unlist(lapply(matched_candidates, function(nm) {
        cfg <- rv$channels[[nm]] %||% rv$available_channels[[nm]]
        c(cfg$model_variable %||% "", cfg$analytical_varkeys %||% character(0))
      }), use.names = FALSE))))
      missing_model_vars <- model_vars[!norm(model_vars) %in% represented_norm]
      sort(unique(c(matched_candidates, missing_model_vars)))
    }
    
    output$ch_mgr_stats <- renderUI({
      manager_vars <- manager_channel_vars()
      n_active <- sum(manager_vars %in% names(rv$channels))
      n_total   <- length(manager_vars)
      n_pending <- sum(!manager_vars %in% names(rv$channels))
      vof_pending <- names(rv$available_channels)[
        vapply(rv$available_channels, \(c) identical(c$source %||% "", "vof"), logical(1)) &
          !names(rv$available_channels) %in% names(rv$channels) &
          names(rv$available_channels) %in% manager_vars
      ]
      div(class = "ch-mgr-stats-row",
          if (length(vof_pending) > 0)
            actionButton(ns("btn_add_suggested"),
                          tagList(icon("wand-magic-sparkles"),
                                  paste0(" Add Suggested (", length(vof_pending), " VOF)")),
                          class = "btn-sm btn-add-suggested"),
          tags$span(paste0(n_active, " active"),    class = "badge-count-blue"),
          if (n_pending > 0)
            tags$span(paste0(n_pending, " not imported"), class = "badge-count-neutral"),
          tags$span(paste0(n_total, " in model"), class = "badge-count-gray"))
    })
    
    observeEvent(input$btn_add_suggested, {
      vof_channels <- Filter(\(c) identical(c$source %||% "", "vof"), rv$available_channels)
      if (!length(vof_channels)) {
        showNotification("No VOF channels available.", type = "warning"); return()
      }
      n_added <- n_skipped <- 0L
      for (nm in names(vof_channels)) {
        if (nm %in% names(rv$channels)) { n_skipped <- n_skipped + 1L }
        else { rv$channels[[nm]] <- vof_channels[[nm]]; n_added <- n_added + 1L }
      }
      if (is.null(rv$selected) && length(rv$channels) > 0)
        rv$selected <- names(rv$channels)[1]
      showNotification(paste0(n_added, " VOF channel(s) added",
                              if (n_skipped > 0)
                                paste0(" (", n_skipped, " already active)") else ""),
                       type = "message", duration = 4)
    }, ignoreInit = TRUE)
    
    output$ch_mgr_list <- renderUI({
      in_vars <- manager_channel_vars()
      
      if (!length(in_vars))
        return(div(class = "ch-mgr-empty", icon("clock", class = "icon-empty"),
                   tags$p("Load required files in Setup first.", class = "preview-empty-msg")))
      
      search_term <- trimws(input$ch_mgr_search %||% "")
      if (nzchar(search_term))
        in_vars <- in_vars[stringr::str_detect(
          in_vars, stringr::regex(search_term, ignore_case = TRUE))]
      
      if (!length(in_vars))
        return(div(class = "ch-mgr-empty",
                   icon("magnifying-glass", class = "icon-preview-empty"),
                   paste0('No variables match "', search_term, '"')))
      
      tagList(lapply(seq_along(in_vars), function(i) {
        v        <- in_vars[i]; is_added <- v %in% names(rv$channels)
        src      <- if (is_added) rv$channels[[v]]$source %||% "manual"
        else if (v %in% names(rv$available_channels))
          rv$available_channels[[v]]$source %||% "vof"
        else "not_added"
        badge <- switch(src,
                        vof              = tags$span("VOF",     class = "badge-vof"),
                        keyword_fallback = tags$span("Keyword", class = "badge-kw"),
                        manual           = tags$span("MFF",     class = "badge-mff"),
                        tags$span("Available", class = "badge-available"))
        btn <- if (is_added) {
          tags$button(tagList(icon("minus"), " Remove"),
                      class = "btn btn-outline-danger btn-sm btn-remove-ch",
                      onclick = paste0("Shiny.setInputValue('", ns("ch_mgr_remove"),
                                       "','", v, "',{priority:'event'});"))
        } else {
          tags$button(tagList(icon("plus"), " Add"),
                      class = "btn btn-sm btn-add-ch",
                      onclick = paste0("Shiny.setInputValue('", ns("ch_mgr_add"),
                                       "','", v, "',{priority:'event'});"))
        }
        div(class = "ch-mgr-row",
            div(class = if (is_added) "ch-mgr-dot-active" else "ch-mgr-dot-inactive"),
            tags$span(v, class = if (is_added) "ch-mgr-var-active" else "ch-mgr-var-inactive"),
            badge, btn)
      }))
    })
    
    observeEvent(input$ch_mgr_add, {
      req(nzchar(input$ch_mgr_add %||% ""))
      v <- input$ch_mgr_add
      if (v %in% names(rv$channels)) return()
      if (v %in% names(rv$available_channels)) {
        rv$channels[[v]] <- rv$available_channels[[v]]
      } else {
        varname_include <- get_varname_include_fallback(v)
        act_kw   <- detect_activity_keyword(trimws(stringr::str_remove(
          stringr::str_remove(v, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$")))
        spend_kw <- detect_spend_keyword(data()$all_rags, varname_include)
        an       <- data()$analytical
        actual_mv <- if (!is.null(an)) {
          an_num <- names(an)[sapply(an, is.numeric)]
          if (v %in% an_num) v
          else {
            cand <- paste0(v, "_Total_Total_Total")
            if (cand %in% an_num) cand
            else { pm <- an_num[startsWith(an_num, v)]; if (length(pm)) pm[1] else v }
          }
        } else v
        roi_info <- lookup_roi(actual_mv, v)
        rv$channels[[v]] <- list(
          channel_name = v, model_variable = actual_mv,
          varname_include = varname_include, analytical_varkeys = actual_mv,
          min_period = if (!is.null(an)) min(an$Period, na.rm = TRUE) else NULL,
          max_period = if (!is.null(an)) max(an$Period, na.rm = TRUE) else NULL,
          segment_overrides = list(), activity_keyword = act_kw,
          spend_keyword = spend_kw, split_columns = c("VariableName"),
          saved_merges = list(), dimension_breaks = list(),
          roi = roi_info$value, source = "manual",
          media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
          sub_channel = "", effect = "")
      }
      if (is.null(rv$selected)) rv$selected <- v
      showNotification(paste0("'", v, "' added"), type = "message", duration = 2)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ch_mgr_remove, {
      req(nzchar(input$ch_mgr_remove %||% ""))
      v <- input$ch_mgr_remove
      if (!v %in% names(rv$channels)) return()
      rv$channels[[v]] <- NULL
      if (identical(rv$selected, v))
        rv$selected <- names(rv$channels)[1] %||% NULL
      showNotification(paste0("'", v, "' removed"), type = "message", duration = 2)
    }, ignoreInit = TRUE)
    
    # ═══════════════════════════════════════════════════════════════════
    # SAVE / LOAD CONFIG
    # ═══════════════════════════════════════════════════════════════════
    output$dl_config_csv <- downloadHandler(
      filename = \() paste0("channel_splits_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        if (!length(rv$channels)) {
          readr::write_csv(data.frame(), file); return()
        }
        df <- export_channels_csv(rv$channels)
        if (nrow(df) > 0 && "Type" %in% names(df)) {
          cfg_idx <- which(trimws(df$Type) == "Config")
          if (length(cfg_idx) > 0) {
            df$MinPeriod   <- ""
            df$MaxPeriod   <- ""
            df$MediaChannel <- ""
            
            # Look up MediaChannel from ROI file for each Config row
            rois <- tryCatch(data()$channels_rois, error = \(e) NULL)
            has_roi_ch <- !is.null(rois) &&
              all(c("MainModelVariableName", "Channel") %in% names(rois))
            
            for (i in cfg_idx) {
              ch_nm <- df$Channel[i]
              if (ch_nm %in% names(rv$channels)) {
                ch_cfg <- rv$channels[[ch_nm]]
                
                # MinPeriod / MaxPeriod
                min_d <- tryCatch(as.Date(ch_cfg$min_period), error = \(e) NA)
                max_d <- tryCatch(as.Date(ch_cfg$max_period), error = \(e) NA)
                if (!is.null(min_d) && !is.na(min_d))
                  df$MinPeriod[i] <- format(min_d, "%Y-%m-%d")
                if (!is.null(max_d) && !is.na(max_d))
                  df$MaxPeriod[i] <- format(max_d, "%Y-%m-%d")
                
                # MediaChannel from ROI file
                if (has_roi_ch) {
                  roi_info <- lookup_roi(ch_cfg$model_variable %||% ch_nm, ch_nm)
                  if (!is.na(roi_info$channel)) df$MediaChannel[i] <- roi_info$channel
                }
              }
            }
          }
        }
        readr::write_csv(df, file, na = "")
      }
    )
    
    apply_config_import <- function(config_text) {
      req(config_text)
      tryCatch({
        con <- textConnection(config_text)
        on.exit(close(con))
        df  <- read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
        
        if (!all(c("Channel", "Type") %in% names(df))) {
          showNotification("Config file missing Channel or Type columns.",
                           type = "error"); return()
        }
        
        config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
        merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
        break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
        
        is_new_format  <- all(c("Name", "Splits") %in% names(df))
        has_date_cols  <- all(c("MinPeriod", "MaxPeriod") %in% names(df))
        n_restored <- n_imported <- n_built <- n_merges <- n_skipped <- 0L
        an         <- data()$analytical
        
        recover_dates <- function(nm, row_idx) {
          saved_min <- as.Date(NA); saved_max <- as.Date(NA)
          if (has_date_cols) {
            min_str <- trimws(config_rows$MinPeriod[row_idx] %||% "")
            max_str <- trimws(config_rows$MaxPeriod[row_idx] %||% "")
            saved_min <- tryCatch(
              if (nzchar(min_str)) parse_vof_period(min_str) else as.Date(NA),
              error = \(e) as.Date(NA))
            saved_max <- tryCatch(
              if (nzchar(max_str)) parse_vof_period(max_str) else as.Date(NA),
              error = \(e) as.Date(NA))
          }
          if (is.na(saved_min) || is.na(saved_max)) {
            vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
            if (!is.null(vof_d) &&
                all(c("MainModelVariableName", "MinPeriod", "MaxPeriod") %in% names(vof_d))) {
              vof_ch <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
              if (nrow(vof_ch) > 0) {
                raw_mins <- vof_ch$MinPeriod[!is.na(vof_ch$MinPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MinPeriod)))]
                raw_maxs <- vof_ch$MaxPeriod[!is.na(vof_ch$MaxPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MaxPeriod)))]
                vof_mins <- if (length(raw_mins)) parse_vof_period(raw_mins) else as.Date(NA)
                vof_maxs <- if (length(raw_maxs)) parse_vof_period(raw_maxs) else as.Date(NA)
                vof_mins <- vof_mins[!is.na(vof_mins)]; vof_maxs <- vof_maxs[!is.na(vof_maxs)]
                if (length(vof_mins)) saved_min <- min(vof_mins)
                if (length(vof_maxs)) saved_max <- max(vof_maxs)
              }
            }
          }
          list(min_p = if (!is.na(saved_min)) saved_min else
            if (!is.null(an)) min(an$Period, na.rm = TRUE) else NULL,
            max_p = if (!is.na(saved_max)) saved_max else
              if (!is.null(an)) max(an$Period, na.rm = TRUE) else NULL)
        }

        build_channel_from_config_row <- function(nm, row_idx, splits) {
          dates <- recover_dates(nm, row_idx)
          if (nm %in% names(rv$available_channels)) {
            cfg <- rv$available_channels[[nm]]
            cfg$split_columns    <- splits
            cfg$dimension_breaks <- list()
            cfg$saved_merges     <- list()
            cfg$min_period       <- dates$min_p
            cfg$max_period       <- dates$max_p
            return(cfg)
          }

          varname_include <- get_varname_include_fallback(nm)
          act_kw <- detect_activity_keyword(trimws(stringr::str_remove(
            stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$")))
          spend_kw <- detect_spend_keyword(data()$all_rags, varname_include)
          actual_mv <- if (!is.null(an)) {
            an_num <- names(an)[sapply(an, is.numeric)]
            if (nm %in% an_num) nm
            else {
              cand <- paste0(nm, "_Total_Total_Total")
              if (cand %in% an_num) cand
              else { pm <- an_num[startsWith(an_num, nm)]; if (length(pm)) pm[1] else nm }
            }
          } else nm

          roi_info <- lookup_roi(actual_mv, nm)
          list(
            channel_name = nm, model_variable = actual_mv,
            varname_include = varname_include, analytical_varkeys = actual_mv,
            min_period = dates$min_p, max_period = dates$max_p,
            segment_overrides = list(), activity_keyword = act_kw,
            spend_keyword = spend_kw, split_columns = splits,
            saved_merges = list(), dimension_breaks = list(),
            roi = roi_info$value, source = "manual",
            media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
            sub_channel = "", effect = "")
        }
        
        for (i in seq_len(nrow(config_rows))) {
          nm     <- config_rows$Channel[i]
          splits <- Filter(nzchar, trimws(strsplit(config_rows$SplitOrder[i], "\\|")[[1]]))
          if (!length(splits)) splits <- c("VariableName")
          
          if (nm %in% names(rv$channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_restored <- n_restored + 1L
          } else if (nm %in% names(rv$available_channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_imported <- n_imported + 1L
          } else {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_built <- n_built + 1L
          }
        }
        
        if (nrow(break_rows) > 0) {
          for (i in seq_len(nrow(break_rows))) {
            nm  <- break_rows$Channel[i]
            col <- trimws(break_rows$SplitOrder[i] %||% "")
            if (!nm %in% names(rv$channels) || !nzchar(col)) next
            if (is_new_format) {
              part_names_raw <- break_rows$Name[i]  %||% ""
              sep_n_raw      <- break_rows$Splits[i] %||% ""
              part_names <- Filter(nzchar, trimws(strsplit(part_names_raw, "\\|")[[1]]))
              sep_n      <- Filter(nzchar, trimws(strsplit(sep_n_raw, "\\|")[[1]]))
              sep     <- if (length(sep_n) >= 1) sep_n[1] else "_"
              n_parts <- if (length(sep_n) >= 2) as.integer(sep_n[2]) else length(part_names)
            } else {
              break_info <- Filter(nzchar, trimws(strsplit(
                break_rows$BreakInfo[i] %||% "", "\\|")[[1]]))
              sep     <- if (length(break_info) >= 1) break_info[1] else "_"
              n_parts <- if (length(break_info) >= 2) as.integer(break_info[2]) else 2L
              part_names <- if (length(break_info) > 2) break_info[3:length(break_info)]
              else paste0(col, "_", LETTERS[seq_len(n_parts)])
            }
            if (is.na(n_parts) || n_parts < 1L) next
            if (length(part_names) != n_parts || any(!nzchar(part_names))) next
            existing <- rv$channels[[nm]]$dimension_breaks %||% list()
            if (any(sapply(existing, \(b) b$column == col))) next
            rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
              column = col, separator = sep, n_parts = n_parts, names = part_names)))
          }
        }
        
        if (nrow(merge_rows) > 0) {
          for (i in seq_len(nrow(merge_rows))) {
            nm <- merge_rows$Channel[i]
            if (!nm %in% names(rv$channels)) next
            if (is_new_format) {
              merge_name <- trimws(merge_rows$Name[i]  %||% "")
              merged_raw <- trimws(merge_rows$Splits[i] %||% "")
            } else {
              merge_name <- trimws(merge_rows$SplitOrder[i] %||% "")
              merged_raw <- trimws(merge_rows$BreakInfo[i]  %||% "")
            }
            merged <- Filter(nzchar, trimws(strsplit(merged_raw, "\\|")[[1]]))
            if (!nzchar(merge_name) || !length(merged)) next
            existing       <- rv$channels[[nm]]$saved_merges %||% list()
            existing_names <- vapply(existing, \(m) m$new_name %||% "", character(1))
            if (merge_name %in% existing_names) next
            max_id   <- if (length(existing))
              max(vapply(existing, \(m) m$id %||% 0L, integer(1))) else 0L
            cfg_ch   <- rv$channels[[nm]]
            act_kw   <- cfg_ch$activity_keyword %||% "Impressions"
            spend_kw <- cfg_ch$spend_keyword    %||% "Spend"
            new_spend <- stringr::str_replace_all(
              merge_name, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
            if (new_spend == merge_name) new_spend <- paste0(merge_name, "_", spend_kw)
            rv$channels[[nm]]$saved_merges <- c(existing, list(list(
              id = max_id + 1L, new_name = merge_name, merged = as.list(merged),
              view = "focus", spend_merged = list(), new_spend_name = new_spend,
              active = TRUE, saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
            n_merges <- n_merges + 1L
          }
        }
        
        if (is.null(rv$selected) && length(rv$channels) > 0)
          rv$selected <- names(rv$channels)[1]
        
        parts <- c(
          if (n_restored > 0) paste0(n_restored, " channel(s) updated"),
          if (n_imported > 0) paste0(n_imported, " imported from VOF"),
          if (n_built    > 0) paste0(n_built,    " rebuilt from MFF"),
          if (n_merges   > 0) paste0(n_merges,   " merge(s) loaded"),
          if (n_skipped  > 0) paste0(n_skipped,  " not found"))
        showNotification(paste(parts, collapse = " \u2014 "),
                         type = "message", duration = 5)
      }, error = \(e)
      showNotification(paste("Error:", e$message), type = "error", duration = 8))
    }

    preview_config_import <- function(config_text) {
      con <- textConnection(config_text)
      on.exit(close(con))
      df <- read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
      if (!all(c("Channel", "Type") %in% names(df))) {
        stop("Config file missing Channel or Type columns.")
      }
      config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
      merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
      break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
      ch_names <- config_rows$Channel
      ch_names <- ch_names[!is.na(ch_names) & nzchar(trimws(ch_names))]
      list(
        updated  = sum(ch_names %in% names(rv$channels)),
        imported = sum(!ch_names %in% names(rv$channels) &
                         ch_names %in% names(rv$available_channels)),
        rebuilt  = sum(!ch_names %in% names(rv$channels) &
                         !ch_names %in% names(rv$available_channels)),
        breaks   = nrow(break_rows),
        merges   = nrow(merge_rows),
        skipped  = sum(is.na(config_rows$Channel) | !nzchar(trimws(config_rows$Channel %||% ""))),
        total    = length(ch_names)
      )
    }

    observeEvent(input$config_csv_content, {
      req(input$config_csv_content)
      tryCatch({
        preview <- preview_config_import(input$config_csv_content)
        config_import_pending(input$config_csv_content)
        showModal(modalDialog(
          title = tagList(icon("file-import"), " Preview Config Import"),
          div(class = "import-preview-box",
              div(class = "import-preview-grid",
                  div(class = "import-preview-stat",
                      tags$strong(preview$updated), tags$span("overwritten")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$imported), tags$span("imported")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$rebuilt), tags$span("rebuilt")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$breaks), tags$span("breaks")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$merges), tags$span("merges")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$skipped), tags$span("skipped"))),
              tags$p(class = "text-muted small mb-0",
                     paste0(preview$total, " channel config row(s) detected. ",
                            "Review counts before applying."))),
          footer = tagList(
            actionButton(ns("btn_apply_config_import"),
                         tagList(icon("check"), " Apply Import"),
                         class = "btn-primary"),
            modalButton("Cancel")
          ),
          easyClose = TRUE, size = "m"))
      }, error = \(e) {
        config_import_pending(NULL)
        showNotification(paste("Config preview error:", e$message),
                         type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$btn_apply_config_import, {
      req(config_import_pending())
      apply_config_import(config_import_pending())
      config_import_pending(NULL)
      removeModal()
    })
    
    list(
      channels      = reactive(rv$channels),
      qa_status = reactive({
        ch <- rv$channels
        n_total <- length(ch)
        dirty <- names(ch)[vapply(names(ch), \(nm) isTRUE(dirty_channels[[nm]]), logical(1))]
        configured <- names(ch)[vapply(ch, \(cfg)
          length(cfg$split_columns %||% character(0)) > 0 &&
            nzchar(cfg$model_variable %||% ""), logical(1))]
        missing_roi <- names(ch)[vapply(names(ch), \(nm) {
          is.na(effective_roi(ch[[nm]], nm))
        }, logical(1))]
        with_breaks <- names(ch)[vapply(ch, \(cfg)
          length(cfg$dimension_breaks %||% list()) > 0, logical(1))]
        with_merges <- names(ch)[vapply(ch, \(cfg)
          sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1))) > 0,
          logical(1))]
        list(
          total       = n_total,
          configured  = length(configured),
          dirty       = length(dirty),
          dirty_names = dirty,
          missing_roi = length(missing_roi),
          with_breaks = length(with_breaks),
          with_merges = length(with_merges)
        )
      }),
      update_merges = function(nm, merges) {
        rv$channels[[nm]]$saved_merges <- merges
      }
    )
  })
}
