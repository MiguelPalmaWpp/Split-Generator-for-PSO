# Render channel cards and the selected editor, with cached summaries for
# repeated navigation. Deletion changes active channel state only after the
# existing confirmation action.
register_channels_cards <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    render_channel_card <- function(nm, cfg) {
      is_dirty <- isTRUE(dirty_channels[[nm]])
      is_vof   <- identical(cfg$source %||% "vof", "vof")
      is_kw    <- identical(cfg$source %||% "", "keyword_fallback")
      roi_val  <- effective_roi_cached(nm, cfg)
      has_roi  <- !is.na(roi_val)
      n_brk    <- length(cfg$dimension_breaks %||% list())
      n_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1)))
      has_dates <- !is.na(cfg$min_period %||% NA) && !is.na(cfg$max_period %||% NA)
      configured <- length(cfg$split_columns %||% character(0)) > 0 &&
        nzchar(cfg$model_variable %||% "")
      
      src_label <- if (is_vof) "VOF" else if (is_kw) "KW" else "MFF"
      src_class <- if (is_vof) "ch-badge-vof" else if (is_kw) "ch-badge-kw" else "ch-badge-mff"
      
      div(
        class = "ch-card",
        `data-channel` = nm,
        tags$span(icon("xmark"), class = "ch-card-remove", title = "Remove channel",
                  onclick = paste0("Shiny.setInputValue('", ns("delete_nm"), "','",
                                   nm, "',{priority:'event'});")),
        tags$div(
          class = "ch-card-body",
          onclick = paste0("Shiny.setInputValue('", ns("select_nm"), "','",
                           nm, "',{priority:'event'});",
                           "var root=document.getElementById('", ns("channel_list_root"), "');",
                           "if(root){root.querySelectorAll('.ch-card.selected').forEach(function(el){el.classList.remove('selected');});",
                           "this.closest('.ch-card').classList.add('selected');}"),
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

    # Reuse a card until its config/audit signature changes.
    render_channel_card_cached <- function(nm, cfg) {
      card_key <- paste(
        nm,
        isTRUE(dirty_channels[[nm]]),
        channel_config_signature(cfg),
        length(cfg$dimension_breaks %||% list()),
        length(cfg$saved_merges %||% list()),
        rois_signature(),
        sep = "::"
      )
      cached <- cache_get(channel_card_cache, card_key)
      if (!is.null(cached)) return(cached)
      card <- render_channel_card(nm, cfg)
      cache_set(channel_card_cache, card_key, card, max_items = 200L)
      card
    }
    
    output$ch_list <- renderUI({
      profile_step("channel_list", {
        nms <- names(rv$channels)
        if (!length(nms)) {
          div(class = "ch-empty-state",
              icon("file-import", class = "icon-empty-lg"),
              tags$p("No channels loaded.", class = "ch-empty-msg"),
              tags$p(tagList("Click ", tags$strong("Import / Manage Channels"),
                             " to get started."), class = "ch-empty-hint"))
        } else {
          search_term <- ch_search_term()
          if (nzchar(search_term)) {
            nms <- nms[stringr::str_detect(nms, stringr::regex(search_term, ignore_case = TRUE))]
          }
          if (!length(nms)) {
            div(class = "ch-empty-state",
                icon("magnifying-glass", class = "icon-empty-lg"),
                tags$p("No channels match your search.", class = "ch-empty-msg"))
          } else {
            tagList(lapply(nms, function(nm)
              render_channel_card_cached(nm, rv$channels[[nm]])))
          }
        }
      })
    })

    output$ch_selected_marker <- renderUI({
      selected <- rv$selected %||% ""
      tags$script(HTML(sprintf(
        "(function(){
          var root = document.getElementById('%s');
          if (!root) return;
          var selected = %s;
          root.querySelectorAll('.ch-card').forEach(function(el) {
            el.classList.toggle('selected', el.getAttribute('data-channel') === selected);
          });
        })();",
        ns("channel_list_root"),
        jsonlite::toJSON(selected, auto_unbox = TRUE)
      )))
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
      was_selected <- identical(rv$selected, nm)
      rv$channels[[nm]] <- NULL; dirty_channels[[nm]] <- NULL
      clear_manager_channel_cache()
      clear_channel_card_cache(nm)
      if (was_selected) {
        rv$selected <- names(rv$channels)[1] %||% NULL
        clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                             audit = TRUE, cards = FALSE)
      }
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

      tagList(
        uiOutput(ns("channel_audit_ui")),
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

        div(class = "section-block",
            div(class = "section-title-row",
                icon("tag", class = "icon-blue-sm"),
                tags$strong("Dimension Renames"),
                tags$small("Rename split dimensions without changing values.",
                           class = "section-subtitle")),
            if (!aliases_enabled()) {
              actionButton(ns("btn_enable_aliases"),
                           tagList(icon("tag"),
                                   " Do you need to rename a dimension for this channel?"),
                           class = "btn-enable-breaks btn-sm")
            } else {
              tagList(div(class = "d-flex justify-content-end mb-2",
                          actionButton(ns("btn_add_alias"), tagList(icon("plus"), " Add Rename"),
                                       class = "btn-outline-secondary btn-sm")),
                      uiOutput(ns("aliases_list")))
            }),

        hr(class = "mb-4"),
        uiOutput(ns("split_dimensions_ui"))
      )
    })

    output$channel_audit_ui <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(div(class = "preview-empty",
                   icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Select a channel to load audit.",
                          class = "preview-empty-msg")))
      cfg <- rv$channels[[rv$selected]]
      audit <- selected_channel_audit(rv$selected, cfg)
      long_info <- summarize_useful_longitudinals(cfg)
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
      
      roi_info <- lookup_roi(cfg$model_variable %||% rv$selected, rv$selected, cfg)
      roi_ch <- roi_info$channel
      roi_val <- effective_roi_cached(rv$selected, cfg)
      
      info_title <- source_label(cfg)
      info_class <- switch(cfg$source %||% "vof",
                           vof = "info-box-vof", keyword_fallback = "info-box-kw", "info-box-mff")
      info_icon  <- switch(cfg$source %||% "vof",
                           vof = "icon-blue-sm", keyword_fallback = "icon-kw-sm", "icon-mff-sm")
      
      tagList(
        div(class = info_class,
            div(class = "card-header-inner mb-2",
                icon("circle-info", class = info_icon),
                tags$strong(info_title, class = "info-box-title"),
                tags$span("(audit)", class = "section-subtitle")),
            render_channel_audit(rv$selected, cfg, audit),
            tags$details(
              class = "channel-audit-details",
              tags$summary(tagList(icon("list-check"), tags$span("Audit details"))),
              div(
                class = "channel-audit-detail-grid",
                mk_info_row("Effective preview range",
                            paste0(audit$date_mode, " | ",
                                   fmt_audit_count(audit$date_rows), " row(s)")),
                mk_info_row("Connection status", cfg$connection_status %||% "Not audited"),
                if (nzchar(cfg$model_details_type %||% ""))
                  mk_info_row("ModelDetails type", cfg$model_details_type),
                mk_info_row("Modeled variable", cfg$modeled_variable %||% cfg$model_variable %||% "—"),
                mk_info_row("Modeled status", cfg$modeled_type %||% cfg$model_details_type %||% "—"),
                mk_info_row("Modeled role",
                            if (identical(normalize_model_metric(cfg$modeled_role %||% cfg$model_metric %||% "activity"), "spend"))
                              "Spend/Cost" else "Activity"),
                mk_info_row("ForIndices",
                            if (nzchar(cfg$for_indices_variable %||% ""))
                              cfg$for_indices_variable
                            else if (length(cfg$for_indices_varname_include %||% character(0)))
                              paste(cfg$for_indices_varname_include, collapse = ", ")
                            else "Not found"),
                if (nzchar(cfg$for_indices_type %||% ""))
                  mk_info_row("ForIndices status", cfg$for_indices_type),
                mk_info_row("Role connection", cfg$role_pair_status %||% "Missing"),
                mk_info_row("Role source", cfg$role_pair_source %||% "Not found"),
                if (is.finite(suppressWarnings(as.numeric(cfg$role_pair_coverage %||% NA_real_))))
                  mk_info_row("Role coverage",
                              paste0(round(as.numeric(cfg$role_pair_coverage) * 100), "%")),
                if (nzchar(cfg$role_pair_reason %||% ""))
                  mk_info_row("Role evidence", cfg$role_pair_reason),
                mk_info_row("Analytical mapping",
                            if (length(cfg$matched_analytical_cols %||% character(0)))
                              paste(cfg$matched_analytical_cols, collapse = ", ") else "\u2014"),
                mk_info_row("RAE source variables",
                            if (length(cfg$source_variable_names_rae %||% character(0))) {
                              vals <- cfg$source_variable_names_rae
                              shown <- head(vals, 6)
                              tagList(
                                tags$span(paste(shown, collapse = ", ")),
                                if (length(vals) > length(shown))
                                  tags$span(paste0(" +", length(vals) - length(shown), " more"),
                                            class = "audit-muted")
                              )
                            } else {
                              "\u2014"
                            }),
                mk_info_row("Source", source_label(cfg)),
                if (!is.na(roi_ch)) mk_info_row("Channel", roi_ch),
                if (nzchar(cfg$sub_channel %||% "")) mk_info_row("Sub Channel", cfg$sub_channel),
                if (nzchar(cfg$effect %||% ""))      mk_info_row("Effect", cfg$effect),
                mk_info_row("Model metric",
                            if (identical(normalize_model_metric(cfg$model_metric %||% "activity"), "spend"))
                              "Spend/Cost" else "Activity"),
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
                mk_info_row("GeoLabel",
                            {
                              gl <- normalize_geo_label(cfg$geo_label %||% "")
                              if (nzchar(gl)) paste("GeoLabel", sub("^GeoLabel", "", gl)) else "None"
                            }),
                if ((cfg$vof_geo_group_size %||% 0L) > 1L)
                  mk_info_row("Geo group",
                              paste0(cfg$vof_geo_order %||% NA_integer_,
                                     " of ", cfg$vof_geo_group_size %||% 0L)),
                mk_info_row("Geo rule",
                            {
                              sig <- cfg$vof_geo_signature %||% "all"
                              sig <- sub("^token:", "", sig)
                              sig <- sub("^include:", "include ", sig)
                              sig <- sub("^exclude:", "exclude ", sig)
                              if (nzchar(sig)) sig else "all"
                            }),
                if (!is.na(roi_val))
                  mk_info_row("ROI", format(round(roi_val, 2), big.mark = ",")),
                mk_info_row("Breaks", audit$n_breaks),
                if ((cfg$time_break_partition_count %||% 0L) > 0L)
                  mk_info_row("Active time partitions",
                              paste0(cfg$time_break_partition_count, " active time partition",
                                     if ((cfg$time_break_partition_count %||% 0L) != 1L) "s" else "")),
                mk_info_row("Useful longitudinal filters",
                            if (nzchar(cfg$useful_longitudinal_filters_text %||% "")) {
                              tagList(
                                tags$strong(cfg$useful_longitudinal_filters_text),
                                div(class = "audit-muted",
                                    "Applied from AnalyticalVariableName mapping during channel filtering.")
                              )
                            } else if (length(long_info$rows)) {
                              tagList(
                                long_info$rows,
                                div(class = "audit-muted",
                                    "Applied from AnalyticalVariableName mapping during channel filtering.")
                              )
                            } else {
                              "None"
                            }),
                mk_info_row("Config keywords",
                            if (isTRUE(cfg$config_imported))
                              "Loaded from config when present; otherwise inferred from VOF/RAE."
                            else "Inferred from VOF/RAE."),
                if (nzchar(audit$date_message %||% ""))
                  mk_info_row("Date note", audit$date_message),
                if (!is.null(cfg$time_break_label) && nzchar(cfg$time_break_label %||% ""))
                  mk_info_row("Time segment",
                              tags$span(cfg$time_break_label, class = "badge-blue"))
              )
            ))
      )
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
