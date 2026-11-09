# Register Import / Manage Channels selection and add/remove actions.
# Generated VOF candidates come from the Media Index; explicit non-modeled
# ModelDetails rows cannot be restored through a manual add.
register_channels_manager <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    observeEvent(input$btn_manage_channels, {
      manager_modal_open(TRUE)
      showModal(modalDialog(
        title = tagList(icon("file-import"), " Import / Manage Channels"),
        size = "l", easyClose = FALSE,
        layout_columns(col_widths = c(8, 4), class = "mb-3",
                       textInput(ns("ch_mgr_search"), NULL,
                                 placeholder = "Search variables...", width = "100%"),
                       uiOutput(ns("ch_mgr_stats"))),
        div(style = "height:460px; overflow-y:auto; border:1px solid #e3e8ef; border-radius:8px;",
            uiOutput(ns("ch_mgr_list"))),
        footer = actionButton(ns("ch_mgr_close"), "Close", class = "btn btn-secondary")))
    })

    observeEvent(input$ch_mgr_close, {
      manager_modal_open(FALSE)
      removeModal()
      if ((is.null(rv$selected) || !rv$selected %in% names(rv$channels)) &&
          length(rv$channels) > 0) {
        rv$selected <- names(rv$channels)[1]
      }
    }, ignoreInit = TRUE)

    # Cache modeled-variable choices separately from rendered manager rows.
    manager_model_vars <- function() {
      an <- data()$analytical
      md <- data()$details
      key <- paste(
        isolate(manager_cache_version()),
        if (!is.null(md)) nrow(md) else 0L,
        if (!is.null(md)) paste(names(md), collapse = "|") else "",
        if (!is.null(md) && "VariableName" %in% names(md))
          sum(nchar(as.character(md$VariableName)), na.rm = TRUE) else 0L,
        if (!is.null(md) && "Type" %in% names(md))
          sum(nchar(as.character(md$Type)), na.rm = TRUE) else 0L,
        if (!is.null(an)) nrow(an) else 0L,
        if (!is.null(an)) paste(names(an), collapse = "|") else "",
        sep = "::"
      )
      if (identical(manager_model_vars_cache$key, key) &&
          !is.null(manager_model_vars_cache$value)) {
        return(manager_model_vars_cache$value)
      }
      out <- profile_step("manager_model_vars", {
        if (!is.null(md) && all(c("Type", "VariableName") %in% names(md))) {
          md %>%
            dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "\\bnone\\b")) %>%
            dplyr::pull(VariableName) %>%
            unique()
        } else if (!is.null(an)) {
          exclude_cols <- c("Geography", "Product", "BP_Year", "Period")
          setdiff(names(an)[sapply(an, is.numeric)], exclude_cols)
        } else {
          character(0)
        }
      })
      manager_model_vars_cache$key <- key
      manager_model_vars_cache$value <- out
      out
    }

    manager_channel_vars <- function() {
      model_vars <- manager_model_vars()
      key <- paste(
        isolate(manager_cache_version()),
        paste(model_vars, collapse = "|"),
        paste(names(rv$channels), collapse = "|"),
        paste(names(rv$available_channels), collapse = "|"),
        sep = "::"
      )
      if (identical(manager_channel_vars_cache$key, key) &&
          !is.null(manager_channel_vars_cache$value)) {
        return(manager_channel_vars_cache$value)
      }
      out <- profile_step("manager_channel_vars", {
        candidates <- unique(c(names(rv$channels), names(rv$available_channels)))
        candidates <- candidates[nzchar(candidates)]
        # A VOF channel rejected by ModelDetails cannot return through the
        # manual-channel fallback.
        inactive <- inactive_vof_channel_names(candidates)
        if (length(inactive)) candidates <- setdiff(candidates, inactive)
        if (!length(model_vars)) {
          sort(candidates)
        } else {
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
      })
      manager_channel_vars_cache$key <- key
      manager_channel_vars_cache$value <- out
      out
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

    render_manager_row_cached <- function(v) {
      is_added <- v %in% names(rv$channels)
      cfg <- if (is_added) {
        rv$channels[[v]]
      } else if (v %in% names(rv$available_channels)) {
        rv$available_channels[[v]]
      } else {
        NULL
      }
      src <- if (!is.null(cfg)) cfg$source %||% "manual" else "not_added"
      row_key <- paste(
        v,
        isTRUE(is_added),
        src,
        if (!is.null(cfg)) channel_config_signature(cfg) else "missing",
        sep = "::"
      )
      cached <- cache_get(manager_row_cache, row_key)
      if (!is.null(cached)) return(cached)

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
      row <- div(class = "ch-mgr-row",
                 div(class = if (is_added) "ch-mgr-dot-active" else "ch-mgr-dot-inactive"),
                 tags$span(v, class = if (is_added) "ch-mgr-var-active" else "ch-mgr-var-inactive"),
                 badge, btn)
      cache_set(manager_row_cache, row_key, row, max_items = 400L)
      row
    }

    observeEvent(input$btn_add_suggested, {
      profile_step("ch_mgr_add_suggested", {
        manager_vars <- manager_channel_vars()
        vof_names <- names(rv$available_channels)[
          vapply(rv$available_channels, \(c) identical(c$source %||% "", "vof"), logical(1)) &
            !names(rv$available_channels) %in% names(rv$channels) &
            names(rv$available_channels) %in% manager_vars
        ]
        vof_channels <- rv$available_channels[vof_names]
        if (!length(vof_channels)) {
          showNotification("No VOF channels available.", type = "warning"); return()
        }
        channels_next <- rv$channels
        n_added <- n_skipped <- 0L
        for (nm in names(vof_channels)) {
          if (nm %in% names(channels_next)) {
            n_skipped <- n_skipped + 1L
          } else {
            channels_next[[nm]] <- vof_channels[[nm]]
            n_added <- n_added + 1L
          }
        }
        rv$channels <- channels_next
        clear_manager_channel_cache()
        clear_preview_caches(source = FALSE, split = FALSE, brk = FALSE,
                             audit = FALSE, cards = TRUE)
        if (is.null(rv$selected) && length(rv$channels) > 0 && !isTRUE(manager_modal_open()))
          rv$selected <- names(rv$channels)[1]
        showNotification(paste0(n_added, " VOF channel(s) added",
                                if (n_skipped > 0)
                                  paste0(" (", n_skipped, " already active)") else ""),
                         type = "message", duration = 4)
      })
    }, ignoreInit = TRUE)
    
    output$ch_mgr_list <- renderUI({
      profile_step("ch_mgr_list", {
        in_vars <- manager_channel_vars()

        if (!length(in_vars))
          return(div(class = "ch-mgr-empty", icon("clock", class = "icon-empty"),
                     tags$p("Load required files in Setup first.", class = "preview-empty-msg")))

        search_term <- ch_mgr_search_term()
        if (nzchar(search_term))
          in_vars <- in_vars[stringr::str_detect(
            in_vars, stringr::regex(search_term, ignore_case = TRUE))]

        if (!length(in_vars))
          return(div(class = "ch-mgr-empty",
                     icon("magnifying-glass", class = "icon-preview-empty"),
                     paste0('No variables match "', search_term, '"')))

        total_vars <- length(in_vars)
        visible_vars <- head(in_vars, manager_render_limit)
        showing_note <- if (total_vars > length(visible_vars)) {
          div(class = "ch-mgr-limit-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" Showing ", length(visible_vars), " of ",
                     format(total_vars, big.mark = ","),
                     " variables. Use search to narrow the list."))
        } else {
          NULL
        }

        tagList(
          showing_note,
          lapply(visible_vars, render_manager_row_cached)
        )
      })
    })
    
    observeEvent(input$ch_mgr_add, {
      profile_step("ch_mgr_add", {
        req(nzchar(input$ch_mgr_add %||% ""))
        v <- input$ch_mgr_add
        if (v %in% names(rv$channels)) return()
        if (length(inactive_vof_channel_names(v))) {
          showNotification(
            paste0("Channel not added: ModelDetails marks '", v,
                   "' as not modelled (NONE)."),
            type = "warning", duration = 7
          )
          return()
        }
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
            model_metric = "activity",
            spend_keyword = spend_kw, split_columns = c("VariableName"),
            saved_merges = list(), dimension_breaks = list(),
            roi = roi_info$value, source = "manual",
            media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
            sub_channel = "", effect = "")
        }
        clear_manager_channel_cache()
        clear_channel_card_cache(v)
        if (is.null(rv$selected) && !isTRUE(manager_modal_open())) rv$selected <- v
        showNotification(paste0("'", v, "' added"), type = "message", duration = 2)
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$ch_mgr_remove, {
      profile_step("ch_mgr_remove", {
        req(nzchar(input$ch_mgr_remove %||% ""))
        v <- input$ch_mgr_remove
        if (!v %in% names(rv$channels)) return()
        was_selected <- identical(rv$selected, v)
        rv$channels[[v]] <- NULL
        dirty_channels[[v]] <- NULL
        clear_manager_channel_cache()
        clear_channel_card_cache(v)
        if (was_selected) {
          rv$selected <- names(rv$channels)[1] %||% NULL
          clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                               audit = TRUE, cards = FALSE)
        }
        showNotification(paste0("'", v, "' removed"), type = "message", duration = 2)
      })
    }, ignoreInit = TRUE)
  }, envir = ctx$scope)
  invisible(NULL)
}
