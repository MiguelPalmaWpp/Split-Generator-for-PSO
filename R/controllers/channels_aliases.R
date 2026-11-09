# Register break/rename modal builders and alias actions for the selected
# channel. The source dimension is preserved; aliases change output naming.
register_channels_aliases <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Share the break editor modal with the break controller without changing
    # its existing input IDs or edit-state contract.
    show_break_modal <- function(edit_index = NULL) {
      req(rv$selected, rv$selected %in% names(rv$channels))
      nm <- rv$selected
      cfg <- rv$channels[[nm]]
      md <- main_data()
      if (is.null(md)) {
        showNotification("Upload RAE Datafile first.", type = "warning")
        return(invisible(NULL))
      }

      breaks <- cfg$dimension_breaks %||% list()
      is_edit <- !is.null(edit_index) && edit_index >= 1L && edit_index <= length(breaks)
      current <- if (is_edit) breaks[[edit_index]] else NULL
      already_broken <- vapply(breaks, \(b) b$column %||% "", character(1))
      if (is_edit) already_broken <- already_broken[-edit_index]
      available <- setdiff(c("Campaign", "Outlet", "Creative"), already_broken)
      if (is_edit && nzchar(current$column %||% "") &&
          !current$column %in% available) {
        available <- c(current$column, available)
      }
      if (!length(available)) {
        showNotification("All dimensions already have a break.", type = "warning")
        return(invisible(NULL))
      }

      if (is_edit) {
        break_edit_state(list(channel = nm, index = as.integer(edit_index)))
        break_n_parts_cache(as.integer(current$n_parts %||% length(current$names %||% character(0))))
      } else {
        break_edit_state(NULL)
        break_n_parts_cache(2L)
      }
      title <- if (is_edit) "Edit Dimension Break" else "Configure Dimension Break"
      action <- if (is_edit) "Save Changes" else "Add Break"
      showModal(modalDialog(
        title = tagList(icon(if (is_edit) "pen" else "scissors"), paste0(" ", title)),
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            " This break will be applied to both activity and spend columns automatically."),
        selectInput(ns("break_col"), "Column to break", choices = available,
                    selected = current$column %||% available[[1]]),
        textInput(ns("break_sep"), "Separator",
                  value = current$separator %||% default_break_separator),
        uiOutput(ns("break_preview_ui")), uiOutput(ns("break_names_ui")),
        footer = tagList(actionButton(ns("btn_confirm_break"),
                                      tagList(icon("check"), paste0(" ", action)),
                                      class = "btn-primary") %>%
                         tagAppendAttributes(
                           onclick = "this.disabled=true; this.classList.add('disabled');"
                         ),
                         modalButton("Cancel")),
        easyClose = FALSE, size = "m"))
      invisible(NULL)
    }

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
            div(class = "break-item-actions",
                actionButton(ns(paste0("edit_break_", i)), icon("pen"),
                             title = "Edit break", `aria-label` = "Edit break",
                             class = "btn btn-link p-0 btn-break-edit"),
                actionButton(ns(paste0("remove_break_", i)), icon("xmark"),
                             title = "Remove break", `aria-label` = "Remove break",
                             class = "btn btn-link p-0 btn-break-remove")))
      }))
    })
    
    observe({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return()
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!is.null(session$userData$remove_break_obs))
        lapply(session$userData$remove_break_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      remove_obs <- lapply(seq_along(breaks), function(i) {
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
              dirty_channels[[local_nm]] <- TRUE
              clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                                   audit = TRUE, cards = TRUE)
              showNotification(paste0("Break on '", brk$column, "' removed."),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
      edit_obs <- lapply(seq_along(breaks), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("edit_break_", local_i)]], {
            if (identical(rv$selected, local_nm) &&
                local_i <= length(rv$channels[[local_nm]]$dimension_breaks %||% list())) {
              show_break_modal(local_i)
            }
          }, ignoreInit = TRUE)
        })
      })
      session$userData$remove_break_obs <- c(remove_obs, edit_obs)
    })

    # Aliases change a split dimension label while preserving its values.
    show_alias_modal <- function(edit_index = NULL) {
      req(rv$selected, rv$selected %in% names(rv$channels))
      nm <- rv$selected
      cfg <- rv$channels[[nm]]
      aliases <- cfg$dimension_aliases %||% list()
      is_edit <- !is.null(edit_index) && edit_index >= 1L && edit_index <= length(aliases)
      current <- if (is_edit) aliases[[edit_index]] else NULL
      cross_cols <- get_cross_cols()
      choices <- effective_split_choices(cfg$dimension_breaks %||% list(), cross_cols, list())
      other_aliases <- if (is_edit) aliases[-edit_index] else aliases
      used_sources <- vapply(other_aliases, \(a) a$source %||% "", character(1))
      choices <- setdiff(choices, used_sources)
      if (is_edit && nzchar(current$source %||% "") &&
          !current$source %in% choices) {
        choices <- c(current$source, choices)
      }
      if (!length(choices)) {
        showNotification("No dimensions available to rename.", type = "warning")
        return(invisible(NULL))
      }

      if (is_edit) {
        alias_edit_state(list(channel = nm, index = as.integer(edit_index)))
      } else {
        alias_edit_state(NULL)
      }
      title <- if (is_edit) "Edit Dimension Rename" else "Configure Dimension Rename"
      action <- if (is_edit) "Save Changes" else "Add Rename"
      showModal(modalDialog(
        title = tagList(icon(if (is_edit) "pen" else "tag"), paste0(" ", title)),
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            " This creates an alias for the selected dimension. Values are not changed."),
        selectInput(ns("alias_source"), "Dimension to rename", choices = choices,
                    selected = current$source %||% choices[[1]]),
        textInput(ns("alias_name"), "New dimension name",
                  value = current$alias %||% ""),
        uiOutput(ns("alias_preview_ui")),
        footer = tagList(actionButton(ns("btn_confirm_alias"),
                                      tagList(icon("check"), paste0(" ", action)),
                                      class = "btn-primary"),
                         modalButton("Cancel")),
        easyClose = FALSE, size = "m"))
      invisible(NULL)
    }

    output$aliases_list <- renderUI({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return(NULL)
      aliases <- rv$channels[[rv$selected]]$dimension_aliases %||% list()
      if (!length(aliases))
        return(tags$p(class = "text-muted small mb-0",
                      icon("circle-info", class = "icon-xs"),
                      " No renames configured. Click \"Add Rename\" to alias a dimension."))
      tagList(lapply(seq_along(aliases), function(i) {
        als <- aliases[[i]]
        div(class = "break-item", icon("tag", class = "icon-blue-sm"),
            div(class = "break-item-text",
                tags$strong(als$source %||% ""),
                tags$span(" renamed as ", class = "text-muted"),
                tags$span(als$alias %||% "", class = "text-blue fw-semibold")),
            div(class = "break-item-actions",
                actionButton(ns(paste0("edit_alias_", i)), icon("pen"),
                             title = "Edit rename", `aria-label` = "Edit rename",
                             class = "btn btn-link p-0 btn-break-edit"),
                actionButton(ns(paste0("remove_alias_", i)), icon("xmark"),
                             title = "Remove rename", `aria-label` = "Remove rename",
                             class = "btn btn-link p-0 btn-break-remove")))
      }))
    })

    observe({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return()
      aliases <- rv$channels[[rv$selected]]$dimension_aliases %||% list()
      if (!is.null(session$userData$remove_alias_obs))
        lapply(session$userData$remove_alias_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      remove_obs <- lapply(seq_along(aliases), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("remove_alias_", local_i)]], {
            curr <- rv$channels[[local_nm]]$dimension_aliases %||% list()
            if (local_i <= length(curr)) {
              als <- curr[[local_i]]
              splits <- rv$channels[[local_nm]]$split_columns %||% character(0)
              splits[splits == (als$alias %||% "")] <- als$source %||% ""
              rv$channels[[local_nm]]$split_columns <- unique(splits[nzchar(splits)])
              rv$channels[[local_nm]]$dimension_aliases <- curr[-local_i]
              if (!length(rv$channels[[local_nm]]$dimension_aliases)) aliases_enabled(FALSE)
              dirty_channels[[local_nm]] <- TRUE
              clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                                   audit = TRUE, cards = TRUE)
              showNotification(paste0("Rename removed: ", als$alias, " -> ", als$source),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
      edit_obs <- lapply(seq_along(aliases), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("edit_alias_", local_i)]], {
            if (identical(rv$selected, local_nm) &&
                local_i <= length(rv$channels[[local_nm]]$dimension_aliases %||% list())) {
              show_alias_modal(local_i)
            }
          }, ignoreInit = TRUE)
        })
      })
      session$userData$remove_alias_obs <- c(remove_obs, edit_obs)
    })

    observeEvent(input$btn_add_alias, {
      show_alias_modal()
    })

    alias_preview_values <- reactive({
      source <- input$alias_source %||% ""
      alias <- trimws(input$alias_name %||% "")
      empty_preview <- list(
        source = source,
        alias = alias,
        filtered_rows = 0L,
        unique_vals = character(0),
        filter_message = "",
        filter_mode = "empty"
      )
      if (!nzchar(source) || is.null(rv$selected) ||
          !rv$selected %in% names(rv$channels)) {
        return(empty_preview)
      }
      cfg <- rv$channels[[rv$selected]]
      date_filter <- selected_channel_source(rv$selected, cfg, include_spend = FALSE)
      cache_key <- paste(
        rv$selected %||% "",
        source,
        alias,
        date_filter$key,
        breaks_signature(cfg$dimension_breaks %||% list()),
        sep = "::"
      )
      if (identical(alias_preview_cache$key, cache_key) &&
          !is.null(alias_preview_cache$data)) {
        return(alias_preview_cache$data)
      }

      out <- profile_step("alias_preview", {
        md <- date_filter$data
        if (!nrow(md)) {
          empty_preview
        } else {
          break_cols <- unique(vapply(cfg$dimension_breaks %||% list(),
                                      \(b) b$column %||% "", character(1)))
          cols_needed <- unique(c("VariableName", "Period", source, break_cols))
          cols_needed <- intersect(cols_needed[nzchar(cols_needed)], names(md))
          md_small <- md[, cols_needed, drop = FALSE]
          if (length(cfg$dimension_breaks %||% list())) {
            md_small <- tryCatch(
              apply_dimension_breaks(md_small, cfg$dimension_breaks %||% list()),
              error = \(e) md_small
            )
          }
          if (!source %in% names(md_small)) {
            modifyList(empty_preview, list(
              filtered_rows = nrow(date_filter$data),
              filter_message = date_filter$message %||% "",
              filter_mode = date_filter$mode
            ))
          } else {
            vals <- clean_split_part(md_small[[source]])
            vals <- unique(vals)
            vals <- vals[!is.na(vals) & nzchar(vals)]
            list(
              source = source,
              alias = alias,
              filtered_rows = nrow(md_small),
              unique_vals = vals,
              filter_message = date_filter$message %||% "",
              filter_mode = date_filter$mode
            )
          }
        }
      })
      alias_preview_cache$key <- cache_key
      alias_preview_cache$data <- out
      out
    })

    output$alias_preview_ui <- renderUI({
      source <- input$alias_source %||% ""
      alias <- trimws(input$alias_name %||% "")
      if (!nzchar(source))
        return(tags$p(class = "text-muted small mt-2", "Select a dimension."))
      if (!nzchar(alias))
        alias <- "NewName"

      preview <- alias_preview_values()
      values <- preview$unique_vals %||% character(0)
      rows <- lapply(values, function(v) {
        tags$tr(
          tags$td(v, class = "break-preview-td-orig"),
          tags$td(v, class = "break-preview-td")
        )
      })

      tagList(
        div(class = "break-info-box",
            icon("tag", class = "icon-blue-sm"),
            tags$span(source, class = "fw-semibold"),
            tags$span(" -> ", class = "text-muted"),
            tags$span(alias, class = "text-blue fw-semibold"),
            tags$span(class = "text-muted small", " | values are not changed")),
        div(class = "break-preview-stats",
            tags$span(class = "break-preview-chip",
                      paste0("Filtered rows ", format(preview$filtered_rows, big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Unique values ", format(length(values), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-ok",
                      paste0(source, " -> ", alias))),
        if (nzchar(preview$filter_message %||% ""))
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", preview$filter_message)),
        if (!length(values)) {
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              " No values found for this dimension in the filtered channel data.")
        } else {
          div(
            class = "break-preview-scroll alias-preview-scroll",
            tags$table(
              class = "table table-sm table-borderless mb-0 alias-preview-table",
              tags$thead(tags$tr(
                tags$th(source, class = "break-preview-th-orig"),
                tags$th(alias, class = "break-preview-th")
              )),
              tags$tbody(rows)
            )
          )
        }
      )
    })

    observeEvent(input$btn_confirm_alias, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      nm <- rv$selected
      cfg <- rv$channels[[nm]]
      source <- trimws(input$alias_source %||% "")
      alias <- trimws(input$alias_name %||% "")
      cross_cols <- get_cross_cols()
      source_choices <- effective_split_choices(cfg$dimension_breaks %||% list(), cross_cols, list())
      if (!nzchar(source) || !source %in% source_choices) {
        showNotification("Select a valid source dimension.", type = "warning")
        return()
      }
      if (!nzchar(alias)) {
        showNotification("New dimension name must be non-empty.", type = "warning")
        return()
      }
      if (identical(source, alias)) {
        showNotification("New dimension name must be different from the source.", type = "warning")
        return()
      }
      existing <- cfg$dimension_aliases %||% list()
      edit <- alias_edit_state()
      edit_index <- if (!is.null(edit) && identical(edit$channel, nm)) {
        as.integer(edit$index %||% NA_integer_)
      } else NA_integer_
      is_edit <- !is.na(edit_index) && edit_index >= 1L && edit_index <= length(existing)
      current <- if (is_edit) existing[[edit_index]] else NULL
      comparison_aliases <- if (is_edit) existing[-edit_index] else existing
      existing_sources <- vapply(comparison_aliases, \(a) a$source %||% "", character(1))
      existing_aliases <- vapply(comparison_aliases, \(a) a$alias %||% "", character(1))
      if (source %in% existing_sources) {
        showNotification("This dimension already has a rename.", type = "warning")
        return()
      }
      conflicts <- setdiff(source_choices, source)
      conflicts <- union(conflicts, existing_aliases)
      if (alias %in% conflicts) {
        showNotification("New dimension name conflicts with an existing dimension.", type = "warning")
        return()
      }
      md <- main_data()
      if (!is.null(md) && alias %in% names(md) && !identical(alias, source)) {
        showNotification("New dimension name already exists as a RAE column.", type = "warning")
        return()
      }
      splits <- cfg$split_columns %||% character(0)
      if (is_edit) {
        splits[splits == (current$alias %||% "")] <- current$source %||% ""
      }
      splits[splits == source] <- alias
      rv$channels[[nm]]$split_columns <- unique(splits[nzchar(splits)])
      updated_alias <- list(source = source, alias = alias)
      if (is_edit) {
        existing[[edit_index]] <- updated_alias
      } else {
        existing <- c(existing, list(updated_alias))
      }
      rv$channels[[nm]]$dimension_aliases <- existing
      dirty_channels[[nm]] <- TRUE
      alias_edit_state(NULL)
      removeModal()
      clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE, audit = TRUE, cards = TRUE)
      showNotification(
        paste0("Rename ", if (is_edit) "updated" else "added", ": ", source, " -> ", alias),
        type = "message"
      )
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
