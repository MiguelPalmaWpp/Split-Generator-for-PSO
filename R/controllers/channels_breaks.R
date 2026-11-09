# Register dimension-break configuration and preview for the selected channel.
# Preview uses the same Unknown-filled transformation as Process and Export.
register_channels_breaks <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    auto_n_parts <- reactive({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
      md  <- main_data()
      if (is.null(md) || !col %in% names(md)) return(2L)
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        cfg_p <- rv$channels[[rv$selected]]
        md <- selected_channel_source(rv$selected, cfg_p, include_spend = FALSE)$data
      } else {
        md <- filter_preview_dates(md, NULL)$data
      }
      if (nrow(md) == 0 || !col %in% names(md)) return(2L)
      vals <- profile_step("break_auto_n_parts", {
        vals <- unique(clean_split_part(md[[col]]))
        vals[!is.na(vals)]
      })
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
      show_break_modal()
    })

    # Limit preview work to observed source values while preserving the final
    # part names and missing-part behavior.
    break_preview_unique_values <- reactive({
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% default_break_separator
      n   <- auto_n_parts()
      md  <- main_data()

      empty_preview <- list(
        filtered_rows = 0L,
        filter_mode = "empty",
        filter_message = "",
        raw_vals = character(0),
        unique_vals = character(0),
        parts = list()
      )
      if (is.null(md) || !col %in% names(md)) return(empty_preview)

      cfg_p <- if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        rv$channels[[rv$selected]] else NULL
      date_filter <- selected_channel_source(rv$selected, cfg_p, include_spend = FALSE)
      vi <- if (!is.null(cfg_p))
        cfg_p$varname_include[nzchar(cfg_p$varname_include %||% "")]
      else character(0)

      cache_key <- paste(
        rv$selected %||% "",
        col,
        sep,
        n,
        paste(vi, collapse = "|"),
        date_filter$key,
        date_filter$mode,
        sep = "::"
      )
      if (identical(break_preview_cache$key, cache_key) &&
          !is.null(break_preview_cache$data)) {
        return(break_preview_cache$data)
      }

      cols_needed <- intersect(c("VariableName", "Period", col), names(date_filter$data))
      md_small <- date_filter$data[, cols_needed, drop = FALSE]
      if (nrow(md_small) == 0 || !col %in% names(md_small)) {
        break_preview_cache$key <- cache_key
        break_preview_cache$data <- empty_preview
        return(empty_preview)
      }

      preview_calc <- profile_step("break_preview", {
        raw_vals <- clean_split_part(md_small[[col]])
        unique_vals <- unique(raw_vals)
        if (!length(unique_vals)) unique_vals <- NA_character_
        parts <- lapply(unique_vals, function(v) {
          if (is.na(v)) character(0) else strsplit(v, sep, fixed = TRUE)[[1]]
        })
        list(raw_vals = raw_vals, unique_vals = unique_vals, parts = parts)
      })

      out <- list(
        filtered_rows = nrow(md_small),
        filter_mode = date_filter$mode,
        filter_message = date_filter$message,
        raw_vals = preview_calc$raw_vals,
        unique_vals = preview_calc$unique_vals,
        parts = preview_calc$parts
      )
      break_preview_cache$key <- cache_key
      break_preview_cache$data <- out
      out
    })
    
    output$break_preview_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
      n   <- auto_n_parts(); md <- main_data()
      if (is.null(md) || !col %in% names(md))
        return(tags$p(class = "text-muted small mt-2", "Upload data to see preview."))

      preview_data <- break_preview_unique_values()
      if (preview_data$filtered_rows == 0)
        return(tags$p(class = "text-muted small mt-2",
                      "No data matches this channel."))

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
      separators <- c(default_break_separator, "_", "--", "|", "/")
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
          if (is.na(value)) "Unknown" else value
        }, character(1))
        values <- values[nzchar(values)]
        if (length(values)) paste(values, collapse = "_") else "Unknown"
      }, character(1)), 3)
      rows <- lapply(seq_along(vals), function(i) {
        p <- parts[[i]]
        original <- if (is.na(vals[i])) "Unknown" else vals[i]
        cells <- c(list(tags$td(original, class = "break-preview-td-orig")),
                   lapply(seq_len(n), function(j) {
                     if (length(p) < j)
                       tags$td("Unknown", class = "break-preview-td-warn",
                               style = "color:#adb5bd; font-style:italic;")
                     else if (j == n) {
                       value <- clean_split_part(paste(p[j:length(p)], collapse = sep))
                      tags$td(if (is.na(value)) "Unknown" else value,
                               class = "break-preview-td")
                     } else {
                       value <- clean_split_part(p[j])
                      tags$td(if (is.na(value)) "Unknown" else value,
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
                      paste0("Unknown-filled ", format(missing_parts_n, big.mark = ",")))),
        div(class = "break-preview-dist", dist_text),
        if (nzchar(preview_data$filter_message %||% ""))
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", preview_data$filter_message)),
        if (!nzchar(suggestion) && sum(part_counts > 1) == 0)
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              " No multi-part values found with this separator in the filtered values."),
        if (nzchar(suggestion))
          div(class = "break-preview-note break-preview-note-suggest",
              icon("wand-magic-sparkles", class = "icon-xs"),
              paste0(" ", suggestion, " for more useful splits.")),
        tags$strong("Preview (filtered to channel variables and Reporting Period):",
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
                     "VariableName is preserved internally for Process; missing parts are filled with Unknown.")),
        if (missing_parts_n > 0)
          div(class = "small text-muted mt-1", icon("circle-info", class = "icon-xs"),
              paste0(" ", missing_parts_n, " missing part(s) will be filled with Unknown.")))
    })
    
    output$break_names_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; n <- auto_n_parts()
      edit <- break_edit_state()
      current <- NULL
      if (!is.null(edit) && identical(edit$channel, rv$selected) &&
          edit$index >= 1L &&
          edit$index <= length(rv$channels[[rv$selected]]$dimension_breaks %||% list())) {
        current <- rv$channels[[rv$selected]]$dimension_breaks[[edit$index]]
      }
      current_names <- as.character(current$names %||% character(0))
      tagList(tags$strong("Name each part:", class = "break-names-label"),
              lapply(seq_len(n), function(i) {
                div(class = "break-part-row",
                    tags$span(paste0("Part ", i, ":"), class = "break-part-label"),
                    textInput(ns(paste0("break_part_", i)), NULL,
                              value = if (i <= length(current_names)) current_names[[i]]
                              else paste0(col, "_", LETTERS[i]),
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
      col        <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
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
      edit <- break_edit_state()
      edit_index <- if (!is.null(edit) && identical(edit$channel, nm)) {
        as.integer(edit$index %||% NA_integer_)
      } else NA_integer_
      is_edit <- !is.na(edit_index) && edit_index >= 1L && edit_index <= length(existing)
      current <- if (is_edit) existing[[edit_index]] else NULL
      other_breaks <- if (is_edit) existing[-edit_index] else existing
      other_names <- unique(unlist(lapply(other_breaks, \(b) b$names %||% character(0))))
      alias_names <- vapply(rv$channels[[nm]]$dimension_aliases %||% list(),
                            \(a) a$alias %||% "", character(1))
      name_conflicts <- intersect(part_names, c(other_names, alias_names))
      if (length(name_conflicts)) {
        enable_confirm_break()
        showNotification(paste0("Names conflict with another break or rename: ",
                                paste(name_conflicts, collapse = ", ")), type = "warning")
        return()
      }

      updated_break <- list(
        column = col,
        separator = sep,
        n_parts = n,
        names = part_names,
        missing_part_value = canonical_break_missing_part_value()
      )
      if (is_edit) {
        old_names <- as.character(current$names %||% character(0))
        mapped_n <- min(length(old_names), length(part_names))
        name_map <- if (mapped_n) stats::setNames(part_names[seq_len(mapped_n)],
                                                  old_names[seq_len(mapped_n)])
        else character(0)
        removed_names <- if (length(old_names) > mapped_n) old_names[-seq_len(mapped_n)]
        else character(0)

        splits <- rv$channels[[nm]]$split_columns %||% character(0)
        if (length(name_map)) {
          for (old_name in names(name_map)) splits[splits == old_name] <- name_map[[old_name]]
        }
        splits <- setdiff(splits, removed_names)

        aliases <- rv$channels[[nm]]$dimension_aliases %||% list()
        if (length(aliases)) {
          keep_alias <- !vapply(aliases, \(a) (a$source %||% "") %in% removed_names, logical(1))
          removed_alias_names <- vapply(aliases[!keep_alias], \(a) a$alias %||% "", character(1))
          aliases <- aliases[keep_alias]
          aliases <- lapply(aliases, function(a) {
            source <- a$source %||% ""
            if (source %in% names(name_map)) a$source <- unname(name_map[[source]])
            a
          })
          splits <- setdiff(splits, removed_alias_names)
          rv$channels[[nm]]$dimension_aliases <- aliases
          if (!length(aliases)) aliases_enabled(FALSE)
        }
        rv$channels[[nm]]$split_columns <- unique(splits[nzchar(splits)])
        existing[[edit_index]] <- updated_break
      } else {
        existing <- c(existing, list(updated_break))
      }
      removeModal()
      clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE, audit = TRUE, cards = TRUE)
      rv$channels[[nm]]$dimension_breaks <- existing
      dirty_channels[[nm]] <- TRUE
      break_edit_state(NULL)
      showNotification(paste0("Break ", if (is_edit) "updated" else "added", ": ",
                              col, " \u2192 ", paste(part_names, collapse = " | ")),
                       type = "message")
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
