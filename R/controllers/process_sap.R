# Apply downloaded SAP edits, retain merge history, and expose undo/reset.
# VariableSplit selects current modeled splits; informational dimensions in
# the uploaded plan cannot redirect an aggregation.
register_process_sap <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Validate current VariableSplit keys before saving or applying any group.
    apply_splits_aggregation_plan <- function(plan) {
      nm <- req(input$channel_select); req(valid_nm(nm))
      if (is.null(plan)) return()
      names(plan) <- clean_sap_column_names(names(plan))
      plan <- hydrate_sap_variable_split(
        plan,
        tryCatch(isolate(current_model_data()), error = function(e) NULL)
      )
      names(plan) <- sub("^\ufeff", "", names(plan))
      names(plan) <- sub("^<U\\+FEFF>", "", names(plan))
      names(plan) <- sub("^Ã¯\\.\\.", "", names(plan))

      missing_cols <- setdiff(c("VariableSplit", "MergeName"), names(plan))
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", "),
                               "| Parsed:", paste(names(plan), collapse = ", ")),
                         type = "error", duration = 8); return()
      }

      plan$VariableSplit <- trimws(as.character(plan$VariableSplit))
      plan$MergeName <- trimws(as.character(plan$MergeName))

      plan_active <- plan[!is.na(plan$MergeName) &
                            nzchar(plan$MergeName) &
                            !is.na(plan$VariableSplit) &
                            nzchar(plan$VariableSplit), ]
      if (nrow(plan_active) == 0) return()

      current_splits <- tryCatch(
        unique(as.character(isolate(current_model_data())$VariableSplit)),
        error = function(e) character(0)
      )
      current_splits <- current_splits[!is.na(current_splits) & nzchar(current_splits)]
      missing_splits <- setdiff(unique(plan_active$VariableSplit), current_splits)
      if (length(missing_splits)) {
        showNotification(
          paste0(
            "SAP contains ", length(missing_splits),
            " VariableSplit value(s) that are not available in the current view. ",
            "Download a current SAP before applying aggregations."
          ),
          type = "error",
          duration = 10
        )
        return()
      }

      res         <- results_store[[nm]]; req(res)
      cfg         <- channels()[[nm]]
      view_filter <- input$period_filter %||% "focus"
      groups      <- split(plan_active, trimws(as.character(plan_active$MergeName)))
      set_hist(nm, c(get_hist(nm), list(results_store[[nm]])))
      new_log <- list(); new_saved <- list(); n_skipped <- 0L

      withProgress(message = "Applying Splits Aggregation Plan...", value = 0, {
        for (i in seq_along(groups)) {
          grp        <- groups[[i]]; merge_name <- names(groups)[i]
          incProgress(1 / length(groups))
          selected_splits <- unique(trimws(as.character(grp$VariableSplit)))
          selected_splits <- selected_splits[nzchar(selected_splits)]
          selected_splits <- selected_splits[!is.na(selected_splits)]
          act_kw   <- cfg$activity_keyword %||% "Impressions"
          spend_kw <- cfg$spend_keyword    %||% "Spend"
          merge_metric <- active_model_metric()
          metric_diag <- if (identical(merge_metric, "spend")) res$cost_diagnoses else res$act_diagnoses

          if (!length(selected_splits)) next
          matching_periods <- if (!is.null(metric_diag) &&
                                  all(c("VariableSplit", "period") %in% names(metric_diag))) {
            unique(metric_diag$period[metric_diag$VariableSplit %in% selected_splits])
          } else character(0)
          matching_periods <- matching_periods[!is.na(matching_periods) & nzchar(matching_periods)]
          merge_view <- if (view_filter %in% matching_periods) {
            view_filter
          } else if (length(matching_periods)) {
            matching_periods[1]
          } else {
            view_filter
          }
          if (FALSE && !nrow(filter(res$act_diagnoses,
                           VariableSplit %in% selected_splits,
                           period == merge_view))) {
            n_skipped <- n_skipped + 1L
            showNotification(paste0("'", merge_name, "': no data â€” skipped."),
                             type = "warning", duration = 5); next
          }

          spend_splits   <- if (identical(merge_metric, "spend")) selected_splits else
            str_replace_all(selected_splits, regex(act_kw, ignore_case = TRUE), spend_kw)
          new_spend_name <- if (identical(merge_metric, "spend")) merge_name else
            str_replace_all(merge_name, regex(act_kw, ignore_case = TRUE), spend_kw)
          if (new_spend_name == merge_name && !identical(merge_metric, "spend"))
            new_spend_name <- paste0(merge_name, "_", spend_kw)
          cost_splits <- if (!is.null(res$cost_diagnoses) &&
                             "VariableSplit" %in% names(res$cost_diagnoses))
            res$cost_diagnoses$VariableSplit else character(0)
          matching_cost <- intersect(spend_splits, cost_splits)

          merge_entry <- list(new_name = merge_name, merged = as.list(selected_splits),
                              view = merge_view, spend_merged = as.list(matching_cost),
                              new_spend_name = new_spend_name, metric = merge_metric)
          res     <- apply_single_merge(res, merge_entry, cfg)
          new_log <- c(new_log, list(list(
            merged = selected_splits, new_name = merge_name, view = merge_view,
            spend_merged = matching_cost, new_spend_name = new_spend_name,
            metric = merge_metric)))
          new_saved <- c(new_saved, list(list(
            merged = as.list(selected_splits), new_name = merge_name, view = merge_view,
            spend_merged = as.list(matching_cost), new_spend_name = new_spend_name,
            metric = merge_metric,
            active = TRUE, saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
        }
      })

      set_res(nm, res); set_log(nm, c(get_log(nm), new_log))
      if (!is.null(update_merges) && length(new_saved) > 0) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        max_id   <- if (length(existing)) max(sapply(existing, \(m) m$id %||% 0L)) else 0L
        for (i in seq_along(new_saved)) new_saved[[i]]$id <- max_id + i
        saved_after <- c(existing, new_saved)
        update_merges(nm, saved_after)
        mark_result_current(nm, cfg, saved_after)
      }
      n_ok <- length(new_log)
      showNotification(
        paste0(n_ok, " aggregation group(s) applied",
               if (n_skipped > 0) paste0(" (", n_skipped, " skipped)") else "",
               if (!is.null(update_merges) && n_ok > 0) " â€” saved to config." else "."),
        type = if (n_ok > 0) "message" else "warning")
    }

    observeEvent(input$merge_plan_content, {
      req(input$merge_plan_content)
      plan <- tryCatch(
        read_sap_plan_content(input$merge_plan_content),
        error = function(e) {
          showNotification(pso_public_error(
            e, "process.sap.content", "SAP could not be read. Check the file format."
          ),
                           type = "error", duration = 8)
          NULL
        }
      )
      apply_splits_aggregation_plan(plan)
    })

    observeEvent(input$merge_plan_file, {
      upload <- req(input$merge_plan_file)
      ext <- tolower(tools::file_ext(upload$name[[1]]))
      plan <- tryCatch({
        if (identical(ext, "xlsx")) {
          as.data.frame(readxl::read_excel(upload$datapath[[1]]),
                        check.names = FALSE, stringsAsFactors = FALSE)
        } else {
          content <- paste(readLines(upload$datapath[[1]], warn = FALSE,
                                     encoding = "UTF-8"), collapse = "\n")
          read_sap_plan_content(content)
        }
      }, error = function(e) {
        showNotification(pso_public_error(
          e, "process.sap.file", "SAP could not be read. Check the file format."
        ),
                         type = "error", duration = 8)
        NULL
      })
      apply_splits_aggregation_plan(plan)
    }, ignoreInit = TRUE)

    output$config_merge_report <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      logs <- get_log(nm)
      cfg_logs <- Filter(\(x) identical(x$source %||% "", "config"), logs)
      if (!length(cfg_logs)) return(NULL)
      n_ok <- sum(vapply(cfg_logs, \(x) isTRUE(x$applied), logical(1)))
      n_review <- length(cfg_logs) - n_ok
      review <- Filter(\(x) !isTRUE(x$applied), cfg_logs)
      detail_rows <- lapply(utils::head(review, 4), function(x) {
        missing <- c(x$missing %||% character(0), x$ambiguous %||% character(0))
        missing <- missing[!is.na(missing) & nzchar(missing)]
        examples <- x$closest_examples %||% character(0)
        div(
          class = "config-merge-report-row",
          tags$strong(x$new_name %||% "Unnamed merge"),
          tags$span(class = "text-muted",
                    paste0("Matched ", x$matched_count %||% 0L, "/",
                           x$requested_count %||% 0L,
                           if (nzchar(x$view %||% "")) paste0(" | ", x$view) else "")),
          if (length(missing))
            tags$small(class = "text-muted",
                       paste0("Missing: ", paste(utils::head(missing, 3), collapse = " | "),
                              if (length(missing) > 3) paste0(" +", length(missing) - 3, " more") else "")),
          if (length(examples))
            tags$small(class = "text-muted",
                       paste0("Closest: ", paste(utils::head(examples, 3), collapse = " | ")))
        )
      })
      div(
        class = paste("config-merge-report", if (n_review > 0) "review" else "ok"),
        div(class = "config-merge-report-head",
            tags$strong(if (n_review > 0) "Merge application report" else "Config reproduced"),
            tags$span(class = if (n_review > 0) "badge-stale" else "badge-ready",
                      paste0(n_ok, "/", length(cfg_logs), " applied"))),
        if (n_review > 0)
          tagList(
            tags$p(class = "text-muted small mb-1",
                   "Some saved merges did not match the generated splits for this run."),
            detail_rows
          )
        else
          tags$p(class = "text-muted small mb-0",
                 "All saved config merges matched generated splits.")
      )
    })
    # Undo
    observeEvent(input$btn_undo, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      hist <- get_hist(nm)
      if (!length(hist)) {
        showNotification("No merges to undo.", type = "warning"); return()
      }
      set_res(nm, hist[[length(hist)]]); set_hist(nm, hist[-length(hist)])
      log <- get_log(nm)
      if (length(log) > 0) set_log(nm, log[-length(log)])
      if (!is.null(update_merges)) {
        existing <- channels()[[nm]]$saved_merges %||% list()
        if (length(existing) > 0) {
          saved_after <- existing[-length(existing)]
          update_merges(nm, saved_after)
          mark_result_current(nm, channels()[[nm]], saved_after)
        }
      }
      showNotification("Last merge undone \u2014 removed from config.", type = "message")
    })
    # Reset merges
    observeEvent(input$btn_reset_merges, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", class = "banner-icon-yellow"),
                        " Reset all merges"),
        tags$p("Reset all merges for ", tags$strong(nm), "?"),
        tags$p(class = "text-muted small",
               "This will also clear them from the saved config."),
        footer = tagList(
          actionButton(session$ns("btn_confirm_reset"), "Reset", class = "btn-danger"),
          modalButton("Cancel")),
        easyClose = TRUE, size = "s"))
    })

    observeEvent(input$btn_confirm_reset, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      orig <- get_orig(nm); req(orig)
      set_res(nm, orig); set_log(nm, list()); set_hist(nm, list())
      if (!is.null(update_merges)) {
        update_merges(nm, list())
        mark_result_current(nm, channels()[[nm]], list())
      }
      removeModal()
      showNotification(paste("All merges reset for", nm, "\u2014 config cleared."),
                       type = "message")
    }, ignoreInit = TRUE)
    # Merge history card
    output$merge_history_card <- renderUI({
      nm   <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      log  <- get_log(nm); hist <- get_hist(nm)
      if (!length(log)) return(NULL)
      card(
        card_header(
          div(class = "card-header-inner",
              icon("code-merge", class = "icon-blue-sm"),
              "Merge History",
              tags$small(paste0(length(log), " merge",
                                if (length(log) != 1) "s" else "",
                                " \u2014 auto-saved to config"),
                         class = "merge-history-subtitle"))
        ),
        tagList(
          div(class = "mb-3",
              lapply(rev(seq_along(log)), function(i) {
                m       <- log[[i]]; is_last <- i == length(log)
                vb <- switch(m$view %||% "all",
                             "focus"    = tags$span("FOCUS",     class = "badge-focus-sm"),
                             "nonfocus" = tags$span("NON-FOCUS", class = "badge-nonfocus-sm"),
                             tags$span("ALL", class = "badge-all-sm"))
                div(class = "merge-history-row",
                    icon("arrow-right",
                         class = if (is_last) "merge-icon-latest" else "merge-icon-old"),
                    div(class = "flex-1-mw0",
                        div(class = "d-flex align-items-center gap-2 flex-wrap",
                            tags$strong(m$new_name,
                                        class = if (is_last) "merge-name-latest"
                                        else "merge-name-old"),
                            vb,
                            if (is_last) tags$span("latest", class = "latest-marker")),
                        tags$div(class = "merge-splits-text",
                                 paste(strip_common_prefix(m$merged), collapse = " + "))))
              })),
          div(class = "d-flex gap-2",
              if (length(hist) > 0)
                actionButton(session$ns("btn_undo"),
                             tagList(icon("rotate-left"), " Undo Last"),
                             class = "btn-outline-secondary btn-sm flex-fill"),
              actionButton(session$ns("btn_reset_merges"),
                           tagList(icon("trash"), " Reset All"),
                           class = paste("btn-outline-danger btn-sm",
                                         if (length(hist) > 0) "flex-fill" else "w-100")))
        )
      )
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
