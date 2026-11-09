# Render role diagnostics and canonical Total Check for the selected channel.
# Reconciliation details show only mismatches; stage counts remain available
# for explaining which effective RAE filter changed the row count.
register_process_diagnostics <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    output$diag_act <- DT::renderDT({
      nm  <- req(input$channel_select); req(results_store[[nm]])
      metric <- active_model_metric()
      total_col <- metric_total_col(metric)
      pct_col <- metric_pct_col(metric)
      df  <- current_model_data(); req(nrow(df) > 0)
      req(total_col %in% names(df))
      if (!pct_col %in% names(df)) {
        grand <- sum(df[[total_col]], na.rm = TRUE)
        df[[pct_col]] <- round(df[[total_col]] / pmax(grand, 1) * 100, 4)
      }

      threshold   <- input$threshold_pct %||% 1
      finite_pcts <- df[[pct_col]][is.finite(df[[pct_col]])]
      max_pct     <- if (length(finite_pcts) > 0 && max(finite_pcts) > 0)
        max(finite_pcts) else 1

      df_display <- df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      df_display <- metric_display_columns(df_display, metric)
      total_col_display <- attr(df_display, "total_col") %||% total_col
      pct_col_display <- attr(df_display, "pct_col") %||% pct_col

      num_fmt  <- intersect(c("sd", "min", "quartile_1", "median",
                              "quartile_3", "max_no_outlier", "max",
                              "SD", "Min", "Q1", "Median", "Q3",
                              "Max No Outlier", "Max"),
                            names(df_display))
      col_defs <- list(list(className = "dt-left", targets = 0))
      activity_target <- which(names(df_display) == total_col_display) - 1
      if (length(activity_target) == 1 && !is.na(activity_target)) {
        col_defs <- c(col_defs, list(
          list(targets = activity_target,
               render = JS("function(d,t){if(t!=='display')return d;",
                           "var n=parseFloat(d);",
                           "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
                           "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
                           "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
                           "return n.toLocaleString();}"))))
      }

      dt <- df_display %>%
        datatable(
          selection = "none",
          options   = list(
            scrollX = TRUE, scrollY = "420px", paging = TRUE, pageLength = 50,
            lengthChange = FALSE, dom = "frtip",
            deferRender = TRUE, scroller = TRUE, autoWidth = FALSE,
            initComplete = dt_blue_callback, columnDefs = col_defs),
          rownames = FALSE)

      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "", digits = 0, mark = ",")
      dt %>%
        formatStyle(pct_col_display,
                    background         = styleColorBar(c(0, max_pct), "#EBF3FB"),
                    backgroundSize     = "100% 90%",
                    backgroundRepeat   = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle(pct_col_display,
                    color = styleInterval(threshold, c("#dc3545", "#333")))
    }, server = TRUE)

    output$for_indices_status_ui <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      cfg <- channels()[[nm]] %||% list()
      status <- cfg$role_pair_status %||% "Missing"
      source <- cfg$role_pair_source %||% "Not found"
      coverage <- suppressWarnings(as.numeric(cfg$role_pair_coverage %||% NA_real_))
      missing_sources <- cfg$role_pair_missing_sources %||% character(0)
      if (identical(status, "Matched")) {
        return(div(
          class = "process-for_indices-status is-ready",
          icon("link"),
          tags$span(paste0("ForIndices connected from ", source, "."))
        ))
      }
      if (identical(status, "Partial")) {
        coverage_text <- if (is.finite(coverage))
          paste0(round(coverage * 100), "% source coverage. ") else ""
        missing_text <- if (length(missing_sources))
          paste0("Missing: ", paste(utils::head(missing_sources, 3), collapse = ", "),
                 if (length(missing_sources) > 3) "…" else "") else ""
        return(div(
          class = "process-for_indices-status is-warning",
          icon("triangle-exclamation"),
          tags$span(paste0(coverage_text, missing_text))
        ))
      }
      div(
        class = "process-for_indices-status is-missing",
        icon("circle-info"),
        tags$span(cfg$role_pair_reason %||%
                    "No compatible ForIndices variable was found in VOF/Details or RAE.")
      )
    })
    # ForIndices table
    output$diag_cost <- DT::renderDT({
      nm <- input$channel_select
      if (!valid_nm(nm))
        return(info_table("Select a channel to review ForIndices.", "info"))
      if (is.null(results_store[[nm]]))
        return(info_table("Process this channel first to review ForIndices.", "info"))

      cfg <- channels()[[nm]] %||% list()
      metric <- active_for_indices_metric()
      for_indices_df <- tryCatch(
        current_for_indices_data(),
        error = function(e) tibble::tibble()
      )
      if (is.null(for_indices_df) || nrow(for_indices_df) == 0) {
        status <- cfg$role_pair_status %||% "Missing"
        msg <- if (identical(status, "Missing")) {
          "No compatible ForIndices variable was found in VOF/Details or RAE."
        } else {
          paste0("No ", metric_label(metric),
                 " ForIndices rows remain after the channel filters.")
        }
        return(info_table(msg, if (identical(status, "Missing")) "info" else "warning"))
      }
      df_display <- for_indices_df %>%
        mutate(Split = strip_common_prefix(VariableSplit)) %>%
        select(Split, everything(), -VariableSplit)
      df_display <- metric_display_columns(df_display, metric)
      total_col_display <- attr(df_display, "total_col") %||% metric_total_col(metric)
      pct_col_display <- attr(df_display, "pct_col") %||% metric_pct_col(metric)
      num_fmt <- intersect(c("sd", "min", "quartile_1", "median",
                             "quartile_3", "max_no_outlier", "max",
                             "SD", "Min", "Q1", "Median", "Q3",
                             "Max No Outlier", "Max"), names(df_display))
      threshold <- input$threshold_pct %||% 1
      max_pct <- if (pct_col_display %in% names(df_display)) {
        finite_pcts <- df_display[[pct_col_display]][is.finite(df_display[[pct_col_display]])]
        if (length(finite_pcts) > 0 && max(finite_pcts) > 0) max(finite_pcts) else 1
      } else {
        1
      }
      col_defs <- list(list(className = "dt-left", targets = 0))
      total_target <- which(names(df_display) == total_col_display) - 1
      if (length(total_target) == 1 && !is.na(total_target)) {
        col_defs <- c(col_defs, list(
          list(targets = total_target,
               render = JS("function(d,t){if(t!=='display')return d;",
                           "var n=parseFloat(d);",
                           "if(n>=1e9)return(n/1e9).toFixed(1)+'B';",
                           "if(n>=1e6)return(n/1e6).toFixed(1)+'M';",
                           "if(n>=1e3)return(n/1e3).toFixed(0)+'K';",
                           "return n.toLocaleString();}"))))
      }
      dt <- df_display %>%
        datatable(
          options = list(
            scrollX = TRUE, scrollY = "420px", paging = TRUE, pageLength = 50,
            lengthChange = FALSE, dom = "frtip",
            deferRender = TRUE, scroller = TRUE, autoWidth = FALSE,
            initComplete = dt_blue_callback, columnDefs = col_defs),
          selection = "none",
          rownames = FALSE)
      if (length(num_fmt) > 0)
        dt <- dt %>% formatCurrency(num_fmt, currency = "", digits = 0, mark = ",")
      if (pct_col_display %in% names(df_display)) {
        dt <- dt %>%
          formatStyle(pct_col_display,
                      background         = styleColorBar(c(0, max_pct), "#EBF3FB"),
                      backgroundSize     = "100% 90%",
                      backgroundRepeat   = "no-repeat",
                      backgroundPosition = "center") %>%
          formatStyle(pct_col_display,
                      color = styleInterval(threshold, c("#dc3545", "#333")))
      }
      dt
    }, server = TRUE)
    # Resolve through the same versioned cache as the sidebar, including SAP
    # apply/undo/reset. Opening this tab is not required to refresh status.
    total_check_data <- reactive({
      nm <- req(input$channel_select)
      req(current_total_check(nm))
    })

    output$total_check_summary_ui <- renderUI({
      check <- total_check_data()
      summary <- check$summary %||% list()
      status <- check$status %||% "error"
      status_label <- switch(
        status,
        ok = "Reconciled",
        mismatch = "Review required",
        "Unable to validate"
      )
      status_icon <- switch(
        status,
        ok = icon("circle-check"),
        mismatch = icon("triangle-exclamation"),
        icon("circle-xmark")
      )
      range_label <- if (!is.null(summary$min_period) &&
                         !is.na(summary$min_period) &&
                         !is.na(summary$max_period)) {
        paste(format(summary$min_period), "→", format(summary$max_period))
      } else {
        "Not available"
      }
      filters <- check$applied_filters %||% character(0)
      warnings <- check$warnings %||% character(0)
      filter_text <- if (length(filters)) paste(filters, collapse = " | ") else "No additional filters"

      div(
        class = paste("total-check-summary", paste0("is-", status)),
        div(
          class = "total-check-summary-header",
          div(
            class = "total-check-summary-title",
            status_icon,
            tags$span(status_label)
          ),
          tags$span(summary$message %||% "", class = "total-check-summary-message")
        ),
        div(
          class = "total-check-summary-grid",
          div(class = "total-check-summary-item",
              tags$span("Analytical variable", class = "total-check-label"),
              tags$strong(summary$model_variable %||% "")),
          div(class = "total-check-summary-item",
              tags$span("Modeled metric", class = "total-check-label"),
              tags$strong(metric_label(summary$modeled_metric %||% "activity"))),
          div(class = "total-check-summary-item",
              tags$span("Effective range", class = "total-check-label"),
              tags$strong(range_label)),
          div(class = "total-check-summary-item",
              tags$span("Comparison level", class = "total-check-label"),
              tags$strong(summary$comparison_level %||% "Not available")),
          div(class = "total-check-summary-item",
              tags$span("Keys checked", class = "total-check-label"),
              tags$strong(format(summary$keys %||% 0L, big.mark = ","))),
          div(class = "total-check-summary-item",
              tags$span("Mismatches", class = "total-check-label"),
              tags$strong(format(summary$mismatches %||% 0L, big.mark = ",")))
        ),
        div(
          class = "total-check-filter-line",
          icon("filter"),
          tags$span(filter_text)
        ),
        if (length(warnings)) {
          div(
            class = "total-check-warning-list",
            lapply(warnings, function(message) div(icon("triangle-exclamation"), message))
          )
        }
      )
    })

    output$total_check_details_ui <- renderUI({
      check <- total_check_data()
      stage_counts <- check$stage_counts %||% data.frame()
      status <- check$status %||% "error"
      detail <- check$detail %||% data.frame()
      mismatch_count <- if (nrow(detail) && "Status" %in% names(detail)) {
        sum(detail$Status != "OK", na.rm = TRUE)
      } else {
        0L
      }
      tags$details(
        class = "total-check-details",
        open = if (!identical(status, "ok")) TRUE else NULL,
        tags$summary(
          icon("table-list"),
          paste0(
            if (identical(status, "ok")) "Show mismatches" else "Review mismatches",
            " (", format(mismatch_count, big.mark = ","), ")"
          )
        ),
        if (nrow(stage_counts)) {
          div(
            class = "total-check-stage-strip",
            lapply(seq_len(nrow(stage_counts)), function(i) {
              div(
                class = "total-check-stage",
                tags$span(stage_counts$Stage[[i]]),
                tags$strong(format(stage_counts$Rows[[i]], big.mark = ","))
              )
            })
          )
        },
        DTOutput(session$ns("diag_check"))
      )
    })

    output$diag_check <- DT::renderDT({
      check <- total_check_data()
      detail <- check$detail %||% tibble::tibble()
      if (nrow(detail) && "Status" %in% names(detail)) {
        detail <- detail %>% dplyr::filter(.data$Status != "OK")
      }
      if (!nrow(detail)) {
        message <- if (identical(check$status %||% "", "ok")) {
          "All checked keys reconcile. No mismatches to display."
        } else {
          check$summary$message %||% "No mismatch detail is available."
        }
        return(
          datatable(
            data.frame(Message = message),
            options = list(initComplete = dt_blue_callback, dom = "t"),
            rownames = FALSE
          )
        )
      }

      display <- detail %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(c(
              "ModelTotal", "RAEEffectiveTotal", "SplitTotal",
              "ModelVsRAEDiff", "RAEVsSplitDiff", "ModelVsSplitDiff"
            )),
            function(x) round(x, 4)
          )
        )
      datatable(
        display,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          initComplete = dt_blue_callback,
          dom = "Bfrtip",
          autoWidth = FALSE,
          buttons = make_export_buttons("total_check", input$channel_select)
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          "Status",
          backgroundColor = styleEqual(
            c("OK", "Source/Filter mismatch", "Processing mismatch", "Missing key"),
            c("#eaf7ef", "#fff4d6", "#fdecec", "#f3e8ff")
          ),
          color = styleEqual(
            c("OK", "Source/Filter mismatch", "Processing mismatch", "Missing key"),
            c("#176b3a", "#7a5200", "#9f1d1d", "#6b21a8")
          ),
          fontWeight = "600"
        )
    }, server = TRUE)
  }, envir = ctx$scope)
  invisible(NULL)
}
