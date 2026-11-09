# Build modeled and ForIndices table data from exact split_manifest roles.
# The modeled metric owns SAP controls; ForIndices remains a read-only view.
register_process_role_data <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    output$period_filter_ui <- renderUI({
      nm  <- input$channel_select; if (!valid_nm(nm)) return(NULL)
      res <- results_store[[nm]]
      has_data <- (!is.null(res$act_diagnoses) && nrow(res$act_diagnoses) > 0) ||
        (!is.null(res$rag) && nrow(res$rag) > 0)
      if (is.null(res) || !has_data) return(NULL)
      counts <- period_metric_counts()
      n_focus <- counts$focus %||% 0L
      n_nf <- counts$nonfocus %||% 0L
      div(class = "process-view-strip",
          div(class = "process-view-main",
              tags$span(icon("filter", class = "icon-blue-sm"),
                        tags$strong(" View", class = "process-control-label")),
              div(class = "process-period-choice",
                  radioButtons(session$ns("period_filter"), NULL,
                               choices  = c("Focus" = "focus", "Non-Focus" = "nonfocus"),
                               selected = isolate(input$period_filter %||% "focus"),
                               inline = TRUE))),
          div(class = "process-view-counts",
              tags$span(paste0("Focus: ", n_focus), class = "badge-focus"),
              tags$span(paste0("Non-Focus: ", n_nf), class = "badge-nonfocus")))
    })
    # current_act_data
    # Rebuild display data from processed modeled splits and manifest roles.
    build_current_act_data <- function(filter_val = "focus") {
      nm  <- req(input$channel_select)
      res <- req(results_store[[nm]])

      add_pct <- function(df) {
        if (is.null(df) || nrow(df) == 0 || !"total_activity" %in% names(df))
          return(df %||% tibble())
        grand <- sum(df$total_activity, na.rm = TRUE)
        df %>% mutate(pct_total_activity = round(total_activity / pmax(grand, 1) * 100, 4))
      }

      cfg        <- channels()[[nm]]
      gcfg       <- config()
      spend_kw_f <- cfg$spend_keyword %||% "Spend"

      req(!is.null(gcfg$start_report_date), !is.null(gcfg$end_report_date),
          length(gcfg$start_report_date) == 1, length(gcfg$end_report_date) == 1)

      rag_df     <- as.data.frame(res$rag)
      num_cols_r <- names(rag_df)[sapply(rag_df, is.numeric)]
      num_cols_r <- num_cols_r[!grepl(spend_kw_f, num_cols_r, ignore.case = TRUE)]
      if (!length(num_cols_r)) return(tibble(VariableSplit = character()))

      dt     <- data.table::as.data.table(rag_df)
      agg_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), by = "Period", .SDcols = num_cols_r]
      rag_agg        <- as.data.frame(agg_dt)
      rag_agg$Period <- as.Date(rag_agg$Period, origin = "1970-01-01")

      start_d <- as.Date(gcfg$start_report_date)
      end_d   <- as.Date(gcfg$end_report_date)
      req(!is.na(start_d), !is.na(end_d))

      rag_agg <- switch(filter_val,
                        "focus"    = rag_agg[rag_agg$Period >= start_d & rag_agg$Period <= end_d, ],
                        "nonfocus" = rag_agg[rag_agg$Period <  start_d, ],
                        rag_agg)
      if (nrow(rag_agg) == 0) return(tibble(VariableSplit = character()))

      act_kw  <- cfg$activity_keyword %||% "Clicks"
      id_cols <- intersect("Period", names(rag_agg))

      act_cols_keyword <- grep(act_kw, names(rag_agg), ignore.case = TRUE, value = TRUE)
      act_cols_merged  <- if (
        is.null(res$act_diagnoses) || nrow(res$act_diagnoses) == 0 ||
        !"VariableSplit" %in% names(res$act_diagnoses)
      ) character(0) else {
        intersect(unique(res$act_diagnoses$VariableSplit),
                  setdiff(names(rag_agg), id_cols))
      }
      act_cols <- union(act_cols_keyword, act_cols_merged)
      if (!length(act_cols)) return(tibble(VariableSplit = character()))

      df <- splits_summary(rag_agg[, c(id_cols, act_cols), drop = FALSE], "activity")
      if (is.null(df) || nrow(df) == 0) return(tibble(VariableSplit = character()))

      df %>%
        filter(total_activity > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        add_pct() %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
    }

    selected_result_version <- reactive({
      nm <- input$channel_select %||% ""
      if (!nzchar(nm)) return(0L)
      as.integer(result_versions[[nm]] %||% 0L)
    })

    build_current_spend_from_rag <- function(res, cfg, filter_val = "focus") {
      if (is.null(res) || is.null(res$rag)) return(empty_spend_diag())
      rag_df <- as.data.frame(res$rag)
      if (!nrow(rag_df)) return(empty_spend_diag())

      if ("Period" %in% names(rag_df)) {
        gcfg <- config()
        start_d <- suppressWarnings(as.Date(gcfg$start_report_date))
        end_d <- suppressWarnings(as.Date(gcfg$end_report_date))
        rag_df$Period <- as.Date(rag_df$Period, origin = "1970-01-01")
        if (!is.na(start_d) && !is.na(end_d)) {
          rag_df <- switch(filter_val,
                           "focus" = rag_df[rag_df$Period >= start_d & rag_df$Period <= end_d, ],
                           "nonfocus" = rag_df[rag_df$Period < start_d, ],
                           rag_df)
          if (!nrow(rag_df)) return(empty_spend_diag())
        }
      }

      spend_kw <- cfg$spend_keyword %||% "Spend"
      id_cols <- intersect(c(res$cross_cols %||% character(0),
                             config()$cross_cols %||% character(0),
                             "Geography", "Product", "Period", "BP_Year"),
                           names(rag_df))
      num_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_cols)
      spend_cols_keyword <- grep(spend_kw, num_cols, ignore.case = TRUE, value = TRUE)
      spend_cols_diag <- if (!is.null(res$cost_diagnoses) &&
                             "VariableSplit" %in% names(res$cost_diagnoses)) {
        intersect(unique(res$cost_diagnoses$VariableSplit), num_cols)
      } else {
        character(0)
      }
      spend_cols <- unique(c(spend_cols_keyword, spend_cols_diag))
      if (!length(spend_cols)) return(empty_spend_diag())

      active_cols <- spend_cols[vapply(spend_cols, function(col) {
        vals <- suppressWarnings(as.numeric(rag_df[[col]]))
        any(!is.na(vals) & vals != 0)
      }, logical(1))]
      if (!length(active_cols)) return(empty_spend_diag())

      keep_cols <- union(intersect("Period", names(rag_df)), active_cols)
      diag_source <- rag_df[, keep_cols, drop = FALSE]
      if ("Period" %in% names(diag_source)) {
        diag_dt <- data.table::as.data.table(diag_source)
        diag_dt <- diag_dt[, lapply(.SD, sum, na.rm = TRUE),
                           by = "Period", .SDcols = active_cols]
        diag_source <- as.data.frame(diag_dt)
      }
      out <- splits_summary(diag_source, "spend")
      if (is.null(out) || !nrow(out)) return(empty_spend_diag())

      out <- out %>%
        filter(!is.na(total_spend) & total_spend > 0) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
      if (nrow(out)) {
        grand <- sum(out$total_spend, na.rm = TRUE)
        out <- out %>%
          mutate(pct_total_spend = round(total_spend / pmax(grand, 1) * 100, 4))
      }
      out
    }
    # current_spend_data
    # Build the secondary spend view from RAE only when no processed spend
    # result is available for the channel.
    build_current_spend_from_rae <- function(nm, cfg, filter_val = "focus") {
      d <- tryCatch(data(), error = \(e) NULL)
      if (is.null(d) || is.null(d$all_rags) ||
          !"VariableName" %in% names(d$all_rags) ||
          !"Period" %in% names(d$all_rags)) {
        return(empty_spend_diag())
      }

      source_data <- as.data.frame(d$all_rags)
      source_data$Period <- if (inherits(source_data$Period, "Date")) {
        source_data$Period
      } else {
        parse_period_robust(source_data$Period)
      }
      source_data <- source_data[!is.na(source_data$Period), , drop = FALSE]
      if (!nrow(source_data)) return(empty_spend_diag())

      min_p <- tryCatch(as.Date(cfg$min_period), error = \(e) as.Date(NA))
      max_p <- tryCatch(as.Date(cfg$max_period), error = \(e) as.Date(NA))
      if (!is.na(min_p)) source_data <- source_data[source_data$Period >= min_p, , drop = FALSE]
      if (!is.na(max_p)) source_data <- source_data[source_data$Period <= max_p, , drop = FALSE]
      if (!nrow(source_data)) return(empty_spend_diag())

      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (length(vi) > 0) {
        vi <- expand_varname_include_with_spend(
          unique(source_data$VariableName),
          vi,
          cfg$spend_keyword %||% NULL
        )
        vi <- expand_analytical_keys_to_variable_names(
          unique(source_data$VariableName),
          vi
        )
        vi <- unique(trimws(as.character(vi)))
        vi <- vi[!is.na(vi) & nzchar(vi)]
      }
      if (length(vi) > 0) {
        vn <- trimws(as.character(source_data$VariableName))
        match_mode <- cfg$varname_match_mode %||%
          if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
        keep <- if (identical(match_mode, "exact")) {
          tolower(vn) %in% tolower(vi)
        } else {
          pattern <- paste(
            paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")),
            collapse = "|"
          )
          grepl(pattern, vn, ignore.case = TRUE, perl = TRUE)
        }
        source_data <- source_data[keep %in% TRUE, , drop = FALSE]
      }
      if (!nrow(source_data)) return(empty_spend_diag())

      filter_regex <- function(df, col, pats) {
        if (!col %in% names(df)) return(df)
        for (p in pats %||% character(0)) {
          if (nchar(p %||% "") > 0)
            df <- df[!grepl(p, df[[col]], ignore.case = TRUE), , drop = FALSE]
        }
        df
      }

      source_data <- filter_regex(source_data, "VariableName", cfg$varname_exclude)
      source_data <- filter_regex(source_data, "Campaign", cfg$campaign_exclude)
      source_data <- filter_regex(source_data, "Outlet", cfg$outlet_exclude)
      source_data <- filter_regex(source_data, "Creative", cfg$creative_exclude)
      has_geo_overrides <- length(cfg$segment_overrides %||% list()) > 0 &&
        any(vapply(cfg$segment_overrides %||% list(), function(o) {
          length(o$geography_exclude %||% character(0)) > 0
        }, logical(1)))
      if (!has_geo_overrides)
        source_data <- filter_regex(source_data, "Geography", cfg$geography_exclude)

      source_data <- tryCatch(
        filter_to_analytical_varkey_combinations(
          source_data,
          cfg,
          d$schema_metadata %||% NULL
        ),
        error = \(e) source_data
      )
      if (has_geo_overrides) {
        seg_ovr <- Filter(\(o) isTRUE(o$seg == 1L), cfg$segment_overrides %||% list())
        geo_exc <- if (length(seg_ovr) > 0)
          seg_ovr[[1]]$geography_exclude %||% character(0)
        else
          cfg$geography_exclude %||% character(0)
        source_data <- filter_regex(source_data, "Geography", geo_exc)
      }
      if (!nrow(source_data)) return(empty_spend_diag())

      gcfg <- config()
      start_d <- tryCatch(as.Date(gcfg$start_report_date), error = \(e) as.Date(NA))
      end_d <- tryCatch(as.Date(gcfg$end_report_date), error = \(e) as.Date(NA))
      update_label <- gcfg$update_label %||% "Focus"
      if (!is.na(end_d)) source_data <- source_data[source_data$Period <= end_d, , drop = FALSE]
      if (!nrow(source_data)) return(empty_spend_diag())

      source_data$VariableValue <- suppressWarnings(as.numeric(as.character(source_data$VariableValue)))
      source_data$VariableValue[is.na(source_data$VariableValue)] <- 0
      source_data <- apply_dimension_breaks(
        source_data,
        cfg$dimension_breaks %||% list(),
        channel_name = cfg$channel_name %||% nm
      )
      source_data <- apply_dimension_aliases(source_data, cfg$dimension_aliases %||% list())
      split_cols_technical <- unique(c("VariableName", cfg$split_columns %||% character(0)))
      source_data$SplitName <- build_split_name_from_columns(source_data, split_cols_technical)

      period_tag <- rep(NA_character_, nrow(source_data))
      if (!is.na(start_d))
        period_tag[source_data$Period < start_d] <- "nonfocus"
      if (!is.na(start_d) && !is.na(end_d))
        period_tag[source_data$Period >= start_d & source_data$Period <= end_d] <- "focus"
      if (is.na(start_d) && !is.na(end_d))
        period_tag[source_data$Period <= end_d] <- "focus"

      keep <- !is.na(period_tag) & period_tag == filter_val
      source_data <- source_data[keep, , drop = FALSE]
      if (!nrow(source_data)) return(empty_spend_diag())

      nf_sfx <- {
        tbr <- cfg$time_break_label %||% ""
        if (nzchar(tbr)) paste0("Before ", update_label, "|", tbr)
        else paste0("Before ", update_label)
      }
      source_data$VariableSplit <- if (identical(filter_val, "focus")) {
        paste0(source_data$SplitName, "_", update_label)
      } else {
        paste0(source_data$SplitName, "_", nf_sfx)
      }

      spend_kw <- cfg$spend_keyword %||% "Spend"
      source_data <- source_data[
        grepl(spend_kw, source_data$VariableSplit, ignore.case = TRUE),
        , drop = FALSE
      ]
      if (!nrow(source_data)) return(empty_spend_diag())

      agg <- data.table::as.data.table(source_data)[
        , .(VariableValue = sum(VariableValue, na.rm = TRUE)),
        by = .(Period, VariableSplit)
      ]
      if (!nrow(agg)) return(empty_spend_diag())

      wide <- data.table::dcast(
        agg,
        Period ~ VariableSplit,
        value.var = "VariableValue",
        fun.aggregate = sum,
        fill = 0
      )
      out <- splits_summary(as.data.frame(wide), "spend")
      if (is.null(out) || !nrow(out)) return(empty_spend_diag())

      out <- out %>%
        filter(!is.na(total_spend) & total_spend > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
      if (nrow(out)) {
        grand <- sum(out$total_spend, na.rm = TRUE)
        out <- out %>%
          mutate(pct_total_spend = round(total_spend / pmax(grand, 1) * 100, 4))
      }
      out
    }

    build_current_spend_data <- function(filter_val = "focus") {
      nm  <- req(input$channel_select)
      res <- req(results_store[[nm]])
      cfg <- channels()[[nm]] %||% list()
      rag_out <- build_current_spend_from_rag(res, cfg, filter_val)
      if (!is.null(rag_out) && nrow(rag_out) > 0) return(rag_out)
      cost_df <- res$cost_diagnoses
      if (is.null(cost_df) || nrow(cost_df) == 0 ||
          !"VariableSplit" %in% names(cost_df) ||
          !"total_spend" %in% names(cost_df)) {
        return(build_current_spend_from_rae(nm, cfg, filter_val))
      }
      if ("period" %in% names(cost_df)) {
        cost_df <- cost_df[cost_df$period == filter_val, , drop = FALSE]
      }
      out <- cost_df %>%
        filter(!is.na(total_spend) & total_spend > 0) %>%
        select(-any_of(c("seg", "period", "model_var"))) %>%
        mutate(across(where(is.numeric), \(x) round(x, 4)))
      if (nrow(out)) {
        grand <- sum(out$total_spend, na.rm = TRUE)
        out <- out %>%
          mutate(pct_total_spend = round(total_spend / pmax(grand, 1) * 100, 4))
      }
      if (!nrow(out)) build_current_spend_from_rae(nm, cfg, filter_val) else out
    }

    # Prevent activity/spend columns from other roles entering the selected
    # Focus or Non-Focus table even when names look similar.
    filter_data_for_role <- function(df, res, role, metric, period_scope) {
      if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df))
        return(df %||% tibble::tibble())
      manifest <- res$split_manifest %||% tibble::tibble()
      required <- c("Role", "VariableSplit", "PeriodScope")
      if (!nrow(manifest) || !all(required %in% names(manifest))) return(df)
      role_rows <- manifest %>%
        dplyr::filter(
          .data$Role == role,
          .data$PeriodScope == period_scope
        )
      if ("MetricRole" %in% names(role_rows)) {
        role_rows <- role_rows %>%
          dplyr::filter(
            !nzchar(.data$MetricRole) |
              .data$MetricRole == normalize_model_metric(metric)
          )
      }
      split_names <- unique(role_rows$VariableSplit)
      split_names <- split_names[!is.na(split_names) & nzchar(split_names)]
      if (!length(split_names)) return(df[0, , drop = FALSE])
      df[df$VariableSplit %in% split_names, , drop = FALSE]
    }

    build_role_data <- function(role = c("modeled", "for_indices"),
                                metric, filter_val = "focus") {
      role <- match.arg(role)
      nm <- req(input$channel_select)
      res <- req(results_store[[nm]])
      raw <- if (identical(normalize_model_metric(metric), "spend")) {
        build_current_spend_data(filter_val)
      } else {
        build_current_act_data(filter_val)
      }
      filter_data_for_role(raw, res, role, metric, filter_val)
    }

    current_model_data <- reactive({
      metric <- active_model_metric()
      build_role_data("modeled", metric, input$period_filter %||% "focus")
    }) %>% bindCache(input$channel_select, input$period_filter,
                     selected_result_version(), cache = performance_cache)

    current_for_indices_data <- reactive({
      metric <- active_for_indices_metric()
      build_role_data("for_indices", metric, input$period_filter %||% "focus")
    }) %>% bindCache(input$channel_select, input$period_filter,
                     selected_result_version(), cache = performance_cache)

    period_metric_counts <- reactive({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]]))
        return(list(focus = 0L, nonfocus = 0L))
      metric <- active_model_metric()
      count_for <- function(period) {
        tryCatch(
          nrow(build_role_data("modeled", metric, period)),
          error = function(e) 0L
        )
      }
      list(
        focus = count_for("focus"),
        nonfocus = count_for("nonfocus")
      )
    }) %>% bindCache(input$channel_select, selected_result_version(),
                     cache = performance_cache)
    # Activity KPIs
    output$activity_kpis <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      metric <- active_model_metric()
      total_col <- metric_total_col(metric)
      pct_col <- metric_pct_col(metric)
      df <- current_model_data(); if (nrow(df) == 0 || !total_col %in% names(df)) return(NULL)
      if (!pct_col %in% names(df)) {
        grand <- sum(df[[total_col]], na.rm = TRUE)
        df[[pct_col]] <- round(df[[total_col]] / pmax(grand, 1) * 100, 4)
      }
      threshold     <- input$threshold_pct %||% 1
      total_splits  <- nrow(df)
      above_thresh  <- sum(df[[pct_col]] >= threshold, na.rm = TRUE)
      below_thresh  <- sum(df[[pct_col]] <  threshold, na.rm = TRUE)
      channel_total <- sum(df[[total_col]], na.rm = TRUE)
      kpis <- list(
        list(label = "Total splits",    value = total_splits,
             icon = "layer-group", box_class = "kpi-box kpi-box-blue",
             icon_class = "kpi-icon-blue"),
        list(label = paste0("Above ", threshold, "%"), value = above_thresh,
             icon = "arrow-up",   box_class = "kpi-box kpi-box-green",
             icon_class = "kpi-icon-green"),
        list(label = paste0("Below ", threshold, "% (review)"), value = below_thresh,
             icon = "arrow-down", box_class = "kpi-box kpi-box-red",
             icon_class = "kpi-icon-red"),
        list(label = paste(metric_label(metric), "total"), value = fmt_compact(channel_total),
             icon = "chart-bar",  box_class = "kpi-box kpi-box-blue",
             icon_class = "kpi-icon-blue")
      )
      div(class = "process-kpi-grid",
          lapply(kpis, function(k)
            div(class = k$box_class,
                icon(k$icon, class = k$icon_class),
                div(class = "kpi-copy",
                    tags$strong(k$value, class = "kpi-value"),
                    tags$small(k$label,  class = "kpi-label")))))
    })
    # Threshold UI
    output$threshold_ui <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(class = "process-threshold-strip",
          div(class = "process-threshold-main",
              tags$span(icon("sliders", class = "icon-blue-sm"),
                        tags$strong("Small split threshold", class = "process-control-label")),
              div(class = "process-threshold-input",
                  numericInput(session$ns("threshold_pct"), NULL,
                               value = isolate(input$threshold_pct %||% 1),
                               min = 0, max = 100, step = 0.5),
                  tags$span("%", class = "pct-symbol"))),
          tags$span(class = "process-hint-text",
                    icon("circle-info", class = "icon-xs"),
                    " Splits below threshold are shown in red. Default is 1%."))
    })
    output$merge_plan_toolbar <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm) || is.null(results_store[[nm]])) return(NULL)
      div(class = "process-merge-actions",
          downloadButton(session$ns("dl_merge_plan"),
                         label = tagList(icon("download"), " Download SAP"),
                         class = "btn-outline-secondary btn-sm"),
          div(class = "sap-file-input",
              fileInput(session$ns("merge_plan_file"), NULL,
                        accept = c(".xlsx", ".csv", ".tsv", ".txt"),
                        buttonLabel = "Apply SAP",
                        placeholder = "", width = NULL)),
          tags$span(class = "hint-text",
                    icon("circle-info", class = "icon-xs"),
                    " Assign the same ", tags$strong("MergeName"),
                    " to the splits you want to aggregate."))
    })

    output$dl_merge_plan <- downloadHandler(
      filename = function() {
        nm <- input$channel_select %||% "channel"
        paste0("splits_aggregation_plan_", nm, "_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        nm <- isolate(input$channel_select)
        df <- tryCatch(isolate(current_model_data()), error = function(e) NULL)
        res <- if (valid_nm(nm)) isolate(results_store[[nm]]) else NULL
        cfg <- if (valid_nm(nm)) isolate(channels()[[nm]]) else list()
        df_out <- build_splits_aggregation_plan(
          df = df,
          res = res %||% list(),
          cfg = cfg %||% list(),
          period_scope = isolate(input$period_filter %||% "focus"),
          metric = isolate(active_model_metric())
        )
        threshold <- isolate(input$threshold_pct %||% 1)
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "SAP")
        openxlsx::writeData(wb, "SAP", df_out, keepNA = FALSE,
                            headerStyle = openxlsx::createStyle(
                              fgFill = "#EAF3FC", textDecoration = "bold",
                              fontColour = "#29435F", border = "bottom",
                              borderColour = "#9DB9D5"
                            ))
        openxlsx::freezePane(wb, "SAP", firstRow = TRUE)
        openxlsx::addFilter(wb, "SAP", rows = 1, cols = seq_len(ncol(df_out)))
        openxlsx::setColWidths(wb, "SAP", cols = seq_len(ncol(df_out)), widths = "auto")
        pct_col <- grep("^Pct Total (Activity|Spend)$", names(df_out))
        if (length(pct_col) && nrow(df_out)) {
          red_style <- openxlsx::createStyle(
            fontColour = "#C62828", fgFill = "#FDECEC", textDecoration = "bold"
          )
          openxlsx::conditionalFormatting(
            wb, "SAP", cols = pct_col[[1]], rows = 2:(nrow(df_out) + 1L),
            rule = paste0(openxlsx::int2col(pct_col[[1]]), "2<", threshold),
            style = red_style,
            type = "expression"
          )
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  }, envir = ctx$scope)
  invisible(NULL)
}
