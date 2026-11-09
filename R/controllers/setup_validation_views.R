# Render Setup's cross-file alignment checks and concise validation summary.
# Differences are diagnostic; these views do not rewrite RAE or Analytical.
register_setup_validation_views <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Compare source key/date coverage before Channels consumes the files.
    comparison_result <- reactive({
      req(rv$main_data, rv$analytical, rv$cross_cols)
      
      mode       <- input$app_mode %||% "build"
      df_main    <- rv$main_data
      df_an      <- rv$analytical
      cross_cols <- rv$cross_cols
      checks     <- list()
      
      for (col in cross_cols) {
        key <- paste0("xs_", col)
        if (!col %in% names(df_an) || !col %in% names(df_main)) {
          checks[[key]] <- list(label = col,
                                n_an = if (col %in% names(df_an)) dplyr::n_distinct(df_an[[col]]) else 0L,
                                n_main = 0L,
                                n_miss = if (col %in% names(df_an)) dplyr::n_distinct(df_an[[col]]) else 0L,
                                n_extra = 0L, sample_miss = character(0), sample_extra = character(0),
                                status = if (!col %in% names(df_main)) "red" else "na"); next
        }
        an_vals <- unique(as.character(df_an[[col]]))
        mn_vals <- unique(as.character(df_main[[col]]))
        missing <- setdiff(an_vals, mn_vals); extra <- setdiff(mn_vals, an_vals)
        checks[[key]] <- list(label = col, n_an = length(an_vals), n_main = length(mn_vals),
                              n_miss = length(missing), n_extra = length(extra),
                              sample_miss = head(missing, 3), sample_extra = head(extra, 3),
                              status = if (length(missing) > 0) "red"
                              else if (length(extra) > 0) "yellow" else "green")
      }
      
      an_min <- min(df_an$Period, na.rm = TRUE); an_max <- max(df_an$Period, na.rm = TRUE)
      mn_min <- min(df_main$Period, na.rm = TRUE); mn_max <- max(df_main$Period, na.rm = TRUE)
      
      time_st <- if (mode == "update") {
        if (mn_max < an_min) "red" else if (mn_max > an_max) "yellow" else "green"
      } else {
        if (mn_min > an_min || mn_max < an_max) "red"
        else if (mn_min < an_min || mn_max > an_max) "yellow" else "green"
      }
      checks$time_scope <- list(label = "Time Scope",
                                an_range   = paste0(format(an_min), " -> ", format(an_max)),
                                main_range = paste0(format(mn_min), " -> ", format(mn_max)),
                                status = time_st, mode = mode)
      
      an_periods  <- sort(unique(df_an$Period)); mn_periods <- sort(unique(df_main$Period))
      an_min_p    <- min(an_periods); an_max_p <- max(an_periods)
      mn_in_range <- mn_periods[mn_periods >= an_min_p & mn_periods <= an_max_p]
      n_common    <- length(intersect(an_periods, mn_periods))
      n_an_total  <- length(an_periods)
      n_mn_extra  <- sum(mn_periods < an_min_p) + sum(mn_periods > an_max_p)
      
      max_offset <- if (length(mn_in_range) > 0) {
        sample_mn  <- head(mn_in_range, min(20, length(mn_in_range)))
        an_numeric <- suppressWarnings(as.numeric(an_periods))
        an_numeric <- an_numeric[is.finite(an_numeric)]
        offsets    <- vapply(sample_mn, function(p) {
          p_num <- suppressWarnings(as.numeric(p))
          if (!is.finite(p_num) || !length(an_numeric)) return(NA_real_)
          min(abs(an_numeric - p_num), na.rm = TRUE)
        }, numeric(1))
        fin <- offsets[is.finite(offsets)]; if (length(fin) > 0) max(fin) else 0L
      } else if (mode == "update") 0L else if (length(mn_periods) > 0) 999L else 0L
      
      pa_st <- if (mode == "update") {
        if (mn_max < an_min) "red" else if (max_offset > 7) "yellow" else "green"
      } else {
        if (n_common >= n_an_total * 0.95 && max_offset == 0) "green"
        else if (max_offset <= 7) "yellow" else "red"
      }
      checks$period_align <- list(
        n_common = n_common, n_an = n_an_total, n_mn_extra = n_mn_extra,
        max_offset = max_offset,
        an_sample = paste(format(head(an_periods, 3)), collapse = ", "),
        mn_sample = if (length(mn_in_range) > 0)
          paste(format(head(mn_in_range, 3)), collapse = ", ")
        else if (length(mn_periods) > 0)
          paste(format(head(mn_periods, 3)), collapse = ", ")
        else "\u2014",
        status = pa_st, mode = mode)
      
      checks$weekday <- local({
        if (length(an_periods) < 4 || length(mn_periods) < 4)
          return(list(status = "na", an_day = NA, mn_day = NA))
        dominant_wday <- function(periods) {
          wdays     <- as.integer(format(as.Date(periods), "%u"))
          day_names <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
          day_names[as.integer(names(sort(table(wdays), decreasing = TRUE)[1]))]
        }
        an_day <- tryCatch(dominant_wday(an_periods), error = \(e) NA)
        mn_day <- tryCatch(dominant_wday(mn_periods), error = \(e) NA)
        if (is.na(an_day) || is.na(mn_day)) return(list(status = "na", an_day = an_day, mn_day = mn_day))
        list(status = if (an_day == mn_day) "green" else "red", an_day = an_day, mn_day = mn_day)
      })
      
      vv    <- df_main$VariableValue; na_vv <- sum(is.na(vv))
      checks$variable_value <- list(n_na = na_vv, n_total = length(vv),
                                    status = if (na_vv > 0) "red" else "green")
      
      check_dim_na <- function(col_name) {
        if (!col_name %in% names(df_main))
          return(list(label = col_name, n_na = 0, n_total = 0, status = "na"))
        vals <- df_main[[col_name]]; n_tot <- length(vals)
        n_na <- sum(is.na(vals) | (is.character(vals) & !nzchar(trimws(vals))), na.rm = TRUE)
        list(label = col_name, n_na = n_na, n_total = n_tot,
             status = if (n_na > 0) "yellow" else "green")
      }
      checks$campaign <- check_dim_na("Campaign")
      checks$outlet   <- check_dim_na("Outlet")
      checks$creative <- check_dim_na("Creative")
      
      all_s <- c(sapply(paste0("xs_", cross_cols), \(k) checks[[k]]$status %||% "na"),
                 checks$time_scope$status, checks$period_align$status,
                 checks$weekday$status, checks$variable_value$status,
                 checks$campaign$status, checks$outlet$status, checks$creative$status)
      overall <- if (any(all_s == "red")) "red"
      else if (any(all_s == "yellow")) "yellow"
      else if (all(all_s %in% c("green","na","pending"))) "green" else "pending"
      
      list(checks = checks, overall = overall, mode = mode)
      
    }) %>% bindCache(
      nrow(rv$main_data)  %||% 0L,
      nrow(rv$analytical) %||% 0L,
      paste(rv$cross_cols, collapse = ","),
      input$app_mode %||% "build",
      cache = performance_cache
    )
    
    observe({
      result               <- tryCatch(comparison_result(), error = \(e) NULL)
      rv$validation_status <- if (is.null(result)) "pending" else result$overall
    })
    
    output$cross_section_info <- renderUI({
      if (is.null(rv$cross_cols))
        return(div(class = "global-muted-box",
                   div(class = "global-muted-box-inner",
                       tags$span(class = "global-muted-title", "Cross-sections"),
                       tags$span(class = "global-muted-text",
                                 "Detected after uploading the Analytical Dataset."))))
      div(class = "global-muted-box",
          div(class = "global-muted-box-inner",
              tags$span(class = "global-muted-title", "Cross-sections"),
              lapply(rv$cross_cols, function(col) tags$span(col, class = "code-tag-blue"))))
    })
    
    output$suffix_preview <- renderUI({
      mode   <- input$app_mode %||% "build"
      preset <- input$period_preset %||% "last52"
      lbl    <- if (identical(input$app_mode, "update"))
        input$update_label_update %||% "" else input$update_label %||% "Last52w"
      
      if (mode == "update") {
        tagList(
          tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
          div(class = "suffix-box",
              div(class = "mb-10",
                  tags$span("Past (non-focus)", class = "preview-label"),
                  tags$code(class = "code-tag", paste0("_Before ", lbl)),
                  tags$span(" from Past Analytical", class = "text-muted small ms-2")),
              div(class = "mb-10",
                  tags$span("New (all focus)", class = "preview-label-focus"),
                  tags$code(class = "code-tag-blue", paste0("_", lbl)),
                  tags$span(" all RAE Datafile splits", class = "text-muted small ms-2"))))
      } else if (preset == "all") {
        div(class = "text-muted small p-2", "No column suffix - All Period selected.")
      } else {
        tagList(
          tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
          div(class = "suffix-box",
              div(class = "mb-10",
                  tags$span("Non-Focus", class = "preview-label"),
                  tags$code(class = "code-tag", paste0("_Before ", lbl))),
              div(class = "mb-10",
                  tags$span("Focus", class = "preview-label-focus"),
                  tags$code(class = "code-tag-blue", paste0("_", lbl))),
              hr(class = "hr-sm"),
              div(tags$span("Time-break (auto-detected from VOF):", class = "preview-label"),
                  tags$code(class = "code-tag d-block mb-1",
                            paste0("_Before ", lbl, "|FirstTimeBreak")),
                  tags$code(class = "code-tag d-block",
                            paste0("_Before ", lbl, "|SecondTimeBreak")),
                  tags$p(class = "hint-text",
                         "Applied automatically when VOF has multiple entries."))))
      }
    })
    
    output$validation_alerts <- renderUI({
      if (is.null(rv$dates_df)) return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates)) return(NULL)
      start <- dates$start; end <- dates$end
      d <- rv$dates_df; min_d <- min(d$Period); max_d <- max(d$Period)
      alerts <- list()
      if (start <= min_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        "Start Date is at or before model scope start.")))
      if (start > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        "Start Date is outside the date spine.")))
      if (end > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        paste0("End Date (", end, ") exceeds last period (", max_d, ")."))))
      if (end < start) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        "End Date is before Start Date.")))
      focus_n <- d %>% dplyr::filter(dplyr::between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0)
        alerts <- c(alerts, list(div(class = "alert alert-warning alert-sm p-2 mb-1",
                                     paste0("Focus period has only ", focus_n,
                                            " week(s). Minimum: 4."))))
      if (!length(alerts)) div(class = "setup-param-status setup-param-status-ok",
                               "Date parameters look good.")
      else tagList(alerts)
    })
    
    # Summarize blocking issues and warnings without changing processing rules.
    validation_view_state <- reactive({
      required_loaded <- vapply(required_base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
      n_required_loaded <- sum(required_loaded)
      base_ready <- required_files_ready()
      rois_missing <- is.null(rv$channels_rois)
      contract_warnings <- contract_warning_items()
      status <- rv$validation_status %||% "pending"
      severity <- if (!base_ready) "blocker" else if (identical(status, "red")) "blocker"
      else if (rois_missing) "warning"
      else if (length(contract_warnings)) "warning"
      else if (identical(status, "yellow")) "warning"
      else if (identical(status, "green")) "ok" else "pending"
      message <- if (!base_ready) {
        missing <- c(main = "RAE Datafile", analytical = "Analytical Dataset",
                     vof = "VOF Metadata", details = "ModelDetails")[!required_loaded]
        paste0("Missing: ", paste(unname(missing), collapse = ", "))
      } else if (rois_missing) {
        paste(
          "You can continue and use Channels, Process and Export.",
          "However, ROIs by Channel is not loaded, so Seed For Indices.csv will export without ROI values."
        )
      } else if (length(contract_warnings)) {
        "Files are loaded, but Setup found schema or mapping warnings. Review Upload summary before processing."
      } else if (identical(severity, "ok")) {
        "Files validated. Channels and processing can continue."
      } else if (identical(severity, "warning")) {
        "Validation has warnings. Review the table below before processing."
      } else if (identical(severity, "blocker")) {
        "Validation has blockers. Fix the rows marked in red before continuing."
      } else {
        "Validation pending. The comparison will update automatically."
      }
      list(
        severity = severity,
        message = message,
        n_required_loaded = n_required_loaded,
        rois_missing = rois_missing,
        base_ready = base_ready
      )
    })

    output$validation_card <- renderUI({
      state <- validation_view_state()
      severity <- state$severity
      is_open <- !identical(severity, "ok")
      title <- if (severity == "ok") "Validation OK"
      else if (state$rois_missing && state$base_ready) "ROIs by Channel missing"
      else if (severity == "warning") "Warnings found"
      else if (severity == "blocker") "Action needed"
      else "Pending validation"
      icon_name <- if (severity == "ok") "circle-check"
      else if (severity == "warning") "triangle-exclamation"
      else if (severity == "blocker") "circle-xmark" else "clock"
      count_text <- paste0(
        state$n_required_loaded, "/4 required",
        if (state$rois_missing) " | ROI values missing" else ""
      )

      tags$details(
        class = paste("setup-validation-shell setup-validation-section",
                      paste0("setup-validation-", severity),
                      if (identical(severity, "ok")) "setup-validation-collapsed-ok" else ""),
        open = if (is_open) "open" else NULL,
        tags$summary(
          class = "setup-validation-summary-toggle",
          div(
            class = "setup-validation-title",
            icon(icon_name, class = "setup-validation-icon"),
            div(
              tags$strong("File Validation"),
              tags$span(title, class = "setup-validation-status")
            )
          ),
          tags$span(state$message, class = "setup-validation-message"),
          tags$span(count_text, class = "setup-validation-count"),
          tags$span(
            class = "setup-validation-action",
            tags$span(class = "validation-toggle-show", "Show details"),
            tags$span(class = "validation-toggle-hide", "Hide details")
          )
        ),
        div(
          class = "setup-validation-body",
          uiOutput(ns("validation_summary")),
          uiOutput(ns("file_comparison"))
        )
      )
    })

    output$validation_summary <- renderUI({
      state <- validation_view_state()
      severity <- state$severity
      rois_missing <- state$rois_missing
      base_ready <- state$base_ready
      div(
        class = paste("qa-summary", paste0("qa-summary-", severity),
                      if (rois_missing && base_ready) "qa-summary-roi-missing" else ""),
        div(class = "qa-summary-main",
            icon(if (severity == "ok") "circle-check"
                 else if (severity == "warning") "triangle-exclamation"
                 else if (severity == "blocker") "circle-xmark" else "clock",
                 class = "qa-summary-icon"),
            div(tags$strong(if (severity == "ok") "Validation OK"
                            else if (rois_missing && base_ready) "ROIs by Channel missing"
                            else if (severity == "warning") "Warnings found"
                            else if (severity == "blocker") "Action needed"
                            else "Pending validation"),
                tags$span(state$message, class = "qa-summary-text"))),
        tags$span(
          paste0(state$n_required_loaded, "/4 required",
                 if (rois_missing) " | ROI values missing" else ""),
          class = "qa-summary-count"
        )
      )
    })

    output$file_comparison <- renderUI({
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload RAE Datafile and Analytical Dataset to begin validation."
        else if (is.null(rv$analytical)) "Upload the Analytical Dataset to complete validation."
        else "Upload the RAE Datafile to complete validation."
        return(div(class = "text-center py-4 text-muted", tags$p(msg)))
      }
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      if (is.null(result))
        return(div(class = "text-muted small p-2", "Computing validation..."))
      
      checks     <- result$checks
      overall    <- result$overall
      mode       <- result$mode
      cross_cols <- rv$cross_cols %||% character(0)
      
      worst_status <- function(...) {
        s <- c(...)
        if ("red" %in% s) "red" else if ("yellow" %in% s) "yellow"
        else if ("green" %in% s) "green" else "na"
      }
      mk_badge <- function(status, text = NULL) {
        label <- switch(status, green = "OK", yellow = "Warning", red = "Blocked",
                        pending = "Pending", "N/A")
        tags$td(
          class = "val-status-cell",
          tags$span(text %||% label,
                    class = paste("val-status-pill", paste0("val-status-", status)))
        )
      }
      mk_pill <- function(n, type) {
        if (n == 0) return(NULL)
        tags$span(switch(type, ok = paste(n, "passed"), warn = paste(n, "warning"),
                         paste(n, "failed")),
                  class = paste("val-pill", switch(type, ok = "val-pill-ok",
                                                   warn = "val-pill-warn", "val-pill-err")))
      }
      mk_sep <- function(title, subtitle, pills = NULL) {
        tags$tr(class = "val-sep-row",
                tags$td(colspan = "5",
                        div(class = "val-sep-inner",
                            tags$strong(title),
                            tags$small(subtitle, class = "section-subtitle"),
                            div(class = "ms-auto d-flex gap-2", pills))))
      }
      
      ts_status <- worst_status(checks$time_scope$status, checks$period_align$status,
                                checks$weekday$status)
      
      ts_impl <- local({
        ts <- checks$time_scope; pa <- checks$period_align; wd <- checks$weekday
        if (mode == "update") {
          scope_msg <- switch(ts$status,
                              green  = paste0("Focus period data only — expected in Model Update. ",
                                              "Analytical contains full history; RAE Datafile covers the new focus period."),
                              yellow = "RAE Datafile extends beyond Analytical. Check that the focus period is correct.",
                              red    = "RAE Datafile appears older than Analytical. Verify files are correct.", NULL)
          align_msg <- switch(pa$status,
                              green  = if (pa$n_common == 0)
                                "No overlapping periods — RAE Datafile covers new focus period only. Expected in Model Update."
                              else paste0(pa$n_common, " overlapping period(s) found with Analytical."),
                              yellow = paste0("Small period offset (+/- ", pa$max_offset,
                                              " days). Total Check will auto-align."),
                              red    = "No valid focus periods found. Verify RAE Datafile date range.", NULL)
          impl_cls <- switch(ts_status, green = "impl-text impl-ok",
                             yellow = "impl-text impl-warn", red = "impl-text impl-err", "impl-text")
          div(class = impl_cls,
              if (!is.null(scope_msg)) scope_msg,
              if (!is.null(align_msg)) tags$div(class = "mt-1", align_msg))
        } else {
          scope_msg  <- switch(ts$status,
                               green  = "Date scopes are fully aligned.",
                               yellow = "RAE Datafile scope differs from Analytical.",
                               red    = "RAE Datafile is missing periods required by the model.", NULL)
          period_msg <- switch(pa$status,
                               green  = if (pa$n_mn_extra > 0)
                                 paste0(format(pa$n_mn_extra, big.mark = ","),
                                        " extra RAE Datafile periods become non-focus splits.")
                               else "All periods match exactly.",
                               yellow = paste0("Small offset (+/-", pa$max_offset,
                                               " days) - Total Check will auto-align."),
                               red    = paste0("Only ", pa$n_common, "/", pa$n_an,
                                               " periods found - activity splits will be empty."), NULL)
          weekday_msg <- if (!is.null(wd) && wd$status == "red")
            tags$div(class = "text-danger fw-semibold mt-1",
                     paste0("Day-of-week mismatch: Analytical uses ", wd$an_day,
                            ", RAE Datafile uses ", wd$mn_day,
                            " - period alignment will be unreliable."))
          else NULL
          impl_cls <- switch(ts_status, green = "impl-text impl-ok",
                             yellow = "impl-text impl-warn", red = "impl-text impl-err", "impl-text")
          div(div(class = impl_cls,
                  if (!is.null(scope_msg))   scope_msg,
                  if (!is.null(period_msg))  tags$div(class = "mt-1", period_msg),
                  if (!is.null(weekday_msg)) weekday_msg),
              if (ts$status %in% c("yellow", "red"))
                build_timeline_html(ts$an_range, ts$main_range))
        }
      })
      
      xs_rows <- lapply(cross_cols, function(col) {
        key <- paste0("xs_", col); chk <- checks[[key]]
        if (is.null(chk) || chk$status == "na") return(NULL)
        badge_txt <- switch(chk$status, green = "Match",
                            yellow = paste0("+", chk$n_extra, " extra"),
                            red    = paste0(chk$n_miss, " missing"), "N/A")
        impl <- switch(chk$status,
                       green  = div(class = "impl-text impl-ok",
                                    paste0("All ", chk$n_an, " ", col, " values present in RAE Datafile.")),
                       yellow = div(class = "impl-text impl-warn",
                                    tags$div(paste0("RAE Datafile has ", chk$n_extra, " additional ", col, " value(s).")),
                                    if (length(chk$sample_extra) > 0)
                                      tags$div(class = "text-muted small",
                                               paste0("Extra: ", paste(chk$sample_extra, collapse = " | ")))),
                       red    = div(class = "impl-text impl-err",
                                    tags$div(paste0(chk$n_miss, " Analytical ", col, " value(s) not found in RAE Datafile.")),
                                    if (length(chk$sample_miss) > 0)
                                      tags$div(class = "text-muted small",
                                               paste0("Missing: ", paste(chk$sample_miss, collapse = " | ")))),
                       tags$span("-", class = "text-muted"))
        tags$tr(tags$td(col, class = "td-val-bold"),
                tags$td(as.character(chk$n_an),   class = "td-val"),
                tags$td(as.character(chk$n_main), class = "td-val"),
                tags$td(impl, class = "td-val"), mk_badge(chk$status, badge_txt))
      })
      
      xs_statuses <- sapply(paste0("xs_", cross_cols), \(k) checks[[k]]$status %||% "na")
      fa_s <- Filter(\(s) s != "na", c(xs_statuses, ts_status))
      dq_s <- c(checks$variable_value$status, checks$campaign$status,
                checks$outlet$status, checks$creative$status)
      
      mk_dq_row <- function(chk, label, critical = FALSE) {
        res_a <- if (chk$status == "green")
          tags$span(if (critical) "No NAs" else "No empty", class = "val-cell-ok-muted")
        else
          tags$span(paste0(format(chk$n_na, big.mark = ","),
                           if (critical) " NAs" else " empty"),
                    class = if (critical) "text-danger fw-semibold" else "text-warning fw-semibold")
        res_b <- if (chk$n_na == 0)
          tags$span(paste0("All ", format(chk$n_total, big.mark = ","), " rows filled"),
                    class = "text-muted")
        else
          tags$span(paste0(round(chk$n_na / max(chk$n_total, 1) * 100, 1), "% empty - ",
                           format(chk$n_na, big.mark = ","), " rows"),
                    class = if (critical) "text-danger small" else "text-warning small")
        impl <- if (chk$status == "green")
          div(class = "impl-text impl-ok",
              if (critical) "All rows have a value - splits will be complete."
              else "Column is complete - no unwanted splits.")
        else if (critical) div(class = "impl-text impl-err",
                               "Missing values - those splits will have zero activity.")
        else               div(class = "impl-text impl-warn",
                               "Empty values may create unwanted splits.")
        tags$tr(tags$td(label, class = "td-val-bold"),
                tags$td(res_a, class = "td-val"), tags$td(res_b, class = "td-val"),
                tags$td(impl, class = "td-val"),
                mk_badge(chk$status, if (chk$n_na == 0) "OK"
                         else if (critical) "Blocked" else "Review"))
      }
      
      banner_conf <- if (mode == "update") {
        switch(overall,
               green  = list(cls = "banner-validation banner-green",
                             text = "Files validated for Model Update — focus period data confirmed."),
               yellow = list(cls = "banner-validation banner-yellow",
                             text = "Warnings found — review before processing."),
               red    = list(cls = "banner-validation banner-red",
                             text = "Critical issues found — fix before processing."),
               list(cls = "banner-validation banner-neutral",
                    text = "Validating files for Model Update..."))
      } else {
        switch(overall,
               green  = list(cls = "banner-validation banner-green",
                             text = "All checks passed - ready to proceed to Channels."),
               yellow = list(cls = "banner-validation banner-yellow",
                             text = "Warnings found - review before processing."),
               red    = list(cls = "banner-validation banner-red",
                             text = "Critical issues found - fix before processing."),
               list(cls = "banner-validation banner-neutral", text = "Validating files..."))
      }
      
      banner <- div(class = banner_conf$cls,
                    tags$strong(banner_conf$text, class = "banner-text"))
      
      update_info <- if (mode == "update")
        div(class = "alert alert-info alert-sm p-2 mb-3",
            icon("circle-info"), " ",
            tags$strong("Model Update mode:"),
            " RAE Datafile contains focus period only. ",
            "Analytical Dataset contains full history. Low period overlap is expected.")
      else NULL
      
      the_table <- div(class = "table-responsive",
                       tags$table(class = "table table-sm val-table mb-0",
                                  tags$thead(tags$tr(style = "border-bottom:2px solid #5B9BD5;",
                                                     tags$th("Check",       class = "th-val"),
                                                     tags$th("Analytical",  class = "th-val"),
                                                     tags$th("RAE Datafile", class = "th-val"),
                                                     tags$th("Implication", class = "th-val th-impl"),
                                                     tags$th("Status",      class = "th-val"))),
                                  tags$tbody(
                                    mk_sep("File Alignment",
                                           if (mode == "update")
                                             "Analytical (full history) vs RAE Datafile (focus period only)"
                                           else "Analytical vs RAE Datafile",
                                           tagList(mk_pill(sum(fa_s == "green"), "ok"),
                                                   mk_pill(sum(fa_s == "yellow"), "warn"),
                                                   mk_pill(sum(fa_s == "red"), "err"))),
                                    xs_rows,
                                    tags$tr(tags$td("Time Scope",                 class = "td-val-bold"),
                                            tags$td(checks$time_scope$an_range,   class = "td-val td-nowrap"),
                                            tags$td(checks$time_scope$main_range, class = "td-val td-nowrap"),
                                            tags$td(ts_impl, class = "td-val"), mk_badge(ts_status)),
                                    mk_sep("Data Quality", "RAE Datafile only",
                                           tagList(mk_pill(sum(dq_s %in% c("green","na")), "ok"),
                                                   mk_pill(sum(dq_s == "yellow"), "warn"),
                                                   mk_pill(sum(dq_s == "red"), "err"))),
                                    mk_dq_row(checks$variable_value, "VariableValue", critical = TRUE),
                                    mk_dq_row(checks$campaign, "Campaign"),
                                    mk_dq_row(checks$outlet,   "Outlet"),
                                    mk_dq_row(checks$creative, "Creative"))))
      
      tagList(update_info, the_table)
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
