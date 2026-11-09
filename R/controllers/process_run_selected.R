# Register Process Selected, imported-config invalidation, and sidebar status.
# The same pipeline is used for local fallback and background jobs.
register_process_run_selected <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Validate inputs, integrate a single channel result, and mark review when
    # Total Check or saved SAP references require attention.
    run_one <- function(nm, do_gc = TRUE) {
      op_single <- identical(operation_status$current_id(), "process-selected")
      d    <- data()
      cfg  <- channels()[[nm]]; req(cfg)
      cfg$model_metric <- normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      )
      cfg <- reconcile_channel_metric_keywords(cfg)
      gcfg <- config()

      if (is.null(d$all_rags)) {
        showNotification(paste0(nm, ": All RAGs data not uploaded."),
                         type = "error", duration = 8); return()
      }
      if (is.null(d$analytical)) {
        showNotification("AnalyticalDataset not uploaded.",
                         type = "error", duration = 10); return()
      }
      if (is.null(d$dates_df)) {
        showNotification("dates_df is NULL.", type = "error", duration = 10); return()
      }
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        showNotification("Reporting period not configured. Check Setup tab.",
                         type = "error", duration = 10); return()
      }
      if (is.null(gcfg$cross_cols)) {
        showNotification("Cross-sections not detected. Upload Analytical first.",
                         type = "error", duration = 8); return()
      }

      model_var <- cfg$model_variable %||% ""
      if (!nzchar(model_var)) {
        showNotification(paste0(nm, ": model_variable not configured."),
                         type = "error", duration = 8); return()
      }
      if (!model_var %in% names(d$analytical)) {
        showNotification(paste0(nm, ": model variable '", model_var,
                                "' not found in AnalyticalDataset."),
                         type = "error", duration = 10); return()
      }

      res_stored <- NULL; err_stored <- NULL
      t_channel <- proc.time()
      operation_status$update("Filtering RAE", 0.16, nm)
      rags_nm <- channel_source_data(
        d$all_rags, cfg, d$all_rags_indexed, d$data_signature
      )
      if (isTRUE(getOption("pso.profile", FALSE))) {
        message("[mod_process] ", nm, ": RAE rows ",
                format(nrow(d$all_rags), big.mark = ","),
                " -> ", format(nrow(rags_nm), big.mark = ","))
      }

      withProgress(message = paste0("Processing: ", nm), value = 0, {
        operation_status$update("Building splits", 0.28, nm)
        job <- run_sync_channel_pipeline(nm, d, cfg, gcfg, rags_nm)
        err_stored <- if (isTRUE(job$ok)) NULL else job$error
        clean_result <- job$clean
        res_stored <- job$final
        config_merge_report <- job$merge_report %||% list()
        total_check_now <- job$total_check
      })

      if (!is.null(err_stored)) {
        public_error <- pso_public_error(
          err_stored, paste0("process.selected.", nm),
          "Channel processing failed. Review the channel audit and Setup files."
        )
        set_error(nm, public_error)
        elapsed <- round((proc.time() - t_channel)[["elapsed"]], 3)
        if (isTRUE(getOption("pso.profile", FALSE))) {
          message("[mod_process] ", nm, " failed in ", elapsed, "s")
        }
        showNotification(paste(nm, "error:", public_error),
                         type = "error", duration = 12)
        if (op_single) operation_status$fail(
          paste0("Processing failed for ", nm), public_error
        )
        rm(rags_nm)
        if (isTRUE(do_gc)) gc(verbose = FALSE, full = FALSE)
        return()
      }

      if (!is.null(res_stored)) {
        clean_store[[nm]] <- clean_result
        operation_status$update("Applying saved SAP", 0.76,
                                paste0(length(config_merge_report), " saved aggregation(s)"))
        n_applied <- sum(vapply(config_merge_report, function(x) {
          isTRUE(x$applied)
        }, logical(1)))

        res_stored$model_metric <- cfg$model_metric
        set_res(nm, res_stored); set_orig(nm, res_stored)
        set_log(nm, config_merge_report); set_hist(nm, list())
        set_error(nm, NULL)
        result_signatures[[nm]] <- process_result_signature(cfg, job$data_signature)
        async_total_checks[[nm]] <- list(
          signature = pso_cache_key(
            "async-total-check", job$data_signature,
            channel_signature(canonical_process_cfg(cfg)),
            result_versions[[nm]] %||% 0L
          ),
          value = total_check_now
        )

        n_review <- length(config_merge_report) - n_applied
        operation_status$update("Running diagnostics", 0.92, nm)
        total_check_status <- "Not run"
        total_check_warning <- character(0)
        if (op_single) {
          if (!is.null(total_check_now)) {
            total_check_status <- switch(
              total_check_now$status %||% "error",
              ok = "Reconciled",
              mismatch = "Review required",
              "Unable to validate"
            )
            if (!identical(total_check_now$status %||% "", "ok")) {
              total_check_warning <- paste0("Total Check: ", total_check_status, ".")
            }
          }
        }
        msg <- paste0(nm, " processed",
                      if (length(config_merge_report) > 0)
                        paste0("; ", n_applied, "/", length(config_merge_report),
                               " config merge(s) applied",
                               if (n_review > 0) paste0(", ", n_review, " need review") else "")
                      else "")
        elapsed <- round((proc.time() - t_channel)[["elapsed"]], 3)
        if (isTRUE(getOption("pso.profile", FALSE))) {
          message("[mod_process] ", nm, " processed in ", elapsed, "s")
        }
        showNotification(msg,
                         type = if (n_review > 0) "warning" else "message",
                         duration = if (n_review > 0) 8 else 4)
        rm(res_stored, rags_nm)
        if (isTRUE(do_gc)) gc(verbose = FALSE, full = FALSE)
        if (op_single) {
          operation_status$complete(
            paste0(nm, " processed in ", elapsed, "s. ", n_applied,
                   " SAP aggregation(s) applied. Total Check: ", total_check_status, "."),
            warnings = c(
              if (n_review > 0) paste0(n_review, " saved SAP aggregation(s) require review."),
              total_check_warning
            )
          )
        }
      }
    }

    observeEvent(config_import_event(), {
      evt <- config_import_event()
      if (is.null(evt) || is.null(evt$channels)) return()
      imported <- unique(evt$channels)
      imported <- imported[imported %in% names(channels())]
      if (!length(imported)) return()

      showNotification(
        paste0("Splits Metadata imported for ", length(imported),
               " channel(s). Review Channels, then process when ready."),
        type = "message",
        duration = 7
      )
      results_trigger(isolate(results_trigger()) + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_one, {
      nm <- req(input$channel_select); req(valid_nm(nm))
      if (start_async_operation(nm, "process-selected", "Process selected channel")) {
        select_modeled_tab()
        return()
      }
      if (operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning")
        return()
      }
      operation_status$start(
        "process-selected", "Process selected channel",
        c("Preparing channel", "Filtering RAE", "Building splits",
          "Applying saved SAP", "Running diagnostics", "Completed"),
        total_items = 1L, detail = nm
      )
      tryCatch(
        run_one(nm),
        error = function(e) {
          public_error <- pso_public_error(
            e, paste0("process.selected.", nm),
            "Channel processing failed. Review the channel audit and Setup files."
          )
          if (operation_status$is_busy()) {
            operation_status$fail(paste0("Processing failed for ", nm), public_error)
          }
          stop(shiny::safeError(public_error))
        }
      )
      if (operation_status$is_busy()) {
        operation_status$fail(
          paste0("Processing could not be completed for ", nm),
          "Review the required Setup inputs and the channel configuration."
        )
      }
      select_modeled_tab()
    })

    output$batch_summary <- renderUI({
      batch <- batch_summary_state()
      if (is.null(batch)) return(NULL)
      div(class = "process-batch-summary",
          tags$span(icon("list-check"), class = "process-batch-icon"),
          if (batch$processed > 0)
            tags$span(paste0(batch$processed, " processed"), class = "badge-ready"),
          if ((batch$review %||% 0L) > 0)
            tags$span(paste0(batch$review, " review"), class = "badge-stale"),
          if (batch$already_done > 0)
            tags$span(paste0(batch$already_done, " already done"), class = "badge-count-neutral"),
          if (batch$skipped > 0)
            tags$span(paste0(batch$skipped, " skipped"), class = "badge-not-ready"),
          if (batch$failed > 0)
            tags$span(paste0(batch$failed, " failed"), class = "badge-error"),
          tags$span(paste0(batch$elapsed, "s"), class = "process-batch-time"))
    })

    output$error_summary <- renderUI({
      errs <- reactiveValuesToList(process_errors)
      errs <- errs[!vapply(errs, is.null, logical(1))]
      if (!length(errs)) return(NULL)
      div(class = "process-error-box",
          div(class = "process-error-title",
              icon("triangle-exclamation"), tags$strong("Processing errors")),
          tagList(lapply(names(errs), function(nm)
            div(class = "process-error-row",
                tags$span(nm, class = "process-error-name"),
                tags$span(errs[[nm]], class = "process-error-message")))))
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
