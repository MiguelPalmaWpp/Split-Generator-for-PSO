# Register Process All and retry actions. Each channel succeeds or fails
# independently, so a bad channel does not abort the remaining queue.
register_process_run_batch <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    observeEvent(input$btn_all, {
      select_modeled_tab()
      if (isTRUE(is_batch_processing())) {
        showNotification("Processing is already running.", type = "warning"); return()
      }
      nms <- names(channels())
      if (!length(nms)) {
        showNotification("No channels configured.", type = "warning"); return()
      }

      d    <- data(); gcfg <- config(); ch <- channels()

      if (is.null(d$all_rags)) {
        showNotification("All RAGs not uploaded.", type = "error"); return()
      }
      if (is.null(d$analytical)) {
        showNotification("Upload AnalyticalDataset first.", type = "error"); return()
      }
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        showNotification("Configure reporting period first.", type = "error"); return()
      }
      if (is.null(gcfg$cross_cols)) {
        showNotification("Cross-sections not detected. Upload Analytical first.",
                         type = "error"); return()
      }

      stale <- stale_names()
      to_process   <- nms[vapply(nms, \(nm) is.null(results_store[[nm]]) || nm %in% stale,
                                 logical(1))]
      already_done <- length(nms) - length(to_process)

      if (!length(to_process)) {
        showNotification(paste0("All ", length(nms), " channels already processed."),
                         type = "message", duration = 5); return()
      }

      if (start_async_operation(
        to_process, "process-all", "Process all channels", already_done
      )) return()

      n_total  <- length(to_process)
      n_ok     <- 0L; n_review_channels <- 0L; n_skipped <- 0L; n_err <- 0L
      err_msgs <- character(0); review_msgs <- character(0)
      t_start  <- proc.time()
      if (operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning")
        return()
      }
      op_items <- lapply(to_process, operation_item)
      operation_status$start(
        "process-all", "Process all channels",
        c("Preparing channels", "Filtering RAE", "Building splits",
          "Applying saved SAP", "Running diagnostics", "Completed"),
        total_items = n_total,
        detail = paste0(n_total, " channel(s) queued")
      )

      cross_cols_val <- gcfg$cross_cols %||% "Geography"
      is_batch_processing(TRUE)
      on.exit({
        is_batch_processing(FALSE)
        results_trigger(isolate(results_trigger()) + 1L)
        if (identical(operation_status$current_id(), "process-all")) {
          operation_status$fail("Process All stopped before completion.")
        }
      }, add = TRUE)

      withProgress(message = paste0("Processing ", n_total, " channel(s)..."),
                   value = 0, {
                     for (i in seq_along(to_process)) {
                       nm  <- to_process[i]; cfg <- ch[[nm]]
                       op_items[[i]] <- operation_item(nm, "Processing", paste0(i, " of ", n_total))
                       operation_status$update(
                         "Processing channels", (i - 1) / n_total, nm,
                         op_items,
                         list(Completed = n_ok - n_review_channels,
                              Review = n_review_channels,
                              Failed = n_err, Skipped = n_skipped)
                       )
                       setProgress((i - 1) / n_total,
                                   message = paste0("(", i, "/", n_total, ")  ", nm))
                       if (is.null(cfg)) {
                         n_skipped <- n_skipped + 1L
                         op_items[[i]] <- operation_item(nm, "Skipped", "Channel configuration unavailable")
                         next
                       }

                       model_var   <- cfg$model_variable %||% ""
                       skip_reason <- if (!nzchar(model_var)) "model_variable not configured"
                       else if (!model_var %in% names(d$analytical))
                         paste0("'", model_var, "' not in Analytical")
                       else NULL

                       if (!is.null(skip_reason)) {
                         n_skipped <- n_skipped + 1L
                         err_msgs  <- c(err_msgs, paste0(nm, ": ", skip_reason))
                         process_errors[[nm]] <- skip_reason
                         op_items[[i]] <- operation_item(nm, "Skipped", skip_reason)
                         next
                       }

                       rags_nm <- tryCatch(
                         channel_source_data(
                           d$all_rags, cfg, d$all_rags_indexed, d$data_signature
                         ),
                         error = function(e) e
                       )
                       if (inherits(rags_nm, "error")) {
                         err_msg <- pso_public_error(
                           rags_nm, paste0("process.batch.source.", nm),
                           "Channel source data could not be prepared."
                         )
                         n_err <- n_err + 1L
                         err_msgs <- c(err_msgs, paste0(nm, ": ", err_msg))
                         process_errors[[nm]] <- err_msg
                         op_items[[i]] <- operation_item(nm, "Failed", err_msg)
                         next
                       }
                       if (isTRUE(getOption("pso.profile", FALSE))) {
                         message("[mod_process] ", nm, ": RAE rows ",
                                 format(nrow(d$all_rags), big.mark = ","),
                                 " -> ", format(nrow(rags_nm), big.mark = ","))
                       }

                       job <- tryCatch(
                         run_sync_channel_pipeline(nm, d, cfg, gcfg, rags_nm),
                         error = function(e) list(
                           ok = FALSE, error = conditionMessage(e), clean = NULL,
                           final = NULL, merge_report = list(), total_check = NULL
                         )
                       )
                       err_msg <- if (isTRUE(job$ok)) NULL else job$error
                       clean_result <- job$clean
                       res_stored <- job$final
                       config_merge_report <- job$merge_report %||% list()
                       total_check_now <- job$total_check

                       rm(rags_nm)
                       if (i %% 5L == 0L) gc(verbose = FALSE, full = FALSE)

                       if (!is.null(err_msg)) {
                         err_msg <- pso_public_error(
                           err_msg, paste0("process.batch.channel.", nm),
                           "Channel processing failed. Review the channel audit and Setup files."
                         )
                         n_err    <- n_err + 1L
                         err_msgs <- c(err_msgs, paste0(nm, ": ", err_msg))
                         process_errors[[nm]] <- err_msg
                         op_items[[i]] <- operation_item(nm, "Failed", err_msg)
                         next
                       }

                       if (!is.null(res_stored)) {
                         clean_store[[nm]] <- clean_result
                         results_store[[nm]]   <- res_stored
                         bump_result_version(nm)
                         original_store[[nm]]  <- res_stored
                         merge_log_store[[nm]] <- config_merge_report
                         history_store[[nm]]   <- list()
                         process_errors[[nm]]  <- NULL
                          result_signatures[[nm]] <- process_result_signature(cfg, job$data_signature)
                          async_total_checks[[nm]] <- list(
                            signature = pso_cache_key(
                              "async-total-check", job$data_signature,
                              channel_signature(canonical_process_cfg(cfg)),
                              result_versions[[nm]] %||% 0L
                           ),
                           value = total_check_now
                         )
                         n_ok <- n_ok + 1L; rm(res_stored)
                         n_review_item <- sum(vapply(config_merge_report, function(x) {
                           identical(x$status %||% "", "needs_review")
                         }, logical(1)))
                         total_review <- is.null(total_check_now) ||
                           !identical(total_check_now$status %||% "error", "ok")
                         needs_review <- n_review_item > 0L || total_review
                         if (needs_review) {
                           n_review_channels <- n_review_channels + 1L
                           reasons <- c(
                             if (total_review) paste0(
                               "Total Check: ",
                               total_check_now$summary$message %||% "review required"
                             ),
                             if (n_review_item > 0L)
                               paste(n_review_item, "SAP item(s) require review")
                           )
                           review_msgs <- c(review_msgs, paste0(nm, ": ", paste(reasons, collapse = "; ")))
                         }
                         op_items[[i]] <- operation_item(
                           nm,
                           if (needs_review) "Review" else "Completed",
                           if (needs_review) paste(reasons, collapse = "; ") else "Processed"
                         )
                       }
                     }

                     gc(verbose = FALSE, full = TRUE)
                     setProgress(1.0, message = "Done!")
                   })

      elapsed  <- round((proc.time() - t_start)[["elapsed"]], 1)
      processed_clean <- n_ok - n_review_channels
      batch_summary_state(list(processed = processed_clean,
                               review = n_review_channels,
                               already_done = already_done,
                               skipped = n_skipped, failed = n_err,
                               elapsed = elapsed))
      parts    <- c(
        if (processed_clean > 0) paste0(processed_clean, " processed"),
        if (n_review_channels > 0) paste0(n_review_channels, " review"),
        if (already_done > 0) paste0(already_done, " already done"),
        if (n_skipped    > 0) paste0(n_skipped,    " skipped"),
        if (n_err        > 0) paste0(n_err,        " failed"),
        paste0(elapsed, "s"))

      operation_status$complete(
        paste(parts, collapse = " | "),
        warnings = c(err_msgs, review_msgs),
        items = op_items,
        auto_close_ms = 3000L
      )
    })

    observeEvent(input$btn_failed, {
      select_modeled_tab()
      errs <- reactiveValuesToList(process_errors)
      failed <- names(errs)[!vapply(errs, is.null, logical(1))]
      failed <- failed[failed %in% names(channels())]
      if (!length(failed)) {
        showNotification("No failed channels to reprocess.", type = "message"); return()
      }
      if (start_async_operation(
        failed, "reprocess-failed", "Reprocess failed channels"
      )) return()
      if (isTRUE(is_batch_processing())) {
        showNotification("Processing is already running.", type = "warning"); return()
      }
      if (operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning"); return()
      }
      op_items <- lapply(failed, operation_item)
      operation_status$start(
        "reprocess-failed", "Reprocess failed channels",
        c("Preparing channels", "Processing channels", "Running diagnostics", "Completed"),
        length(failed), paste0(length(failed), " channel(s) queued")
      )
      is_batch_processing(TRUE)
      on.exit({
        is_batch_processing(FALSE)
        results_trigger(isolate(results_trigger()) + 1L)
        if (identical(operation_status$current_id(), "reprocess-failed")) {
          operation_status$fail("Reprocessing stopped before completion.")
        }
      }, add = TRUE)
      n_ok <- 0L; n_review <- 0L; n_err <- 0L; t_start <- proc.time()
      withProgress(message = paste0("Reprocessing ", length(failed), " failed channel(s)..."),
                   value = 0, {
                     for (i in seq_along(failed)) {
                       op_items[[i]] <- operation_item(failed[[i]], "Processing")
                       operation_status$update("Processing channels", (i - 1) / length(failed),
                                               failed[[i]], op_items)
                       setProgress((i - 1) / length(failed),
                                   message = paste0("(", i, "/", length(failed), ") ", failed[[i]]))
                       before_err <- process_errors[[failed[[i]]]]
                       run_one(failed[[i]], do_gc = FALSE)
                       after_err <- process_errors[[failed[[i]]]]
                       if (is.null(after_err)) n_ok <- n_ok + 1L
                       else if (!identical(before_err, after_err) || !is.null(after_err)) n_err <- n_err + 1L
                       review_reasons <- if (is.null(after_err))
                         channel_review_messages(failed[[i]]) else character(0)
                       needs_review <- length(review_reasons) > 0L
                       if (needs_review) n_review <- n_review + 1L
                       op_items[[i]] <- operation_item(
                         failed[[i]], if (!is.null(after_err)) "Failed" else if (needs_review) "Review" else "Completed",
                         after_err %||% (if (needs_review)
                           paste(review_reasons, collapse = "; ") else "Processed")
                       )
                     }
                   })
      gc(verbose = FALSE, full = TRUE)
      elapsed <- round((proc.time() - t_start)[["elapsed"]], 1)
      batch_summary_state(list(processed = n_ok - n_review, review = n_review, already_done = 0L,
                               skipped = 0L, failed = n_err, elapsed = elapsed))
      operation_status$complete(
        paste0(n_ok - n_review, " processed | ", n_review, " review | ", n_err, " failed | ", elapsed, "s"),
        warnings = c(
          if (n_err > 0L) paste0(n_err, " channel(s) still failed."),
          if (n_review > 0L) paste0(n_review, " channel(s) require review.")
        ),
        items = op_items, auto_close_ms = 3000L
      )
    })

    observeEvent(input$btn_changed, {
      select_modeled_tab()
      changed <- stale_names()
      changed <- changed[changed %in% names(channels())]
      if (!length(changed)) {
        showNotification("No changed channels to reprocess.", type = "message"); return()
      }
      if (start_async_operation(
        changed, "reprocess-changed", "Reprocess changed channels"
      )) return()
      if (isTRUE(is_batch_processing())) {
        showNotification("Processing is already running.", type = "warning"); return()
      }
      if (operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning"); return()
      }
      op_items <- lapply(changed, operation_item)
      operation_status$start(
        "reprocess-changed", "Reprocess changed channels",
        c("Preparing channels", "Processing channels", "Running diagnostics", "Completed"),
        length(changed), paste0(length(changed), " channel(s) queued")
      )
      is_batch_processing(TRUE)
      on.exit({
        is_batch_processing(FALSE)
        results_trigger(isolate(results_trigger()) + 1L)
        if (identical(operation_status$current_id(), "reprocess-changed")) {
          operation_status$fail("Reprocessing stopped before completion.")
        }
      }, add = TRUE)
      n_ok <- 0L; n_review <- 0L; n_err <- 0L; t_start <- proc.time()
      withProgress(message = paste0("Reprocessing ", length(changed), " changed channel(s)..."),
                   value = 0, {
                     for (i in seq_along(changed)) {
                       op_items[[i]] <- operation_item(changed[[i]], "Processing")
                       operation_status$update("Processing channels", (i - 1) / length(changed),
                                               changed[[i]], op_items)
                       setProgress((i - 1) / length(changed),
                                   message = paste0("(", i, "/", length(changed), ") ", changed[[i]]))
                       run_one(changed[[i]], do_gc = FALSE)
                       if (changed[[i]] %in% stale_names()) n_err <- n_err + 1L
                       else n_ok <- n_ok + 1L
                       err_now <- process_errors[[changed[[i]]]]
                       review_reasons <- if (is.null(err_now))
                         channel_review_messages(changed[[i]]) else character(0)
                       needs_review <- length(review_reasons) > 0L
                       if (needs_review) n_review <- n_review + 1L
                       op_items[[i]] <- operation_item(
                         changed[[i]], if (!is.null(err_now)) "Failed" else if (needs_review) "Review" else "Completed",
                         err_now %||% (if (needs_review)
                           paste(review_reasons, collapse = "; ") else "Processed")
                       )
                     }
                   })
      gc(verbose = FALSE, full = TRUE)
      elapsed <- round((proc.time() - t_start)[["elapsed"]], 1)
      batch_summary_state(list(processed = n_ok - n_review, review = n_review, already_done = 0L,
                               skipped = 0L, failed = n_err, elapsed = elapsed))
      operation_status$complete(
        paste0(n_ok - n_review, " processed | ", n_review, " review | ", n_err, " failed | ", elapsed, "s"),
        warnings = c(
          if (n_err > 0L) paste0(n_err, " channel(s) failed."),
          if (n_review > 0L) paste0(n_review, " channel(s) require review.")
        ),
        items = op_items, auto_close_ms = 3000L
      )
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
