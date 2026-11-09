# Coordinate at most two in-flight channel jobs per session over the shared
# worker pool. Only this session integrates results into reactive stores.
register_process_async_scheduler <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Normalize the modeled metric before signing or submitting a channel.
    canonical_process_cfg <- function(cfg) {
      if (is.null(cfg)) return(NULL)
      cfg$model_metric <- normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      )
      reconcile_channel_metric_keywords(cfg)
    }

    async_enabled <- isTRUE(ensure_pso_async(here::here()))
    async_state <- new.env(parent = emptyenv())
    async_state$active <- FALSE
    async_state$id <- ""
    async_state$title <- ""
    async_state$queue <- character(0)
    async_state$slots <- vector("list", 2L)
    async_state$items <- list()
    async_state$completed <- 0L
    async_state$processed <- 0L
    async_state$failed <- 0L
    async_state$review <- 0L
    async_state$discarded <- 0L
    async_state$skipped <- 0L
    async_state$already_done <- 0L
    async_state$started <- NULL
    async_state$warnings <- character(0)
    async_state$dispatching <- FALSE

    async_items <- function() unname(async_state$items)

    update_async_modal <- function(detail = "") {
      total <- length(async_state$items)
      terminal_statuses <- c("Completed", "Review", "Failed", "Discarded", "Skipped")
      done <- sum(vapply(
        async_state$items,
        function(item) (item$status %||% "") %in% terminal_statuses,
        logical(1)
      ))
      async_state$completed <- as.integer(done)
      progress_detail <- paste0(
        done, " of ", total, " completed",
        if (nzchar(detail)) paste0(" | ", detail) else ""
      )
      operation_status$update(
        stage = "Processing channels",
        progress = if (total) as.numeric(done) / as.numeric(total) else 1,
        detail = progress_detail,
        items = async_items(),
        counts = list(
          Completed = async_state$processed,
          Review = async_state$review,
          Failed = async_state$failed,
          Discarded = async_state$discarded,
          Pending = max(0L, total - done - sum(vapply(
            async_state$slots, function(x) !is.null(x), logical(1)
          )))
        )
      )
    }

    set_async_item <- function(nm, status, detail = "") {
      async_state$items[[nm]] <- operation_item(nm, status, detail)
      invisible(NULL)
    }

    safe_update_async_modal <- function(detail = "") {
      tryCatch(update_async_modal(detail), error = function(e) {
        message("[pso.async] Progress display update failed: ", conditionMessage(e))
        NULL
      })
    }

    # Keep one failed channel from stopping the remaining batch queue.
    record_async_failure <- function(nm, message, safe = FALSE) {
      if (!isTRUE(safe)) {
        message <- pso_public_error(
          message, paste0("process.async.failure.", nm),
          "Channel processing failed. Review the channel audit and Setup files."
        )
      }
      process_errors[[nm]] <- message
      async_state$failed <- async_state$failed + 1L
      async_state$warnings <- c(async_state$warnings, paste0(nm, ": ", message))
      set_async_item(nm, "Failed", message)
      invisible(NULL)
    }

    validate_process_inputs <- function(nm, d, gcfg, cfg) {
      if (is.null(cfg)) return("Channel configuration unavailable")
      if (is.null(d$all_rags)) return("All RAGs data not uploaded")
      if (is.null(d$analytical)) return("AnalyticalDataset not uploaded")
      if (is.null(d$dates_df)) return("Date mapping is unavailable")
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date))
        return("Reporting period not configured")
      if (is.null(gcfg$cross_cols)) return("Cross-sections not detected")
      model_var <- cfg$model_variable %||% ""
      if (!nzchar(model_var)) return("model_variable not configured")
      if (!model_var %in% names(d$analytical))
        return(paste0("'", model_var, "' not found in AnalyticalDataset"))
      NULL
    }

    # Snapshot only serializable inputs before dispatching work to a worker.
    prepare_async_payload <- function(nm) {
      d <- isolate(data())
      gcfg <- isolate(config())
      cfg <- isolate(channels()[[nm]])
      issue <- validate_process_inputs(nm, d, gcfg, cfg)
      if (!is.null(issue)) return(list(error = issue, safe_error = TRUE))

      cfg <- canonical_process_cfg(cfg)
      prepared_at <- proc.time()
      rags_nm <- channel_source_data(
        d$all_rags, cfg, d$all_rags_indexed, d$data_signature
      )
      payload <- build_async_channel_payload(
        channel = nm,
        cfg = cfg,
        all_rags = rags_nm,
        analytical = d$analytical,
        dates_df = d$dates_df,
        global_config = gcfg,
        schema_metadata = d$schema_metadata,
        operation_id = async_state$id,
        data_signature_value = current_async_data_signature(),
        config_signature_value = channel_signature(cfg),
        result_version = result_versions[[nm]] %||% 0L
      )
      payload$preparation_seconds <- round(
        (proc.time() - prepared_at)[["elapsed"]], 3
      )
      max_bytes <- as.numeric(getOption("pso.mirai.queue_memory_mb", 512)) * 1024^2
      list(
        payload = payload,
        use_sync = is.finite(max_bytes) &&
          payload$payload_size_estimate_bytes > max_bytes / 2
      )
    }

    run_sync_channel_pipeline <- function(nm, d, cfg, gcfg, rags_nm) {
      cfg <- canonical_process_cfg(cfg)
      payload <- build_async_channel_payload(
        channel = nm,
        cfg = cfg,
        all_rags = rags_nm,
        analytical = d$analytical,
        dates_df = d$dates_df,
        global_config = gcfg,
        schema_metadata = d$schema_metadata,
        operation_id = "synchronous",
        data_signature_value = current_async_data_signature(),
        config_signature_value = channel_signature(cfg),
        result_version = result_versions[[nm]] %||% 0L
      )
      process_channel_pipeline(payload)
    }

    # Apply worker output on the session process after validating its snapshot.
    # Discard late results if upload/config/result versions changed in flight.
    integrate_async_result <- function(job) {
      integration_started <- proc.time()
      nm <- job$channel %||% ""
      if (!isTRUE(async_state$active) ||
          !identical(job$operation_id %||% "", async_state$id)) return("ignored")

      cfg_now <- isolate(channels()[[nm]])
      cfg_compare <- canonical_process_cfg(cfg_now)
      stale <- is.null(cfg_now) ||
        !identical(job$config_signature %||% "", channel_signature(cfg_compare)) ||
        !identical(job$data_signature %||% "", current_async_data_signature()) ||
        !identical(as.integer(job$result_version %||% 0L),
                   as.integer(result_versions[[nm]] %||% 0L))
      if (stale) {
        async_state$discarded <- async_state$discarded + 1L
        async_state$warnings <- c(
          async_state$warnings,
          paste0(nm, ": result discarded because files or configuration changed.")
        )
        set_async_item(nm, "Discarded", "Configuration changed; reprocess required")
        return("discarded")
      }

      if (!isTRUE(job$ok)) {
        msg <- pso_public_error(
          job$error %||% "Unknown processing error",
          paste0("process.async.channel.", nm),
          "Channel processing failed. Review the channel audit and Setup files."
        )
        process_errors[[nm]] <- msg
        async_state$failed <- async_state$failed + 1L
        async_state$warnings <- c(async_state$warnings, paste0(nm, ": ", msg))
        set_async_item(nm, "Failed", msg)
        return("failed")
      }

      clean_store[[nm]] <- job$clean
      results_store[[nm]] <- job$final
      bump_result_version(nm)
      original_store[[nm]] <- job$final
      merge_log_store[[nm]] <- job$merge_report %||% list()
      history_store[[nm]] <- list()
      process_errors[[nm]] <- NULL
      result_signatures[[nm]] <- process_result_signature(cfg_now, job$data_signature)
      async_total_checks[[nm]] <- list(
        signature = pso_cache_key(
          "async-total-check", job$data_signature,
          channel_signature(canonical_process_cfg(cfg_now)), result_versions[[nm]]
        ),
        value = job$total_check
      )

      merge_review <- sum(vapply(job$merge_report %||% list(), function(x) {
        identical(x$status %||% "", "needs_review")
      }, logical(1)))
      total_review <- !identical(job$total_check$status %||% "error", "ok")
      if (merge_review > 0L || total_review) {
        async_state$review <- async_state$review + 1L
        detail <- c(
          if (merge_review > 0L) paste0(merge_review, " SAP item(s)"),
          if (total_review) "Total Check requires review"
        )
        set_async_item(nm, "Review", paste(detail, collapse = " | "))
        async_state$warnings <- c(
          async_state$warnings,
          paste0(nm, ": ", paste(detail, collapse = " | "), ".")
        )
      } else {
        async_state$processed <- async_state$processed + 1L
        set_async_item(
          nm, "Completed",
          paste0("Processed in ", round(job$execution_seconds %||% 0, 1), "s")
        )
      }
      if (isTRUE(getOption("pso.profile", FALSE))) {
        queue_seconds <- as.numeric(difftime(
          job$worker_started_at, job$queued_at, units = "secs"
        ))
        transfer_seconds <- as.numeric(difftime(
          Sys.time(), job$completed_at, units = "secs"
        ))
        integration_seconds <- (proc.time() - integration_started)[["elapsed"]]
        message(sprintf(
          paste0("[pso.async] %s payload-estimate=%.1fMB prepare=%.3fs queue=%.3fs ",
                 "execute=%.3fs transfer=%.3fs integrate=%.3fs"),
          nm, (job$payload_size_estimate_bytes %||% 0) / 1024^2,
          job$preparation_seconds %||% NA_real_, queue_seconds,
          job$execution_seconds %||% NA_real_, transfer_seconds,
          integration_seconds
        ))
      }
      "integrated"
    }

    finish_async_operation <- function() {
      if (!isTRUE(async_state$active)) return(invisible(NULL))
      elapsed <- round(as.numeric(difftime(Sys.time(), async_state$started,
                                           units = "secs")), 1)
      batch_summary_state(list(
        processed = async_state$processed,
        review = async_state$review,
        already_done = async_state$already_done,
        skipped = async_state$skipped + async_state$discarded,
        failed = async_state$failed,
        elapsed = elapsed
      ))
      parts <- c(
        if (async_state$processed) paste0(async_state$processed, " processed"),
        if (async_state$review) paste0(async_state$review, " review"),
        if (async_state$failed) paste0(async_state$failed, " failed"),
        if (async_state$discarded) paste0(async_state$discarded, " discarded"),
        if (async_state$already_done) paste0(async_state$already_done, " already done"),
        paste0(elapsed, "s")
      )
      async_state$active <- FALSE
      is_batch_processing(FALSE)
      results_trigger(isolate(results_trigger()) + 1L)
      operation_status$complete(
        paste(parts, collapse = " | "),
        warnings = async_state$warnings,
        items = async_items(),
        auto_close_ms = 3000L
      )
      invisible(NULL)
    }

    async_tasks <- if (async_enabled) {
      lapply(seq_len(2L), function(i) {
        shiny::ExtendedTask$new(function(payload) {
          submit_pso_mirai(payload)
        })
      })
    } else list()

    dispatch_async_jobs <- NULL
    complete_async_slot <- function(slot, job = NULL, task_error = NULL) {
      slot_job <- async_state$slots[[slot]]
      if (is.null(slot_job)) return(invisible(NULL))
      on.exit(later::later(dispatch_async_jobs, delay = 0), add = TRUE)
      nm <- slot_job$channel
      if (!is.null(task_error)) {
        job <- list(
          ok = FALSE, error = task_error, channel = nm,
          operation_id = slot_job$operation_id,
          data_signature = slot_job$data_signature,
          config_signature = slot_job$config_signature,
          result_version = slot_job$result_version
        )
      }
      worker_error <- if (!isTRUE(job$ok)) {
        job$error %||% "Background worker returned no result."
      } else NULL
      if (!is.null(worker_error)) {
        version_before <- result_versions[[nm]] %||% 0L
        fallback_error <- tryCatch({
          run_one(nm, do_gc = FALSE)
          process_errors[[nm]]
        }, error = function(e) conditionMessage(e))
        version_after <- result_versions[[nm]] %||% 0L
        if (is.null(fallback_error) && version_after > version_before) {
          merge_review <- any(vapply(merge_log_store[[nm]] %||% list(), function(x) {
            identical(x$status %||% "", "needs_review")
          }, logical(1)))
          check <- async_total_checks[[nm]]$value
          total_review <- !is.null(check) && !identical(check$status %||% "error", "ok")
          if (merge_review || total_review) {
            async_state$review <- async_state$review + 1L
            set_async_item(nm, "Review", "Processed synchronously; review diagnostics")
          } else {
            merge_review <- any(vapply(merge_log_store[[nm]] %||% list(), function(x) {
              identical(x$status %||% "", "needs_review")
            }, logical(1)))
            check <- async_total_checks[[nm]]$value
            total_review <- is.null(check) || !identical(check$status %||% "error", "ok")
            if (merge_review || total_review) {
              async_state$review <- async_state$review + 1L
              set_async_item(nm, "Review", "Processed synchronously; diagnostics require review")
            } else {
              async_state$processed <- async_state$processed + 1L
              set_async_item(nm, "Completed", "Processed with synchronous fallback")
            }
          }
          async_state$warnings <- c(
            async_state$warnings,
            paste0(nm, ": background processing failed; synchronous fallback succeeded.")
          )
        } else {
          details <- paste0(
            "Background: ", worker_error,
            if (!is.null(fallback_error)) paste0(" | Synchronous fallback: ", fallback_error)
            else " | Synchronous fallback produced no result."
          )
          record_async_failure(nm, details)
        }
      } else {
        integration_error <- tryCatch({
          integrate_async_result(job)
          NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(integration_error))
          record_async_failure(nm, paste0("Could not integrate result: ", integration_error))
      }
      async_state$slots[slot] <- list(NULL)
      async_state$completed <- async_state$completed + 1L
      safe_update_async_modal(nm)
      invisible(NULL)
    }

    if (length(async_tasks)) {
      for (slot in seq_along(async_tasks)) local({
        slot_id <- slot
        observeEvent(async_tasks[[slot_id]]$status(), {
          status <- async_tasks[[slot_id]]$status()
          if (identical(status, "success")) {
            value <- tryCatch(async_tasks[[slot_id]]$result(), error = function(e) e)
            if (inherits(value, "error")) {
              complete_async_slot(slot_id, task_error = conditionMessage(value))
            } else {
              complete_async_slot(slot_id, job = value)
            }
          } else if (identical(status, "error")) {
            msg <- tryCatch({ async_tasks[[slot_id]]$result(); "Worker failed" },
                            error = function(e) conditionMessage(e))
            complete_async_slot(slot_id, task_error = msg)
          }
        }, ignoreInit = TRUE)
      })
    }

    # Fill free worker slots in queue order; Review is terminal and never
    # prevents the next channel from being dispatched.
    dispatch_async_jobs <- function() {
      if (!isTRUE(async_state$active) || isTRUE(async_state$dispatching))
        return(invisible(NULL))
      async_state$dispatching <- TRUE
      on.exit(async_state$dispatching <- FALSE, add = TRUE)
      free <- which(vapply(async_state$slots, is.null, logical(1)))
      for (slot in free) {
        if (!length(async_state$queue)) break
        nm <- async_state$queue[[1]]
        async_state$queue <- async_state$queue[-1]
        prepared <- tryCatch(prepare_async_payload(nm), error = function(e) list(error = conditionMessage(e)))
        if (!is.null(prepared$error)) {
          record_async_failure(nm, prepared$error, safe = isTRUE(prepared$safe_error))
          async_state$completed <- async_state$completed + 1L
          next
        }
        if (isTRUE(prepared$use_sync)) {
          set_async_item(nm, "Processing", "Large payload; synchronous fallback")
          safe_update_async_modal(nm)
          version_before <- result_versions[[nm]] %||% 0L
          sync_error <- tryCatch({
            run_one(nm, do_gc = FALSE)
            process_errors[[nm]]
          }, error = function(e) conditionMessage(e))
          version_after <- result_versions[[nm]] %||% 0L
          if (is.null(sync_error) && version_after > version_before) {
            async_state$processed <- async_state$processed + 1L
            set_async_item(nm, "Completed", "Processed with synchronous fallback")
          } else {
            record_async_failure(
              nm,
              sync_error %||% "Synchronous fallback ended without producing a result."
            )
          }
          async_state$completed <- async_state$completed + 1L
          next
        }
        payload <- prepared$payload
        payload$queued_at <- Sys.time()
        async_state$slots[[slot]] <- list(
          channel = nm,
          operation_id = payload$operation_id,
          data_signature = payload$data_signature,
          config_signature = payload$config_signature,
          result_version = payload$result_version
        )
        set_async_item(
          nm, "Processing",
          paste0("Payload ~", round(payload$payload_size_estimate_bytes / 1024^2, 1), " MB")
        )
        safe_update_async_modal(nm)
        invoke_error <- tryCatch({
          async_tasks[[slot]]$invoke(payload)
          NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(invoke_error)) {
          record_async_failure(nm, invoke_error)
          async_state$slots[slot] <- list(NULL)
          async_state$completed <- async_state$completed + 1L
          safe_update_async_modal(nm)
        }
      }
      busy <- any(vapply(async_state$slots, function(x) !is.null(x), logical(1)))
      if (!length(async_state$queue) && !busy &&
          async_state$completed >= length(async_state$items)) {
        finish_async_operation()
      } else if (length(async_state$queue) && any(vapply(
        async_state$slots, is.null, logical(1)
      ))) {
        later::later(dispatch_async_jobs, delay = 0)
      }
      invisible(NULL)
    }

    start_async_operation <- function(nms, id, title, already_done = 0L) {
      # Batch jobs stay sequential until parallel processing is proven safe
      # across full production datasets. Single-channel work may use mirai.
      if (!async_enabled || !length(nms) || !identical(id, "process-selected"))
        return(FALSE)
      if (isTRUE(async_state$active) || operation_status$is_busy()) {
        showNotification("Another operation is already running.", type = "warning")
        return(TRUE)
      }
      async_state$active <- TRUE
      async_state$id <- paste0(id, "-", format(Sys.time(), "%Y%m%d%H%M%OS3"))
      async_state$title <- title
      async_state$queue <- unique(nms)
      async_state$slots <- vector("list", 2L)
      async_state$items <- setNames(lapply(async_state$queue, operation_item), async_state$queue)
      async_state$completed <- 0L
      async_state$processed <- 0L
      async_state$failed <- 0L
      async_state$review <- 0L
      async_state$discarded <- 0L
      async_state$skipped <- 0L
      async_state$already_done <- as.integer(already_done)
      async_state$started <- Sys.time()
      async_state$warnings <- character(0)
      is_batch_processing(TRUE)
      operation_status$start(
        async_state$id, title,
        c("Preparing channels", "Processing channels", "Running diagnostics", "Completed"),
        total_items = length(nms),
        detail = paste0(length(nms), " channel(s) queued")
      )
      update_async_modal("Dispatching workers")
      dispatch_async_jobs()
      TRUE
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
