# -----------------------------------------------------------------------
# R/mod_process.R
# -----------------------------------------------------------------------
# Process turns a channel configuration and source files into split results.
# It also owns asynchronous batch scheduling, SAP edits, metric tables, and
# the canonical Total Check displayed to users.
mod_process_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),

    tagList(
      card(
        card_header("Run Processing"),
        div(class = "process-run-panel",
            div(class = "process-field-block",
                selectInput(ns("channel_select"), "Select Channel", choices = NULL)),
            uiOutput(ns("role_summary_ui")),
            div(class = "process-run-actions",
                actionButton(ns("btn_one"), "Process Selected",
                             class = "btn-success btn-sm process-action-primary operation-trigger"),
                actionButton(ns("btn_all"), "Process All",
                             class = "btn-warning btn-sm process-action-primary operation-trigger"),
                actionButton(ns("btn_failed"), "Reprocess Failed Only",
                             class = "btn-outline-danger btn-sm process-action-secondary operation-trigger"),
                actionButton(ns("btn_changed"), "Reprocess Changed",
                             class = "btn-outline-secondary btn-sm process-action-secondary operation-trigger"))),
        hr(class = "hr-sm"),
        div(class = "process-run-summary",
            uiOutput(ns("batch_summary")),
            uiOutput(ns("error_summary"))),
        uiOutput(ns("status"))
      ),
      uiOutput(ns("merge_history_card"))
    ),

    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "flex-fill", uiOutput(ns("channel_pos"))),
          actionButton(ns("btn_next"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      uiOutput(ns("process_tabs"))
    )
  )
}
# -----------------------------------------------------------------------
# Server: result stores, processing pipelines, SAP, and metric views.
# -----------------------------------------------------------------------
mod_process_server <- function(id, data, config, channels,
                               update_merges = NULL,
                               config_import_event = reactive(NULL),
                               performance_cache = NULL,
                               operation_status = NULL) {
  moduleServer(id, function(input, output, session) {
    performance_cache <- ensure_performance_cache(performance_cache)
    operation_status <- operation_status %||% new_operation_status(session)

    results_store       <- reactiveValues()
    original_store      <- reactiveValues()
    merge_log_store     <- reactiveValues()
    history_store       <- reactiveValues()
    clean_store         <- reactiveValues()
    process_errors      <- reactiveValues()
    result_signatures   <- reactiveValues()
    result_versions     <- reactiveValues()
    async_total_checks  <- reactiveValues()
    results_trigger     <- reactiveVal(0L)
    is_batch_processing <- reactiveVal(FALSE)
    batch_summary_state <- reactiveVal(NULL)

    get_res  <- function(nm) results_store[[nm]]
    bump_result_version <- function(nm) {
      result_versions[[nm]] <- as.integer(result_versions[[nm]] %||% 0L) + 1L
    }
    set_res  <- function(nm, val) {
      results_store[[nm]] <- val
      bump_result_version(nm)
      if (!isTRUE(is_batch_processing()))
        results_trigger(isolate(results_trigger()) + 1L)
    }
    get_orig <- function(nm) original_store[[nm]]
    set_orig <- function(nm, val) { original_store[[nm]] <- val }
    get_log  <- function(nm) merge_log_store[[nm]] %||% list()
    set_log  <- function(nm, val) { merge_log_store[[nm]] <- val }
    get_hist <- function(nm) history_store[[nm]] %||% list()
    set_hist <- function(nm, val) { history_store[[nm]] <- val }
    set_error <- function(nm, msg = NULL) {
      process_errors[[nm]] <- msg
      if (!isTRUE(is_batch_processing()))
        results_trigger(isolate(results_trigger()) + 1L)
    }

    current_total_check <- function(nm) {
      cfg <- channels()[[nm]]
      d <- data()
      cached <- async_total_checks[[nm]]
      if (is.null(cfg) || is.null(d) || is.null(cached)) return(NULL)
      expected <- pso_cache_key(
        "async-total-check", current_async_data_signature(d),
        channel_signature(canonical_process_cfg(cfg)),
        result_versions[[nm]] %||% 0L
      )
      if (identical(cached$signature %||% "", expected)) cached$value else NULL
    }

    channel_review_messages <- function(nm) {
      messages <- character(0)
      check <- current_total_check(nm)
      if (is.null(check)) {
        messages <- c(messages, "Total Check unavailable")
      } else if (!identical(check$status %||% "error", "ok")) {
        messages <- c(messages, check$summary$message %||% "Total Check requires review")
      }
      merge_review <- sum(vapply(get_log(nm), function(item) {
        identical(item$status %||% "", "needs_review")
      }, logical(1)))
      if (merge_review > 0L) {
        messages <- c(messages, paste(merge_review, "SAP item(s) require review"))
      }
      unique(messages)
    }

    valid_nm <- function(nm)
      !is.null(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)

    select_modeled_tab <- function() {
      session$onFlushed(function() {
        tryCatch(bslib::nav_select("metric_tabs", "modeled", session = session),
                 error = function(e) NULL)
      }, once = TRUE)
    }

    metric_label <- function(metric) {
      if (identical(normalize_model_metric(metric), "spend")) "Spend/Cost" else "Activity"
    }

    for_indices_metric_for_cfg <- function(cfg) {
      modeled <- normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      )
      role <- cfg$for_indices_role %||% ""
      if (is.na(role) || !nzchar(role) ||
          identical(normalize_model_metric(role), modeled)) {
        role <- if (identical(modeled, "spend")) "activity" else "spend"
      }
      normalize_model_metric(role)
    }

    active_model_metric <- reactive({
      nm <- input$channel_select
      cfg <- if (valid_nm(nm)) channels()[[nm]] else NULL
      cfg <- reconcile_channel_metric_keywords(cfg)
      normalize_model_metric(cfg$modeled_role %||% cfg$model_metric %||% "activity")
    })

    active_for_indices_metric <- reactive({
      nm <- input$channel_select
      cfg <- if (valid_nm(nm)) channels()[[nm]] else NULL
      for_indices_metric_for_cfg(cfg %||% list())
    })

    metric_total_col <- function(metric) {
      if (identical(normalize_model_metric(metric), "spend")) "total_spend" else "total_activity"
    }

    metric_pct_col <- function(metric) {
      if (identical(normalize_model_metric(metric), "spend")) "pct_total_spend" else "pct_total_activity"
    }

    metric_display_columns <- sap_metric_display_columns

    output$role_summary_ui <- renderUI({
      nm <- input$channel_select
      if (!valid_nm(nm)) return(NULL)
      cfg <- channels()[[nm]] %||% list()
      modeled <- normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      )
      for_indices_role <- for_indices_metric_for_cfg(cfg)
      for_indices_found <- (cfg$role_pair_status %||% "Missing") %in%
        c("Matched", "Partial") &&
        length(cfg$for_indices_varname_include %||% character(0)) > 0
      source <- cfg$role_pair_source %||%
        if (identical(cfg$role_pair_status %||% "", "Matched"))
          "VOF / ModelDetails" else "Not found"
      div(
        class = "process-role-summary",
        div(class = "process-role-row",
            tags$span("Modeled", class = "process-role-label"),
            tags$strong(metric_label(modeled), class = "process-role-value modeled")),
        div(class = "process-role-row",
            tags$span("ForIndices", class = "process-role-label"),
            tags$strong(
              if (for_indices_found) metric_label(for_indices_role) else "Not found",
              class = paste("process-role-value",
                            if (for_indices_found) "for_indices" else "missing"))),
        div(class = "process-role-source",
            icon(if (identical(source, "RAE fallback")) "database" else "link"),
            tags$span(source))
      )
    })

    output$process_tabs <- renderUI({
      nm <- input$channel_select
      cfg <- if (valid_nm(nm)) channels()[[nm]] %||% list() else list()
      modeled <- normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      )
      for_indices <- for_indices_metric_for_cfg(cfg)
      navset_card_underline(
        id = session$ns("metric_tabs"),
        selected = "modeled",
        header = uiOutput(session$ns("period_filter_ui")),
        nav_panel(
          tags$span(metric_label(modeled), HTML("&nbsp;&middot;&nbsp;"), "Modeled"),
          value = "modeled",
          uiOutput(session$ns("activity_kpis")),
          uiOutput(session$ns("threshold_ui")),
          uiOutput(session$ns("merge_plan_toolbar")),
          uiOutput(session$ns("config_merge_report")),
          DTOutput(session$ns("diag_act"))
        ),
        nav_panel(
          tags$span(metric_label(for_indices), HTML("&nbsp;&middot;&nbsp;"), "ForIndices"),
          value = "for_indices",
          uiOutput(session$ns("for_indices_status_ui")),
          DTOutput(session$ns("diag_cost"))
        ),
        nav_panel(
          "Total Check",
          value = "total_check",
          uiOutput(session$ns("total_check_summary_ui")),
          uiOutput(session$ns("total_check_details_ui"))
        )
      )
    })

    observeEvent(input$channel_select, {
      nm <- input$channel_select
      if (!valid_nm(nm)) return()
      select_modeled_tab()
      tryCatch(DT::dataTableProxy(session$ns("diag_act")) %>% DT::selectRows(NULL),
               error = function(e) NULL)
    }, ignoreInit = TRUE)

    channel_source_data <- function(all_rags, cfg, indexed_rags = NULL,
                                    source_signature = NULL) {
      if (is.null(all_rags) || is.null(cfg) || !"VariableName" %in% names(all_rags))
        return(all_rags)

      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (!length(vi))
        return(all_rags)

      source_sig <- source_signature %||% data_signature(all_rags)
      vars_key <- pso_cache_key("process-source-variables", source_sig)
      available_vars <- pso_cache_get(performance_cache, vars_key)
      if (is.null(available_vars)) {
        available_vars <- unique(trimws(as.character(all_rags$VariableName)))
        pso_cache_set(performance_cache, vars_key, available_vars)
      }

      if (!length(cfg$modeled_varname_include %||% character(0))) {
        vi <- expand_varname_include_with_spend(
          available_vars,
          vi,
          cfg$spend_keyword %||% NULL
        )
      }
      vi <- expand_analytical_keys_to_variable_names(
        available_vars,
        vi
      )
      vi <- unique(trimws(as.character(vi)))
      vi <- vi[!is.na(vi) & nzchar(vi)]
      if (!length(vi))
        return(all_rags)

      cache_key <- pso_cache_key(
        "process-channel-source",
        source_sig,
        channel_signature(cfg), vi
      )
      cached <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached)) return(data.table::copy(cached))

      match_mode <- cfg$varname_match_mode %||%
        if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
      out <- subset_indexed_rae(
        indexed_rags %||% build_indexed_rae(all_rags),
        vi,
        if (identical(match_mode, "exact")) "exact" else "prefix"
      )
      pso_cache_set(performance_cache, cache_key, data.table::copy(out))
      out
    }

    empty_spend_diag <- function() {
      tibble::tibble(
        VariableSplit = character(),
        total_spend = numeric(),
        pct_total_spend = numeric(),
        max_index = numeric(),
        max = numeric(),
        max_no_outlier = numeric(),
        num_weeks_spend = numeric(),
        min_consecutive_weeks = numeric(),
        sd = numeric(),
        min = numeric(),
        quartile_1 = numeric(),
        median = numeric(),
        quartile_3 = numeric()
      )
    }

    info_table <- function(message, tone = c("info", "warning", "error")) {
      tone <- match.arg(tone)
      color <- switch(tone, info = "#2f75b5", warning = "#856404", error = "#721c24")
      bg <- switch(tone, info = "#EBF3FB", warning = "#fff3cd", error = "#f8d7da")
      DT::datatable(
        data.frame(Info = message),
        options = list(initComplete = dt_blue_callback, dom = "t"),
        rownames = FALSE
      ) %>% DT::formatStyle("Info", color = color, backgroundColor = bg,
                            fontWeight = "600")
    }

    # Signatures protect asynchronous results: a worker response is integrated
    # only while its data, configuration, and result version remain current.
    channel_signature <- function(cfg) {
      if (is.null(cfg)) return("")
      paste(c(
        cfg$model_variable %||% "",
        paste(cfg$varname_include %||% character(0), collapse = "|"),
        paste(cfg$modeled_varname_include %||% character(0), collapse = "|"),
        paste(cfg$for_indices_varname_include %||% character(0), collapse = "|"),
        cfg$modeled_role %||% "",
        cfg$role_pair_status %||% "",
        cfg$activity_keyword %||% "",
        cfg$spend_keyword %||% "",
        cfg$time_break_label %||% "",
        canonical_break_missing_part_value(),
        paste(cfg$split_columns %||% character(0), collapse = "|"),
        paste(vapply(cfg$dimension_breaks %||% list(), function(b)
          paste(b$column %||% "", b$separator %||% "",
                b$n_parts %||% "", canonical_break_missing_part_value(),
                paste(b$names %||% character(0), collapse = "~"),
                sep = ":"), character(1)), collapse = "|"),
        paste(vapply(cfg$saved_merges %||% list(), function(m)
          paste(m$new_name %||% "", isTRUE(m$active),
                paste(unlist(m$merged %||% list()), collapse = "~"),
                sep = ":"), character(1)), collapse = "|"),
        as.character(cfg$min_period %||% ""),
        as.character(cfg$max_period %||% "")
      ), collapse = "||")
    }

    current_async_data_signature <- function(d = data()) {
      pso_cache_key(
        "async-process-data",
        d$data_signature %||% data_signature(d$all_rags),
        data_signature(d$analytical),
        data_signature(d$dates_df)
      )
    }

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

    record_async_failure <- function(nm, message) {
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
      if (!is.null(issue)) return(list(error = issue))

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
        data_signature_value = current_async_data_signature(d),
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
        data_signature_value = current_async_data_signature(d),
        config_signature_value = channel_signature(cfg),
        result_version = result_versions[[nm]] %||% 0L
      )
      process_channel_pipeline(payload)
    }

    # Apply worker output on the session process after validating its snapshot.
    integrate_async_result <- function(job) {
      integration_started <- proc.time()
      nm <- job$channel %||% ""
      if (!isTRUE(async_state$active) ||
          !identical(job$operation_id %||% "", async_state$id)) return("ignored")

      cfg_now <- isolate(channels()[[nm]])
      cfg_compare <- canonical_process_cfg(cfg_now)
      data_now <- isolate(data())
      stale <- is.null(cfg_now) ||
        !identical(job$config_signature %||% "", channel_signature(cfg_compare)) ||
        !identical(job$data_signature %||% "", current_async_data_signature(data_now)) ||
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
        msg <- job$error %||% "Unknown processing error"
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
      result_signatures[[nm]] <- channel_signature(cfg_now)
      async_total_checks[[nm]] <- list(
        signature = pso_cache_key(
          "async-total-check", current_async_data_signature(data_now),
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
          record_async_failure(nm, prepared$error)
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

    mark_result_current <- function(nm, cfg, saved_merges = NULL) {
      cfg_current <- cfg
      if (!is.null(saved_merges))
        cfg_current$saved_merges <- saved_merges
      result_signatures[[nm]] <- channel_signature(cfg_current)
    }

    stale_names <- reactive({
      results_trigger()
      ch <- channels()
      sigs <- reactiveValuesToList(result_signatures)
      res <- reactiveValuesToList(results_store)
      names(ch)[vapply(names(ch), function(nm) {
        !is.null(res[[nm]]) && !identical(sigs[[nm]], channel_signature(ch[[nm]]))
      }, logical(1))]
    })

    make_export_buttons <- function(prefix, nm) {
      fname <- paste0(prefix, "_", nm, "_",
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
      list(
        list(extend = "csv",   text = "Download CSV",   filename = fname,
             className = "dt-button",
             exportOptions = list(modifier = list(page = "all"))),
        list(extend = "excel", text = "Download Excel", filename = fname,
             className = "dt-button",
             exportOptions = list(modifier = list(page = "all")))
      )
    }

    fmt_compact <- function(x) {
      x <- as.numeric(x)
      dplyr::case_when(
        abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"),
        abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
        abs(x) >= 1e3 ~ paste0(round(x / 1e3, 0), "K"),
        TRUE          ~ formatC(round(x), format = "f",
                                digits = 0, big.mark = ","))
    }

    strip_common_prefix <- function(names_vec) {
      if (length(names_vec) <= 1) return(names_vec)
      parts      <- strsplit(names_vec, "_")
      min_len    <- min(sapply(parts, length))
      if (min_len == 0) return(names_vec)
      common_len <- 0L
      for (i in seq_len(min_len)) {
        if (length(unique(sapply(parts, `[[`, i))) == 1L)
          common_len <- i else break
      }
      if (common_len == 0L) return(names_vec)
      sapply(parts, function(p) {
        rest <- p[(common_len + 1):length(p)]
        if (!length(rest)) paste(p, collapse = "_")
        else paste(rest, collapse = "_")
      })
    }
    # Channel selector
    observe({
      updateSelectInput(session, "channel_select", choices = names(channels()))
    })

    observeEvent(input$btn_prev, {
      nms <- names(channels()); if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur > 1)
        updateSelectInput(session, "channel_select", selected = nms[cur - 1])
    })
    observeEvent(input$btn_next, {
      nms <- names(channels()); if (!length(nms)) return()
      cur <- which(nms == input$channel_select)
      if (length(cur) > 0 && cur < length(nms))
        updateSelectInput(session, "channel_select", selected = nms[cur + 1])
    })

    output$channel_pos <- renderUI({
      nms <- names(channels()); nm <- input$channel_select
      if (!length(nms) || !valid_nm(nm)) return(NULL)
      idx <- which(nms == nm); if (!length(idx)) idx <- 0L
      tagList(
        tags$strong(nm, class = "ch-editor-name"),
        tags$span(paste0(" (", idx, " / ", length(nms), ")"),
                  class = "ch-editor-counter"))
    })
    # Status panel
    status_trigger <- reactive({
      results_trigger()
      names(channels())
      input$channel_select
    }) %>% debounce(300)

    output$status <- renderUI({
      status_trigger()
      ch_names <- names(channels())
      if (!length(ch_names))
        return(tags$p(class = "text-muted small mt-2", "No channels configured."))
      tagList(lapply(ch_names, function(nm) {
        processed <- !is.null(results_store[[nm]])
        stale     <- nm %in% stale_names()
        failed    <- !is.null(process_errors[[nm]])
        merge_log <- get_log(nm)
        n_merges  <- sum(vapply(merge_log, \(m) isTRUE(m$applied %||% TRUE), logical(1)))
        review_messages <- if (processed && !stale && !failed)
          channel_review_messages(nm) else character(0)
        needs_review <- length(review_messages) > 0L
        is_sel    <- identical(input$channel_select, nm)
        saved_m   <- channels()[[nm]]$saved_merges %||% list()
        n_saved   <- sum(vapply(saved_m, \(m) isTRUE(m$active), logical(1)))
        div(
          class = paste("status-item", if (is_sel) "selected" else ""),
          onclick = paste0("Shiny.setInputValue('", session$ns("ch_click"),
                           "','", nm, "',{priority:'event'});"),
          if (needs_review) icon("triangle-exclamation", class = "icon-warning-sm")
          else if (processed) icon("circle-check", class = "icon-success-sm")
          else           icon("circle",        class = "icon-empty-status"),
          tags$span(nm, class = "status-item-name"),
          div(class = "status-badges",
              if (failed)
                tags$span("Failed", class = "badge-error"),
              if (stale)
                tags$span("Needs reprocess", class = "badge-stale"),
              if (processed && !stale && !failed && !needs_review)
                tags$span("Processed", class = "badge-ready"),
              if (processed && !stale && !failed && needs_review)
                tags$span("Review", class = "badge-stale",
                          title = paste(review_messages, collapse = " | ")),
              if (processed && n_merges > 0)
                tags$span(paste0(n_merges, "m"), class = "badge-merge-count"),
              if (n_saved > 0)
                tags$span(paste0(n_saved, " saved"), class = "badge-saved",
                          title = paste0(n_saved,
                                         " merge(s) â€” auto-applied on process"))))
      }))
    })

    observeEvent(input$ch_click, {
      req(nzchar(input$ch_click %||% ""))
      updateSelectInput(session, "channel_select", selected = input$ch_click)
    }, ignoreInit = TRUE)

    observeEvent(input$channel_select, {
      tryCatch(DT::dataTableProxy(session$ns("diag_act")) %>% DT::selectRows(NULL),
               error = \(e) NULL)
    }, ignoreInit = TRUE)
    # run_one
    # Synchronous channel pipeline retained as the fallback and single-channel
    # path. It stores clean and merged results separately for export and undo.
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
        set_error(nm, err_stored)
        elapsed <- round((proc.time() - t_channel)[["elapsed"]], 3)
        if (isTRUE(getOption("pso.profile", FALSE))) {
          message("[mod_process] ", nm, " failed in ", elapsed, "s")
        }
        showNotification(paste(nm, "error:", err_stored),
                         type = "error", duration = 12)
        if (op_single) operation_status$fail(
          paste0("Processing failed for ", nm), err_stored
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
        result_signatures[[nm]] <- channel_signature(cfg)
        async_total_checks[[nm]] <- list(
          signature = pso_cache_key(
            "async-total-check", current_async_data_signature(d),
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
          if (operation_status$is_busy()) {
            operation_status$fail(paste0("Processing failed for ", nm), e$message)
          }
          stop(e)
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
    # Process All uses the sequential pipeline so every channel gets a turn;
    # one channel error is recorded without aborting the remaining batch.
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
                         err_msg <- conditionMessage(rags_nm)
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
                          result_signatures[[nm]] <- channel_signature(canonical_process_cfg(cfg))
                          async_total_checks[[nm]] <- list(
                            signature = pso_cache_key(
                              "async-total-check", current_async_data_signature(d),
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
    # Period filter UI
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
    # Validate SAP rows against the current result, then apply and persist only
    # the requested merge names through the existing merge contract.
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
          showNotification(paste("Error reading file:", conditionMessage(e)),
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
        showNotification(paste("Error reading SAP:", conditionMessage(e)),
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
    # Activity table
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
    # Cache reconciliation by source data, channel configuration, and result
    # version. The detailed table contains only keys that need review.
    total_check_signature <- reactive({
      nm <- req(input$channel_select)
      d <- req(data())
      cfg <- req(channels()[[nm]])
      pso_cache_key(
        "canonical-total-check",
        nm,
        data_signature(d$analytical),
        data_signature(d$all_rags),
        digest::digest(cfg, algo = "xxhash64")
      )
    })

    total_check_data <- reactive({
      selected_result_version()
      nm <- req(input$channel_select)
      res <- req(results_store[[nm]])
      d <- req(data())
      cfg <- req(channels()[[nm]])
      async_check <- async_total_checks[[nm]]
      expected_async_signature <- pso_cache_key(
        "async-total-check", current_async_data_signature(d),
        channel_signature(canonical_process_cfg(cfg)),
        result_versions[[nm]] %||% 0L
      )
      if (!is.null(async_check) &&
          identical(async_check$signature %||% "", expected_async_signature) &&
          !is.null(async_check$value)) {
        return(async_check$value)
      }
      cross_cols <- res$cross_cols %||% config()$cross_cols %||% "Geography"
      build_canonical_total_check(
        analytical = d$analytical,
        all_rags = d$all_rags,
        result = res,
        cfg = cfg,
        cross_cols = cross_cols,
        schema_metadata = d$schema_metadata %||% NULL,
        tolerance = 0.01
      )
    }) %>% bindCache(
      total_check_signature(),
      selected_result_version(),
      cache = performance_cache
    )

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
    list(
      results       = reactive(reactiveValuesToList(results_store)),
      clean_results = reactive(reactiveValuesToList(clean_store)),
      result_versions = reactive(reactiveValuesToList(result_versions)),
      qa_status = reactive({
        ch_names <- names(channels())
        res <- reactiveValuesToList(results_store)
        errs <- reactiveValuesToList(process_errors)
        failed <- names(errs)[!vapply(errs, is.null, logical(1))]
        processed <- ch_names[ch_names %in% names(res)]
        stale <- stale_names()
        merge_logs <- reactiveValuesToList(merge_log_store)
        merge_review_names <- unique(unlist(lapply(names(merge_logs), function(nm) {
          logs <- merge_logs[[nm]] %||% list()
          needs_review <- vapply(logs, function(m) {
            identical(m$status %||% "", "needs_review")
          }, logical(1))
          if (!any(needs_review)) return(character(0))
          bad <- logs[needs_review]
          paste0(nm, ": ", vapply(bad, function(m) {
            m$new_name %||% m$name %||% "Unnamed merge"
          }, character(1)))
        }), use.names = FALSE))
        merge_review_names <- merge_review_names[
          !is.na(merge_review_names) & nzchar(merge_review_names)
        ]
        list(
          total           = length(ch_names),
          processed       = length(processed),
          pending         = length(setdiff(ch_names, processed)),
          failed          = length(intersect(ch_names, failed)),
          failed_names    = intersect(ch_names, failed),
          stale           = length(intersect(ch_names, stale)),
          stale_names     = intersect(ch_names, stale),
          merge_review    = length(merge_review_names),
          merge_review_names = merge_review_names,
          batch_running   = isTRUE(is_batch_processing()),
          last_batch      = batch_summary_state()
        )
      })
    )
  })
}
