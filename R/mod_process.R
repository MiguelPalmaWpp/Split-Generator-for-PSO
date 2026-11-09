# -----------------------------------------------------------------------
# R/mod_process.R
# -----------------------------------------------------------------------
# Process turns a channel configuration and source files into split results.
# It also owns asynchronous batch scheduling, SAP edits, metric tables, and
# the canonical Total Check displayed to users.
mod_process_ui <- function(id) build_process_ui(id)
# -----------------------------------------------------------------------
# Server: own clean/final results, result versions, error state, and SAP
# history. Controllers can process in workers or through the sync fallback.
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

    # Sidebar and diagnostic tab share one result per channel/version. Reuse
    # pipeline checks, then reconcile SAP edits without rerunning processing.
    current_total_check <- function(nm) {
      cfg <- channels()[[nm]]
      d <- data()
      res <- results_store[[nm]]
      cached <- async_total_checks[[nm]]
      if (is.null(cfg) || is.null(d) || is.null(res)) return(NULL)
      expected <- pso_cache_key(
        "async-total-check", current_async_data_signature(),
        channel_signature(canonical_process_cfg(cfg)),
        result_versions[[nm]] %||% 0L
      )
      if (identical(cached$signature %||% "", expected) && !is.null(cached$value)) {
        return(cached$value)
      }
      key <- pso_cache_key("current-total-check", nm, expected)
      check <- pso_cache_get(performance_cache, key)
      if (!is.null(check)) return(check)
      check <- tryCatch(
        build_canonical_total_check(
          analytical = d$analytical, all_rags = d$all_rags,
          result = res, cfg = cfg,
          cross_cols = res$cross_cols %||% config()$cross_cols %||% "Geography",
          schema_metadata = d$schema_metadata %||% NULL, tolerance = 0.01
        ),
        error = function(e) {
          message <- pso_public_error(
            e, paste0("process.total_check.", nm),
            "Total Check could not be completed. Review the channel inputs."
          )
          list(
            status = "error",
            summary = list(
              status = "error", message = message,
              model_variable = cfg$model_variable %||% "",
              modeled_metric = cfg$modeled_role %||% cfg$model_metric %||% "activity"
            ),
            detail = data.frame(), diagnostics = list(),
            stage_counts = data.frame(), applied_filters = character(0),
            warnings = character(0)
          )
        }
      )
      # A nonreactive cache avoids invalidating renderers while they read it.
      pso_cache_set(performance_cache, key, check)
      check
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
    controller_context <- list(
      scope = environment(), input = input, output = output, session = session,
      data = data, config = config, channels = channels,
      update_merges = update_merges,
      config_import_event = config_import_event,
      performance_cache = performance_cache,
      operation_status = operation_status,
      results_store = results_store, original_store = original_store,
      merge_log_store = merge_log_store, history_store = history_store,
      clean_store = clean_store, process_errors = process_errors,
      result_signatures = result_signatures,
      result_versions = result_versions,
      async_total_checks = async_total_checks,
      results_trigger = results_trigger,
      is_batch_processing = is_batch_processing,
      batch_summary_state = batch_summary_state
    )

    register_process_async_prep(controller_context)

    register_process_async_scheduler(controller_context)

    mark_result_current <- function(nm, cfg, saved_merges = NULL) {
      cfg_current <- cfg
      if (!is.null(saved_merges))
        cfg_current$saved_merges <- saved_merges
      previous <- result_signatures[[nm]]
      next_signature <- process_result_signature(cfg_current)
      # Editing SAP only validates the aggregation change, not older source
      # data or other channel rules that have not been reprocessed.
      if (is.list(previous) &&
          identical(previous$inputs, next_signature$inputs) &&
          identical(previous$base_channel, next_signature$base_channel)) {
        result_signatures[[nm]] <- next_signature
      }
    }

    stale_names <- reactive({
      results_trigger()
      ch <- channels()
      sigs <- reactiveValuesToList(result_signatures)
      res <- reactiveValuesToList(results_store)
      inputs <- current_async_data_signature()
      names(ch)[vapply(names(ch), function(nm) {
        !is.null(res[[nm]]) &&
          !identical(sigs[[nm]], process_result_signature(ch[[nm]], inputs))
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
    register_process_run_selected(controller_context)
    # Process All uses the sequential pipeline so every channel gets a turn;
    # one channel error is recorded without aborting the remaining batch.
    register_process_run_batch(controller_context)
    # Period filter UI
    register_process_role_data(controller_context)
    # Validate SAP rows against the current result, then apply and persist only
    # the requested merge names through the existing merge contract.
    register_process_sap(controller_context)
    # Activity table
    register_process_diagnostics(controller_context)
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
