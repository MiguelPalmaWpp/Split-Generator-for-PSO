# Asynchronous channel processing. Workers receive serializable payloads and
# never read Shiny reactives or session-scoped stores.

pso_async_compute <- "pso_process"

# Submit without blocking the Shiny event loop when the bounded dispatcher is
# full. A saturated queue is retried asynchronously and eventually rejected.
submit_pso_mirai <- function(payload) {
  queue_wait_ms <- max(1000L, as.integer(
    getOption("pso.mirai.queue_wait_timeout_ms", 60000L)
  ))
  retry_ms <- max(25L, as.integer(getOption("pso.mirai.queue_retry_ms", 100L)))
  timeout_ms <- as.integer(getOption("pso.mirai.channel_timeout_ms", 900000L))

  promises::promise(function(resolve, reject) {
    started <- Sys.time()
    attempt_submit <- NULL
    attempt_submit <- function() {
      elapsed_ms <- as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000
      if (elapsed_ms >= queue_wait_ms) {
        reject(simpleError(sprintf(
          "Background queue remained full for %s seconds.",
          round(elapsed_ms / 1000, 1)
        )))
        return(invisible(NULL))
      }

      task <- tryCatch(
        mirai::try_mirai(
          process_channel_pipeline(payload),
          payload = payload,
          .compute = pso_async_compute,
          .timeout = timeout_ms
        ),
        error = function(e) e
      )
      if (inherits(task, "error")) {
        reject(task)
      } else if (is.null(task)) {
        later::later(attempt_submit, delay = retry_ms / 1000)
      } else {
        promises::then(
          promises::as.promise(task),
          onFulfilled = resolve,
          onRejected = reject
        )
      }
      invisible(NULL)
    }
    attempt_submit()
  })
}

pso_async_available <- function() {
  isTRUE(getOption("pso.async.enabled", TRUE)) &&
    isTRUE(getOption("pso.async.available", FALSE)) &&
    requireNamespace("mirai", quietly = TRUE) &&
    requireNamespace("promises", quietly = TRUE) &&
    exists("ExtendedTask", envir = asNamespace("shiny"), inherits = FALSE)
}

# Start the shared worker pool and load the pure processing helpers once per
# worker. Session data is supplied later through each channel payload.
initialize_pso_async <- function(app_root = getwd()) {
  if (!isTRUE(getOption("pso.async.enabled", TRUE))) {
    options(pso.async.available = FALSE,
            pso.async.error = "Background processing is disabled by pso.async.enabled.")
    return(FALSE)
  }
  missing <- c(
    if (!requireNamespace("mirai", quietly = TRUE)) "mirai",
    if (!requireNamespace("promises", quietly = TRUE)) "promises",
    if (!exists("ExtendedTask", envir = asNamespace("shiny"), inherits = FALSE))
      "Shiny ExtendedTask"
  )
  if (length(missing)) {
    reason <- paste0("Missing dependency: ", paste(missing, collapse = ", "), ".")
    options(pso.async.available = FALSE, pso.async.error = reason)
    return(FALSE)
  }

  workers <- max(1L, as.integer(getOption("pso.mirai.workers", 2L)))
  queue_memory <- max(64, as.numeric(getOption("pso.mirai.queue_memory_mb", 512)))
  root <- normalizePath(app_root, winslash = "/", mustWork = TRUE)
  helper_files <- c(
    file.path(root, "R", "utils", c(
      "functions.R", "processing.R", "performance.R"
    )),
    file.path(root, "R", "contracts", "data_contracts.R"),
    file.path(root, "R", "services", "total_check.R"),
    file.path(root, "R", "services", "sap.R"),
    file.path(root, "R", "utils", "async_processing.R")
  )

  ok <- tryCatch({
    mirai::daemons(
      n = workers,
      dispatcher = TRUE,
      memory = queue_memory,
      .compute = pso_async_compute
    )
    bootstrap <- mirai::everywhere(
      {
        assign("%||%", function(a, b) if (is.null(a) || length(a) == 0) b else a,
               envir = .GlobalEnv)
        suppressPackageStartupMessages({
          library(dplyr)
          library(tidyr)
          library(stringr)
          library(purrr)
          library(data.table)
          library(lubridate)
        })
        assign("REQUIRED_COLS", c(
          "Geography", "Product", "VariableName", "Period",
          "Campaign", "Outlet", "Creative", "VariableValue"
        ), envir = .GlobalEnv)
        assign("CROSS_SECTION_CANDIDATES", c(
          "Geography", "Product", "Campaign", "Outlet", "Creative"
        ), envir = .GlobalEnv)
        assign("MFF_DIMS_STD", get("CROSS_SECTION_CANDIDATES", .GlobalEnv),
               envir = .GlobalEnv)
        assign("MEDIA_KEYWORD_DICT", list(
          activity = c(
            "Impressions", "Clicks", "GRPs", "Views", "Reach", "Streams",
            "Visits", "Conversions", "Engagements", "Opens", "Installs",
            "Leads", "Circulation", "Circulations", "Delivered", "Sendouts",
            "Sendout", "GRP", "Attendance", "Sents", "Sent", "Spend", "Cost"
          ),
          spend = c("Spend", "Cost", "Investment", "Budget")
        ), envir = .GlobalEnv)
        for (path in helper_files) source(path, local = .GlobalEnv)
        TRUE
      },
      helper_files = helper_files,
      .compute = pso_async_compute
    )
    bootstrap[]
    options(pso.async.error = NULL)
    TRUE
  }, error = function(e) {
    reason <- conditionMessage(e)
    try(mirai::daemons(0, .compute = pso_async_compute), silent = TRUE)
    options(pso.async.error = reason)
    FALSE
  })
  options(pso.async.available = isTRUE(ok))
  isTRUE(ok)
}

ensure_pso_async <- function(app_root = getwd()) {
  if (pso_async_available()) return(TRUE)
  if (!isTRUE(getOption("pso.async.enabled", TRUE))) return(FALSE)
  initialize_pso_async(app_root)
}

shutdown_pso_async <- function() {
  if (requireNamespace("mirai", quietly = TRUE)) {
    try(mirai::daemons(0, .compute = pso_async_compute), silent = TRUE)
  }
  options(pso.async.available = FALSE)
  invisible(NULL)
}

# Reduce a channel's inputs to the data needed by one worker invocation.
build_async_channel_payload <- function(channel, cfg, all_rags, analytical,
                                        dates_df, global_config,
                                        schema_metadata = NULL,
                                        operation_id, data_signature_value,
                                        config_signature_value,
                                        result_version = 0L) {
  cross_cols <- global_config$cross_cols %||% "Geography"
  analytical_cols <- unique(c(cross_cols, "Period", cfg$model_variable %||% ""))
  analytical_cols <- analytical_cols[nzchar(analytical_cols) & analytical_cols %in% names(analytical)]
  analytical_reduced <- analytical[, analytical_cols, drop = FALSE]

  payload <- new_process_payload(
    channel = channel,
    all_rags = all_rags,
    analytical = analytical_reduced,
    dates_df = dates_df,
    cfg = cfg,
    cross_cols = cross_cols,
    start_report_date = global_config$start_report_date,
    end_report_date = global_config$end_report_date,
    update_label = global_config$update_label,
    schema_metadata = schema_metadata,
    operation_id = operation_id,
    data_signature_value = data_signature_value,
    config_signature_value = config_signature_value,
    result_version = result_version %||% 0L
  )
  # Keep a conservative estimate so oversized payloads use the sync fallback
  # before they can exceed the bounded mirai dispatcher queue.
  payload$payload_size_estimate_bytes <- as.numeric(object.size(payload)) * 2
  payload
}

empty_merge_status <- function(merge) {
  requested <- unlist(merge$merged %||% character(0))
  list(
    applied = FALSE,
    new_name = merge$new_name %||% "",
    view = merge$view %||% "focus",
    requested = requested,
    matched = character(0),
    missing = requested,
    ambiguous = character(0),
    closest_examples = character(0),
    matched_count = 0L,
    requested_count = length(requested),
    source = "config",
    status = "needs_review"
  )
}

# Pure worker pipeline: process source data, apply saved SAP merges, and build
# Total Check output. The caller is responsible for integrating the result.
process_channel_pipeline <- function(payload) {
  started <- proc.time()
  worker_started_at <- Sys.time()
  outcome <- tryCatch({
    data_issue <- validate_data_bundle(payload)
    config_issue <- validate_channel_config(payload$cfg)
    if (!is.null(data_issue)) stop(data_issue, call. = FALSE)
    if (!is.null(config_issue)) stop(config_issue, call. = FALSE)
    clean <- process_channel(
      all_rags = payload$all_rags,
      analytical = payload$analytical,
      dates_df = payload$dates_df,
      cfg = payload$cfg,
      cross_cols = payload$cross_cols,
      start_report_date = payload$start_report_date,
      end_report_date = payload$end_report_date,
      update_label = payload$update_label,
      dimension_breaks = payload$cfg$dimension_breaks %||% list(),
      segment_overrides = payload$cfg$segment_overrides %||% list(),
      min_period = payload$cfg$min_period,
      max_period = payload$cfg$max_period,
      schema_metadata = payload$schema_metadata,
      progress_cb = function(detail, value = NULL) NULL
    )
    result_issue <- validate_processed_result(clean)
    if (!is.null(result_issue)) stop(result_issue, call. = FALSE)

    final <- clean
    merge_report <- list()
    saved <- Filter(function(x) isTRUE(x$active), payload$cfg$saved_merges %||% list())
    if (length(saved)) {
      for (merge in saved) {
        final <- tryCatch(
          apply_single_merge(final, merge, payload$cfg, notify = FALSE),
          error = function(e) {
            value <- final
            attr(value, "merge_status") <- empty_merge_status(merge)
            value
          }
        )
        status <- attr(final, "merge_status") %||% empty_merge_status(merge)
        status$source <- "config"
        status$status <- if (isTRUE(status$applied)) "applied" else "needs_review"
        merge_report <- c(merge_report, list(status))
      }
    }
    final$model_metric <- payload$cfg$model_metric

    total_check <- tryCatch(
      build_canonical_total_check(
        analytical = payload$analytical,
        all_rags = payload$all_rags,
        result = final,
        cfg = payload$cfg,
        cross_cols = final$cross_cols %||% payload$cross_cols,
        schema_metadata = payload$schema_metadata,
        tolerance = 0.01
      ),
      error = function(e) list(
        status = "error",
        diagnostics = list(error = conditionMessage(e)),
        detail = data.frame()
      )
    )
    list(ok = TRUE, clean = clean, final = final,
         merge_report = merge_report, total_check = total_check, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, clean = NULL, final = NULL, merge_report = list(),
         total_check = NULL, error = conditionMessage(e))
  })

  elapsed <- round((proc.time() - started)[["elapsed"]], 3)
  c(outcome, list(
    operation_id = payload$operation_id,
    channel = payload$channel,
    data_signature = payload$data_signature,
    config_signature = payload$config_signature,
    result_version = payload$result_version,
    payload_size_estimate_bytes = payload$payload_size_estimate_bytes,
    preparation_seconds = payload$preparation_seconds %||% NA_real_,
    worker_started_at = worker_started_at,
    execution_seconds = elapsed,
    queued_at = payload$queued_at,
    completed_at = Sys.time()
  ))
}
