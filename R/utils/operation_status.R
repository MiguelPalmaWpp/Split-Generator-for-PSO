# Shared per-session operation status controller for uploads, processing, and
# export. It sends UI updates and tracks progress without owning app data.
new_operation_status <- function(session) {
  or_default <- function(value, fallback) if (is.null(value)) fallback else value
  state <- new.env(parent = emptyenv())
  state$busy <- FALSE
  state$id <- NULL
  state$started_at <- NULL
  state$items <- list()
  state$counts <- list()
  state$current_progress <- NULL

  send <- function(payload) {
    session$sendCustomMessage("operationStatus", payload)
    # Long synchronous reads can otherwise hold custom messages until the
    # operation finishes. Flush the websocket output after every real update.
    tryCatch(session$flushOutput(), error = function(e) NULL)
    invisible(payload)
  }

  elapsed <- function() {
    if (is.null(state$started_at)) return(0)
    round(as.numeric(difftime(Sys.time(), state$started_at, units = "secs")), 1)
  }

  list(
    is_busy = function() isTRUE(state$busy),
    current_id = function() or_default(state$id, ""),
    start = function(id, title, stages = character(0), total_items = 0L,
                     detail = "Preparing operation") {
      if (isTRUE(state$busy)) return(FALSE)
      state$busy <- TRUE
      state$id <- as.character(id)
      state$started_at <- Sys.time()
      state$items <- list()
      state$counts <- list()
      state$current_progress <- 0
      send(list(
        action = "start", id = state$id, title = title,
        status = "Running", stage = if (length(stages)) stages[[1]] else detail,
        stages = as.list(stages), totalItems = as.integer(total_items),
        progress = 0, detail = detail
      ))
      TRUE
    },
    update = function(stage = NULL, progress = NULL, detail = NULL,
                      items = NULL, counts = NULL) {
      if (!isTRUE(state$busy)) return(invisible(FALSE))
      if (!is.null(items)) state$items <- items
      if (!is.null(counts)) state$counts <- counts
      if (!is.null(progress) && length(progress) && is.finite(progress[[1]])) {
        state$current_progress <- max(0, min(1, as.numeric(progress[[1]])))
      }
      send(list(
        action = "update", id = state$id, status = "Running",
        stage = or_default(stage, ""), progress = state$current_progress,
        detail = or_default(detail, ""), items = state$items,
        counts = state$counts, elapsed = elapsed()
      ))
      invisible(TRUE)
    },
    complete = function(summary, warnings = character(0), items = NULL,
                        auto_close_ms = 1500L) {
      if (!isTRUE(state$busy)) return(invisible(FALSE))
      has_review <- length(or_default(warnings, character(0))) > 0L
      if (!is.null(items)) state$items <- items
      send(list(
        action = "finish", id = state$id,
        status = if (has_review) "Review required" else "Completed",
        progress = 1, summary = or_default(summary, "Completed"),
        warnings = as.list(or_default(warnings, character(0))),
        items = state$items, counts = state$counts, elapsed = elapsed(),
        autoCloseMs = if (has_review) 0L else as.integer(auto_close_ms)
      ))
      state$busy <- FALSE
      state$id <- NULL
      state$current_progress <- NULL
      invisible(TRUE)
    },
    fail = function(message, technical_detail = NULL, items = NULL) {
      if (!is.null(items)) state$items <- items
      send(list(
        action = "finish", id = or_default(state$id, "operation"),
        status = "Failed", progress = 1,
        summary = or_default(message, "Operation failed"),
        technicalDetail = or_default(technical_detail, ""),
        items = state$items, counts = state$counts, elapsed = elapsed(), autoCloseMs = 0L
      ))
      state$busy <- FALSE
      state$id <- NULL
      state$current_progress <- NULL
      invisible(FALSE)
    },
    close = function() {
      send(list(action = "close", id = or_default(state$id, "")))
      state$busy <- FALSE
      state$id <- NULL
      state$current_progress <- NULL
      invisible(TRUE)
    }
  )
}

operation_item <- function(name, status = "Pending", detail = "") {
  list(name = as.character(name), status = as.character(status), detail = as.character(detail))
}
