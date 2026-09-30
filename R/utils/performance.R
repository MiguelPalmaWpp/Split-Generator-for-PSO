# Session-scoped cache, signatures, profiling, and indexed RAE helpers.
# Store derived data only; reactive objects, render functions, and widgets are
# not valid cache values.
new_performance_cache <- function(max_size = getOption("pso.cache.max_size", 256 * 1024^2),
                                  max_age = getOption("pso.cache.max_age", 30 * 60)) {
  cachem::cache_mem(
    max_size = max_size,
    max_age = max_age,
    evict = "lru"
  )
}

ensure_performance_cache <- function(cache = NULL) {
  if (is.null(cache)) new_performance_cache() else cache
}

pso_cache_key <- function(namespace, ...) {
  ns <- tolower(gsub("[^a-z0-9_-]+", "-", namespace))
  paste0(ns, "-", digest::digest(list(...), algo = "xxhash64", serialize = TRUE))
}

pso_cache_get <- function(cache, key) {
  if (is.null(cache)) return(NULL)
  value <- cache$get(key)
  if (cachem::is.key_missing(value)) NULL else value
}

pso_cache_set <- function(cache, key, value) {
  if (!is.null(cache)) cache$set(key, value)
  invisible(value)
}

pso_cache_reset <- function(cache) {
  if (!is.null(cache)) cache$reset()
  invisible(NULL)
}

pso_profile <- function(label, expr,
                        enabled = isTRUE(getOption("pso.profile", FALSE))) {
  if (!isTRUE(enabled)) return(force(expr))
  elapsed <- system.time(value <- force(expr))
  message(sprintf("[pso.profile] %s: %.3fs", label, elapsed[["elapsed"]]))
  value
}

data_signature <- function(data) {
  if (is.null(data)) return("null")
  digest::digest(
    list(
      rows = NROW(data),
      cols = names(data),
      classes = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
      head = utils::head(data, 3L),
      tail = utils::tail(data, 3L)
    ),
    algo = "xxhash64",
    serialize = TRUE
  )
}

# Add reusable lookup indexes while leaving the canonical source untouched.
build_indexed_rae <- function(data) {
  if (is.null(data)) return(NULL)
  if (!"VariableName" %in% names(data)) {
    return(structure(list(data = data, rows = NULL, variables = NULL),
                     class = "pso_rae_index"))
  }
  variable_raw <- trimws(as.character(data$VariableName))
  rows <- data.table::data.table(
    .pso_row_id = seq_len(nrow(data)),
    .pso_variable_norm = tolower(variable_raw)
  )
  data.table::setindexv(rows, ".pso_variable_norm")
  variables <- unique(data.table::data.table(
    .pso_variable_norm = tolower(variable_raw),
    VariableName = variable_raw
  ))
  structure(
    list(data = data, rows = rows, variables = variables),
    class = "pso_rae_index"
  )
}

subset_indexed_rae <- function(indexed_data, variable_names,
                               match_mode = c("exact", "prefix")) {
  match_mode <- match.arg(match_mode)
  if (is.null(indexed_data)) {
    return(indexed_data)
  }
  if (!inherits(indexed_data, "pso_rae_index")) {
    indexed_data <- build_indexed_rae(indexed_data)
  }
  source_data <- indexed_data$data
  if (is.null(source_data) || !"VariableName" %in% names(source_data)) {
    return(source_data)
  }
  vi <- unique(trimws(as.character(variable_names)))
  vi <- vi[!is.na(vi) & nzchar(vi)]
  if (!length(vi)) return(source_data[0, , drop = FALSE])

  selected_norms <- if (identical(match_mode, "exact")) {
    unique(tolower(vi))
  } else {
    pattern <- paste(
      paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")),
      collapse = "|"
    )
    indexed_data$variables[
      grepl(pattern, VariableName, ignore.case = TRUE, perl = TRUE),
      unique(.pso_variable_norm)
    ]
  }
  lookup <- data.table::data.table(.pso_variable_norm = selected_norms)
  row_ids <- indexed_data$rows[
    lookup,
    on = ".pso_variable_norm",
    nomatch = 0L,
    .pso_row_id
  ]
  source_data[sort(row_ids), , drop = FALSE]
}

# Execute one channel from explicit inputs so the same contract can run in a
# worker or in the synchronous fallback.
process_channel_job <- function(args) {
  tryCatch(
    list(ok = TRUE, result = do.call(process_channel, args), error = NULL),
    error = function(e) list(ok = FALSE, result = NULL, error = conditionMessage(e))
  )
}
