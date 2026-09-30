# Read and validate the app-generated result package for Model Update.
read_model_update_zip <- function(path) {
  if (is.null(path) || !file.exists(path)) stop("Select a results ZIP first.")
  entries <- utils::unzip(path, list = TRUE)$Name
  basenames <- basename(entries)
  required <- c(
    analytical = "Analytical Splits Extended.csv",
    side_mapping = "Side Model Mapping.csv",
    metadata = "Splits Metadata.csv"
  )
  positions <- match(unname(required), basenames)
  names(positions) <- names(required)
  missing <- names(required)[is.na(positions)]
  if (length(missing)) {
    stop(paste0("This results ZIP is missing required file(s): ",
                paste(unname(required[missing]), collapse = ", "), "."))
  }

  temp_dir <- tempfile("pso_model_update_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  selected <- entries[positions]
  utils::unzip(path, files = selected, exdir = temp_dir, junkpaths = TRUE)
  extracted <- file.path(temp_dir, basename(selected))
  names(extracted) <- names(required)

  analytical <- data.table::fread(extracted[["analytical"]], data.table = FALSE,
                                  showProgress = FALSE, check.names = FALSE)
  side_mapping <- data.table::fread(extracted[["side_mapping"]], data.table = FALSE,
                                    showProgress = FALSE, check.names = FALSE)
  metadata <- data.table::fread(extracted[["metadata"]], data.table = FALSE,
                                showProgress = FALSE, check.names = FALSE)
  if (!all(c("VariableSplit", "MainModelVariableName") %in% names(side_mapping)))
    stop("Side Model Mapping must contain VariableSplit and MainModelVariableName.")
  if (!all(c("Channel", "RecordType") %in% names(metadata)))
    stop("Splits Metadata must contain Channel and RecordType.")
  if (!"Period" %in% names(analytical))
    stop("Analytical Splits Extended must contain a Period column.")
  if (!inherits(analytical$Period, "Date")) {
    analytical$Period <- parse_period_robust(analytical$Period)
  }
  metadata <- normalize_splits_metadata_rows(metadata)
  labels <- unique(trimws(as.character(metadata$UpdateLabel[
    metadata$Type == "Config" & nzchar(as.character(metadata$UpdateLabel %||% ""))
  ])))
  labels <- labels[!is.na(labels) & nzchar(labels)]

  list(
    analytical = analytical,
    side_mapping = side_mapping,
    metadata = metadata,
    previous_label = if (length(labels) == 1L) labels[[1]] else "",
    label_candidates = labels,
    files = unname(required)
  )
}

# Infer which MainVars column represents the prior model from exact name overlap.
infer_previous_update_id <- function(side_mapping, mainvars_mapping) {
  if (is.null(side_mapping) || is.null(mainvars_mapping) ||
      !"MainModelVariableName" %in% names(side_mapping)) return("")
  past_names <- trimws(sub("____", "", as.character(side_mapping$MainModelVariableName),
                           fixed = TRUE))
  past_names <- unique(past_names[!is.na(past_names) & nzchar(past_names)])
  if (!length(past_names)) return("")
  raw_names <- trimws(as.character(side_mapping$MainModelVariableName))
  embedded_ids <- sub("^.*____", "", raw_names)
  embedded_ids <- unique(embedded_ids[embedded_ids %in% names(mainvars_mapping)])
  if (length(embedded_ids) == 1L) return(embedded_ids[[1]])
  scores <- vapply(names(mainvars_mapping), function(column) {
    values <- trimws(as.character(mainvars_mapping[[column]]))
    sum(past_names %in% values, na.rm = TRUE)
  }, integer(1))
  best <- names(scores)[scores == max(scores) & scores > 0L]
  if (length(best) == 1L) best else ""
}

# Convert imported SAP metadata into channel-level saved-merge records.
model_update_sap_by_channel <- function(metadata) {
  if (is.null(metadata) || !nrow(metadata)) return(list())
  rows <- metadata[metadata$Type == "Merge", , drop = FALSE]
  if (!nrow(rows)) return(list())
  result <- list()
  for (channel in unique(rows$Channel)) {
    channel_rows <- rows[rows$Channel == channel, , drop = FALSE]
    merges <- list()
    keys <- unique(paste(channel_rows$Name, channel_rows$ModelMetric, sep = "\r"))
    for (i in seq_along(keys)) {
      group <- channel_rows[paste(channel_rows$Name, channel_rows$ModelMetric,
                                  sep = "\r") == keys[[i]], , drop = FALSE]
      parts <- trimws(strsplit(as.character(group$Splits[[1]]), " ||| ", fixed = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (!length(parts) || !nzchar(group$Name[[1]])) next
      merges[[length(merges) + 1L]] <- list(
        id = length(merges) + 1L,
        new_name = group$Name[[1]],
        merged = as.list(parts),
        metric = normalize_model_metric(group$ModelMetric[[1]]),
        view = "focus",
        active = TRUE,
        inherited_from_model_update = TRUE
      )
    }
    if (length(merges)) result[[channel]] <- merges
  }
  result
}

model_update_history_name <- function(x, previous_label, current_label) {
  x <- as.character(x)
  previous_label <- trimws(as.character(previous_label %||% ""))
  current_label <- trimws(as.character(current_label %||% ""))
  if (!nzchar(current_label)) return(x)
  out <- x
  if (nzchar(previous_label)) {
    out <- gsub(paste0("_Before ", previous_label),
                paste0("_Before ", current_label), out, fixed = TRUE)
    out <- gsub(paste0("_", previous_label),
                paste0("_Before ", current_label), out, fixed = TRUE)
  } else {
    out <- sub("_Before [^|]*", paste0("_Before ", current_label), out)
  }
  has_history_suffix <- vapply(strsplit(out, "|", fixed = TRUE), function(parts) {
    endsWith(parts[[1]], paste0("_Before ", current_label))
  }, logical(1))
  for (i in which(!has_history_suffix)) {
    parts <- strsplit(out[[i]], "|", fixed = TRUE)[[1]]
    parts[[1]] <- paste0(parts[[1]], "_Before ", current_label)
    out[[i]] <- paste(parts, collapse = "|")
  }
  out
}

model_update_focus_name <- function(x, previous_label, current_label) {
  x <- as.character(x)
  previous_label <- trimws(as.character(previous_label %||% ""))
  current_label <- trimws(as.character(current_label %||% ""))
  if (!nzchar(previous_label) || !nzchar(current_label)) return(x)
  out <- gsub(paste0("_Before ", previous_label), paste0("_", current_label),
              x, fixed = TRUE)
  gsub(paste0("_", previous_label), paste0("_", current_label), out, fixed = TRUE)
}

relabel_model_update_sap <- function(sap, previous_label, current_label) {
  lapply(sap, function(merges) lapply(merges, function(merge) {
    merge$merged <- as.list(model_update_focus_name(
      unlist(merge$merged, use.names = FALSE), previous_label, current_label
    ))
    merge$new_name <- model_update_focus_name(
      merge$new_name, previous_label, current_label
    )
    merge
  }))
}

remap_model_update_sap <- function(sap, metadata, mainvars_mapping,
                                   previous_id, current_id, current_channels) {
  if (!length(sap) || is.null(metadata) || is.null(mainvars_mapping) ||
      !previous_id %in% names(mainvars_mapping) ||
      !current_id %in% names(mainvars_mapping) || !length(current_channels)) {
    return(sap)
  }
  configs <- metadata[metadata$Type == "Config", , drop = FALSE]
  output <- list()
  unresolved <- character(0)
  for (old_channel in names(sap)) {
    if (old_channel %in% names(current_channels)) {
      target <- old_channel
    } else {
      old_model <- trimws(as.character(configs$ModelVariable[
        match(old_channel, configs$Channel)
      ] %||% ""))
      row <- match(old_model, trimws(as.character(mainvars_mapping[[previous_id]])))
      new_model <- if (!is.na(row))
        trimws(as.character(mainvars_mapping[[current_id]][[row]])) else ""
      candidates <- names(current_channels)[vapply(current_channels, function(cfg) {
        identical(trimws(as.character(cfg$model_variable %||% "")), new_model)
      }, logical(1))]
      if (length(candidates) != 1L) {
        unresolved <- c(unresolved, old_channel)
        next
      }
      target <- candidates[[1]]
    }
    output[[target]] <- c(output[[target]] %||% list(), sap[[old_channel]])
  }
  attr(output, "unresolved_channels") <- unique(unresolved)
  output
}
