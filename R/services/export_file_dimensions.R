# Estimate output dimensions from the prepared export snapshot.
build_export_file_dimensions <- function(export_data, details, analytical,
                                         side_mapping_nonfocus, channels, roi_data) {
  export_data <- export_data %||% list()
  channels <- channels %||% list()
  total_splits <- sum(vapply(export_data, function(item) {
    if (!is.list(item)) return(0L)
    as.integer(nrow(item$final %||% tibble::tibble()))
  }, integer(1)))
  analytical_split_cols <- sum(vapply(export_data, function(item) {
    if (!is.list(item)) return(0L)
    pre <- as.character((item$pre_act %||% tibble::tibble())$VariableSplit %||% character(0))
    final <- as.character((item$final %||% tibble::tibble())$VariableSplit %||% character(0))
    all_names <- unique(c(pre, final))
    as.integer(sum(!is.na(all_names) & nzchar(all_names)))
  }, integer(1)))
  composition_rows <- sum(vapply(export_data, function(item) {
    if (!is.list(item)) return(0L)
    merges <- extract_export_merges(item$cfg %||% list())
    as.integer(sum(vapply(merges, function(merge) {
      as.integer(length(merge$Components %||% character(0)))
    }, integer(1))))
  }, integer(1)))

  analytical_names <- names(analytical %||% list())
  included_vars <- if (!is.null(details) &&
                       all(c("Type", "VariableName") %in% names(details))) {
    details %>%
      dplyr::filter(!stringr::str_detect(
        stringr::str_to_lower(trimws(.data$Type)), "none"
      )) %>%
      dplyr::pull(.data$VariableName) %>% unique()
  } else {
    vars <- unique(vapply(channels, function(channel) {
      channel$model_variable %||% ""
    }, character(1)))
    vars[nzchar(vars)]
  }
  analytical_ids <- length(intersect(
    c("Geography", "Product", "Period", "BP_Year"), analytical_names
  ))
  analytical_vars <- if (!is.null(analytical)) {
    length(intersect(included_vars, analytical_names))
  } else 0L
  analytical_rows <- if (!is.null(analytical)) nrow(analytical) else 0L
  nonfocus_rows <- if (!is.null(side_mapping_nonfocus)) {
    nrow(side_mapping_nonfocus)
  } else 0L
  seed_has_geo <- !is.null(roi_data) && "Geography" %in% names(roi_data)
  seed_roi_cols <- if (!is.null(roi_data)) {
    numeric_cols <- names(roi_data)[vapply(roi_data, is.numeric, logical(1))]
    setdiff(numeric_cols, c("MainModelVariableName", "Channel"))
  } else character(0)
  seed_cols <- 6L + as.integer(seed_has_geo) + length(seed_roi_cols)

  list(
    analytical = if (analytical_rows > 0) list(
      rows = analytical_rows,
      cols = analytical_ids + analytical_vars + analytical_split_cols + nonfocus_rows
    ) else NULL,
    side_map = if (total_splits > 0) list(
      rows = total_splits + nonfocus_rows, cols = 6L
    ) else NULL,
    activity = if (total_splits > 0) list(rows = total_splits, cols = seed_cols) else NULL,
    composition = if (composition_rows > 0) list(rows = composition_rows, cols = 10L) else NULL,
    config = if (length(channels) > 0) list(rows = length(channels), cols = NULL) else NULL
  )
}
