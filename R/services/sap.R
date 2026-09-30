# Pure helpers for building and reading Splits Aggregation Plans.

sap_metric_display_columns <- function(df, metric) {
  metric <- normalize_model_metric(metric)
  total_name <- if (identical(metric, "spend")) "Total Spend" else "Total Activity"
  pct_name <- if (identical(metric, "spend")) "Pct Total Spend" else "Pct Total Activity"
  weeks_name <- if (identical(metric, "spend")) "Weeks With Spend" else "Weeks With Activity"
  rename_map <- c(
    total_activity = "Total Activity",
    pct_total_activity = "Pct Total Activity",
    total_spend = "Total Spend",
    pct_total_spend = "Pct Total Spend",
    max_index = "Max Index",
    max = "Max",
    max_no_outlier = "Max No Outlier",
    num_weeks_activity = "Weeks With Activity",
    num_weeks_spend = "Weeks With Spend",
    min_consecutive_weeks = "Max Consecutive Weeks",
    sd = "SD",
    min = "Min",
    quartile_1 = "Q1",
    median = "Median",
    quartile_3 = "Q3"
  )
  names(df) <- vapply(names(df), function(nm) {
    if (nm %in% names(rename_map)) rename_map[[nm]] else nm
  }, character(1))
  attr(df, "total_col") <- total_name
  attr(df, "pct_col") <- pct_name
  attr(df, "weeks_col") <- weeks_name
  df
}

sap_granularity_columns <- function(cfg, manifest) {
  configured <- unique(setdiff(
    as.character(cfg$split_columns %||% character(0)),
    "VariableName"
  ))
  intersect(configured[nzchar(configured)], names(manifest))
}

build_splits_aggregation_plan <- function(df, res, cfg, period_scope, metric) {
  manifest <- res$split_manifest %||% tibble::tibble()
  granularity_cols <- sap_granularity_columns(cfg, manifest)
  stat_cols <- if (!is.null(df)) setdiff(names(df), "VariableSplit") else character(0)
  output_cols <- c("VariableSplit", granularity_cols, "MergeName", stat_cols)

  if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df)) {
    empty <- stats::setNames(rep(list(character(0)), length(output_cols)), output_cols)
    return(as.data.frame(empty, check.names = FALSE, stringsAsFactors = FALSE))
  }

  granularity <- tibble::tibble(VariableSplit = character())
  required_manifest <- c("Role", "PeriodScope", "VariableSplit")
  if (length(granularity_cols) && nrow(manifest) &&
      all(required_manifest %in% names(manifest))) {
    manifest_rows <- manifest %>%
      dplyr::filter(
        .data$Role == "modeled",
        .data$PeriodScope == period_scope,
        .data$VariableSplit %in% df$VariableSplit
      )
    if (nrow(manifest_rows)) {
      granularity <- dplyr::bind_rows(lapply(
        split(manifest_rows, manifest_rows$VariableSplit),
        function(rows) {
          collapsed <- collapse_manifest_granularity(rows, granularity_cols)
          collapsed[, c("VariableSplit", granularity_cols), drop = FALSE]
        }
      ))
    }
  }

  out <- df
  if (nrow(granularity)) {
    out <- out %>% dplyr::left_join(granularity, by = "VariableSplit")
  } else {
    for (col in granularity_cols) out[[col]] <- NA_character_
  }
  for (col in granularity_cols) {
    values <- clean_split_part(out[[col]])
    values[is.na(values)] <- canonical_break_missing_part_value()
    out[[col]] <- values
  }
  out$MergeName <- NA_character_
  out <- out %>% dplyr::select(
    "VariableSplit",
    dplyr::all_of(granularity_cols),
    "MergeName",
    dplyr::all_of(stat_cols)
  )
  sap_metric_display_columns(out, metric)
}

clean_sap_column_names <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^\\ufeff", "", x)
  x <- sub("^<U\\+FEFF>", "", x)
  sub("^ÃƒÂ¯\\.\\.", "", x)
}

read_sap_plan_content <- function(content) {
  read_attempt <- function(kind) {
    con <- textConnection(content)
    on.exit(close(con), add = TRUE)
    switch(
      kind,
      tab = utils::read.delim(con, stringsAsFactors = FALSE,
                              na.strings = c("", "NA"), check.names = FALSE),
      semi = utils::read.csv2(con, stringsAsFactors = FALSE,
                              na.strings = c("", "NA"), check.names = FALSE),
      csv = utils::read.csv(con, stringsAsFactors = FALSE,
                            na.strings = c("", "NA"), check.names = FALSE)
    )
  }

  first_line <- strsplit(content %||% "", "\\r?\\n")[[1]][1] %||% ""
  preferred <- c(
    if (grepl("\\t", first_line, fixed = TRUE)) "tab",
    if (grepl(",", first_line, fixed = TRUE)) "csv",
    if (grepl(";", first_line, fixed = TRUE)) "semi",
    "csv", "tab", "semi"
  )
  fallback <- NULL
  for (kind in unique(preferred)) {
    plan <- tryCatch(read_attempt(kind), error = function(e) NULL)
    if (is.null(plan)) next
    names(plan) <- clean_sap_column_names(names(plan))
    fallback <- fallback %||% plan
    if (all(c("VariableSplit", "MergeName") %in% names(plan)) ||
        all(c("Split", "MergeName") %in% names(plan))) return(plan)
  }
  fallback
}

hydrate_sap_variable_split <- function(plan, current_data) {
  if (is.null(plan) || !"Split" %in% names(plan) ||
      is.null(current_data) || !nrow(current_data) ||
      !"VariableSplit" %in% names(current_data)) return(plan)

  split_names <- as.character(current_data$VariableSplit)
  parts <- strsplit(split_names, "_", fixed = TRUE)
  min_len <- min(lengths(parts))
  common_len <- 0L
  if (min_len > 0L) {
    for (i in seq_len(min_len)) {
      if (length(unique(vapply(parts, `[[`, character(1), i))) == 1L) {
        common_len <- i
      } else {
        break
      }
    }
  }
  display_names <- if (common_len == 0L) split_names else vapply(parts, function(p) {
    rest <- p[(common_len + 1L):length(p)]
    if (!length(rest)) paste(p, collapse = "_") else paste(rest, collapse = "_")
  }, character(1))
  lookup <- tibble::tibble(VariableSplit = split_names, Split = display_names) %>%
    dplyr::select("Split", "VariableSplit") %>%
    dplyr::distinct(.data$Split, .keep_all = TRUE)
  if (!nrow(lookup)) return(plan)

  plan$.row_id <- seq_len(nrow(plan))
  plan <- plan %>%
    dplyr::left_join(lookup, by = "Split", suffix = c("", ".matched")) %>%
    dplyr::arrange(.data$.row_id)
  if (!"VariableSplit" %in% names(plan)) {
    plan$VariableSplit <- plan$VariableSplit.matched
  } else if ("VariableSplit.matched" %in% names(plan)) {
    missing_split <- is.na(plan$VariableSplit) |
      !nzchar(trimws(as.character(plan$VariableSplit)))
    plan$VariableSplit[missing_split] <- plan$VariableSplit.matched[missing_split]
  }
  plan$.row_id <- NULL
  if ("VariableSplit.matched" %in% names(plan)) plan$VariableSplit.matched <- NULL
  plan
}
