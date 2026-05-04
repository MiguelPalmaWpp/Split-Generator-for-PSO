# ── Statistical helpers ───────────────────────────────────────

min_consec_weeks <- function(x) {
  r <- rle(x > 0)
  if (any(r$values)) min(r$lengths[r$values]) else 0
}

max_no_outlier <- function(x) {
  nz <- x[x > 0]
  if (!length(nz)) return(0)
  q3 <- quantile(nz, .75, na.rm = TRUE)
  max(nz[nz <= q3 + 1.5 * IQR(nz, na.rm = TRUE)], na.rm = TRUE)
}

# ── splits_summary ────────────────────────────────────────────
# Optimized: single pass per column (was 12 separate sapply loops).
# nz (non-zero values) computed ONCE per column — was recomputed 7x
# separately for sd, min, quartile_1, median, quartile_3.

splits_summary <- function(df, metric = "activity") {
  
  df <- as.data.frame(df)
  if (nrow(df) == 0) return(tibble())
  
  period_col <- if ("Period" %in% names(df)) df[["Period"]] else NULL
  num_cols   <- names(df)[sapply(df, is.numeric)]
  if (!length(num_cols)) return(tibble())
  
  d     <- df[, num_cols, drop = FALSE]
  grand <- max(sum(d, na.rm = TRUE), 1)
  
  nm <- switch(tolower(metric),
               activity = c("total_activity", "pct_total_activity", "num_weeks_activity"),
               cost     = c("total_cost",     "pct_total_cost",     "num_weeks_cost"),
               spend    = c("total_spend",    "pct_total_spend",    "num_weeks_spend"),
               c("total",          "pct_total",          "num_weeks")
  )
  
  # Single pass per column — nz computed once, all stats in one function
  result <- bind_rows(lapply(num_cols, function(col) {
    x  <- d[[col]]
    nz <- x[!is.na(x) & x > 0]     # computed ONCE, reused for 7 stats below
    
    total_val    <- sum(x, na.rm = TRUE)
    active       <- !is.na(x) & x > 0
    weeks_active <- if (!is.null(period_col)) {
      length(unique(period_col[active]))
    } else {
      sum(active)
    }
    
    tibble(
      VariableSplit         = col,
      v1                    = total_val,
      v2                    = round(total_val / grand * 100, 4),
      v3                    = weeks_active,
      min_consecutive_weeks = min_consec_weeks(x),
      sd                    = if (length(nz) < 2) NA_real_ else sd(nz),
      min                   = if (!length(nz)) NA_real_ else min(nz),
      quartile_1            = if (!length(nz)) NA_real_ else as.numeric(quantile(nz, .25)),
      median                = if (!length(nz)) NA_real_ else as.numeric(median(nz)),
      quartile_3            = if (!length(nz)) NA_real_ else as.numeric(quantile(nz, .75)),
      max_no_outlier        = max_no_outlier(x),
      max                   = max(x, na.rm = TRUE)
    )
  }))
  
  result$max_index <- result$max / result$v1
  
  col_order <- c("VariableSplit", "v1", "v2", "v3", "max_index",
                 "min_consecutive_weeks", "sd", "min",
                 "quartile_1", "median", "quartile_3", "max_no_outlier", "max")
  result <- result[, col_order]
  names(result)[names(result) == "v1"] <- nm[1]
  names(result)[names(result) == "v2"] <- nm[2]
  names(result)[names(result) == "v3"] <- nm[3]
  
  as_tibble(result[order(-result[[nm[2]]]), ])
}

# ── Robust Period parser ───────────────────────────────────────

parse_period_robust <- function(x) {
  if (inherits(x, c("Date", "IDate")))      return(as.Date(x))
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x))                         return(as.Date(x, origin = "1970-01-01"))
  
  x <- trimws(as.character(x))
  
  fmts <- c(
    "%Y-%m-%d", "%Y/%m/%d",
    "%m/%d/%y", "%m/%d/%Y",
    "%d/%m/%y", "%d/%m/%Y",
    "%m-%d-%y", "%m-%d-%Y",
    "%d-%m-%y", "%d-%m-%Y",
    "%d.%m.%y", "%d.%m.%Y",
    "%Y%m%d"
  )
  
  for (fmt in fmts) {
    result <- suppressWarnings(as.Date(x, format = fmt))
    if (!anyNA(result) && all(as.integer(format(result, "%Y")) > 1900))
      return(result)
  }
  
  best <- NULL; best_valid <- 0L
  for (fmt in fmts) {
    result <- suppressWarnings(as.Date(x, format = fmt))
    valid  <- sum(!is.na(result) & as.integer(format(result, "%Y")) > 1900,
                  na.rm = TRUE)
    if (valid > best_valid) { best_valid <- valid; best <- result }
  }
  
  if (!is.null(best)) {
    n_fail <- sum(is.na(best))
    if (n_fail > 0)
      warning(sprintf(
        "parse_period_robust: %d date(s) could not be parsed. First raw: '%s'",
        n_fail, x[1]
      ))
    return(best)
  }
  
  warning("parse_period_robust: no format matched. First raw: '", x[1], "'")
  as.Date(rep(NA_character_, length(x)))
}

# ── File reader ───────────────────────────────────────────────

read_all_transformed <- function(path, ext) {
  
  raw <- if (tolower(ext) == "parquet") {
    arrow::read_parquet(path)
  } else if (tolower(ext) %in% c("xlsx", "xls")) {
    read_excel(path)
  } else if (tolower(ext) == "gz") {
    data.table::fread(file = path, data.table = FALSE,
                      colClasses = "character", showProgress = FALSE)
  } else if (tolower(ext) == "zip") {
    tmp <- file.path(tempdir(), paste0("unzip_", as.integer(Sys.time())))
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    unzip(path, exdir = tmp)
    csv_files <- list.files(tmp, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    if (!length(csv_files)) stop("No CSV file found inside the ZIP.")
    data.table::fread(csv_files[1], data.table = FALSE,
                      colClasses = "character", showProgress = FALSE)
  } else {
    data.table::fread(file = path, sep = "auto", encoding = "UTF-8",
                      data.table = FALSE, colClasses = "character",
                      showProgress = FALSE)
  }
  
  # ── Normalize "raw" prefix (all_extracted format) ─────────────
  # Some files use rawPeriod, rawVariableName, rawGeography, etc.
  # Remove the prefix so columns match REQUIRED_COLS
  if ("rawPeriod" %in% names(raw)) {
    names(raw) <- sub("^raw", "", names(raw))
    message("  [read_all_transformed] all_extracted format detected — 'raw' prefix removed")
  }
  
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  raw <- raw[, REQUIRED_COLS]
  
  message("  [read_all_transformed] Raw Period sample: '",
          raw$Period[!is.na(raw$Period)][1], "'")
  
  raw %>%
    mutate(
      Period        = parse_period_robust(Period),
      VariableValue = as.numeric(
        gsub(",", "", gsub(" ", "", as.character(VariableValue)))
      )
    ) %>%
    arrange(Geography, Product, VariableName, Period)
}

# ── String utility ────────────────────────────────────────────

parse_text_lines <- function(x) {
  strsplit(x %||% "", "\n")[[1]] %>%
    trimws() %>%
    (\(v) v[nchar(v) > 0])()
}

# ── Auto-detect cross-section columns from AnalyticalDataset ──────────────────
# Rule: columns that appear BEFORE "Period" AND are in CROSS_SECTION_CANDIDATES
auto_detect_cross_cols <- function(analytical) {
  cols       <- names(analytical)
  period_pos <- which(cols == "Period")
  
  if (!length(period_pos))
    stop("Column 'Period' not found in AnalyticalDataset.")
  
  before_period <- cols[seq_len(period_pos - 1)]
  detected      <- intersect(before_period, CROSS_SECTION_CANDIDATES)
  
  if (!length(detected))
    stop("No valid cross-section columns found before 'Period'. ",
         "Expected one of: ", paste(CROSS_SECTION_CANDIDATES, collapse = ", "))
  
  detected
}

# ── Auto-detect file type (all_rags vs all_transformed) ───────────────────────
# Rule: more than 1 unique combination of cross-section values → geographic (all_rags)
#       exactly 1 combination → national (all_transformed)
auto_detect_source_type <- function(df, cross_cols) {
  n_combos <- df %>%
    select(any_of(cross_cols)) %>%
    distinct() %>%
    nrow()
  
  if (n_combos > 1) "all_rags" else "all_transformed"
}