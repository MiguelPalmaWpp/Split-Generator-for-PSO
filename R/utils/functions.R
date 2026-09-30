# -----------------------------------------------------------------------
# R/utils/functions.R
# Shared parsing, metadata, Media Index, and naming rules used by modules.
# Keep these helpers independent of Shiny session state where possible.
# -----------------------------------------------------------------------

# Ordinal tag
ordinal_tag <- function(i) {
  c("First", "Second", "Third", "Fourth", "Fifth")[min(i, 5L)]
}

# =============================================================================
# DATE PARSERS
# =============================================================================

# parse_period_robust
# Date parsing orders are intentionally day-first before month-first because
# VOF/RAE files are commonly authored from LATAM Excel exports.
DATE_PARSE_ORDERS <- c("Ymd", "dmY", "mdY", "ymd", "dmy", "mdy")

parse_lubridate_orders <- function(x, orders) {
  x <- trimws(as.character(x %||% character(0)))
  out <- rep(as.Date(NA_character_), length(x))
  valid <- !is.na(x) & nzchar(x)
  if (!any(valid) || !requireNamespace("lubridate", quietly = TRUE)) return(out)
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x[valid],
    orders = orders,
    quiet = TRUE
  ))
  out[valid] <- as.Date(parsed)
  out
}

parse_date_with_orders <- function(x, orders = DATE_PARSE_ORDERS) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.null(x) || length(x) == 0L) return(as.Date(character(0L)))

  raw <- trimws(as.character(x))
  raw[raw %in% c("", "NA", "N/A", "NULL", "None", "none")] <- NA_character_
  out <- rep(as.Date(NA_character_), length(raw))

  serial_idx <- !is.na(raw) & grepl("^\\d+(\\.0+)?$", raw)
  if (any(serial_idx)) {
    n <- suppressWarnings(as.numeric(raw[serial_idx]))
    serial_ok <- !is.na(n) & n > 20000 & n < 70000
    serial_dates <- rep(as.Date(NA_character_), length(n))
    serial_dates[serial_ok] <- suppressWarnings(as.Date(n[serial_ok], origin = "1899-12-30"))
    out[serial_idx] <- serial_dates
  }

  parse_idx <- is.na(out) & !is.na(raw) & nzchar(raw)
  if (any(parse_idx) && requireNamespace("lubridate", quietly = TRUE)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      raw[parse_idx],
      orders = orders,
      quiet = TRUE
    ))
    out[parse_idx] <- as.Date(parsed)
  }

  parse_idx <- is.na(out) & !is.na(raw) & nzchar(raw)
  if (any(parse_idx)) {
    fmts <- c(
      "%Y%m%d", "%d%m%Y", "%m%d%Y",
      "%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y",
      "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y",
      "%d/%m/%y", "%m/%d/%y",
      "%d.%m.%Y", "%m.%d.%Y",
      "%d-%b-%Y", "%d-%b-%y"
    )
    vals <- raw[parse_idx]
    fallback <- rep(as.Date(NA_character_), length(vals))
    for (fmt in fmts) {
      missing <- is.na(fallback)
      if (!any(missing)) break
      fallback[missing] <- suppressWarnings(as.Date(vals[missing], format = fmt))
    }
    out[parse_idx] <- fallback
  }

  out
}

# Parses period strings from all_rags, analytical datasets and VOF files.
parse_period_robust <- function(x) {
  parse_date_with_orders(x)
}

# parse_vof_period
# Robust VOF period parser
# Returns a Date vector always never integers or numerics.
# Handles: YYYY-MM-DD M/D/YYYY MM/DD/YYYY M/D/YY Excel serials
parse_vof_period <- function(x) {
  parse_date_with_orders(x)
}

parse_period_token_date <- function(token) {
  token <- trimws(as.character(token %||% ""))
  digits <- stringr::str_extract(token, "\\d{8}")
  if (is.na(digits) || !nzchar(digits)) return(as.Date(NA_character_))
  suppressWarnings(as.Date(digits, format = "%Y%m%d"))
}

parse_vof_period_context <- function(x, period_token = NULL, role = c("any", "min", "max")) {
  role <- match.arg(role)
  parsed <- parse_vof_period(x)
  if (is.null(x) || length(x) == 0L) return(parsed)

  raw <- trimws(as.character(x))
  token <- trimws(as.character(period_token %||% rep("", length(raw))))
  if (length(token) == 1L && length(raw) > 1L) token <- rep(token, length(raw))
  if (length(token) != length(raw)) token <- rep("", length(raw))

  token_dates <- as.Date(vapply(token, parse_period_token_date, as.Date(NA_character_)))
  slash_idx <- !is.na(raw) & grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", raw)
  ambiguous_idx <- slash_idx & !is.na(token_dates)
  if (!any(ambiguous_idx)) return(parsed)

  ambiguous_raw <- raw[ambiguous_idx]
  mdY <- parse_lubridate_orders(ambiguous_raw, c("mdY", "mdy"))
  dmY <- parse_lubridate_orders(ambiguous_raw, c("dmY", "dmy"))

  if (all(is.na(mdY))) {
    mdY <- suppressWarnings(as.Date(ambiguous_raw, format = "%m/%d/%Y"))
    mdY2 <- suppressWarnings(as.Date(ambiguous_raw, format = "%m/%d/%y"))
    mdY[is.na(mdY)] <- mdY2[is.na(mdY)]
  }
  if (all(is.na(dmY))) {
    dmY <- suppressWarnings(as.Date(ambiguous_raw, format = "%d/%m/%Y"))
    dmY2 <- suppressWarnings(as.Date(ambiguous_raw, format = "%d/%m/%y"))
    dmY[is.na(dmY)] <- dmY2[is.na(dmY)]
  }

  idx <- which(ambiguous_idx)
  tdates <- token_dates[idx]
  use_mdy <- !is.na(mdY) & !is.na(tdates) & mdY == tdates
  use_dmy <- !is.na(dmY) & !is.na(tdates) & dmY == tdates

  parsed[idx[use_mdy]] <- mdY[use_mdy]
  parsed[idx[!use_mdy & use_dmy]] <- dmY[!use_mdy & use_dmy]
  parsed
}

# parse_period (alias used by infer_schema)
parse_period <- parse_period_robust

# =============================================================================
# SCHEMA INFERENCE HELPERS
# =============================================================================

# shared column contracts
clean_names <- function(x) {
  x <- as.character(x)
  x <- gsub("^\ufeff", "", x, perl = TRUE)
  trimws(x)
}

clean_data_columns <- function(df) {
  if (is.null(df)) return(df)
  names(df) <- clean_names(names(df))
  df
}

column_contract <- function(df, required = character(), optional = character()) {
  cols <- clean_names(names(if (is.null(df)) data.frame() else df))
  required <- clean_names(required)
  optional <- clean_names(optional)
  list(
    present_required = intersect(required, cols),
    missing_required = setdiff(required, cols),
    present_optional = intersect(optional, cols),
    missing_optional = setdiff(optional, cols),
    columns = cols
  )
}

# is_weekly_like
is_weekly_like <- function(dates) {
  dates <- sort(dates[!is.na(dates)])
  if (length(dates) < 2) return(FALSE)
  median_diff <- median(as.numeric(diff(dates)), na.rm = TRUE)
  median_diff >= 6 && median_diff <= 8
}

# =============================================================================
# SCHEMA INFERENCE
# =============================================================================

# infer_schema
# Infers the structural schema of the Analytical dataset.
# Returns: dims (cross-section candidates), time_col, variables, date range, etc.
infer_schema <- function(df, time_col = "Period") {
  if (!is.data.frame(df)) stop("df must be a data.frame")

  names(df) <- clean_names(names(df))

  if (!(time_col %in% names(df))) {
    idx <- which(tolower(names(df)) == tolower(time_col))
    if (length(idx) != 1) stop(sprintf("Time column '%s' not found.", time_col))
    time_col <- names(df)[idx]
  }

  idx_period <- match(time_col, names(df))
  if (is.na(idx_period)) stop("Could not locate time column position.")

  candidates <- if (idx_period == 1) character(0) else names(df)[seq_len(idx_period - 1)]
  dims        <- intersect(candidates, MFF_DIMS_STD)
  vars        <- if (idx_period < ncol(df)) names(df)[(idx_period + 1):ncol(df)]
  else character(0)

  period_parsed <- parse_period(df[[time_col]])
  date_min      <- suppressWarnings(min(period_parsed, na.rm = TRUE))
  date_max      <- suppressWarnings(max(period_parsed, na.rm = TRUE))
  if (is.infinite(date_min)) date_min <- NA
  if (is.infinite(date_max)) date_max <- NA

  list(
    dims        = dims,
    time_col    = time_col,
    variables   = vars,
    variables_n = length(vars),
    rows        = nrow(df),
    cols        = ncol(df),
    date_min    = date_min,
    date_max    = date_max,
    weekly_like = is_weekly_like(period_parsed)
  )
}

# build_schema_metadata
# Builds full schema metadata from the Analytical dataset.
# Returns:
# $xs_dims cross-sectional dimension columns
# $useful_long longitudinal dims with values other than "Total"
# $discarded_long longitudinal dims where all values are "Total"
# $name_lookup data.frame: OriginalName, VariableName, <one col per long dim>
build_schema_metadata <- function(df, schema) {
  tc   <- schema$time_col
  dims <- schema$dims

  # A. Cross-sectional dims: vary across entities within the same period
  xs_dims <- character(0)
  if (length(dims) > 0) {
    counts  <- vapply(dims, function(d) {
      if (length(unique(df[[d]])) <= 1) return(1L)
      max(tapply(df[[d]], df[[tc]], dplyr::n_distinct), na.rm = TRUE)
    }, numeric(1))
    xs_dims <- names(counts)[counts > 1]
  }

  # B. Longitudinal dims: MFF dims that are NOT cross-sectional
  long_dims_expected <- setdiff(MFF_DIMS_STD, xs_dims)
  n_suffix           <- length(long_dims_expected)

 # C. Parse variable names format: BaseName_D1val_D2val_..._Dnval
  numeric_cols  <- names(df)[vapply(df, is.numeric, logical(1))]
  name_analysis <- data.frame(OriginalName = numeric_cols,
                              stringsAsFactors = FALSE)
  for (col in c("VariableName", long_dims_expected))
    name_analysis[[col]] <- NA_character_

  if (n_suffix > 0) {
    for (i in seq_len(nrow(name_analysis))) {
      nm    <- name_analysis$OriginalName[i]
      parts <- strsplit(nm, "_", fixed = TRUE)[[1]]
      if (length(parts) >= (n_suffix + 1)) {
        name_analysis[i, "VariableName"]     <- paste(
          head(parts, length(parts) - n_suffix), collapse = "_")
        name_analysis[i, long_dims_expected] <- tail(parts, n_suffix)
      } else {
        name_analysis[i, "VariableName"] <- nm
      }
    }
  } else {
    name_analysis$VariableName <- name_analysis$OriginalName
  }

  # D. Classify longitudinal dims as useful or discarded
  useful_long    <- character(0)
  discarded_long <- character(0)
  for (d in long_dims_expected) {
    u_vals <- unique(name_analysis[[d]][!is.na(name_analysis[[d]])])
    if (length(u_vals) == 0 || all(u_vals == "Total"))
      discarded_long <- c(discarded_long, d)
    else
      useful_long <- c(useful_long, d)
  }

  list(
    xs_dims        = xs_dims,
    useful_long    = useful_long,
    discarded_long = discarded_long,
    name_lookup    = name_analysis
  )
}

# =============================================================================
# CROSS-SECTION DETECTION
# =============================================================================

# auto_detect_cross_cols
# Kept as fallback when schema_metadata is not available.
# Prefer xs_dims from build_schema_metadata when possible.
auto_detect_cross_cols <- function(analytical) {
  candidates <- intersect(CROSS_SECTION_CANDIDATES, names(analytical))
  found <- Filter(function(col) {
    n <- dplyr::n_distinct(analytical[[col]])
    n > 1 && n < nrow(analytical) * 0.5
  }, candidates)
  if (!length(found)) "Geography" else found
}

# =============================================================================
# FILE READER
# =============================================================================

# read_main_data
# Reads the main data file. Accepts .csv and .zip only (as per UI restrictions).
# read_main_data
# Accepts:
# .zip contains exactly one .txt file (always this structure)
# .csv direct CSV upload (legacy / small files)
read_main_data <- function(path, ext) {

  raw <- if (tolower(ext) == "zip") {

 # Extract ZIP
    tmp <- file.path(tempdir(), paste0("unzip_", as.integer(Sys.time())))
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    unzip(path, exdir = tmp)

 # Find the TXT file always exactly one
    txt_files <- list.files(tmp, pattern = "\\.txt$",
                            full.names = TRUE, recursive = TRUE)

    if (!length(txt_files))
      stop("No TXT file found inside the ZIP.")

 # Read fread auto-detects delimiter (tab, comma, pipe, etc.)
    data.table::fread(txt_files[1],
                      data.table    = FALSE,
                      colClasses    = "character",
                      encoding      = "UTF-8",
                      showProgress  = FALSE)

  } else {
 # Direct CSV upload
    data.table::fread(file         = path,
                      sep          = "auto",
                      encoding     = "UTF-8",
                      data.table   = FALSE,
                      colClasses   = "character",
                      showProgress = FALSE)
  }

 # rawPeriod alias
  if ("rawPeriod" %in% names(raw))
    names(raw) <- sub("^raw", "", names(raw))

 # Validate required columns
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss))
    stop("Missing columns: ", paste(miss, collapse = ", "))

  raw <- raw[, intersect(REQUIRED_COLS, names(raw)), drop = FALSE]

 # Parse and sort
  raw %>%
    dplyr::mutate(
      Period        = parse_period_robust(Period),
      VariableValue = as.numeric(gsub(",", "",
                                      gsub(" ", "",
                                           as.character(VariableValue))))
    ) %>%
    dplyr::arrange(Geography, VariableName, Period)
}

# =============================================================================
# CONFIG CSV
# =============================================================================

# export_channels_csv
apply_dimension_aliases <- function(d, dimension_aliases) {
  if (!length(dimension_aliases)) return(d)
  for (als in dimension_aliases) {
    source <- trimws(as.character(als$source %||% ""))
    alias <- trimws(as.character(als$alias %||% ""))
    if (!nzchar(source) || !nzchar(alias) || identical(source, alias)) next
    if (!source %in% names(d)) next
    if (alias %in% names(d) && !identical(alias, source)) next
    d[[alias]] <- d[[source]]
  }
  d
}

normalize_geo_label <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!length(x) || is.na(x[1]) || !nzchar(x[1])) return("")
  x <- gsub("\\s+", "", x[1])
  if (grepl("^GeoLabel\\d+$", x, ignore.case = TRUE)) {
    num <- sub("^GeoLabel", "", x, ignore.case = TRUE)
    return(paste0("GeoLabel", num))
  }
  x
}

build_split_period_suffix <- function(update_label, focus = TRUE,
                                      time_break_label = "",
                                      geo_label = "") {
  update_label <- trimws(as.character(update_label %||% ""))
  time_break_label <- trimws(as.character(time_break_label %||% ""))
  geo_label <- normalize_geo_label(geo_label)
  if (nzchar(time_break_label)) geo_label <- ""

  suffix <- if (isTRUE(focus)) update_label else paste0("Before ", update_label)
  extras <- c(time_break_label, geo_label)
  extras <- extras[nzchar(extras)]
  if (length(extras)) suffix <- paste0(suffix, "|", paste(extras, collapse = "|"))
  suffix
}

normalize_model_metric <- function(x, default = "activity") {
  x <- tolower(trimws(as.character(x %||% default)[1]))
  if (is.na(x) || !nzchar(x)) return(default)
  if (x %in% c("spend", "cost", "investment", "budget")) "spend" else "activity"
}

reconcile_channel_metric_keywords <- function(cfg, reference_cfg = NULL,
                                              keyword_dict = MEDIA_KEYWORD_DICT) {
  if (is.null(cfg)) return(cfg)
  reference_cfg <- reference_cfg %||% list()

  spend_terms <- unique(keyword_dict$spend %||% c("Spend", "Cost", "Investment", "Budget"))
  spend_terms <- spend_terms[!is.na(spend_terms) & nzchar(trimws(as.character(spend_terms)))]

  has_spend_term <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (!length(x) || !nzchar(x[1]) || !length(spend_terms)) return(FALSE)
    any(stringr::str_detect(x[1], stringr::regex(paste(spend_terms, collapse = "|"),
                                                ignore_case = TRUE)))
  }

  act_kw <- trimws(as.character(cfg$activity_keyword %||% ""))
  spend_kw <- trimws(as.character(cfg$spend_keyword %||% ""))
  same_kw <- nzchar(act_kw) && nzchar(spend_kw) &&
    identical(tolower(act_kw), tolower(spend_kw))

  vars <- trimws(as.character(cfg$varname_include %||% character(0)))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  spend_like_vars <- length(vars) > 0 &&
    mean(vapply(vars, has_spend_term, logical(1))) >= 0.8

  ref_metric <- normalize_model_metric(reference_cfg$model_metric %||% "", default = "")
  model_name_spend <- has_spend_term(cfg$model_variable %||% "") ||
    has_spend_term(cfg$channel_name %||% "")

  if (same_kw && (identical(ref_metric, "spend") || spend_like_vars || model_name_spend)) {
    cfg$model_metric <- "spend"
  } else {
    cfg$model_metric <- normalize_model_metric(
      cfg$model_metric %||% reference_cfg$model_metric %||% "activity"
    )
  }

  if (identical(normalize_model_metric(cfg$model_metric %||% "activity"), "spend")) {
    if (!nzchar(spend_kw)) {
      spend_kw <- trimws(as.character(reference_cfg$spend_keyword %||% "Spend"))
      cfg$spend_keyword <- spend_kw
    }

    current_act <- trimws(as.character(cfg$activity_keyword %||% ""))
    if (!nzchar(current_act) ||
        (nzchar(spend_kw) && identical(tolower(current_act), tolower(spend_kw)))) {
      ref_act <- trimws(as.character(reference_cfg$activity_keyword %||% ""))
      if (!nzchar(ref_act) ||
          (nzchar(spend_kw) && identical(tolower(ref_act), tolower(spend_kw)))) {
        ref_act <- "Activity"
      }
      cfg$activity_keyword <- ref_act
    }
  }

  cfg
}

detect_activity_keyword <- function(var_names,
                                    keyword_dict = MEDIA_KEYWORD_DICT) {
  for (kw in keyword_dict$activity)
    if (any(stringr::str_detect(var_names, stringr::regex(kw, ignore_case = TRUE))))
      return(kw)
  "Impressions"
}

metric_base_name <- function(var_names,
                             keyword_dict = MEDIA_KEYWORD_DICT) {
  x <- trimws(as.character(var_names %||% character(0)))
  x <- stringr::str_remove(x, "_Total(_Total)*$")
  keywords <- unique(c(keyword_dict$activity, keyword_dict$spend))
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords)) {
    escaped <- stringr::str_replace_all(keywords, "([\\W])", "\\\\\\1")
    x <- stringr::str_remove(
      x,
      stringr::regex(
        paste0("\\s*(", paste(escaped, collapse = "|"), ")s?\\s*$"),
        ignore_case = TRUE
      )
    )
  }
  x <- stringr::str_squish(x)
  tolower(x)
}

normalize_model_role_family <- function(x) {
  x <- stringr::str_squish(trimws(as.character(x %||% character(0))))
  x <- stringr::str_remove(
    x,
    stringr::regex(
      "(?:-{2,}|[|_])\\s*(Spend|Cost|Investment|Budget|Activity)\\s*$",
      ignore_case = TRUE
    )
  )
  x <- stringr::str_remove(
    x,
    stringr::regex("\\s+(Spend|Cost|Investment|Budget)\\s*$", ignore_case = TRUE)
  )
  stringr::str_to_lower(stringr::str_squish(x))
}

model_details_type_is_modelled <- function(x) {
  type <- stringr::str_to_lower(trimws(as.character(x %||% "")))
  stringr::str_detect(type, "\\b(in|fixed)\\b") &
    !stringr::str_detect(type, "\\bnone\\b")
}

infer_metric_role <- function(metric = NULL, variable_names = NULL,
                              keyword_dict = MEDIA_KEYWORD_DICT) {
  metric <- stringr::str_to_lower(trimws(as.character(metric %||% character(0))))
  variable_names <- trimws(as.character(variable_names %||% character(0)))
  values <- c(metric, variable_names)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(NA_character_)

  spend_terms <- keyword_dict$spend %||% c("Spend", "Cost", "Investment", "Budget")
  if (any(vapply(spend_terms, function(term) {
    any(stringr::str_detect(values, stringr::regex(term, ignore_case = TRUE)))
  }, logical(1)))) return("spend")

  activity_terms <- setdiff(keyword_dict$activity %||% character(0), spend_terms)
  if (any(vapply(activity_terms, function(term) {
    any(stringr::str_detect(values, stringr::regex(term, ignore_case = TRUE)))
  }, logical(1)))) return("activity")
  NA_character_
}

resolve_modeled_for_indices_roles <- function(vof_df, model_details,
                                              modeled_names = NULL,
                                              keyword_dict = MEDIA_KEYWORD_DICT) {
  empty <- list()
  if (is.null(vof_df) || !nrow(vof_df) ||
      !all(c("MainModelVariableName", "AnalyticalVariableName") %in% names(vof_df))) {
    return(empty)
  }

  vof <- clean_data_columns(vof_df)
  details <- clean_data_columns(model_details)
  vof$MainModelVariableName <- trimws(as.character(vof$MainModelVariableName))
  vof$AnalyticalVariableName <- trimws(as.character(vof$AnalyticalVariableName))

  normalize_name <- function(x) {
    x <- stringr::str_squish(trimws(as.character(x %||% character(0))))
    x <- stringr::str_remove(x, stringr::regex("(_Total)+$", ignore_case = TRUE))
    stringr::str_to_lower(x)
  }

  detail_types <- if (!is.null(details) &&
                      all(c("VariableName", "Type") %in% names(details))) {
    split(trimws(as.character(details$Type)), normalize_name(details$VariableName))
  } else {
    list()
  }
  detail_type_text <- if (length(detail_types)) {
    vapply(detail_types, function(x) {
      x <- unique(x[!is.na(x) & nzchar(x)])
      paste(x, collapse = " | ")
    }, character(1))
  } else setNames(character(0), character(0))
  detail_active <- if (length(detail_types)) {
    vapply(detail_types, function(x) {
      any(model_details_type_is_modelled(x), na.rm = TRUE)
    }, logical(1))
  } else setNames(logical(0), character(0))
  detail_active_type_text <- if (length(detail_types)) {
    vapply(detail_types, function(x) {
      x <- unique(x[model_details_type_is_modelled(x)])
      x <- x[!is.na(x) & nzchar(x)]
      paste(x, collapse = " | ")
    }, character(1))
  } else setNames(character(0), character(0))

  vof$.__main_norm <- normalize_name(vof$MainModelVariableName)
  vof$.__family <- normalize_model_role_family(vof$MainModelVariableName)
  vof$.__details_type <- unname(detail_type_text[vof$.__main_norm])
  vof$.__details_type[is.na(vof$.__details_type)] <- ""
  vof$.__details_explicit <- nzchar(vof$.__details_type)
  vof$.__details_active <- unname(detail_active[vof$.__main_norm])
  vof$.__details_active[is.na(vof$.__details_active)] <- FALSE
  vof$.__details_active_type <- unname(
    detail_active_type_text[vof$.__main_norm]
  )
  vof$.__details_active_type[is.na(vof$.__details_active_type)] <- ""
  vof$.__metric_role <- vapply(seq_len(nrow(vof)), function(i) {
    metric <- if ("Metric" %in% names(vof)) vof$Metric[[i]] else NULL
    infer_metric_role(metric, vof$AnalyticalVariableName[[i]], keyword_dict)
  }, character(1))

  modeled_names <- unique(trimws(as.character(modeled_names %||%
    vof$MainModelVariableName[vof$.__details_active])))
  modeled_names <- modeled_names[!is.na(modeled_names) & nzchar(modeled_names)]
  if (!length(modeled_names)) return(empty)

  pair_sources <- function(modeled_vars, for_indices_vars) {
    modeled_vars <- unique(modeled_vars[!is.na(modeled_vars) & nzchar(modeled_vars)])
    for_indices_vars <- unique(for_indices_vars[!is.na(for_indices_vars) & nzchar(for_indices_vars)])
    if (!length(modeled_vars)) return(data.frame())
    modeled_base <- metric_base_name(modeled_vars, keyword_dict)
    for_indices_base <- metric_base_name(for_indices_vars, keyword_dict)
    rows <- lapply(seq_along(modeled_vars), function(i) {
      hits <- which(for_indices_base == modeled_base[[i]])
      data.frame(
        ModeledAnalyticalVariableName = modeled_vars[[i]],
        ForIndicesAnalyticalVariableName = if (length(hits) == 1L)
          for_indices_vars[[hits]] else "",
        PairBase = modeled_base[[i]],
        PairStatus = if (!length(hits)) "Missing" else if (length(hits) == 1L)
          "Matched" else "Ambiguous",
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(rows)
  }

  out <- list()
  for (mv in modeled_names) {
    modeled_rows <- vof[vof$MainModelVariableName == mv, , drop = FALSE]
    if (!nrow(modeled_rows)) next
    family <- unique(modeled_rows$.__family)[1]
    modeled_role <- infer_metric_role(
      if ("Metric" %in% names(modeled_rows)) modeled_rows$Metric else NULL,
      modeled_rows$AnalyticalVariableName,
      keyword_dict
    )
    if (is.na(modeled_role)) {
      modeled_role <- infer_metric_role(NULL, mv, keyword_dict)
    }

    candidates <- vof[
      vof$.__family == family &
        vof$MainModelVariableName != mv &
        vof$.__details_explicit &
        !vof$.__details_active,
      , drop = FALSE
    ]
    if (!is.na(modeled_role)) {
      opposite <- candidates$.__metric_role != modeled_role &
        !is.na(candidates$.__metric_role)
      candidates <- candidates[opposite, , drop = FALSE]
    }

    candidate_names <- unique(candidates$MainModelVariableName)

    pair_status <- "Missing"
    pair_reason <- "No connected ForIndices variable was found in VOF and ModelDetails."
    for_indices_rows <- candidates[0, , drop = FALSE]
    for_indices_name <- ""
    if (length(candidate_names) == 1L) {
      for_indices_name <- candidate_names[[1]]
      for_indices_rows <- candidates[candidates$MainModelVariableName == for_indices_name, , drop = FALSE]
      pair_status <- "Matched"
      pair_reason <- "Matched through the same VOF variable family and an inactive ModelDetails row."
    } else if (length(candidate_names) > 1L) {
      pair_status <- "Ambiguous"
      pair_reason <- "Multiple inactive ModelDetails variables match the same VOF family."
    }

    # Every VOF source row attached to the active MainModelVariableName belongs
    # to the modeled construct. A modeled variable may legitimately combine
    # Activity and Spend/Cost sources, so never discard rows by metric here.
    modeled_analytical <- unique(modeled_rows$AnalyticalVariableName)
    for_indices_analytical <- unique(for_indices_rows$AnalyticalVariableName)
    source_pairs <- pair_sources(modeled_analytical, for_indices_analytical)
    if (identical(pair_status, "Matched") && nrow(source_pairs) &&
        any(source_pairs$PairStatus == "Ambiguous")) {
      pair_status <- "Ambiguous"
      pair_reason <- "The VOF family matched, but multiple ForIndices source variables share the same base."
    }

    out[[mv]] <- list(
      modeled_variable = mv,
      modeled_type = {
        active_types <- unique(
          modeled_rows$.__details_active_type[
            nzchar(modeled_rows$.__details_active_type)
          ]
        )
        if (length(active_types)) active_types[[1]] else "Fallback"
      },
      modeled_role = modeled_role %||% "",
      modeled_analytical_variables = modeled_analytical,
      for_indices_variable = for_indices_name,
      for_indices_type = unique(for_indices_rows$.__details_type[nzchar(for_indices_rows$.__details_type)])[1] %||% "",
      for_indices_role = if (nrow(for_indices_rows))
        infer_metric_role(
          if ("Metric" %in% names(for_indices_rows)) for_indices_rows$Metric else NULL,
          for_indices_rows$AnalyticalVariableName,
          keyword_dict
        ) %||% "" else "",
      for_indices_analytical_variables = for_indices_analytical,
      pair_status = pair_status,
      pair_reason = pair_reason,
      pair_candidates = candidate_names,
      source_pairs = source_pairs
    )
  }
  out
}

expand_varname_include_with_spend <- function(all_variable_names,
                                              varname_include,
                                              spend_keyword = NULL,
                                              keyword_dict = MEDIA_KEYWORD_DICT) {
  vi <- unique(trimws(as.character(varname_include %||% character(0))))
  vi <- vi[!is.na(vi) & nzchar(vi)]
  all_vn <- unique(trimws(as.character(all_variable_names %||% character(0))))
  all_vn <- all_vn[!is.na(all_vn) & nzchar(all_vn)]
  if (!length(vi) || !length(all_vn)) return(vi)

  spend_terms <- if (!is.null(spend_keyword) && nzchar(trimws(spend_keyword))) {
    unique(c(spend_keyword, keyword_dict$spend))
  } else {
    keyword_dict$spend
  }
  spend_terms <- spend_terms[!is.na(spend_terms) & nzchar(spend_terms)]
  spend_match <- Reduce(`|`, lapply(spend_terms, function(kw) {
    stringr::str_detect(all_vn, stringr::regex(kw, ignore_case = TRUE))
  }))
  spend_candidates <- all_vn[spend_match]
  if (!length(spend_candidates)) return(vi)

  include_base <- unique(metric_base_name(vi, keyword_dict))
  spend_base <- metric_base_name(spend_candidates, keyword_dict)
  compatible <- vapply(spend_base, function(sb) {
    any(include_base == sb |
          startsWith(sb, paste0(include_base, " ")) |
          startsWith(include_base, paste0(sb, " ")))
  }, logical(1))

  unique(c(vi, spend_candidates[compatible]))
}

resolve_rae_for_indices_fallback <- function(all_variable_names,
                                            modeled_variable_names,
                                            modeled_role = NULL,
                                            keyword_dict = MEDIA_KEYWORD_DICT) {
  all_vars <- unique(trimws(as.character(all_variable_names %||% character(0))))
  modeled <- unique(trimws(as.character(modeled_variable_names %||% character(0))))
  all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]
  modeled <- modeled[!is.na(modeled) & nzchar(modeled)]
  empty <- list(
    candidates = character(0),
    role = "",
    status = "Missing",
    coverage = 0,
    missing_sources = modeled,
    source_pairs = data.frame()
  )
  if (!length(all_vars) || !length(modeled)) return(empty)

  modeled <- all_vars[tolower(all_vars) %in% tolower(
    expand_analytical_keys_to_variable_names(all_vars, modeled)
  )]
  modeled <- unique(modeled)
  if (!length(modeled)) return(empty)

  candidate_pool <- all_vars[!tolower(all_vars) %in% tolower(modeled)]
  if (!length(candidate_pool)) {
    empty$missing_sources <- modeled
    return(empty)
  }

  candidate_base <- metric_base_name(candidate_pool, keyword_dict)
  candidate_role <- vapply(candidate_pool, function(x) {
    role <- infer_metric_role(NULL, x, keyword_dict)
    if (is.na(role)) "" else role
  }, character(1))
  fallback_role <- normalize_model_metric(modeled_role %||% "activity")

  rows <- lapply(modeled, function(source) {
    source_base <- metric_base_name(source, keyword_dict)
    source_role <- infer_metric_role(NULL, source, keyword_dict)
    if (is.na(source_role)) source_role <- fallback_role
    target_role <- if (identical(source_role, "spend")) "activity" else "spend"
    compatible <- candidate_base == source_base |
      startsWith(candidate_base, paste0(source_base, " ")) |
      startsWith(source_base, paste0(candidate_base, " "))
    hits <- candidate_pool[compatible & candidate_role == target_role]
    data.frame(
      ModeledSourceVariableName = source,
      ForIndicesSourceVariableName = if (length(hits))
        paste(unique(hits), collapse = " | ") else "",
      PairBase = source_base,
      TargetRole = target_role,
      PairStatus = if (length(hits)) "Matched" else "Missing",
      stringsAsFactors = FALSE
    )
  })
  pairs <- dplyr::bind_rows(rows)
  candidates <- unique(trimws(unlist(strsplit(
    pairs$ForIndicesSourceVariableName[nzchar(pairs$ForIndicesSourceVariableName)],
    "\\s*\\|\\s*"
  ))))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  matched <- pairs$PairStatus == "Matched"
  coverage <- if (nrow(pairs)) sum(matched) / nrow(pairs) else 0
  status <- if (!length(candidates)) "Missing" else if (all(matched)) "Matched" else "Partial"
  resolved_roles <- unique(candidate_role[match(tolower(candidates), tolower(candidate_pool))])
  resolved_roles <- resolved_roles[!is.na(resolved_roles) & nzchar(resolved_roles)]

  list(
    candidates = candidates,
    role = if (length(resolved_roles) == 1L) resolved_roles[[1]] else
      if (identical(fallback_role, "spend")) "activity" else "spend",
    status = status,
    coverage = coverage,
    missing_sources = pairs$ModeledSourceVariableName[!matched],
    source_pairs = pairs
  )
}

expand_analytical_keys_to_variable_names <- function(all_variable_names,
                                                     varname_include) {
  vi <- unique(trimws(as.character(varname_include %||% character(0))))
  vi <- vi[!is.na(vi) & nzchar(vi)]
  all_vn <- unique(trimws(as.character(all_variable_names %||% character(0))))
  all_vn <- all_vn[!is.na(all_vn) & nzchar(all_vn)]
  if (!length(vi) || !length(all_vn)) return(vi)

  vi_l <- tolower(vi)
  matched <- all_vn[vapply(all_vn, function(vn) {
    vn_l <- tolower(trimws(vn))
    any(vi_l == vn_l | startsWith(vi_l, paste0(vn_l, "_")))
  }, logical(1))]

  unique(c(vi, matched))
}

# detect_spend_keyword
detect_spend_keyword <- function(main_data, varname_include,
                                 keyword_dict = MEDIA_KEYWORD_DICT) {
  if (is.null(main_data) || !"VariableName" %in% names(main_data))
    return("Spend")
  main_vn <- unique(trimws(as.character(main_data$VariableName)))
  main_vn <- main_vn[!is.na(main_vn) & nzchar(main_vn)]
  if (!length(main_vn)) return("Spend")
  matching <- if (length(varname_include) > 0) {
    unique(main_vn[
      Reduce("|", lapply(varname_include, function(p)
        grepl(p, main_vn, ignore.case = TRUE)))
    ])
  } else character(0)
  for (kw in keyword_dict$spend)
    if (any(stringr::str_detect(matching, stringr::regex(kw, ignore_case = TRUE))))
      return(kw)

  include_base <- unique(metric_base_name(varname_include, keyword_dict))
  include_base <- include_base[!is.na(include_base) & nzchar(include_base)]
  if (length(include_base)) {
    spend_match <- Reduce(`|`, lapply(keyword_dict$spend, function(kw) {
      stringr::str_detect(main_vn, stringr::regex(kw, ignore_case = TRUE))
    }))
    spend_candidates <- main_vn[spend_match]

    if (length(spend_candidates)) {
      spend_base <- metric_base_name(spend_candidates, keyword_dict)
      paired <- spend_candidates[spend_base %in% include_base]
      if (length(paired)) {
        for (kw in keyword_dict$spend)
          if (any(stringr::str_detect(paired, stringr::regex(kw, ignore_case = TRUE))))
            return(kw)
      }
    }
  }
  "Spend"
}

# =============================================================================
# VAR KEY BUILDER (kept for summary/coverage info)
# =============================================================================

# build_var_key
build_var_key <- function(main_data, vof_analytical_names) {
  if (is.null(main_data) || !"VariableName" %in% names(main_data))
    return(list(type = "standard", key_col = "var_key_v1",
                coverage = 0, distinct_df = NULL))

  dv <- main_data %>%
    dplyr::select(VariableName, dplyr::any_of("Product")) %>%
    dplyr::distinct()
  dv$var_key_v1 <- paste0(dv$VariableName, "_Total_Total_Total")
  cov_v1 <- mean(dv$var_key_v1 %in% vof_analytical_names)

  use_product <- FALSE
  if ("Product" %in% names(dv) && dplyr::n_distinct(dv$Product) > 1) {
    dv$var_key_v2 <- paste0(dv$VariableName, "_", dv$Product,
                            "_Total_Total_Total")
    cov_v2      <- mean(dv$var_key_v2 %in% vof_analytical_names)
    use_product <- cov_v2 > cov_v1 + 0.1
  }

  list(
    type        = if (use_product) "with_product" else "standard",
    key_col     = if (use_product) "var_key_v2" else "var_key_v1",
    coverage    = if (use_product)
      mean(dv$var_key_v2 %in% vof_analytical_names) else cov_v1,
    distinct_df = dv
  )
}

# Timeline HTML builder pure function, no reactive dependencies.
build_timeline_html <- function(an_range, main_range) {
  tryCatch({
    split_range <- function(r) as.Date(trimws(unlist(strsplit(r, "\u2192"))))
    an_d <- split_range(an_range); mn_d <- split_range(main_range)
    if (any(is.na(c(an_d, mn_d)))) return(NULL)

    an_start <- an_d[1]; an_end <- an_d[2]
    dt_start <- mn_d[1]; dt_end <- mn_d[2]
    min_d <- min(an_start, dt_start)
    max_d <- max(an_end,   dt_end)
    span  <- as.numeric(max_d - min_d)
    if (span == 0) return(NULL)

    pct    <- function(d) round(as.numeric(d - min_d) / span * 100, 1)
    pre    <- pct(an_start)
    mid    <- pct(an_end) - pct(an_start)
    post   <- 100 - pct(an_end)
    pre_yr <- round(as.numeric(an_start - dt_start) / 365, 1)

    div(class = "ts-wrap",
        div(class = "ts-bar",
            if (pre  > 0) div(class = "ts-seg ts-hist",
                              style = paste0("width:", pre,  "%")),
            div(class = "ts-seg ts-overlap",
                style = paste0("width:", mid,  "%")),
            if (post > 0) div(class = "ts-seg ts-extra",
                              style = paste0("width:", post, "%"))
        ),
        div(class = "ts-dates",
            tags$span(format(min_d, "%Y-%m-%d")),
            tags$span(format(max_d, "%Y-%m-%d"))),
        div(class = "ts-legend",
            div(class="ts-legend-item",
                tags$span(class="ts-dot ts-hist"),    "Data history"),
            div(class="ts-legend-item",
                tags$span(class="ts-dot ts-overlap"), "Analytical scope"),
            if (post > 0)
              div(class="ts-legend-item",
                  tags$span(class="ts-dot ts-extra"), "Extra future")
        ),
        if (pre > 0)
          div(class = "ts-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", pre_yr, " yr(s) of extra history \u2014",
                     " these become non-focus splits."))
    )
  }, error = \(e) NULL)
}

# Legacy implementation retained for reference. The active implementation lives
# in R/utils/processing.R and accepts schema_metadata.
process_channel_legacy <- function(all_rags,
                            analytical,
                            dates_df,
                            cfg,
                            cross_cols,
                            start_report_date,
                            end_report_date,
                            update_label,
                            dimension_breaks  = list(),
                            segment_overrides = list(),
                            min_period        = NULL,
                            max_period        = NULL,
                            progress_cb       = NULL) {

  pb <- function(detail, value = NULL) {
    if (!is.null(progress_cb)) progress_cb(detail, value)
  }

  pb("Preparing data...", 0.05)

  all_rags   <- as.data.frame(all_rags)
  analytical <- as.data.frame(analytical)
  dates_df   <- as.data.frame(dates_df)

  source_data <- all_rags
  if (is.null(source_data)) stop("All RAGs data not uploaded.")

 # OPT: pre-convert all date params once
  min_p   <- if (!is.null(min_period))
    tryCatch(as.Date(min_period), error = \(e) as.Date(NA)) else as.Date(NA)
  max_p   <- if (!is.null(max_period))
    tryCatch(as.Date(max_period), error = \(e) as.Date(NA)) else as.Date(NA)
  start_d <- as.Date(start_report_date)
  end_d   <- as.Date(end_report_date)

 # Filter to channel's VOF date range
  if (!is.na(min_p)) source_data <- source_data[source_data$Period >= min_p, ]
  if (!is.na(max_p)) source_data <- source_data[source_data$Period <= max_p, ]

 # Constrain to analytical date spine
  if (nrow(dates_df) > 0) {
    an_min_date <- min(dates_df$Period, na.rm = TRUE)
    an_max_date <- max(dates_df$Period, na.rm = TRUE)
    source_data <- source_data[
      source_data$Period >= an_min_date &
        source_data$Period <= an_max_date, ]
  }

  if (nrow(source_data) == 0)
    stop("No data available in the channel's date range (",
         min_period, " \u2192 ", max_period, ").")

  cross_id   <- c(cross_cols, "Period")
  join_key   <- cross_id
  id_protect <- cross_id

 # OPT: rag_base via data.table (faster unique + setorder)
  rag_base_dt <- unique(
    data.table::as.data.table(source_data)[, cross_id, with = FALSE])
  data.table::setorderv(rag_base_dt, "Period")
  rag_base <- as.data.frame(rag_base_dt)

 # OPT: ref_cross_key from rag_base (~2k rows) not source_data (576k)
  cross_data_rb <- rag_base[, cross_cols, drop = FALSE]
  cross_key_rb  <- do.call(paste, c(as.list(cross_data_rb), list(sep = " / ")))
  ref_cross_key <- sort(unique(cross_key_rb))[1]

  model_var <- cfg$model_variable %||% ""
  s_beg     <- c(as.Date(NA_character_))
  s_end     <- c(end_d)

  rag_joins <- list()
  act_rows  <- list()
  cost_rows <- list()

  has_geo_overrides <- length(segment_overrides) > 0 &&
    any(sapply(segment_overrides,
               \(o) length(o$geography_exclude %||% character(0)) > 0))

 # Pre-filter
  pb("Filtering source data...", 0.10)

  d_prefilt <- data.table::as.data.table(source_data)

  vi <- cfg$varname_include[nchar(cfg$varname_include %||% "") > 0]
  if (length(vi) > 0 && "VariableName" %in% names(d_prefilt)) {
    vi <- expand_varname_include_with_spend(
      unique(d_prefilt$VariableName),
      vi,
      cfg$spend_keyword %||% NULL
    )
    vi <- expand_analytical_keys_to_variable_names(
      unique(d_prefilt$VariableName),
      vi
    )
  }
  if (length(vi) > 0) {
    match_mode <- cfg$varname_match_mode %||%
      if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
    if (identical(match_mode, "exact")) {
      d_prefilt <- d_prefilt[
        tolower(trimws(VariableName)) %in% tolower(trimws(vi))
      ]
    } else {
      pattern   <- paste(paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")), collapse = "|")
      d_prefilt <- d_prefilt[grepl(pattern, VariableName, ignore.case = TRUE, perl = TRUE)]
    }
  }

  for (p in cfg$varname_exclude %||% character(0))
    if (nchar(p %||% "") > 0)
      d_prefilt <- d_prefilt[!grepl(p, VariableName, ignore.case = TRUE)]

  if (!has_geo_overrides && "Geography" %in% names(d_prefilt))
    for (p in cfg$geography_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d_prefilt <- d_prefilt[!grepl(p, Geography, ignore.case = TRUE)]

  if ("Campaign" %in% names(d_prefilt))
    for (p in cfg$campaign_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d_prefilt <- d_prefilt[!grepl(p, Campaign, ignore.case = TRUE)]

  if ("Outlet" %in% names(d_prefilt))
    for (p in cfg$outlet_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d_prefilt <- d_prefilt[!grepl(p, Outlet, ignore.case = TRUE)]

  if ("Creative" %in% names(d_prefilt))
    for (p in cfg$creative_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d_prefilt <- d_prefilt[!grepl(p, Creative, ignore.case = TRUE)]

  d_prefilt <- data.table::as.data.table(
    filter_to_analytical_varkey_combinations(
      as.data.frame(d_prefilt), cfg, schema_metadata
    )
  )

 # Segment loop
  pb("Building splits...", 0.20)

  d <- data.table::copy(d_prefilt)

  if (has_geo_overrides && "Geography" %in% names(d)) {
    seg_ovr <- Filter(\(o) isTRUE(o$seg == 1L), segment_overrides)
    geo_exc <- if (length(seg_ovr) > 0)
      seg_ovr[[1]]$geography_exclude %||% character(0)
    else
      cfg$geography_exclude %||% character(0)
    for (p in geo_exc)
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, Geography, ignore.case = TRUE)]
  }

  if (!is.na(s_beg[1])) d <- d[Period >= s_beg[1]]
  d <- d[Period <= s_end[1]]

  if (nrow(d) > 0) {
    d[, VariableValue := suppressWarnings(as.numeric(as.character(VariableValue)))]
    d[is.na(VariableValue), VariableValue := 0]

    d <- apply_dimension_breaks(d, dimension_breaks,
                                channel_name = cfg$channel_name)
    d <- apply_dimension_aliases(d, cfg$dimension_aliases %||% list())
    d <- data.table::as.data.table(d)

    # Keep VariableName in the technical split key so activity/spend detection
    # still works when the visible split order only uses broken dimensions.
    split_cols_technical <- unique(c("VariableName", cfg$split_columns %||% character(0)))
    d[, SplitName := build_split_name_from_columns(d, split_cols_technical)]

 # Pivot wide
    lhs    <- paste(cross_id, collapse = " + ")
    d_wide <- data.table::dcast(d,
                                as.formula(paste(lhs, "~ SplitName")),
                                value.var = "VariableValue",
                                fun.aggregate = sum, fill = 0)
    d_wide <- merge(rag_base_dt, d_wide, by = cross_id, all.x = TRUE)

 # OPT: setnafill instead of for-loop over columns
    num_cols_w <- names(d_wide)[sapply(d_wide, is.numeric)]
    if (length(num_cols_w) > 0)
      data.table::setnafill(d_wide, fill = 0, cols = num_cols_w)

    d_wide <- as.data.frame(d_wide)
    d_wide <- d_wide[order(d_wide$Period), ]

 # Non-focus suffix
    nf_sfx <- build_split_period_suffix(
      update_label,
      focus = FALSE,
      time_break_label = cfg$time_break_label %||% "",
      geo_label = cfg$geo_label %||% ""
    )

 # Non-focus slice
    nf_raw <- as.data.frame(d_wide[d_wide$Period < start_d, ])
    nf     <- keep_nonzero_cols(nf_raw)

    if (ncol(nf) > 1) {
      split_cols_nf <- setdiff(names(nf), id_protect)
      if (length(split_cols_nf) > 0)
        names(nf)[names(nf) %in% split_cols_nf] <-
          paste0(split_cols_nf, "_", nf_sfx)

      act_col_names <- grep(cfg$activity_keyword, names(nf),
                            ignore.case = TRUE, value = TRUE)
      if (length(act_col_names) > 0) {
        nf_act    <- nf[, c(id_protect, act_col_names), drop = FALSE]
        rag_joins <- c(rag_joins, list(list(df = nf_act, key = join_key)))
        act_rows  <- c(act_rows, list(
          splits_summary(get_diag_df(nf_act, cross_cols, ref_cross_key),
                         "activity") %>%
            mutate(period = "nonfocus", seg = 1L, model_var = model_var)))
      }

      cost_col_names <- grep(cfg$spend_keyword, names(nf),
                             ignore.case = TRUE, value = TRUE)
      if (length(cost_col_names) > 0) {
        nf_cost   <- nf[, c(id_protect, cost_col_names), drop = FALSE]
        rag_joins <- c(rag_joins, list(list(df = nf_cost, key = join_key)))
        cost_rows <- c(cost_rows, list(
          splits_summary(get_diag_df(nf_cost, cross_cols, ref_cross_key),
                         "spend") %>%
            mutate(period = "nonfocus", seg = 1L, model_var = model_var)))
      }
    }

 # Focus slice
    fc_raw <- as.data.frame(
      d_wide[d_wide$Period >= start_d & d_wide$Period <= end_d, ])
    fc <- keep_nonzero_cols(fc_raw)

    if (ncol(fc) > 1) {
      split_cols_fc <- setdiff(names(fc), id_protect)
      if (length(split_cols_fc) > 0)
        names(fc)[names(fc) %in% split_cols_fc] <-
          paste0(split_cols_fc, "_", build_split_period_suffix(
            update_label,
            focus = TRUE,
            geo_label = cfg$geo_label %||% ""
          ))

      act_col_fc <- grep(cfg$activity_keyword, names(fc),
                         ignore.case = TRUE, value = TRUE)
      if (length(act_col_fc) > 0) {
        fc_act    <- fc[, c(id_protect, act_col_fc), drop = FALSE]
        rag_joins <- c(rag_joins, list(list(df = fc_act, key = join_key)))
        act_rows  <- c(act_rows, list(
          splits_summary(get_diag_df(fc_act, cross_cols, ref_cross_key),
                         "activity") %>%
            mutate(period = "focus", seg = 1L, model_var = model_var)))
      }

      cost_col_fc <- grep(cfg$spend_keyword, names(fc),
                          ignore.case = TRUE, value = TRUE)
      if (length(cost_col_fc) > 0) {
        fc_cost   <- fc[, c(id_protect, cost_col_fc), drop = FALSE]
        rag_joins <- c(rag_joins, list(list(df = fc_cost, key = join_key)))
        cost_rows <- c(cost_rows, list(
          splits_summary(get_diag_df(fc_cost, cross_cols, ref_cross_key),
                         "spend") %>%
            mutate(period = "focus", seg = 1L, model_var = model_var)))
      }
    }

    rm(d, d_wide, nf_raw, nf, fc_raw, fc)
  }

 # OPT: Assemble RAG setnafill OUTSIDE the merge loop
  # Was: for-loop over columns after EACH merge (grows with every iteration)
  # Now: single setnafill pass at the end over the final table
  pb("Assembling RAG...", 0.82)

  rag_dt <- rag_base_dt
  for (j in rag_joins)
    rag_dt <- merge(rag_dt, data.table::as.data.table(j$df),
                    by = j$key, all.x = TRUE)

  num_cols_r <- names(rag_dt)[sapply(rag_dt, is.numeric)]
  if (length(num_cols_r) > 0)
    data.table::setnafill(rag_dt, fill = 0, cols = num_cols_r)

  rag <- as.data.frame(rag_dt)

  pb("Computing diagnostics...", 0.92)

  act_all <- if (length(act_rows) > 0) bind_rows(act_rows) else tibble()
  if (!"VariableSplit" %in% names(act_all))
    act_all <- tibble(
      VariableSplit = character(), total_activity = numeric(),
      pct_total_activity = numeric(), max_index = numeric(),
      max = numeric(), max_no_outlier = numeric(),
      num_weeks_activity = integer(), min_consecutive_weeks = numeric(),
      sd = numeric(), min = numeric(), quartile_1 = numeric(),
      median = numeric(), quartile_3 = numeric(),
      period = character(), seg = integer(), model_var = character())

  cost_all <- if (length(cost_rows) > 0) bind_rows(cost_rows) else tibble()
  if (!"VariableSplit" %in% names(cost_all))
    cost_all <- tibble(
      VariableSplit = character(), total_spend = numeric(),
      pct_total_spend = numeric(), max_index = numeric(),
      max = numeric(), max_no_outlier = numeric(),
      num_weeks_spend = integer(), min_consecutive_weeks = numeric(),
      sd = numeric(), min = numeric(), quartile_1 = numeric(),
      median = numeric(), quartile_3 = numeric(),
      period = character(), seg = integer(), model_var = character())

  if (nrow(act_all) > 0) {
    act_all <- act_all %>%
      group_by(period) %>%
      mutate(grand_p = sum(total_activity, na.rm = TRUE),
             pct_total_activity = round(
               total_activity / pmax(grand_p, 1) * 100, 4)) %>%
      ungroup() %>% select(-grand_p)
  }

  if (nrow(cost_all) > 0) {
    cost_all <- cost_all %>%
      group_by(period) %>%
      mutate(grand_p = sum(total_spend, na.rm = TRUE),
             pct_total_spend = round(
               total_spend / pmax(grand_p, 1) * 100, 4)) %>%
      ungroup() %>% select(-grand_p)
  }

  pb("Done.", 1.0)
  model_metric <- normalize_model_metric(cfg$model_metric %||% "activity")
  model_diagnoses <- if (identical(model_metric, "spend")) cost_all else act_all

  list(
    rag            = rag,
    cross_cols     = cross_cols,
    ref_cross      = ref_cross_key,
    activity_spend = build_activity_spend(act_all, cost_all, cfg),
    side_mapping   = build_side_mapping(model_diagnoses),
    act_diagnoses  = act_all,
    cost_diagnoses = cost_all,
    model_diagnoses = model_diagnoses,
    model_metric = model_metric
  )
}

# get_useful_long_values
# Extracts the specific values of a useful_long dimension (e.g. Product="Prod1")
# that belong to a channel, derived from its analytical_varkeys via name_lookup.
# Used by process_channel to filter main data to channel-specific rows only,
# preventing channels from capturing data from other products/dimensions.
get_useful_long_values <- function(analytical_varkeys, name_lookup, dim) {
  if (is.null(name_lookup)          ||
      !dim %in% names(name_lookup)  ||
      length(analytical_varkeys) == 0)
    return(character(0))

  rows <- name_lookup[
    name_lookup$OriginalName %in% analytical_varkeys &
      !is.na(name_lookup[[dim]])                     &
      trimws(name_lookup[[dim]]) != "Total"          &
      nzchar(trimws(name_lookup[[dim]])),
    , drop = FALSE]

  unique(trimws(rows[[dim]]))
}

filter_to_analytical_varkey_combinations <- function(d, cfg, schema_metadata) {
  if (is.null(d) || nrow(d) == 0 ||
      is.null(cfg) ||
      length(cfg$analytical_varkeys %||% character(0)) == 0 ||
      is.null(schema_metadata) ||
      is.null(schema_metadata$name_lookup) ||
      nrow(schema_metadata$name_lookup) == 0) {
    return(d)
  }

  lookup_all <- schema_metadata$name_lookup
  candidate_dims <- setdiff(intersect(names(lookup_all), names(d)),
                            c("OriginalName", "VariableName"))
  useful_dims <- unique(c(schema_metadata$useful_long %||% character(0),
                         candidate_dims))
  useful_dims <- useful_dims[vapply(useful_dims, function(dim) {
    if (!dim %in% names(lookup_all) || !dim %in% names(d)) return(FALSE)
    vals <- trimws(as.character(lookup_all[[dim]]))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    any(tolower(vals) != "total")
  }, logical(1))]

  # Longitudinal filters should respect the VOF combinations without requiring
  # the metric word itself to match. For example, "Display Impressions" and
  # "Display Spend" share the same metric base, but "Display Spend_Collection"
  # should not be kept unless that Display/Product combination exists in VOF.
  key_cols <- intersect(useful_dims, names(d))
  key_cols <- intersect(key_cols, names(lookup_all))
  if (!length(key_cols)) return(d)

  use_metric_base <- "VariableName" %in% names(lookup_all) &&
    "VariableName" %in% names(d)

  lookup <- lookup_all[
    lookup_all$OriginalName %in% (cfg$analytical_varkeys %||% character(0)),
    unique(c(if (use_metric_base) "VariableName" else character(0), key_cols)),
    drop = FALSE
  ]
  if (!nrow(lookup)) return(d)

  normalize_key <- function(df) {
    parts <- list()
    if (use_metric_base) {
      parts <- c(parts, list(metric_base_name(df$VariableName)))
    }
    parts <- c(parts, lapply(key_cols, function(col) {
      tolower(trimws(as.character(df[[col]] %||% "")))
    }))
    do.call(
      paste,
      c(parts, list(sep = "\r"))
    )
  }

  allowed <- unique(normalize_key(lookup))
  keep <- normalize_key(as.data.frame(d)) %in% allowed
  d[keep, , drop = FALSE]
}
