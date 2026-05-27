# ═══════════════════════════════════════════════════════════════════════
# R/functions.R
# ═══════════════════════════════════════════════════════════════════════

# ── Ordinal tag ────────────────────────────────────────────────────────────
ordinal_tag <- function(i) {
  c("First", "Second", "Third", "Fourth", "Fifth")[min(i, 5L)]
}

# ══════════════════════════════════════════════════════════════════════════════
# DATE PARSERS
# ══════════════════════════════════════════════════════════════════════════════

# ── parse_period_robust ────────────────────────────────────────────────────
# Parses period strings from all_rags (YYYYMMDD, MM/DD/YYYY, DD.MM.YYYY, etc.)
parse_period_robust <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^(\\d{4})(\\d{2})(\\d{2})$",        "\\1-\\2-\\3", x)
  x <- gsub("^(\\d{1,2})/(\\d{1,2})/(\\d{4})$",   "\\3-\\1-\\2", x)
  x <- gsub("^(\\d{1,2})/(\\d{1,2})/(\\d{2})$",   "20\\3-\\1-\\2", x)
  x <- gsub("^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})$","\\3-\\2-\\1", x)
  suppressWarnings(as.Date(x))
}

# ── parse_vof_period ───────────────────────────────────────────────────────
# ── Robust VOF period parser ─────────────────────────────────────────────
# Returns a Date vector always — never integers or numerics.
# Handles: YYYY-MM-DD · M/D/YYYY · MM/DD/YYYY · M/D/YY · Excel serials
parse_vof_period <- function(x) {
  if (is.null(x) || length(x) == 0L) return(as.Date(character(0L)))
  
  FMTS <- c(
    "%Y-%m-%d",   # 2023-04-24
    "%m/%d/%Y",   # 4/24/2023  ← VOF format
    "%m/%d/%y",   # 4/24/23
    "%Y/%m/%d",   # 2023/04/24
    "%d-%b-%Y",   # 24-Apr-2023
    "%d/%m/%Y"    # 24/04/2023
  )
  
  out <- rep(as.Date(NA_character_), length(x))
  
  for (i in seq_along(x)) {
    v <- x[[i]]
    if (is.na(v)) next
    s <- trimws(as.character(v))
    if (!nzchar(s)) next
    
    for (fmt in FMTS) {
      d <- tryCatch(as.Date(s, format = fmt), error = \(e) as.Date(NA))
      if (!is.na(d)) { out[[i]] <- d; break }
    }
    
    # Excel serial number fallback (e.g. 44675)
    if (is.na(out[[i]])) {
      n <- suppressWarnings(as.numeric(s))
      if (!is.na(n) && n > 20000 && n < 70000)
        out[[i]] <- tryCatch(
          as.Date(n, origin = "1899-12-30"),
          error = \(e) as.Date(NA))
    }
  }
  
  out  
}

# ── parse_period (alias used by infer_schema) ──────────────────────────────
parse_period <- parse_period_robust

# ══════════════════════════════════════════════════════════════════════════════
# SCHEMA INFERENCE HELPERS
# ══════════════════════════════════════════════════════════════════════════════

# ── clean_names ────────────────────────────────────────────────────────────
clean_names <- function(x) trimws(x)

# ── is_weekly_like ─────────────────────────────────────────────────────────
is_weekly_like <- function(dates) {
  dates <- sort(dates[!is.na(dates)])
  if (length(dates) < 2) return(FALSE)
  median_diff <- median(as.numeric(diff(dates)), na.rm = TRUE)
  median_diff >= 6 && median_diff <= 8
}

# ══════════════════════════════════════════════════════════════════════════════
# SCHEMA INFERENCE
# ══════════════════════════════════════════════════════════════════════════════

# ── infer_schema ───────────────────────────────────────────────────────────
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

# ── build_schema_metadata ──────────────────────────────────────────────────
# Builds full schema metadata from the Analytical dataset.
# Returns:
#   $xs_dims        — cross-sectional dimension columns
#   $useful_long    — longitudinal dims with values other than "Total"
#   $discarded_long — longitudinal dims where all values are "Total"
#   $name_lookup    — data.frame: OriginalName, VariableName, <one col per long dim>
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
  
  # C. Parse variable names — format: BaseName_D1val_D2val_..._Dnval
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

# ══════════════════════════════════════════════════════════════════════════════
# CROSS-SECTION DETECTION
# ══════════════════════════════════════════════════════════════════════════════

# ── auto_detect_cross_cols ─────────────────────────────────────────────────
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

# ══════════════════════════════════════════════════════════════════════════════
# FILE READER
# ══════════════════════════════════════════════════════════════════════════════

# ── read_main_data ─────────────────────────────────────────────────────────
# Reads the main data file. Accepts .csv and .zip only (as per UI restrictions).
read_main_data <- function(path, ext) {
  raw <- if (tolower(ext) == "zip") {
    tmp <- file.path(tempdir(), paste0("unzip_", as.integer(Sys.time())))
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    unzip(path, exdir = tmp)
    csv_files <- list.files(tmp, pattern = "\\.csv$",
                            full.names = TRUE, recursive = TRUE)
    if (!length(csv_files)) stop("No CSV file found inside the ZIP.")
    data.table::fread(csv_files[1], data.table = FALSE,
                      colClasses = "character", showProgress = FALSE)
  } else {
    # Default: CSV
    data.table::fread(file = path, sep = "auto", encoding = "UTF-8",
                      data.table = FALSE, colClasses = "character",
                      showProgress = FALSE)
  }
  
  if ("rawPeriod" %in% names(raw)) {
    names(raw) <- sub("^raw", "", names(raw))
  }
  
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  raw <- raw[, intersect(REQUIRED_COLS, names(raw)), drop = FALSE]
  
  raw %>%
    dplyr::mutate(
      Period        = parse_period_robust(Period),
      VariableValue = as.numeric(gsub(",", "", gsub(" ", "",
                                                    as.character(VariableValue))))
    ) %>%
    dplyr::arrange(Geography, VariableName, Period)
}

# ══════════════════════════════════════════════════════════════════════════════
# CONFIG CSV
# ══════════════════════════════════════════════════════════════════════════════

# ── export_channels_csv ────────────────────────────────────────────────────
export_channels_csv <- function(channels) {
  if (!length(channels)) return(data.frame())
  rows <- list()
  for (nm in names(channels)) {
    cfg <- channels[[nm]]
    rows <- c(rows, list(tibble::tibble(
      Channel    = nm,
      Type       = "Config",
      SplitOrder = paste(cfg$split_columns %||% character(0), collapse = "|"),
      Name       = "",
      Splits     = ""
    )))
    for (b in cfg$dimension_breaks %||% list()) {
      rows <- c(rows, list(tibble::tibble(
        Channel    = nm,
        Type       = "Break",
        SplitOrder = b$column,
        Name       = paste(b$names, collapse = "|"),
        Splits     = paste(c(b$separator, b$n_parts), collapse = "|")
      )))
    }
    for (m in cfg$saved_merges %||% list()) {
      if (!isTRUE(m$active)) next
      rows <- c(rows, list(tibble::tibble(
        Channel    = nm,
        Type       = "Merge",
        SplitOrder = "",
        Name       = m$new_name,
        Splits     = paste(unlist(m$merged), collapse = "|")
      )))
    }
  }
  if (!length(rows)) return(data.frame())
  dplyr::bind_rows(rows)
}

# ══════════════════════════════════════════════════════════════════════════════
# KEYWORD DETECTORS
# ══════════════════════════════════════════════════════════════════════════════

# ── detect_activity_keyword ────────────────────────────────────────────────
detect_activity_keyword <- function(var_names,
                                    keyword_dict = MEDIA_KEYWORD_DICT) {
  for (kw in keyword_dict$activity)
    if (any(stringr::str_detect(var_names, stringr::regex(kw, ignore_case = TRUE))))
      return(kw)
  "Impressions"
}

# ── detect_spend_keyword ───────────────────────────────────────────────────
detect_spend_keyword <- function(main_data, varname_include,
                                 keyword_dict = MEDIA_KEYWORD_DICT) {
  if (is.null(main_data) || !"VariableName" %in% names(main_data))
    return("Spend")
  matching <- if (length(varname_include) > 0) {
    unique(main_data$VariableName[
      Reduce("|", lapply(varname_include, function(p)
        grepl(p, main_data$VariableName, ignore.case = TRUE)))
    ])
  } else character(0)
  for (kw in keyword_dict$spend)
    if (any(stringr::str_detect(matching, stringr::regex(kw, ignore_case = TRUE))))
      return(kw)
  "Spend"
}

# ══════════════════════════════════════════════════════════════════════════════
# VAR KEY BUILDER (kept for summary/coverage info)
# ══════════════════════════════════════════════════════════════════════════════

# ── build_var_key ──────────────────────────────────────────────────────────
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

# ══════════════════════════════════════════════════════════════════════════════
# BUILD MEDIA INDEX
# ══════════════════════════════════════════════════════════════════════════════

build_media_index <- function(main_data, analytical, vof_df, model_details,
                              channels_rois = NULL, cross_cols = "Geography",
                              keyword_dict = MEDIA_KEYWORD_DICT,
                              schema_metadata = NULL) {
  
  channels <- list()
  geo_col  <- cross_cols[1]
  
  an_geos <- if (!is.null(analytical) && geo_col %in% names(analytical))
    unique(as.character(analytical[[geo_col]])) else character(0)
  
  an_min_date <- if (!is.null(analytical) && "Period" %in% names(analytical))
    min(analytical$Period, na.rm = TRUE) else as.Date(NA_character_)
  an_max_date <- if (!is.null(analytical) && "Period" %in% names(analytical))
    max(analytical$Period, na.rm = TRUE) else as.Date(NA_character_)
  
  # ── Detect VOF geo column (Geography or Geographies)
  detect_geo_col <- function(df) {
    if ("Geography"   %in% names(df)) return("Geography")
    if ("Geographies" %in% names(df)) return("Geographies")
    return(NULL)
  }
  
  # ── Step 1: IN/FIXED variables
  in_model_vars <- if (!is.null(model_details) &&
                       all(c("Type", "VariableName") %in% names(model_details))) {
    model_details %>%
      dplyr::filter(
        stringr::str_detect(stringr::str_to_lower(trimws(Type)), "\\b(in|fixed)\\b"),
        !stringr::str_detect(stringr::str_to_lower(trimws(Type)), "none")
      ) %>%
      dplyr::pull(VariableName) %>%
      unique()
  } else unique(vof_df$MainModelVariableName)
  
  # ── Step 2: validate + filter VOF
  req_vof  <- c("AnalyticalVariableName", "MainModelVariableName",
                "MinPeriod", "MaxPeriod")
  miss_vof <- setdiff(req_vof, names(vof_df))
  if (length(miss_vof))
    stop("VOF missing columns: ", paste(miss_vof, collapse = ", "))
  
  has_geo <- any(c("Geography", "Geographies") %in% names(vof_df))
  if (!has_geo) stop("VOF missing Geography (or Geographies) column.")
  
  vof_filtered <- vof_df %>%
    dplyr::filter(MainModelVariableName %in% in_model_vars)
  if (!nrow(vof_filtered))
    stop("No VOF rows matched ModelDetails Type='IN'/'FIXED'.")
  
  # ── Step 3: coverage info
  vk_info <- build_var_key(main_data,
                           unique(vof_filtered$AnalyticalVariableName))
  
  # ── Step 4: ROI lookup
  roi_lookup <- NULL
  if (!is.null(channels_rois) &&
      "MainModelVariableName" %in% names(channels_rois)) {
    roi_num <- setdiff(names(channels_rois)[sapply(channels_rois, is.numeric)],
                       "MainModelVariableName")
    if (length(roi_num))
      roi_lookup <- channels_rois %>%
        dplyr::select(MainModelVariableName, dplyr::all_of(roi_num)) %>%
        dplyr::distinct(MainModelVariableName, .keep_all = TRUE)
  }
  
  # ════════════════════════════════════════════════════════════
  # PRE-COMPUTATIONS — run ONCE before the loop
  # ════════════════════════════════════════════════════════════
  
  # OPT-2: Parse ALL VOF dates FIRST — must happen before split so that
  # .min_parsed / .max_parsed are present in every element of vof_by_mv
  vof_min_dates <- tryCatch(
    as.Date(parse_vof_period(vof_filtered$MinPeriod), origin = "1970-01-01"),
    error = \(e) rep(as.Date(NA), nrow(vof_filtered))
  )
  vof_max_dates <- tryCatch(
    as.Date(parse_vof_period(vof_filtered$MaxPeriod), origin = "1970-01-01"),
    error = \(e) rep(as.Date(NA), nrow(vof_filtered))
  )
  vof_filtered$.min_parsed <- vof_min_dates
  vof_filtered$.max_parsed <- vof_max_dates
  
  # OPT-1: Split AFTER adding parsed date columns → O(1) lookup per variable
  vof_by_mv <- split(vof_filtered, vof_filtered$MainModelVariableName)
  
  # OPT-3: Pre-compute unique VariableNames from main_data ONCE
  all_main_vn <- if (!is.null(main_data) && "VariableName" %in% names(main_data))
    unique(as.character(main_data$VariableName)) else character(0)
  
  # OPT-4: Pre-index schema name_lookup as named list → O(1) lookup
  schema_lookup_by_orig <- NULL
  if (!is.null(schema_metadata) &&
      !is.null(schema_metadata$name_lookup) &&
      nrow(schema_metadata$name_lookup) > 0) {
    nl <- schema_metadata$name_lookup
    schema_lookup_by_orig <- split(nl, nl$OriginalName)
  }
  
  # OPT-5: Fast spend keyword detection using pre-computed unique VNs
  detect_spend_kw_fast <- function(varname_include) {
    if (!length(varname_include) || !length(all_main_vn)) return("Spend")
    patterns <- paste0("^", varname_include)
    combined <- paste(patterns, collapse = "|")
    matching <- all_main_vn[grepl(combined, all_main_vn,
                                  ignore.case = TRUE, perl = TRUE)]
    for (kw in keyword_dict$spend)
      if (any(grepl(kw, matching, ignore.case = TRUE))) return(kw)
    "Spend"
  }
  
  # OPT-6: Fast schema derive using pre-indexed lookup
  derive_ch_config_fast <- function(anal_var_names) {
    if (!is.null(schema_lookup_by_orig)) {
      rows <- dplyr::bind_rows(
        schema_lookup_by_orig[intersect(anal_var_names,
                                        names(schema_lookup_by_orig))])
      if (nrow(rows) > 0) {
        base_names <- unique(rows$VariableName[
          !is.na(rows$VariableName) & nzchar(rows$VariableName)])
        if (length(base_names) > 0)
          return(list(
            varname_include = base_names,
            split_columns   = c("VariableName", schema_metadata$useful_long)
          ))
      }
    }
    vi <- unique(stringr::str_remove(anal_var_names, "_Total(_Total)*$"))
    list(varname_include = vi[nzchar(vi)], split_columns = c("VariableName"))
  }
  
  # OPT-7: Pre-index ROI lookup as named list
  roi_by_mv <- if (!is.null(roi_lookup))
    split(roi_lookup, roi_lookup$MainModelVariableName) else list()
  
  # ════════════════════════════════════════════════════════════
  # MAIN LOOP
  # ════════════════════════════════════════════════════════════
  
  for (mv in unique(vof_filtered$MainModelVariableName)) {
    
    vof_rows <- vof_by_mv[[mv]]
    if (is.null(vof_rows) || !nrow(vof_rows)) next
    
    anal_var_names <- unique(vof_rows$AnalyticalVariableName)
    
    ch_cfg <- derive_ch_config_fast(anal_var_names)
    
    act_kw <- if ("Metric" %in% names(vof_rows)) {
      metrics <- unique(vof_rows$Metric[
        !is.na(vof_rows$Metric) & nzchar(vof_rows$Metric)])
      if (length(metrics) > 0) metrics[1]
      else detect_activity_keyword(
        stringr::str_remove(anal_var_names, "_Total_Total_Total$"),
        keyword_dict)
    } else {
      detect_activity_keyword(
        stringr::str_remove(anal_var_names, "_Total_Total_Total$"),
        keyword_dict)
    }
    
    vi_broad <- unique(trimws(stringr::str_remove(
      ch_cfg$varname_include,
      stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)
    )))
    vi_broad        <- vi_broad[nzchar(vi_broad) &
                                  !(vi_broad %in% ch_cfg$varname_include)]
    varname_include <- unique(c(ch_cfg$varname_include, vi_broad))
    varname_include <- varname_include[nzchar(varname_include)]
    
    spend_kw <- detect_spend_kw_fast(varname_include)
    
    # Dates — read from pre-parsed columns (now always Date class)
    min_p <- tryCatch({
      d <- vof_rows$.min_parsed
      d <- d[!is.na(d)]
      if (length(d)) min(d) else NA
    }, error = \(e) NA)
    max_p <- tryCatch({
      d <- vof_rows$.max_parsed
      d <- d[!is.na(d)]
      if (length(d)) max(d) else NA
    }, error = \(e) NA)
    
    if (is.na(min_p) || !is.finite(as.numeric(min_p))) min_p <- an_min_date
    if (is.na(max_p) || !is.finite(as.numeric(max_p))) max_p <- an_max_date
    
    # Geography exclusions
    geo_col_vof       <- detect_geo_col(vof_rows)
    segment_overrides <- if (!is.null(geo_col_vof) && length(an_geos) > 0) {
      geo_strs <- vof_rows[[geo_col_vof]]
      geo_strs <- geo_strs[!is.na(geo_strs) & nzchar(trimws(geo_strs))]
      if (length(geo_strs) > 0 &&
          !any(toupper(geo_strs) %in% c("ALL", "TOTAL", "NATIONAL"))) {
        included <- unique(trimws(
          unlist(strsplit(paste(geo_strs, collapse = ","), ","))))
        included <- included[nzchar(included)]
        geo_exc  <- setdiff(an_geos, included)
        if (length(geo_exc))
          list(list(seg = 1L, geography_exclude = geo_exc))
        else list()
      } else list()
    } else list()
    
    # VOF metadata fields
    media_channel <- if ("MediaChannel" %in% names(vof_rows)) {
      mc <- unique(vof_rows$MediaChannel[
        !is.na(vof_rows$MediaChannel) & nzchar(vof_rows$MediaChannel)])
      if (length(mc)) mc[1] else ""
    } else ""
    
    sub_channel <- if ("SubChannel" %in% names(vof_rows)) {
      sc <- unique(vof_rows$SubChannel[
        !is.na(vof_rows$SubChannel) & nzchar(vof_rows$SubChannel)])
      if (length(sc)) sc[1] else ""
    } else ""
    
    effect <- if ("Effect" %in% names(vof_rows)) {
      ef <- unique(vof_rows$Effect[
        !is.na(vof_rows$Effect) & nzchar(vof_rows$Effect)])
      if (length(ef)) ef[1] else ""
    } else ""
    
    # ROI — O(1) lookup
    roi_val <- NA_real_
    ri      <- roi_by_mv[[mv]]
    if (!is.null(ri) && nrow(ri) > 0) {
      r_num <- setdiff(names(ri), "MainModelVariableName")
      if (length(r_num)) roi_val <- mean(ri[[r_num[1]]], na.rm = TRUE)
    }
    
    channels[[mv]] <- list(
      channel_name      = mv,
      model_variable    = mv,
      varname_include   = varname_include,
      analytical_varkeys = anal_var_names,
      min_period        = min_p,
      max_period        = max_p,
      segment_overrides = segment_overrides,
      activity_keyword  = act_kw,
      spend_keyword     = spend_kw,
      split_columns     = ch_cfg$split_columns,
      saved_merges      = list(),
      dimension_breaks  = list(),
      roi               = roi_val,
      source            = "vof",
      media_channel     = media_channel,
      sub_channel       = sub_channel,
      effect            = effect
    )
  }
  
  # ── Step 6: keyword fallback channels
  if (!is.null(analytical)) {
    cross_id_cols <- c(cross_cols, "Period", "BP_Year")
    model_cols    <- setdiff(names(analytical)[sapply(analytical, is.numeric)],
                             cross_id_cols)
    non_vof_cols  <- setdiff(model_cols, names(channels))
    
    for (col in non_vof_cols) {
      kw_match <- Filter(
        function(kw) stringr::str_detect(
          col, stringr::regex(kw, ignore_case = TRUE)),
        keyword_dict$activity)
      if (!length(kw_match)) next
      
      act_kw <- kw_match[1]
      ch_cfg <- derive_ch_config_fast(col)
      
      vi_broad <- trimws(stringr::str_remove(
        ch_cfg$varname_include,
        stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)
      ))
      varname_include <- unique(c(
        ch_cfg$varname_include,
        vi_broad[nzchar(vi_broad) & vi_broad != ch_cfg$varname_include]
      ))
      varname_include <- varname_include[nzchar(varname_include)]
      
      spend_kw <- detect_spend_kw_fast(varname_include)
      
      roi_val <- NA_real_
      ri      <- roi_by_mv[[col]]
      if (!is.null(ri) && nrow(ri) > 0) {
        r_num <- setdiff(names(ri), "MainModelVariableName")
        if (length(r_num)) roi_val <- ri[[r_num[1]]][1]
      }
      
      channels[[col]] <- list(
        channel_name      = col,
        model_variable    = col,
        varname_include   = varname_include,
        analytical_varkeys = col,
        min_period        = an_min_date,
        max_period        = an_max_date,
        segment_overrides = list(),
        activity_keyword  = act_kw,
        spend_keyword     = spend_kw,
        split_columns     = ch_cfg$split_columns,
        saved_merges      = list(),
        dimension_breaks  = list(),
        roi               = roi_val,
        source            = "keyword_fallback",
        media_channel     = "",
        sub_channel       = "",
        effect            = ""
      )
    }
  }
  
  # ── Step 7: time_break_labels
  vof_sigs <- vapply(names(channels), function(nm) {
    ch <- channels[[nm]]
    if (!identical(ch$source, "vof")) return(NA_character_)
    paste(sort(unique(ch$analytical_varkeys)), collapse = "||")
  }, character(1))
  
  vof_sigs <- vof_sigs[!is.na(vof_sigs)]
  dup_sigs <- names(which(table(vof_sigs) > 1))
  
  for (sig in dup_sigs) {
    group_nms <- names(vof_sigs[vof_sigs == sig])
    min_dates <- sapply(group_nms, function(nm) {
      mp <- channels[[nm]]$min_period
      if (!is.null(mp) && !is.na(mp)) as.numeric(as.Date(mp)) else 0
    })
    sorted_nms <- group_nms[order(min_dates)]
    for (i in seq_along(sorted_nms))
      channels[[sorted_nms[i]]]$time_break_label <-
      paste0(ordinal_tag(i), "TimeBreak")
  }
  
  n_vof      <- sum(sapply(channels, \(c) identical(c$source, "vof")))
  n_fallback <- sum(sapply(channels, \(c) identical(c$source, "keyword_fallback")))
  n_with_roi <- sum(sapply(channels, \(c) !is.na(c$roi %||% NA_real_)))
  
  list(
    channels        = channels,
    var_key_info    = vk_info,
    schema_metadata = schema_metadata,
    summary = list(
      total_channels = length(channels),
      from_vof       = n_vof,
      from_fallback  = n_fallback,
      with_roi       = n_with_roi,
      var_key_type   = vk_info$type,
      vof_coverage   = round(vk_info$coverage * 100, 1),
      xs_dims        = if (!is.null(schema_metadata))
        schema_metadata$xs_dims else character(0),
      useful_long    = if (!is.null(schema_metadata))
        schema_metadata$useful_long else character(0),
      discarded_long = if (!is.null(schema_metadata))
        schema_metadata$discarded_long else character(0)
    )
  )
}

# ── Timeline HTML builder — pure function, no reactive dependencies ──────
# Used by mod_setup output$file_comparison for Time Scope warnings.
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



