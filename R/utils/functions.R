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
  
  raw_vals <- trimws(as.character(x))
  slash_vals <- raw_vals[grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", raw_vals)]
  slash_order <- "mdy"
  if (length(slash_vals)) {
    parts <- strsplit(slash_vals, "/", fixed = TRUE)
    first <- suppressWarnings(as.integer(vapply(parts, `[`, character(1), 1L)))
    second <- suppressWarnings(as.integer(vapply(parts, `[`, character(1), 2L)))
    dmy_votes <- sum(first > 12, na.rm = TRUE)
    mdy_votes <- sum(second > 12, na.rm = TRUE)
    if (dmy_votes > mdy_votes) slash_order <- "dmy"
  }

  slash_fmts <- if (identical(slash_order, "dmy")) {
    c("%d/%m/%Y", "%d/%m/%y", "%m/%d/%Y", "%m/%d/%y")
  } else {
    c("%m/%d/%Y", "%m/%d/%y", "%d/%m/%Y", "%d/%m/%y")
  }

  out <- rep(as.Date(NA_character_), length(x))
  
  for (i in seq_along(x)) {
    v <- x[[i]]
    if (is.na(v)) next
    s <- trimws(as.character(v))
    if (!nzchar(s)) next
    
    fmts <- if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", s)) {
      "%Y-%m-%d"
    } else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", s)) {
      "%Y/%m/%d"
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", s)) {
      slash_fmts
    } else if (grepl("^\\d{1,2}-[[:alpha:]]{3}-\\d{2,4}$", s)) {
      c("%d-%b-%Y", "%d-%b-%y")
    } else {
      c("%Y-%m-%d", "%Y/%m/%d", "%d-%b-%Y", "%d-%b-%y", slash_fmts)
    }

    for (fmt in fmts) {
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
# ── read_main_data ─────────────────────────────────────────────────────────
# Accepts:
#   .zip  → contains exactly one .txt file (always this structure)
#   .csv  → direct CSV upload (legacy / small files)
read_main_data <- function(path, ext) {
  
  raw <- if (tolower(ext) == "zip") {
    
    # ── Extract ZIP ────────────────────────────────────────────────────
    tmp <- file.path(tempdir(), paste0("unzip_", as.integer(Sys.time())))
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    unzip(path, exdir = tmp)
    
    # ── Find the TXT file — always exactly one ─────────────────────────
    txt_files <- list.files(tmp, pattern = "\\.txt$",
                            full.names = TRUE, recursive = TRUE)
    
    if (!length(txt_files))
      stop("No TXT file found inside the ZIP.")
    
    # Read — fread auto-detects delimiter (tab, comma, pipe, etc.)
    data.table::fread(txt_files[1],
                      data.table    = FALSE,
                      colClasses    = "character",
                      encoding      = "UTF-8",
                      showProgress  = FALSE)
    
  } else {
    # ── Direct CSV upload ──────────────────────────────────────────────
    data.table::fread(file         = path,
                      sep          = "auto",
                      encoding     = "UTF-8",
                      data.table   = FALSE,
                      colClasses   = "character",
                      showProgress = FALSE)
  }
  
  # ── rawPeriod alias ────────────────────────────────────────────────────
  if ("rawPeriod" %in% names(raw))
    names(raw) <- sub("^raw", "", names(raw))
  
  # ── Validate required columns ──────────────────────────────────────────
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss))
    stop("Missing columns: ", paste(miss, collapse = ", "))
  
  raw <- raw[, intersect(REQUIRED_COLS, names(raw)), drop = FALSE]
  
  # ── Parse and sort ─────────────────────────────────────────────────────
  raw %>%
    dplyr::mutate(
      Period        = parse_period_robust(Period),
      VariableValue = as.numeric(gsub(",", "",
                                      gsub(" ", "",
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
  normalize_model_var <- function(x) {
    out <- trimws(as.character(x))
    out <- stringr::str_replace_all(out, "\\s+", " ")
    out <- stringr::str_remove(out, stringr::regex("(_Total)+$", ignore_case = TRUE))
    stringr::str_to_lower(out)
  }
  
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
  
  vof_df <- vof_df %>%
    dplyr::mutate(
      MainModelVariableName = trimws(as.character(MainModelVariableName)),
      AnalyticalVariableName = trimws(as.character(AnalyticalVariableName)),
      .mv_norm = normalize_model_var(MainModelVariableName)
    )
  in_model_norm <- normalize_model_var(in_model_vars)
  analytical_model_cols <- if (!is.null(analytical)) {
    cross_id_cols <- c(cross_cols, "Period", "BP_Year")
    setdiff(names(analytical)[sapply(analytical, is.numeric)], cross_id_cols)
  } else character(0)
  analytical_model_norm <- normalize_model_var(analytical_model_cols)
  vof_filtered <- vof_df %>%
    dplyr::filter(
      .mv_norm %in% in_model_norm |
        AnalyticalVariableName %in% analytical_model_cols |
        normalize_model_var(AnalyticalVariableName) %in% analytical_model_norm
    )
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

  vof_metric_role <- function(metric) {
    m <- stringr::str_to_lower(trimws(as.character(metric)))
    dplyr::case_when(
      stringr::str_detect(m, "spend|cost|investment|budget") ~ "spend",
      stringr::str_detect(m, paste(
        c("activity", keyword_dict$activity),
        collapse = "|"
      )) ~ "activity",
      TRUE ~ NA_character_
    )
  }

  keyword_from_metric <- function(metric, role = c("activity", "spend")) {
    role <- match.arg(role)
    dict <- if (identical(role, "activity")) keyword_dict$activity else keyword_dict$spend
    metric <- trimws(as.character(metric))
    matched <- dict[vapply(dict, function(kw) {
      any(stringr::str_detect(metric, stringr::regex(kw, ignore_case = TRUE)))
    }, logical(1))]
    if (length(matched)) matched[1] else NA_character_
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
    metric_roles <- if ("Metric" %in% names(vof_rows))
      vof_metric_role(vof_rows$Metric) else rep(NA_character_, nrow(vof_rows))
    activity_vars <- unique(vof_rows$AnalyticalVariableName[
      metric_roles == "activity" | is.na(metric_roles)
    ])
    spend_vars <- unique(vof_rows$AnalyticalVariableName[
      metric_roles == "spend"
    ])
    if (!length(activity_vars)) activity_vars <- anal_var_names
    
    ch_cfg <- derive_ch_config_fast(anal_var_names)
    
    act_kw <- detect_activity_keyword(
      stringr::str_remove(activity_vars, "_Total_Total_Total$"),
      keyword_dict)
    
    vi_broad <- unique(trimws(stringr::str_remove(
      ch_cfg$varname_include,
      stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)
    )))
    vi_broad        <- vi_broad[nzchar(vi_broad) &
                                  !(vi_broad %in% ch_cfg$varname_include)]
    varname_include <- unique(c(ch_cfg$varname_include, vi_broad))
    varname_include <- varname_include[nzchar(varname_include)]
    
    spend_kw <- if (length(spend_vars)) {
      detected <- detect_spend_keyword(
        data.frame(VariableName = stringr::str_remove(spend_vars, "_Total(_Total)*$")),
        stringr::str_remove(spend_vars, "_Total(_Total)*$"),
        keyword_dict)
      metric_kw <- if ("Metric" %in% names(vof_rows))
        keyword_from_metric(vof_rows$Metric[metric_roles == "spend"], "spend")
      else NA_character_
      if (!is.na(detected) && nzchar(detected)) detected
      else if (!is.na(metric_kw) && nzchar(metric_kw)) metric_kw
      else detect_spend_kw_fast(varname_include)
    } else {
      detect_spend_kw_fast(varname_include)
    }
    
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
    vof_claimed_cols <- unique(unlist(lapply(channels, function(ch) {
      if (!identical(ch$source, "vof")) return(character(0))
      c(ch$channel_name %||% "", ch$model_variable %||% "", ch$analytical_varkeys %||% character(0))
    }), use.names = FALSE))
    vof_claimed_cols <- vof_claimed_cols[nzchar(vof_claimed_cols)]
    non_vof_cols <- setdiff(model_cols, names(channels))
    non_vof_cols <- non_vof_cols[
      !(non_vof_cols %in% vof_claimed_cols) &
        !(normalize_model_var(non_vof_cols) %in% normalize_model_var(vof_claimed_cols))
    ]
    
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
    range_df <- data.frame(
      nm = group_nms,
      min_date = as.Date(vapply(group_nms, function(nm) {
        mp <- channels[[nm]]$min_period
        if (!is.null(mp) && !is.na(mp)) as.character(as.Date(mp)) else NA_character_
      }, character(1))),
      max_date = as.Date(vapply(group_nms, function(nm) {
        mp <- channels[[nm]]$max_period
        if (!is.null(mp) && !is.na(mp)) as.character(as.Date(mp)) else NA_character_
      }, character(1))),
      stringsAsFactors = FALSE
    )
    range_df$range_key <- paste(range_df$min_date, range_df$max_date, sep = "|")
    unique_ranges <- range_df[!duplicated(range_df$range_key), , drop = FALSE]
    unique_ranges <- unique_ranges[order(unique_ranges$min_date, unique_ranges$max_date), , drop = FALSE]
    if (nrow(unique_ranges) <= 1) next
    range_labels <- setNames(
      paste0(vapply(seq_len(nrow(unique_ranges)), ordinal_tag, character(1)), "TimeBreak"),
      unique_ranges$range_key
    )
    for (nm in group_nms) {
      key <- range_df$range_key[range_df$nm == nm][1]
      channels[[nm]]$time_break_label <- range_labels[[key]] %||% ""
    }
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
  
  # ── OPT: pre-convert all date params once ─────────────────────────────
  min_p   <- if (!is.null(min_period))
    tryCatch(as.Date(min_period), error = \(e) as.Date(NA)) else as.Date(NA)
  max_p   <- if (!is.null(max_period))
    tryCatch(as.Date(max_period), error = \(e) as.Date(NA)) else as.Date(NA)
  start_d <- as.Date(start_report_date)
  end_d   <- as.Date(end_report_date)
  
  # ── Filter to channel's VOF date range ──────────────────────────────────
  if (!is.na(min_p)) source_data <- source_data[source_data$Period >= min_p, ]
  if (!is.na(max_p)) source_data <- source_data[source_data$Period <= max_p, ]
  
  # ── Constrain to analytical date spine ──────────────────────────────────
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
  
  # ── OPT: rag_base via data.table (faster unique + setorder) ─────────────
  rag_base_dt <- unique(
    data.table::as.data.table(source_data)[, cross_id, with = FALSE])
  data.table::setorderv(rag_base_dt, "Period")
  rag_base <- as.data.frame(rag_base_dt)
  
  # ── OPT: ref_cross_key from rag_base (~2k rows) not source_data (576k) ─
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
  
  # ── Pre-filter ────────────────────────────────────────────────────────────
  pb("Filtering source data...", 0.10)
  
  d_prefilt <- data.table::as.data.table(source_data)
  
  vi <- cfg$varname_include[nchar(cfg$varname_include %||% "") > 0]
  if (length(vi) > 0) {
    pattern   <- paste(paste0("^", vi), collapse = "|")
    d_prefilt <- d_prefilt[grepl(pattern, VariableName, ignore.case = TRUE)]
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
  
  # ── Segment loop ─────────────────────────────────────────────────────────
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
    d <- data.table::as.data.table(d)
    
    # Keep VariableName in the technical split key so activity/spend detection
    # still works when the visible split order only uses broken dimensions.
    split_cols_technical <- unique(c("VariableName", cfg$split_columns %||% character(0)))
    d[, SplitName := build_split_name_from_columns(d, split_cols_technical)]
    
    # ── Pivot wide ───────────────────────────────────────────────────────────
    lhs    <- paste(cross_id, collapse = " + ")
    d_wide <- data.table::dcast(d,
                                as.formula(paste(lhs, "~ SplitName")),
                                value.var = "VariableValue",
                                fun.aggregate = sum, fill = 0)
    d_wide <- merge(rag_base_dt, d_wide, by = cross_id, all.x = TRUE)
    
    # ── OPT: setnafill instead of for-loop over columns ──────────────────
    num_cols_w <- names(d_wide)[sapply(d_wide, is.numeric)]
    if (length(num_cols_w) > 0)
      data.table::setnafill(d_wide, fill = 0, cols = num_cols_w)
    
    d_wide <- as.data.frame(d_wide)
    d_wide <- d_wide[order(d_wide$Period), ]
    
    # ── Non-focus suffix ─────────────────────────────────────────────────────
    nf_sfx <- {
      tbr <- cfg$time_break_label %||% ""
      if (nzchar(tbr)) paste0("Before ", update_label, "|", tbr)
      else             paste0("Before ", update_label)
    }
    
    # ── Non-focus slice ──────────────────────────────────────────────────────
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
    
    # ── Focus slice ──────────────────────────────────────────────────────────
    fc_raw <- as.data.frame(
      d_wide[d_wide$Period >= start_d & d_wide$Period <= end_d, ])
    fc <- keep_nonzero_cols(fc_raw)
    
    if (ncol(fc) > 1) {
      split_cols_fc <- setdiff(names(fc), id_protect)
      if (length(split_cols_fc) > 0)
        names(fc)[names(fc) %in% split_cols_fc] <-
          paste0(split_cols_fc, "_", update_label)
      
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
  
  # ── OPT: Assemble RAG — setnafill OUTSIDE the merge loop ────────────────
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
  
  list(
    rag            = rag,
    cross_cols     = cross_cols,
    ref_cross      = ref_cross_key,
    activity_spend = build_activity_spend(act_all, cost_all, cfg),
    side_mapping   = build_side_mapping(act_all),
    act_diagnoses  = act_all,
    cost_diagnoses = cost_all
  )
}

# ── get_useful_long_values ─────────────────────────────────────────────────
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
