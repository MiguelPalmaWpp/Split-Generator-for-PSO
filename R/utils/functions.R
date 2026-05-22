# ═══════════════════════════════════════════════════════════════════════
# R/functions.R
# ═══════════════════════════════════════════════════════════════════════

# ── Ordinal tag ────────────────────────────────────────────────────────────
ordinal_tag <- function(i) {
  c("First", "Second", "Third", "Fourth", "Fifth")[min(i, 5L)]
}

# ── Date parsers ───────────────────────────────────────────────────────────
parse_period_robust <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^(\\d{4})(\\d{2})(\\d{2})$",       "\\1-\\2-\\3", x)
  x <- gsub("^(\\d{1,2})/(\\d{1,2})/(\\d{4})$",  "\\3-\\1-\\2", x)
  x <- gsub("^(\\d{1,2})/(\\d{1,2})/(\\d{2})$",  "20\\3-\\1-\\2", x)
  x <- gsub("^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})$", "\\3-\\2-\\1", x)
  suppressWarnings(as.Date(x))
}

parse_vof_period <- function(x) {
  x <- trimws(as.character(x))
  fmts <- c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d", "%d/%m/%Y")
  for (fmt in fmts) {
    result <- suppressWarnings(as.Date(x, format = fmt))
    if (!anyNA(result) &&
        all(as.integer(format(result, "%Y")) > 1900, na.rm = TRUE))
      return(result)
  }
  as.Date(NA_character_)
}

# ── Cross-section detection ────────────────────────────────────────────────
auto_detect_cross_cols <- function(analytical) {
  candidates <- intersect(CROSS_SECTION_CANDIDATES, names(analytical))
  found <- Filter(function(col) {
    n <- n_distinct(analytical[[col]])
    n > 1 && n < nrow(analytical) * 0.5
  }, candidates)
  if (!length(found)) "Geography" else found
}

# ── File reader ────────────────────────────────────────────────────────────
read_main_data <- function(path, ext) {
  raw <- if (tolower(ext) == "parquet") {
    arrow::read_parquet(path)
  } else if (tolower(ext) %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (tolower(ext) == "gz") {
    data.table::fread(file = path, data.table = FALSE,
                      colClasses = "character", showProgress = FALSE)
  } else if (tolower(ext) == "zip") {
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
    data.table::fread(file = path, sep = "auto", encoding = "UTF-8",
                      data.table = FALSE, colClasses = "character",
                      showProgress = FALSE)
  }
  
  if ("rawPeriod" %in% names(raw)) {
    names(raw) <- sub("^raw", "", names(raw))
    message("[read_main_data] all_extracted format — 'raw' prefix removed")
  }
  
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  raw <- raw[, intersect(REQUIRED_COLS, names(raw)), drop = FALSE]
  
  raw %>%
    mutate(
      Period        = parse_period_robust(Period),
      VariableValue = as.numeric(gsub(",", "", gsub(" ", "",
                                                    as.character(VariableValue))))
    ) %>%
    arrange(Geography, Product, VariableName, Period)
}

# ── Config CSV ─────────────────────────────────────────────────────────────
export_channels_csv <- function(channels) {
  if (!length(channels)) return(data.frame())
  rows <- list()
  for (nm in names(channels)) {
    cfg <- channels[[nm]]
    rows <- c(rows, list(tibble(
      Channel    = nm,
      Type       = "Config",
      SplitOrder = paste(cfg$split_columns %||% character(0), collapse = "|"),
      Name       = "",
      Splits     = ""
    )))
    for (b in cfg$dimension_breaks %||% list()) {
      rows <- c(rows, list(tibble(
        Channel    = nm,
        Type       = "Break",
        SplitOrder = b$column,
        Name       = paste(b$names, collapse = "|"),
        Splits     = paste(c(b$separator, b$n_parts), collapse = "|")
      )))
    }
    for (m in cfg$saved_merges %||% list()) {
      if (!isTRUE(m$active)) next
      rows <- c(rows, list(tibble(
        Channel    = nm,
        Type       = "Merge",
        SplitOrder = "",
        Name       = m$new_name,
        Splits     = paste(unlist(m$merged), collapse = "|")
      )))
    }
  }
  if (!length(rows)) return(data.frame())
  bind_rows(rows)
}

# ── Media keyword detectors ────────────────────────────────────────────────
detect_activity_keyword <- function(var_names,
                                    keyword_dict = MEDIA_KEYWORD_DICT) {
  for (kw in keyword_dict$activity) {
    if (any(str_detect(var_names, regex(kw, ignore_case = TRUE)))) return(kw)
  }
  "Impressions"
}

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
  for (kw in keyword_dict$spend) {
    if (any(str_detect(matching, regex(kw, ignore_case = TRUE)))) return(kw)
  }
  "Spend"
}

# ── var_key builder ────────────────────────────────────────────────────────
build_var_key <- function(main_data, vof_analytical_names) {
  if (is.null(main_data) || !"VariableName" %in% names(main_data))
    return(list(type = "standard", key_col = "var_key_v1",
                coverage = 0, distinct_df = NULL))
  
  dv <- main_data %>% select(VariableName, any_of("Product")) %>% distinct()
  dv$var_key_v1 <- paste0(dv$VariableName, "_Total_Total_Total")
  cov_v1 <- mean(dv$var_key_v1 %in% vof_analytical_names)
  
  use_product <- FALSE
  if ("Product" %in% names(dv) && n_distinct(dv$Product) > 1) {
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

# ═══════════════════════════════════════════════════════════════════════
# build_media_index
# ═══════════════════════════════════════════════════════════════════════
build_media_index <- function(main_data, analytical, vof_df, model_details,
                              channels_rois = NULL,
                              cross_cols    = "Geography",
                              keyword_dict  = MEDIA_KEYWORD_DICT) {
  channels <- list()
  geo_col  <- cross_cols[1]
  
  an_geos <- if (!is.null(analytical) && geo_col %in% names(analytical))
    unique(as.character(analytical[[geo_col]])) else character(0)
  
  an_min_date <- if (!is.null(analytical) && "Period" %in% names(analytical))
    min(analytical$Period, na.rm = TRUE) else as.Date(NA_character_)
  an_max_date <- if (!is.null(analytical) && "Period" %in% names(analytical))
    max(analytical$Period, na.rm = TRUE) else as.Date(NA_character_)
  
  # ── Step 1: filter ModelDetails to Type = IN / FIXED ────────────────────
  in_model_vars <- if (
    !is.null(model_details) &&
    all(c("Type", "VariableName") %in% names(model_details))
  ) {
    model_details %>%
      filter(
        str_detect(str_to_lower(trimws(Type)), "\\b(in|fixed)\\b"),
        !str_detect(str_to_lower(trimws(Type)), "none")
      ) %>%
      pull(VariableName) %>% unique()
  } else {
    unique(vof_df$MainModelVariableName)
  }
  
  # ── Step 2: validate VOF ─────────────────────────────────────────────────
  req_vof  <- c("AnalyticalVariableName", "MainModelVariableName",
                "MinPeriod", "MaxPeriod", "Geographies")
  miss_vof <- setdiff(req_vof, names(vof_df))
  if (length(miss_vof))
    stop("VOF missing columns: ", paste(miss_vof, collapse = ", "))
  
  vof_filtered <- vof_df %>%
    filter(MainModelVariableName %in% in_model_vars)
  
  if (!nrow(vof_filtered)) {
    stop(
      "No VOF rows matched ModelDetails Type='IN'/'FIXED'.\n",
      "ModelDetails IN variables (", length(in_model_vars), "): ",
      paste(head(in_model_vars, 3), collapse = ", "), "...\n",
      "VOF MainModelVariableName (",
      n_distinct(vof_df$MainModelVariableName), "): ",
      paste(head(unique(vof_df$MainModelVariableName), 3), collapse = ", "), "..."
    )
  }
  
  # ── Step 3: var_key detection ────────────────────────────────────────────
  vk_info <- build_var_key(main_data, unique(vof_filtered$AnalyticalVariableName))
  
  # ── Step 4: ROI lookup ───────────────────────────────────────────────────
  roi_lookup <- NULL
  if (!is.null(channels_rois) &&
      "MainModelVariableName" %in% names(channels_rois)) {
    roi_num <- setdiff(
      names(channels_rois)[sapply(channels_rois, is.numeric)],
      "MainModelVariableName")
    if (length(roi_num)) {
      roi_lookup <- channels_rois %>%
        select(MainModelVariableName, all_of(roi_num)) %>%
        distinct(MainModelVariableName, .keep_all = TRUE)
    }
  }
  
  # ── Step 5: one channel per MainModelVariableName ────────────────────────
  for (mv in unique(vof_filtered$MainModelVariableName)) {
    vof_rows       <- vof_filtered %>% filter(MainModelVariableName == mv)
    anal_var_names <- unique(vof_rows$AnalyticalVariableName)
    
    act_kw <- detect_activity_keyword(
      str_remove(anal_var_names, "_Total_Total_Total$"), keyword_dict)
    
    vi_exact <- unique(str_remove(anal_var_names, "_Total_Total_Total$"))
    vi_exact <- vi_exact[nzchar(vi_exact)]
    
    vi_broad <- unique(str_trim(
      str_remove(vi_exact,
                 regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE))
    ))
    vi_broad <- vi_broad[nzchar(vi_broad) & vi_broad != vi_exact]
    
    varname_include <- unique(c(vi_exact, vi_broad))
    varname_include <- varname_include[nzchar(varname_include)]
    
    spend_kw <- detect_spend_keyword(main_data, varname_include, keyword_dict)
    
    parsed_min <- parse_vof_period(vof_rows$MinPeriod)
    parsed_max <- parse_vof_period(vof_rows$MaxPeriod)
    min_period <- if (any(!is.na(parsed_min)))
      min(parsed_min, na.rm = TRUE) else as.Date(NA_character_)
    max_period <- if (any(!is.na(parsed_max)))
      max(parsed_max, na.rm = TRUE) else as.Date(NA_character_)
    min_period <- if (is.na(min_period) || !is.finite(as.numeric(min_period)))
      an_min_date else min_period
    max_period <- if (is.na(max_period) || !is.finite(as.numeric(max_period)))
      an_max_date else max_period
    
    geo_strs <- vof_rows$Geographies[nzchar(trimws(vof_rows$Geographies %||% ""))]
    segment_overrides <- if (length(geo_strs) > 0 && length(an_geos) > 0) {
      included <- unique(trimws(unlist(strsplit(paste(geo_strs, collapse = ","), ","))))
      included <- included[nzchar(included)]
      geo_exc  <- setdiff(an_geos, included)
      if (length(geo_exc))
        list(list(seg = 1L, geography_exclude = geo_exc))
      else list()
    } else list()
    
    roi_val <- NA_real_
    if (!is.null(roi_lookup)) {
      ri <- roi_lookup %>% filter(MainModelVariableName == mv)
      if (nrow(ri) > 0) {
        r_num <- setdiff(names(ri), "MainModelVariableName")
        if (length(r_num)) roi_val <- mean(ri[[r_num[1]]], na.rm = TRUE)
      }
    }
    
    channels[[mv]] <- list(
      channel_name       = mv,
      model_variable     = mv,
      varname_include    = varname_include,
      analytical_varkeys = anal_var_names,
      min_period         = min_period,
      max_period         = max_period,
      segment_overrides  = segment_overrides,
      activity_keyword   = act_kw,
      spend_keyword      = spend_kw,
      split_columns      = c("VariableName"),
      saved_merges       = list(),
      dimension_breaks   = list(),
      roi                = roi_val,
      source             = "vof"
    )
  }
  
  # ── Step 6: fallback — non-VOF Analytical variables ──────────────────────
  if (!is.null(analytical)) {
    cross_id_cols <- c(cross_cols, "Period", "BP_Year")
    model_cols    <- setdiff(
      names(analytical)[sapply(analytical, is.numeric)], cross_id_cols)
    non_vof_cols  <- setdiff(model_cols, names(channels))
    
    for (col in non_vof_cols) {
      kw_match <- Filter(
        function(kw) str_detect(col, regex(kw, ignore_case = TRUE)),
        keyword_dict$activity)
      if (!length(kw_match)) next
      
      act_kw  <- kw_match[1]
      base_vn <- str_remove(col, "_Total_Total_Total$")
      
      vi_broad <- str_trim(str_remove(
        base_vn,
        regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)))
      varname_include <- if (nzchar(vi_broad) && vi_broad != base_vn)
        c(base_vn, vi_broad) else base_vn
      
      spend_kw <- detect_spend_keyword(main_data, varname_include, keyword_dict)
      
      roi_val <- NA_real_
      if (!is.null(roi_lookup)) {
        ri <- roi_lookup %>% filter(MainModelVariableName == col)
        if (nrow(ri) > 0) {
          r_num <- setdiff(names(ri), "MainModelVariableName")
          if (length(r_num)) roi_val <- ri[[r_num[1]]][1]
        }
      }
      
      channels[[col]] <- list(
        channel_name       = col,
        model_variable     = col,
        varname_include    = varname_include,
        analytical_varkeys = col,
        min_period         = an_min_date,
        max_period         = an_max_date,
        segment_overrides  = list(),
        activity_keyword   = act_kw,
        spend_keyword      = spend_kw,
        split_columns      = c("VariableName"),
        saved_merges       = list(),
        dimension_breaks   = list(),
        roi                = roi_val,
        source             = "keyword_fallback"
      )
    }
  }
  
  # ── Step 7: assign time_break_labels ────────────────────────────────────
  vof_sigs <- vapply(names(channels), function(nm) {
    ch <- channels[[nm]]
    if (!identical(ch$source, "vof")) return(NA_character_)
    paste(sort(unique(ch$analytical_varkeys)), collapse = "||")
  }, character(1))
  
  vof_sigs  <- vof_sigs[!is.na(vof_sigs)]
  dup_sigs  <- names(which(table(vof_sigs) > 1))
  
  for (sig in dup_sigs) {
    group_nms <- names(vof_sigs[vof_sigs == sig])
    min_dates <- sapply(group_nms, function(nm) {
      mp <- channels[[nm]]$min_period
      if (!is.null(mp) && !is.na(mp)) as.numeric(as.Date(mp)) else 0
    })
    sorted_nms <- group_nms[order(min_dates)]
    for (i in seq_along(sorted_nms))
      channels[[sorted_nms[i]]]$time_break_label <- paste0(ordinal_tag(i), "TimeBreak")
  }
  
  # ── Return ─────────────────────────────────────────────────────────────
  n_vof      <- sum(sapply(channels, \(c) identical(c$source, "vof")))
  n_fallback <- sum(sapply(channels, \(c) identical(c$source, "keyword_fallback")))
  n_with_roi <- sum(sapply(channels, \(c) !is.na(c$roi %||% NA_real_)))
  
  list(
    channels     = channels,
    var_key_info = vk_info,
    summary = list(
      total_channels = length(channels),
      from_vof       = n_vof,
      from_fallback  = n_fallback,
      with_roi       = n_with_roi,
      var_key_type   = vk_info$type,
      vof_coverage   = round(vk_info$coverage * 100, 1)
    )
  )
}