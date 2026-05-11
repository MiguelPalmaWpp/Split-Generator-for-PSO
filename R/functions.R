# ════════════════════════════════════════════════════════════════
# R/functions.R
# ════════════════════════════════════════════════════════════════

# ── Statistical helpers ───────────────────────────────────────────
min_consec_weeks <- function(x) {
  r <- rle(x > 0)
  if (any(r$values)) min(r$lengths[r$values]) else 0
}

max_no_outlier <- function(x) {
  nz <- x[x > 0]
  if (!length(nz)) return(0)
  q3  <- quantile(nz, .75, na.rm = TRUE)
  max(nz[nz <= q3 + 1.5 * IQR(nz, na.rm = TRUE)], na.rm = TRUE)
}

# ── splits_summary — optimized single pass ────────────────────────
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
               c("total", "pct_total", "num_weeks")
  )
  
  result <- bind_rows(lapply(num_cols, function(col) {
    x  <- d[[col]]
    nz <- x[!is.na(x) & x > 0]
    
    total_val   <- sum(x, na.rm = TRUE)
    active      <- !is.na(x) & x > 0
    weeks_active <- if (!is.null(period_col))
      length(unique(period_col[active])) else sum(active)
    
    tibble(
      VariableSplit         = col,
      v1                    = total_val,
      v2                    = round(total_val / grand * 100, 4),
      v3                    = weeks_active,
      min_consecutive_weeks = min_consec_weeks(x),
      sd             = if (length(nz) < 2) NA_real_ else sd(nz),
      min            = if (!length(nz)) NA_real_ else min(nz),
      quartile_1     = if (!length(nz)) NA_real_ else as.numeric(quantile(nz, .25)),
      median         = if (!length(nz)) NA_real_ else as.numeric(median(nz)),
      quartile_3     = if (!length(nz)) NA_real_ else as.numeric(quantile(nz, .75)),
      max_no_outlier = max_no_outlier(x),
      max            = max(x, na.rm = TRUE)
    )
  }))
  
  result$max_index <- result$max / result$v1
  
  col_order <- c("VariableSplit", "v1", "v2", "v3", "max_index",
                 "min_consecutive_weeks", "sd", "min", "quartile_1",
                 "median", "quartile_3", "max_no_outlier", "max")
  result <- result[, col_order]
  names(result)[names(result) == "v1"] <- nm[1]
  names(result)[names(result) == "v2"] <- nm[2]
  names(result)[names(result) == "v3"] <- nm[3]
  
  as_tibble(result[order(-result[[nm[2]]]), ])
}

# ── Robust Period parser ───────────────────────────────────────────
parse_period_robust <- function(x) {
  if (inherits(x, c("Date", "IDate")))    return(as.Date(x))
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  
  x    <- trimws(as.character(x))
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%y", "%m/%d/%Y",
            "%d/%m/%y", "%d/%m/%Y", "%m-%d-%y", "%m-%d-%Y",
            "%d-%m-%y", "%d-%m-%Y", "%d.%m.%y", "%d.%m.%Y", "%Y%m%d")
  
  for (fmt in fmts) {
    result <- suppressWarnings(as.Date(x, format = fmt))
    if (!anyNA(result) &&
        all(as.integer(format(result, "%Y")) > 1900))
      return(result)
  }
  
  best       <- NULL
  best_valid <- 0L
  for (fmt in fmts) {
    result <- suppressWarnings(as.Date(x, format = fmt))
    valid  <- sum(!is.na(result) &
                    as.integer(format(result, "%Y")) > 1900, na.rm = TRUE)
    if (valid > best_valid) { best_valid <- valid; best <- result }
  }
  
  if (!is.null(best)) {
    n_fail <- sum(is.na(best))
    if (n_fail > 0)
      warning(sprintf("parse_period_robust: %d date(s) could not be parsed. First raw: '%s'",
                      n_fail, x[1]))
    return(best)
  }
  
  warning("parse_period_robust: no format matched. First raw: '", x[1], "'")
  as.Date(rep(NA_character_, length(x)))
}

# ── File reader ────────────────────────────────────────────────────
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
  
  # Normalize "raw" prefix (all_extracted format)
  if ("rawPeriod" %in% names(raw)) {
    names(raw) <- sub("^raw", "", names(raw))
    message("[read_all_transformed] all_extracted format detected — 'raw' prefix removed")
  }
  
  # ── OPTIMIZATION : keep only required columns ─────────────────
  available_required <- intersect(REQUIRED_COLS, names(raw))
  miss <- setdiff(REQUIRED_COLS, names(raw))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  raw <- raw[, available_required, drop = FALSE]
  
  message("[read_all_transformed] Raw Period sample: '",
          raw$Period[!is.na(raw$Period)][1], "'")
  
  raw %>%
    mutate(
      Period       = parse_period_robust(Period),
      VariableValue = as.numeric(
        gsub(",", "", gsub(" ", "", as.character(VariableValue)))
      )
    ) %>%
    arrange(Geography, Product, VariableName, Period)
}

# ── String utility ─────────────────────────────────────────────────
parse_text_lines <- function(x) {
  strsplit(x %||% "", "\n")[[1]] %>%
    trimws() %>%
    (\(v) v[nchar(v) > 0])()
}

# ── Auto-detect cross-section columns ─────────────────────────────
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

# ── Auto-detect source type ────────────────────────────────────────
auto_detect_source_type <- function(df, cross_cols) {
  n_combos <- df %>%
    select(any_of(cross_cols)) %>%
    distinct() %>%
    nrow()
  
  if (n_combos > 1) "all_rags" else "all_transformed"
}

# ── VOF Metadata helpers ───────────────────────────────────────────
extract_activity_keyword <- function(var_name) {
  base  <- sub("_Total_Total_Total.*", "", var_name)
  words <- trimws(strsplit(base, " ")[[1]])
  words <- words[nzchar(words)]
  words[length(words)]
}

extract_base_varname <- function(var_name) {
  base <- sub("_Total_Total_Total.*", "", var_name)
  kw   <- extract_activity_keyword(var_name)
  trimws(sub(paste0("(?i)\\s+", kw, "$"), "", base, perl = TRUE))
}

# ── parse_vof_to_channels ─────────────────────────────────────────
parse_vof_to_channels <- function(vof_df,
                                  analytical_geos = character(0)) {
  req_cols <- c("AnalyticalVariableName", "MediaChannel",
                "MinPeriod", "MaxPeriod", "Geographies",
                "MainModelVariableName")
  missing  <- setdiff(req_cols, names(vof_df))
  if (length(missing) > 0)
    stop("VOF is missing columns: ", paste(missing, collapse = ", "))
  
  channels_list <- list()
  
  for (media_ch in unique(vof_df$MediaChannel)) {
    if (!nzchar(trimws(media_ch %||% ""))) next
    ch_rows <- vof_df[vof_df$MediaChannel == media_ch, ]
    
    # ── Order model variables chronologically ──────────────────
    mv_summary <- ch_rows %>%
      select(MainModelVariableName, MinPeriod, MaxPeriod) %>%
      distinct() %>%
      mutate(sort_date = suppressWarnings(if_else(
        nzchar(trimws(MaxPeriod %||% "")),
        as.Date(MaxPeriod, format = "%m/%d/%Y"),
        as.Date("2999-12-31")
      ))) %>%
      arrange(sort_date)
    
    model_vars <- unique(mv_summary$MainModelVariableName)
    n_segs     <- length(model_vars)
    
    # ── Break dates ────────────────────────────────────────────
    break_dates <- character(0)
    if (nrow(mv_summary) > 1) {
      for (i in seq_len(nrow(mv_summary) - 1)) {
        max_p  <- trimws(mv_summary$MaxPeriod[i] %||% "")
        parsed <- suppressWarnings(as.Date(max_p, format = "%m/%d/%Y"))
        if (!is.na(parsed))
          break_dates <- c(break_dates, format(parsed, "%Y-%m-%d"))
      }
    }
    
    # ── Activity keyword + varname_include ─────────────────────
    act_kw <- tryCatch(
      extract_activity_keyword(ch_rows$AnalyticalVariableName[1]),
      error = \(e) "Impressions"
    )
    base_names <- unique(sapply(ch_rows$AnalyticalVariableName,
                                extract_base_varname))
    base_names <- base_names[nzchar(base_names)]
    
    # ── Per-segment geography handling ─────────────────────────
    # For each segment, compute its geography_exclude
    seg_geo_lists <- lapply(seq_len(n_segs), function(i) {
      mv      <- model_vars[i]
      mv_rows <- ch_rows[ch_rows$MainModelVariableName == mv, ]
      geo_str <- mv_rows$Geographies[
        nzchar(trimws(mv_rows$Geographies %||% ""))]
      
      # Empty = national = no geo filter
      if (!length(geo_str)) return(character(0))
      
      included <- unique(trimws(unlist(
        strsplit(paste(geo_str, collapse = ","), ",")
      )))
      included <- included[nzchar(included)]
      
      if (!length(analytical_geos) || !length(included))
        return(character(0))
      
      setdiff(analytical_geos, included)
    })
    
    # ── Global vs per-segment ──────────────────────────────────
    # If all segments have identical geo excludes → use global (simple)
    # If they differ → use per-segment overrides
    all_same <- length(unique(lapply(seg_geo_lists, sort))) == 1
    
    if (all_same) {
      global_geo_exclude <- seg_geo_lists[[1]]
      segment_overrides  <- list()
    } else {
      global_geo_exclude <- character(0)
      segment_overrides  <- lapply(seq_len(n_segs), function(i) {
        list(
          seg               = i,
          geography_exclude = seg_geo_lists[[i]]
        )
      })
    }
    
    channels_list[[media_ch]] <- list(
      channel_name      = media_ch,
      model_variables   = model_vars,
      break_dates       = break_dates,
      varname_include   = base_names,
      varname_exclude   = character(0),
      geography_exclude = global_geo_exclude,
      campaign_exclude  = character(0),
      outlet_exclude    = character(0),
      creative_exclude  = character(0),
      split_columns     = c("VariableName", "Campaign"),
      activity_keyword  = act_kw,
      spend_keyword     = "Spend",
      saved_merges      = list(),
      dimension_breaks  = list(),
      segment_overrides = segment_overrides
    )
  }
  
  channels_list
}

# ── CSV helpers ────────────────────────────────────────────────────
arr_to_str <- function(x) {
  x <- unlist(x %||% character(0))
  x <- x[nzchar(trimws(x))]
  if (!length(x)) return("")
  paste(x, collapse = "|")
}

str_to_arr <- function(x) {
  if (is.null(x) || is.na(x) ||
      !nzchar(trimws(as.character(x)))) return(character(0))
  Filter(nzchar, trimws(strsplit(as.character(x), "\\|")[[1]]))
}

# ── Export channels + merges + breaks + segment overrides ─────────
export_channels_csv <- function(channels) {
  if (!length(channels)) return(data.frame())
  rows <- list()
  
  for (nm in names(channels)) {
    cfg    <- channels[[nm]]
    merges <- cfg$saved_merges     %||% list()
    breaks <- cfg$dimension_breaks %||% list()
    segs   <- cfg$segment_overrides %||% list()
    
    empty_seg_cols <- list(
      `Seg Number`     = "",
      `Seg Geo Exclude` = ""
    )
    
    # ── Config row ───────────────────────────────────────────────
    rows <- c(rows, list(data.frame(
      Channel            = nm,
      Type               = "Config",
      `Model Variables`  = arr_to_str(cfg$model_variables),
      `Break Dates`      = arr_to_str(cfg$break_dates),
      `Include Vars`     = arr_to_str(cfg$varname_include),
      `Exclude Vars`     = arr_to_str(cfg$varname_exclude),
      `Exclude Geo`      = arr_to_str(cfg$geography_exclude),
      `Exclude Campaign` = arr_to_str(cfg$campaign_exclude),
      `Exclude Outlet`   = arr_to_str(cfg$outlet_exclude),
      `Exclude Creative` = arr_to_str(cfg$creative_exclude),
      `Split Order`      = arr_to_str(cfg$split_columns),
      `Activity Kw`      = cfg$activity_keyword %||% "Impressions",
      `Spend Kw`         = cfg$spend_keyword    %||% "Spend",
      `Merged Splits`    = "", `Merge Name` = "", View = "",
      `Spend Merged`     = "", `Spend Name` = "", Active = "",
      `Break Column`     = "", Separator = "", Parts = "",
      `Part Names`       = "",
      `Seg Number`       = "",
      `Seg Geo Exclude`  = "",
      stringsAsFactors   = FALSE, check.names = FALSE
    )))
    
    # ── Segment override rows ────────────────────────────────────
    for (so in segs) {
      rows <- c(rows, list(data.frame(
        Channel            = nm,
        Type               = "Segment",
        `Model Variables`  = "", `Break Dates` = "",
        `Include Vars`     = "", `Exclude Vars` = "",
        `Exclude Geo`      = "", `Exclude Campaign` = "",
        `Exclude Outlet`   = "", `Exclude Creative` = "",
        `Split Order`      = "", `Activity Kw` = "", `Spend Kw` = "",
        `Merged Splits`    = "", `Merge Name` = "", View = "",
        `Spend Merged`     = "", `Spend Name` = "", Active = "",
        `Break Column`     = "", Separator = "", Parts = "",
        `Part Names`       = "",
        `Seg Number`       = as.character(so$seg),
        `Seg Geo Exclude`  = arr_to_str(so$geography_exclude %||% character(0)),
        stringsAsFactors   = FALSE, check.names = FALSE
      )))
    }
    
    # ── Break rows ────────────────────────────────────────────────
    for (brk in breaks) {
      rows <- c(rows, list(data.frame(
        Channel = nm, Type = "Break",
        `Model Variables` = "", `Break Dates` = "",
        `Include Vars` = "", `Exclude Vars` = "",
        `Exclude Geo` = "", `Exclude Campaign` = "",
        `Exclude Outlet` = "", `Exclude Creative` = "",
        `Split Order` = "", `Activity Kw` = "", `Spend Kw` = "",
        `Merged Splits` = "", `Merge Name` = "", View = "",
        `Spend Merged` = "", `Spend Name` = "", Active = "",
        `Break Column`   = brk$column,
        Separator        = brk$separator,
        Parts            = as.character(brk$n_parts),
        `Part Names`     = arr_to_str(brk$names),
        `Seg Number`     = "",
        `Seg Geo Exclude` = "",
        stringsAsFactors = FALSE, check.names = FALSE
      )))
    }
    
    # ── Merge rows ────────────────────────────────────────────────
    for (m in merges) {
      rows <- c(rows, list(data.frame(
        Channel = nm, Type = "Merge",
        `Model Variables` = "", `Break Dates` = "",
        `Include Vars` = "", `Exclude Vars` = "",
        `Exclude Geo` = "", `Exclude Campaign` = "",
        `Exclude Outlet` = "", `Exclude Creative` = "",
        `Split Order` = "", `Activity Kw` = "", `Spend Kw` = "",
        `Merged Splits`   = arr_to_str(m$merged),
        `Merge Name`      = m$new_name       %||% "",
        View              = m$view           %||% "focus",
        `Spend Merged`    = arr_to_str(m$spend_merged),
        `Spend Name`      = m$new_spend_name %||% "",
        Active            = as.character(isTRUE(m$active)),
        `Break Column`    = "", Separator = "", Parts = "",
        `Part Names`      = "",
        `Seg Number`      = "",
        `Seg Geo Exclude` = "",
        stringsAsFactors  = FALSE, check.names = FALSE
      )))
    }
  }
  
  bind_rows(rows)
}

# ── Import channels from unified CSV ──────────────────────────────
import_channels_csv <- function(df) {
  if (!nrow(df)) return(list())
  names(df) <- trimws(names(df))
  channels  <- list()
  
  # Config rows
  config_rows <- df[trimws(df$Type) == "Config", ]
  for (i in seq_len(nrow(config_rows))) {
    nm <- config_rows$Channel[i]
    channels[[nm]] <- list(
      channel_name      = nm,
      model_variables   = str_to_arr(config_rows[i, "Model Variables"]),
      break_dates       = str_to_arr(config_rows[i, "Break Dates"]),
      varname_include   = str_to_arr(config_rows[i, "Include Vars"]),
      varname_exclude   = str_to_arr(config_rows[i, "Exclude Vars"]),
      geography_exclude = str_to_arr(config_rows[i, "Exclude Geo"]),
      campaign_exclude  = str_to_arr(config_rows[i, "Exclude Campaign"]),
      outlet_exclude    = str_to_arr(config_rows[i, "Exclude Outlet"]),
      creative_exclude  = str_to_arr(config_rows[i, "Exclude Creative"]),
      split_columns     = str_to_arr(config_rows[i, "Split Order"]),
      activity_keyword  = as.character(config_rows[i, "Activity Kw"] %||% "Impressions"),
      spend_keyword     = as.character(config_rows[i, "Spend Kw"]    %||% "Spend"),
      saved_merges      = list(),
      dimension_breaks  = list(),
      segment_overrides = list()
    )
  }
  
  # Segment override rows
  if ("Seg Number" %in% names(df)) {
    seg_rows <- df[trimws(df$Type) == "Segment", ]
    if (nrow(seg_rows) > 0) {
      for (nm in unique(seg_rows$Channel)) {
        if (!nm %in% names(channels)) next
        ch_s <- seg_rows[seg_rows$Channel == nm, ]
        channels[[nm]]$segment_overrides <- lapply(
          seq_len(nrow(ch_s)), function(j) {
            list(
              seg               = as.integer(ch_s[j, "Seg Number"]),
              geography_exclude = str_to_arr(ch_s[j, "Seg Geo Exclude"])
            )
          }
        )
      }
    }
  }
  
  # Break rows
  break_rows <- df[trimws(df$Type) == "Break", ]
  if (nrow(break_rows) > 0) {
    for (nm in unique(break_rows$Channel)) {
      if (!nm %in% names(channels)) next
      ch_b <- break_rows[break_rows$Channel == nm, ]
      channels[[nm]]$dimension_breaks <- lapply(
        seq_len(nrow(ch_b)), function(j) {
          list(
            column    = ch_b[j, "Break Column"],
            separator = ch_b[j, "Separator"],
            n_parts   = as.integer(ch_b[j, "Parts"]),
            names     = str_to_arr(ch_b[j, "Part Names"])
          )
        }
      )
    }
  }
  
  # Merge rows
  merge_rows <- df[trimws(df$Type) == "Merge", ]
  if (nrow(merge_rows) > 0) {
    for (nm in unique(merge_rows$Channel)) {
      if (!nm %in% names(channels)) next
      ch_m <- merge_rows[merge_rows$Channel == nm, ]
      channels[[nm]]$saved_merges <- lapply(
        seq_len(nrow(ch_m)), function(j) {
          list(
            id             = j,
            merged         = as.list(str_to_arr(ch_m[j, "Merged Splits"])),
            new_name       = ch_m[j, "Merge Name"]  %||% "",
            view           = ch_m[j, "View"]         %||% "focus",
            spend_merged   = as.list(str_to_arr(ch_m[j, "Spend Merged"])),
            new_spend_name = ch_m[j, "Spend Name"]   %||% "",
            active         = isTRUE(as.logical(ch_m[j, "Active"])),
            saved_at       = format(Sys.time(), "%Y-%m-%d %H:%M")
          )
        }
      )
    }
  }
  
  channels
}