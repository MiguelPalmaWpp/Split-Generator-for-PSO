# -----------------------------------------------------------------------
# R/utils/processing.R
# Shared split construction, role manifests, and effective RAE filters.
# -----------------------------------------------------------------------

keep_nonzero_cols <- function(df) {
  df   <- as.data.frame(df)
  keep <- vapply(df, function(col) {
    if (!is.numeric(col)) return(TRUE)
    sum(col, na.rm = TRUE) != 0
  }, logical(1))
  df[, keep, drop = FALSE]
}

is_empty_split_part <- function(x) {
  v <- trimws(as.character(x))
  is.na(x) | is.na(v) | toupper(v) %in% c("", "NA", "N/A", "NULL", "NONE")
}

clean_split_part <- function(x) {
  v <- trimws(as.character(x))
  v[is_empty_split_part(x)] <- NA_character_
  v
}

canonical_break_missing_part_value <- function(...) "Unknown"

# Apply configured dimension breaks once for every role and output. Missing
# parts are explicit Unknown values so previews and exported split names agree.
apply_dimension_breaks <- function(d, dimension_breaks, channel_name = NULL) {
  if (!length(dimension_breaks)) return(d)
  for (brk in dimension_breaks) {
    col <- brk$column; sep <- brk$separator; n <- brk$n_parts
    missing_part_value <- canonical_break_missing_part_value()
    if (!col %in% names(d)) next
    source_values <- clean_split_part(d[[col]])
    split_values <- strsplit(source_values, sep, fixed = TRUE)

    for (i in seq_len(n)) {
      values <- vapply(split_values, function(p) {
        if (!length(p) || length(p) < i) {
          missing_part_value
        } else if (i == n) {
          paste(p[i:length(p)], collapse = sep)
        } else {
          p[i]
        }
      }, character(1))
      values <- clean_split_part(values)
      values[is.na(values)] <- missing_part_value
      d[[brk$names[i]]] <- values
    }
    d[[col]] <- NULL
  }
  d
}

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

build_split_name_from_columns <- function(d, split_cols, fallback_col = "VariableName") {
  split_cols_present <- intersect(split_cols, names(d))
  if (!length(split_cols_present) && fallback_col %in% names(d))
    split_cols_present <- fallback_col

  if (!length(split_cols_present))
    return(rep("Unknown", nrow(d)))

  parts <- lapply(split_cols_present, function(col) {
    v <- clean_split_part(d[[col]])
    ifelse(is.na(v), "", v)
  })

  combined <- if (length(parts) == 1L) {
    parts[[1]]
  } else {
    Reduce(function(a, b)
      ifelse(nzchar(a) & nzchar(b), paste(a, b, sep = "_"),
             ifelse(nzchar(a), a, b)),
      parts)
  }

  fallback <- if (fallback_col %in% names(d)) clean_split_part(d[[fallback_col]])
  else rep(NA_character_, nrow(d))
  combined[!nzchar(combined) & !is.na(fallback)] <- fallback[!nzchar(combined) & !is.na(fallback)]
  combined[!nzchar(combined)] <- "Unknown"
  combined
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

get_diag_df <- function(df, cross_cols, ref_cross_key) {
  if (nrow(df) == 0) return(df)
  cross_data <- df[, cross_cols, drop = FALSE]
  cross_key  <- do.call(paste, c(as.list(cross_data), list(sep = " / ")))
  split_cols <- setdiff(names(df)[sapply(df, is.numeric)], cross_cols)
  has_signal <- function(rows) {
    if (!length(split_cols) || !nrow(rows)) return(nrow(rows) > 0)
    vals <- as.data.frame(rows[, split_cols, drop = FALSE])
    any(vapply(vals, function(x) any(!is.na(x) & x != 0), logical(1)))
  }
  out <- df[cross_key == ref_cross_key, , drop = FALSE]
  if (has_signal(out)) return(out)
  fallback_keys <- sort(unique(cross_key))
  for (fallback_key in fallback_keys) {
    candidate <- df[cross_key == fallback_key, , drop = FALSE]
    if (has_signal(candidate)) return(candidate)
  }
  if (nrow(out) > 0) out else df[cross_key == fallback_keys[1], , drop = FALSE]
}

build_model_total <- function(analytical, cross_id, model_variables,
                              break_dates) {
  n_vars        <- length(model_variables)
  break_dates_d <- as.Date(break_dates %||% character(0))
  base <- analytical %>%
    select(all_of(cross_id)) %>%
    mutate(ModelTotal = 0)
  for (i in seq_len(n_vars)) {
    mv <- model_variables[i]
    if (!mv %in% names(analytical)) {
      warning(sprintf("build_model_total: '%s' not found (segment %d).", mv, i))
      next
    }
    seg_start <- if (i == 1) as.Date("1900-01-01") else break_dates_d[i - 1] + 1
    seg_end   <- if (i == n_vars) as.Date("2999-12-31") else break_dates_d[i]
    seg_vals  <- analytical %>%
      filter(Period >= seg_start, Period <= seg_end) %>%
      select(all_of(cross_id), model_val = !!sym(mv))
    base <- base %>%
      left_join(seg_vals, by = cross_id) %>%
      mutate(ModelTotal = if_else(!is.na(model_val), model_val, ModelTotal)) %>%
      select(-model_val)
  }
  base
}

normalize_model_metric <- function(x, default = "activity") {
  x <- tolower(trimws(as.character(x %||% default)[1]))
  if (is.na(x) || !nzchar(x)) return(default)
  if (x %in% c("spend", "cost", "investment", "budget")) "spend" else "activity"
}

build_activity_spend <- function(act_all, cost_all, cfg) {
  if ((nrow(act_all) == 0 || !"VariableSplit" %in% names(act_all)) &&
      (nrow(cost_all) == 0 || !"VariableSplit" %in% names(cost_all)))
    return(tibble())
  channel_name <- trimws(as.character(cfg$channel_name %||%
                                        cfg$model_variable %||% ""))[1]
  if (is.na(channel_name)) channel_name <- ""
  cost_key <- if (nrow(cost_all) > 0 && "VariableSplit" %in% names(cost_all))
    cost_all %>%
    select(VariableSplit_c = VariableSplit, total_spend) %>%
    mutate(key = str_remove_all(VariableSplit_c,
                                regex(cfg$spend_keyword, ignore_case = TRUE)))
  else
    tibble(key = character(), total_spend = numeric(),
           VariableSplit_c = character())
  if (nrow(act_all) > 0 && "VariableSplit" %in% names(act_all)) {
    return(act_all %>%
      select(VariableSplit, total_activity, model_var) %>%
      mutate(key = str_remove_all(VariableSplit,
                                  regex(cfg$activity_keyword,
                                        ignore_case = TRUE))) %>%
      left_join(cost_key, by = "key") %>%
      mutate(Channel               = channel_name,
             MainModelVariableName = model_var) %>%
      select(VariableSplit, total_activity, total_spend,
             Channel, MainModelVariableName))
  }
  cost_all %>%
    select(VariableSplit, total_spend, model_var) %>%
    mutate(total_activity = NA_real_,
           Channel = channel_name,
           MainModelVariableName = model_var) %>%
    select(VariableSplit, total_activity, total_spend,
           Channel, MainModelVariableName)
}

build_side_mapping <- function(metric_all) {
  if (nrow(metric_all) == 0 || !"VariableSplit" %in% names(metric_all))
    return(tibble())
  metric_all %>%
    select(VariableSplit, model_var) %>%
    mutate(MainModelVariableName = model_var,
           Weight = 1, MinWeight = 0.5, MaxWeight = 2) %>%
    select(-model_var)
}

build_side_mapping_from_manifest <- function(split_manifest, model_variable) {
  if (is.null(split_manifest) || !nrow(split_manifest) ||
      !all(c("Role", "VariableSplit") %in% names(split_manifest))) {
    return(tibble::tibble())
  }
  split_manifest %>%
    dplyr::filter(.data$Role == "modeled",
                  !is.na(.data$VariableSplit), nzchar(.data$VariableSplit)) %>%
    dplyr::distinct(.data$VariableSplit) %>%
    dplyr::mutate(
      MainModelVariableName = model_variable %||% "",
      Weight = 1,
      MinWeight = 0.5,
      MaxWeight = 2
    )
}

resolve_role_rae_variables <- function(all_variable_names, cfg) {
  all_vars <- unique(trimws(as.character(all_variable_names %||% character(0))))
  all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]
  match_includes <- function(includes) {
    includes <- unique(trimws(as.character(includes %||% character(0))))
    includes <- includes[!is.na(includes) & nzchar(includes)]
    if (!length(includes) || !length(all_vars)) return(character(0))
    expanded <- expand_analytical_keys_to_variable_names(all_vars, includes)
    hits <- all_vars[tolower(all_vars) %in% tolower(expanded)]
    if (!length(hits) && !identical(cfg$varname_match_mode %||% "exact", "exact")) {
      hits <- all_vars[vapply(tolower(all_vars), function(vn) {
        any(startsWith(vn, paste0(tolower(includes), " ")) |
              startsWith(vn, paste0(tolower(includes), "_")) |
              vn == tolower(includes))
      }, logical(1))]
    }
    unique(hits)
  }

  modeled <- match_includes(
    cfg$modeled_varname_include %||% cfg$varname_include %||% character(0)
  )
  for_indices <- match_includes(cfg$for_indices_varname_include %||% character(0))

  if (!length(modeled) && length(all_vars)) {
    role <- normalize_model_metric(cfg$modeled_role %||% cfg$model_metric %||% "activity")
    kw <- if (identical(role, "spend")) cfg$spend_keyword %||% "Spend"
    else cfg$activity_keyword %||% "Activity"
    modeled <- all_vars[grepl(kw, all_vars, ignore.case = TRUE)]
  }
  for_indices <- setdiff(for_indices, modeled)
  list(modeled = modeled, for_indices = for_indices)
}

# Apply the channel audit contract to raw RAE and retain stage counts for
# diagnostics. This is the shared source filter for processing and Total Check.
filter_effective_channel_rae <- function(all_rags, cfg,
                                         min_period = NULL,
                                         max_period = NULL,
                                         exact_periods = NULL,
                                         schema_metadata = NULL,
                                         segment_overrides = NULL,
                                         role = c("all", "modeled")) {
  role <- match.arg(role)
  stage_counts <- list()
  applied_filters <- character(0)
  warnings <- character(0)

  stage <- function(name, data) {
    stage_counts[[length(stage_counts) + 1L]] <<- data.frame(
      Stage = name,
      Rows = if (is.null(data)) 0L else nrow(data),
      stringsAsFactors = FALSE
    )
  }
  finish <- function(data, date_data = data, failure_stage = "", reason = "") {
    list(
      data = as.data.frame(data),
      date_data = as.data.frame(date_data),
      stage_counts = dplyr::bind_rows(stage_counts),
      applied_filters = unique(applied_filters),
      warnings = unique(warnings),
      failure_stage = failure_stage,
      reason = reason
    )
  }
  fail <- function(name, data, date_data, reason) {
    last_stage <- if (length(stage_counts)) {
      stage_counts[[length(stage_counts)]]$Stage[[1]]
    } else {
      ""
    }
    if (!identical(last_stage, name)) stage(name, data)
    finish(data[0, , drop = FALSE], date_data, name, reason)
  }

  if (is.null(all_rags) || !is.data.frame(all_rags) ||
      !all(c("Period", "VariableName", "VariableValue") %in% names(all_rags))) {
    empty <- if (is.data.frame(all_rags)) all_rags[0, , drop = FALSE] else data.frame()
    return(finish(
      empty, empty, "input",
      "RAE is missing Period, VariableName or VariableValue."
    ))
  }

  d <- as.data.frame(all_rags)
  stage("input", d)
  d$Period <- if (inherits(d$Period, "Date")) d$Period else parse_period_robust(d$Period)
  invalid_periods <- sum(is.na(d$Period))
  if (invalid_periods > 0L) {
    warnings <- c(warnings, paste(invalid_periods, "RAE rows have an invalid Period."))
  }
  d <- d[!is.na(d$Period), , drop = FALSE]
  stage("parse_period", d)
  if (!nrow(d)) return(fail("parse_period", d, d, "RAE has no parseable Period values."))

  min_p <- tryCatch(as.Date(min_period), error = function(e) as.Date(NA))
  max_p <- tryCatch(as.Date(max_period), error = function(e) as.Date(NA))
  exact_periods <- parse_period_robust(exact_periods %||% as.Date(character(0)))
  exact_periods <- unique(exact_periods[!is.na(exact_periods)])
  if (length(min_p) == 0L) min_p <- as.Date(NA)
  if (length(max_p) == 0L) max_p <- as.Date(NA)
  if (!is.na(min_p)) {
    d <- d[d$Period >= min_p, , drop = FALSE]
    applied_filters <- c(applied_filters, paste0("Period >= ", format(min_p)))
  }
  if (!is.na(max_p)) {
    d <- d[d$Period <= max_p, , drop = FALSE]
    applied_filters <- c(applied_filters, paste0("Period <= ", format(max_p)))
  }
  stage("date_range", d)
  if (!nrow(d) && !length(exact_periods)) {
    return(fail("date_range", d, d, "No RAE rows are inside the channel date range."))
  }

  if (length(exact_periods)) {
    d <- d[d$Period %in% exact_periods, , drop = FALSE]
    applied_filters <- c(applied_filters, paste(length(exact_periods), "exact Analytical periods"))
    stage("analytical_periods", d)
    if (!nrow(d)) {
      return(fail(
        "analytical_periods", d, d,
        "RAE has no rows on the exact Period values present in Analytical."
      ))
    }
  }
  date_data <- d

  available_vars <- unique(trimws(as.character(d$VariableName)))
  role_vars <- resolve_role_rae_variables(available_vars, cfg)
  vi <- if (identical(role, "modeled")) {
    role_vars$modeled
  } else {
    cfg$varname_include %||% character(0)
  }
  vi <- unique(trimws(as.character(vi)))
  vi <- vi[!is.na(vi) & nzchar(vi)]
  if (identical(role, "all") && length(vi) &&
      !length(cfg$modeled_varname_include %||% character(0))) {
    vi <- expand_varname_include_with_spend(
      available_vars, vi, cfg$spend_keyword %||% NULL
    )
  }
  if (length(vi)) {
    vi <- expand_analytical_keys_to_variable_names(available_vars, vi)
    match_mode <- cfg$varname_match_mode %||%
      if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
    vn <- trimws(as.character(d$VariableName))
    keep <- if (identical(match_mode, "exact")) {
      tolower(vn) %in% tolower(vi)
    } else {
      pattern <- paste(
        paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")),
        collapse = "|"
      )
      grepl(pattern, vn, ignore.case = TRUE, perl = TRUE)
    }
    d <- d[keep %in% TRUE, , drop = FALSE]
    applied_filters <- c(
      applied_filters,
      paste0(if (identical(role, "modeled")) "Modeled VariableName: " else "VariableName: ",
             paste(sort(unique(vi)), collapse = ", "))
    )
  }
  stage("variable_name", d)
  if (!nrow(d)) {
    return(fail("variable_name", d, date_data, "No RAE rows matched the channel VariableName contract."))
  }

  exclude_regex <- function(data, column, patterns, label) {
    patterns <- unique(as.character(patterns %||% character(0)))
    patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
    if (!length(patterns)) return(data)
    if (!column %in% names(data)) {
      warnings <<- c(warnings, paste0(label, " filter could not be applied because ", column, " is missing."))
      return(data)
    }
    for (pattern in patterns) {
      data <- data[!grepl(pattern, as.character(data[[column]]), ignore.case = TRUE), , drop = FALSE]
    }
    applied_filters <<- c(applied_filters, paste0(label, " excludes: ", paste(patterns, collapse = ", ")))
    data
  }

  d <- exclude_regex(d, "VariableName", cfg$varname_exclude, "VariableName")
  d <- exclude_regex(d, "Campaign", cfg$campaign_exclude, "Campaign")
  d <- exclude_regex(d, "Outlet", cfg$outlet_exclude, "Outlet")
  d <- exclude_regex(d, "Creative", cfg$creative_exclude, "Creative")
  stage("channel_exclusions", d)
  if (!nrow(d)) {
    return(fail("channel_exclusions", d, date_data, "Channel exclusion filters removed every RAE row."))
  }

  segment_overrides <- segment_overrides %||% cfg$segment_overrides %||% list()
  has_geo_overrides <- length(segment_overrides) > 0L &&
    any(vapply(segment_overrides, function(o) {
      length(o$geography_exclude %||% character(0)) > 0L
    }, logical(1)))
  if (!has_geo_overrides) {
    d <- exclude_regex(d, "Geography", cfg$geography_exclude, "Geography")
  }
  stage("geography", d)
  if (!nrow(d)) return(fail("geography", d, date_data, "Geography filters removed every RAE row."))

  if (!is.null(schema_metadata) && !is.null(schema_metadata$name_lookup) &&
      length(cfg$analytical_varkeys %||% character(0)) > 0L) {
    long_result <- tryCatch(
      list(
        data = filter_to_analytical_varkey_combinations(d, cfg, schema_metadata),
        error = NULL
      ),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
    if (!is.null(long_result$error)) {
      return(finish(
        d[0, , drop = FALSE], date_data, "longitudinal",
        paste0("Useful longitudinal filters failed: ", long_result$error)
      ))
    }
    d <- long_result$data
    useful_filters <- cfg$useful_longitudinal_filters %||% list()
    if (length(useful_filters)) {
      applied_filters <- c(applied_filters, vapply(names(useful_filters), function(dim) {
        paste0(dim, ": ", paste(useful_filters[[dim]], collapse = ", "))
      }, character(1)))
    } else {
      applied_filters <- c(applied_filters, "Analytical longitudinal combinations")
    }
  }
  stage("longitudinal", d)
  if (!nrow(d)) {
    return(fail("longitudinal", d, date_data, "Useful longitudinal filters removed every RAE row."))
  }

  if (has_geo_overrides) {
    seg_ovr <- Filter(function(o) isTRUE(o$seg == 1L), segment_overrides)
    geo_exc <- if (length(seg_ovr)) {
      seg_ovr[[1]]$geography_exclude %||% character(0)
    } else {
      cfg$geography_exclude %||% character(0)
    }
    d <- exclude_regex(d, "Geography", geo_exc, "VOF geography")
  }
  stage("segment_geography", d)
  if (!nrow(d)) {
    return(fail("segment_geography", d, date_data, "VOF geography rules removed every RAE row."))
  }

  finish(d, date_data)
}

build_role_split_manifest <- function(d, cfg, update_label,
                                      start_date, end_date) {
  empty <- tibble::tibble(
    Role = character(), MetricRole = character(), SourceVariableName = character(),
    PairKey = character(), SplitKey = character(), PeriodScope = character(),
    SplitName = character(), VariableSplit = character(),
    PairedVariableSplit = character(), PairStatus = character()
  )
  if (is.null(d) || !nrow(d) ||
      !all(c("VariableName", "SplitName", ".__role", ".__pair_key", "Period") %in% names(d))) {
    return(empty)
  }

  start_date <- tryCatch(as.Date(start_date), error = function(e) as.Date(NA))
  end_date <- tryCatch(as.Date(end_date), error = function(e) as.Date(NA))
  period <- if (inherits(d$Period, "Date")) d$Period else as.Date(d$Period)
  scope <- rep(NA_character_, nrow(d))
  if (!is.na(start_date)) scope[period < start_date] <- "nonfocus"
  if (!is.na(start_date) && !is.na(end_date)) {
    scope[period >= start_date & period <= end_date] <- "focus"
  }
  if (is.na(start_date) && !is.na(end_date)) scope[period <= end_date] <- "focus"

  granularity_cols <- unique(setdiff(
    as.character(cfg$split_columns %||% character(0)),
    "VariableName"
  ))
  granularity_cols <- intersect(granularity_cols[nzchar(granularity_cols)], names(d))

  base <- data.frame(
    Role = as.character(d$.__role),
    SourceVariableName = trimws(as.character(d$VariableName)),
    PairKey = as.character(d$.__pair_key),
    SplitKey = as.character(d$.__split_key),
    PeriodScope = scope,
    SplitName = as.character(d$SplitName),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (col in granularity_cols) {
    values <- clean_split_part(d[[col]])
    values[is.na(values)] <- canonical_break_missing_part_value()
    base[[col]] <- values
  }
  base <- unique(base[
    base$Role %in% c("modeled", "for_indices") & !is.na(base$PeriodScope),
    , drop = FALSE
  ])
  if (!nrow(base)) return(empty)

  base$MetricRole <- vapply(base$SourceVariableName, function(x) {
    role <- infer_metric_role(NULL, x)
    if (is.na(role)) "" else role
  }, character(1))
  base$VariableSplit <- ifelse(
    base$PeriodScope == "focus",
    paste0(base$SplitName, "_", build_split_period_suffix(
      update_label,
      focus = TRUE,
      geo_label = cfg$geo_label %||% ""
    )),
    paste0(base$SplitName, "_", build_split_period_suffix(
      update_label,
      focus = FALSE,
      time_break_label = cfg$time_break_label %||% "",
      geo_label = cfg$geo_label %||% ""
    ))
  )
  base$PairedVariableSplit <- ""
  base$PairStatus <- ifelse(base$Role == "modeled", "Missing", "Counterpart")

  modeled_idx <- which(base$Role == "modeled")
  for_indices_idx <- which(base$Role == "for_indices")
  for (i in modeled_idx) {
    hits <- for_indices_idx[
      base$PairKey[for_indices_idx] == base$PairKey[[i]] &
        base$PeriodScope[for_indices_idx] == base$PeriodScope[[i]]
    ]
    if (length(hits) == 1L) {
      base$PairedVariableSplit[[i]] <- base$VariableSplit[[hits]]
      base$PairStatus[[i]] <- "Matched"
      base$PairedVariableSplit[[hits]] <- base$VariableSplit[[i]]
    } else if (length(hits) > 1L) {
      base$PairStatus[[i]] <- "Ambiguous"
    }
  }
  tibble::as_tibble(base)
}

split_manifest_core_columns <- function() {
  c(
    "Role", "MetricRole", "SourceVariableName", "PairKey", "SplitKey",
    "PeriodScope", "SplitName", "VariableSplit", "PairedVariableSplit",
    "PairStatus"
  )
}

collapse_manifest_granularity <- function(rows, granularity_cols) {
  if (is.null(rows) || !nrow(rows) || !length(granularity_cols)) return(rows)
  out <- rows[1, , drop = FALSE]
  for (col in intersect(granularity_cols, names(rows))) {
    values <- clean_split_part(rows[[col]])
    values[is.na(values)] <- canonical_break_missing_part_value()
    values <- unique(values)
    out[[col]] <- if (length(values) == 1L) values[[1]] else "Multiple"
  }
  out
}

build_modeled_for_indices_totals <- function(act_all, cost_all, cfg,
                                            split_manifest = NULL) {
  if (is.null(split_manifest) || !nrow(split_manifest) ||
      !all(c("Role", "VariableSplit") %in% names(split_manifest))) {
    return(build_activity_spend(act_all, cost_all, cfg))
  }
  modeled <- split_manifest %>%
    dplyr::filter(.data$Role == "modeled") %>%
    dplyr::distinct(.data$VariableSplit, .data$MetricRole,
                    .data$PairedVariableSplit, .keep_all = TRUE)
  if (!nrow(modeled)) return(build_activity_spend(act_all, cost_all, cfg))

  act_totals <- if (!is.null(act_all) && nrow(act_all) &&
                    all(c("VariableSplit", "total_activity") %in% names(act_all))) {
    act_all %>%
      dplyr::group_by(.data$VariableSplit) %>%
      dplyr::summarise(total_activity = sum(.data$total_activity, na.rm = TRUE),
                       .groups = "drop")
  } else tibble::tibble(VariableSplit = character(), total_activity = numeric())
  spend_totals <- if (!is.null(cost_all) && nrow(cost_all) &&
                      all(c("VariableSplit", "total_spend") %in% names(cost_all))) {
    cost_all %>%
      dplyr::group_by(.data$VariableSplit) %>%
      dplyr::summarise(total_spend = sum(.data$total_spend, na.rm = TRUE),
                       .groups = "drop")
  } else tibble::tibble(VariableSplit = character(), total_spend = numeric())

  activity_lookup <- stats::setNames(act_totals$total_activity, act_totals$VariableSplit)
  spend_lookup <- stats::setNames(spend_totals$total_spend, spend_totals$VariableSplit)
  value_at <- function(lookup, key) {
    if (is.na(key) || !nzchar(key) || !key %in% names(lookup)) return(NA_real_)
    as.numeric(lookup[[key]])
  }

  out <- modeled %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_activity = if (.data$MetricRole == "activity")
        value_at(activity_lookup, .data$VariableSplit) else
        value_at(activity_lookup, .data$PairedVariableSplit),
      total_spend = if (.data$MetricRole == "spend")
        value_at(spend_lookup, .data$VariableSplit) else
        value_at(spend_lookup, .data$PairedVariableSplit)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      VariableSplit = .data$VariableSplit,
      total_activity = .data$total_activity,
      total_spend = .data$total_spend,
      Channel = cfg$channel_name %||% cfg$model_variable %||% "",
      MainModelVariableName = cfg$model_variable %||% ""
    )
  out
}

# splits_summary
splits_summary <- function(df, type = "activity") {
  if (is.null(df) || nrow(df) == 0)
    return(tibble(VariableSplit = character()))

  id_cols    <- intersect(c("Geography", "Product", "Period", "BP_Year"),
                          names(df))
  split_cols <- setdiff(names(df)[sapply(df, is.numeric)], id_cols)
  if (!length(split_cols)) return(tibble(VariableSplit = character()))

  result <- bind_rows(lapply(split_cols, function(col) {
    vals     <- df[[col]]
    non_zero <- vals[!is.na(vals) & vals > 0]
    if (!length(non_zero)) return(NULL)

    active_rle <- rle(!is.na(vals) & vals > 0)
    min_consec <- if (any(active_rle$values))
      max(active_rle$lengths[active_rle$values]) else 0L
    max_idx <- round(max(non_zero) / sum(non_zero), 4)

    if (type == "activity") {
      tibble(
        VariableSplit         = col,
        total_activity        = sum(vals, na.rm = TRUE),
        pct_total_activity    = NA_real_,
        max_index             = max_idx,
        max                   = max(non_zero),
        max_no_outlier        = as.numeric(quantile(non_zero, 0.95)),
        num_weeks_activity    = sum(!is.na(vals) & vals > 0),
        min_consecutive_weeks = as.numeric(min_consec),
        sd                    = if (length(non_zero) > 1) sd(non_zero) else 0,
        min                   = min(non_zero),
        quartile_1            = as.numeric(quantile(non_zero, 0.25)),
        median                = as.numeric(quantile(non_zero, 0.50)),
        quartile_3            = as.numeric(quantile(non_zero, 0.75))
      )
    } else {
      tibble(
        VariableSplit         = col,
        total_spend           = sum(vals, na.rm = TRUE),
        pct_total_spend       = NA_real_,
        max_index             = max_idx,
        max                   = max(non_zero),
        max_no_outlier        = as.numeric(quantile(non_zero, 0.95)),
        num_weeks_spend       = sum(!is.na(vals) & vals > 0),
        min_consecutive_weeks = as.numeric(min_consec),
        sd                    = if (length(non_zero) > 1) sd(non_zero) else 0,
        min                   = min(non_zero),
        quartile_1            = as.numeric(quantile(non_zero, 0.25)),
        median                = as.numeric(quantile(non_zero, 0.50)),
        quartile_3            = as.numeric(quantile(non_zero, 0.75))
      )
    }
  }))

  if (!"VariableSplit" %in% names(result))
    return(tibble(VariableSplit = character()))
  result
}

# apply_single_merge
# New function added to support interactive merging in mod_process.
# Merges selected splits in the RAG and updates all diagnostic structures.
apply_single_merge <- function(res, merge_entry, cfg, notify = TRUE) {
  new_name        <- merge_entry$new_name
  selected_splits <- unlist(merge_entry$merged)
  selected_splits <- unique(trimws(as.character(selected_splits)))
  selected_splits <- selected_splits[!is.na(selected_splits) & nzchar(selected_splits)]
  if (!length(selected_splits)) return(res)
  view_filter     <- merge_entry$view %||% "focus"
  act_kw          <- cfg$activity_keyword %||% "Impressions"
  spend_kw        <- cfg$spend_keyword    %||% "Spend"
  merge_metric    <- normalize_model_metric(merge_entry$metric %||%
                                             cfg$model_metric %||% "activity")
  is_spend_merge  <- identical(merge_metric, "spend")

  normalize_cost_diagnoses <- function(df) {
    needed <- list(
      VariableSplit = character(),
      total_spend = numeric(),
      pct_total_spend = numeric(),
      max_index = numeric(),
      max = numeric(),
      max_no_outlier = numeric(),
      num_weeks_spend = numeric(),
      min_consecutive_weeks = numeric(),
      sd = numeric(),
      min = numeric(),
      quartile_1 = numeric(),
      median = numeric(),
      quartile_3 = numeric(),
      period = character(),
      seg = integer(),
      model_var = character()
    )
    if (is.null(df)) df <- tibble::tibble()
    df <- tibble::as_tibble(df)
    n <- nrow(df)
    for (nm in names(needed)) {
      if (!nm %in% names(df)) {
        prototype <- needed[[nm]]
        df[[nm]] <- if (n == 0) prototype else rep(NA, n)
      }
    }
    df
  }

  # Guard: ensure diagnostic tibbles have VariableSplit column
  if (is.null(res$act_diagnoses) ||
      !"VariableSplit" %in% names(res$act_diagnoses))
    res$act_diagnoses <- tibble::tibble(
      VariableSplit = character(), period = character())
  res$cost_diagnoses <- normalize_cost_diagnoses(res$cost_diagnoses)
  if (is.null(res$activity_spend) ||
      !"VariableSplit" %in% names(res$activity_spend))
    res$activity_spend <- tibble::tibble(
      VariableSplit = character(), total_activity = numeric(),
      total_spend = numeric(), Channel = character(),
      MainModelVariableName = character())
  if (is.null(res$side_mapping) ||
      !"VariableSplit" %in% names(res$side_mapping))
    res$side_mapping <- tibble::tibble(VariableSplit = character())

  split_time_suffix <- function(x) {
    x <- as.character(x)
    m <- regexpr("_Before\\s+.*$", x, ignore.case = TRUE, perl = TRUE)
    ifelse(m > 0, substring(x, m), "")
  }

  split_without_time <- function(x) {
    stringr::str_remove(as.character(x), stringr::regex("_Before\\s+.*$", ignore_case = TRUE))
  }

  split_signature <- function(x) {
    base <- split_without_time(x)
    suffix <- tolower(trimws(split_time_suffix(x)))
    vars <- unique(trimws(as.character(cfg$varname_include %||% character(0))))
    vars <- vars[!is.na(vars) & nzchar(vars)]
    vars <- vars[order(nchar(vars), decreasing = TRUE)]

    matched_var <- ""
    remainder <- base
    for (vn in vars) {
      vn_l <- tolower(vn)
      base_l <- tolower(base)
      if (identical(base_l, vn_l)) {
        matched_var <- vn
        remainder <- ""
        break
      }
      prefix <- paste0(vn, "_")
      if (startsWith(base_l, tolower(prefix))) {
        matched_var <- vn
        remainder <- substring(base, nchar(prefix) + 1L)
        break
      }
    }

    if (!nzchar(matched_var)) {
      pieces <- strsplit(base, "_", fixed = TRUE)[[1]]
      matched_var <- pieces[1] %||% ""
      remainder <- if (length(pieces) > 1) paste(pieces[-1], collapse = "_") else ""
    }

    parts <- strsplit(remainder, "_", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    paste(
      tolower(trimws(matched_var)),
      suffix,
      paste(sort(tolower(parts)), collapse = "|"),
      sep = "||"
    )
  }

  closest_split_examples <- function(x, candidates, n = 3L) {
    if (!length(candidates)) return(character(0))
    d <- utils::adist(x, candidates, ignore.case = TRUE)
    candidates[order(as.numeric(d))[seq_len(min(n, length(candidates)))]]
  }

  resolve_merge_splits <- function(requested, rag_split_names, view_filter) {
    requested <- unique(trimws(as.character(requested)))
    requested <- requested[!is.na(requested) & nzchar(requested)]
    exact <- intersect(requested, rag_split_names)
    unresolved <- setdiff(requested, exact)

    candidates <- if (identical(view_filter, "focus")) {
      rag_split_names[!grepl("Before", rag_split_names, fixed = TRUE)]
    } else {
      rag_split_names[grepl("Before", rag_split_names, fixed = TRUE)]
    }
    if (!length(candidates)) candidates <- rag_split_names

    mapped <- character(0)
    ambiguous <- character(0)
    missing <- character(0)
    if (length(unresolved)) {
      cand_sig <- vapply(candidates, split_signature, character(1))
      for (nm in unresolved) {
        sig <- split_signature(nm)
        hits <- candidates[cand_sig == sig]
        hits <- hits[!is.na(hits) & nzchar(hits)]
        if (length(hits) == 1L) {
          mapped <- c(mapped, hits)
        } else if (length(hits) > 1L) {
          ambiguous <- c(ambiguous, nm)
        } else {
          missing <- c(missing, nm)
        }
      }
    }

    list(
      cols = unique(c(exact, mapped)),
      missing = missing,
      ambiguous = ambiguous,
      candidates = candidates
    )
  }

  # Find matching RAG columns
  id_cols         <- intersect(c(res$cross_cols, "Period"), names(res$rag))
  rag_split_names <- setdiff(
    names(res$rag)[sapply(as.data.frame(res$rag), is.numeric)], id_cols)
  resolved <- resolve_merge_splits(selected_splits, rag_split_names, view_filter)
  rag_cols <- resolved$cols
  manifest <- res$split_manifest %||% tibble::tibble()

  # Merges are defined on the modeled side. If the user selected the
  # ForIndices view, translate those columns through the canonical
  # manifest before changing the RAG. The paired side is merged below using
  # the same manifest relationship.
  if (nrow(manifest) &&
      all(c("Role", "VariableSplit", "PairedVariableSplit") %in% names(manifest)) &&
      length(rag_cols)) {
    for_indices_lookup <- manifest %>%
      dplyr::filter(.data$Role == "for_indices") %>%
      dplyr::transmute(
        for_indices_split = .data$VariableSplit,
        modeled_split = .data$PairedVariableSplit
      ) %>%
      dplyr::filter(
        !is.na(.data$for_indices_split), nzchar(.data$for_indices_split),
        !is.na(.data$modeled_split), nzchar(.data$modeled_split)
      ) %>%
      dplyr::distinct(.data$for_indices_split, .keep_all = TRUE)

    if (nrow(for_indices_lookup) &&
        all(rag_cols %in% for_indices_lookup$for_indices_split)) {
      translated <- for_indices_lookup$modeled_split[
        match(rag_cols, for_indices_lookup$for_indices_split)
      ]
      if (length(translated) == length(rag_cols) &&
          all(translated %in% rag_split_names)) {
        rag_cols <- unique(translated)
      }
    }
  }

  merge_status <- function(applied, missing = character(), ambiguous = character(),
                           examples = character(), matched = rag_cols) {
    list(
      applied = isTRUE(applied),
      new_name = new_name,
      view = view_filter,
      metric = merge_metric,
      requested = selected_splits,
      matched = matched,
      missing = missing,
      ambiguous = ambiguous,
      closest_examples = examples,
      matched_count = length(matched),
      requested_count = length(unique(selected_splits))
    )
  }

  if (length(rag_cols) != length(unique(selected_splits))) {
    unresolved <- c(resolved$missing, resolved$ambiguous)
    unresolved <- unresolved[!is.na(unresolved) & nzchar(unresolved)]
    sample_missing <- paste(utils::head(unresolved, 2), collapse = " | ")
    examples <- if (length(unresolved)) {
      paste(closest_split_examples(unresolved[1], resolved$candidates), collapse = " | ")
    } else ""
    if (isTRUE(notify)) {
      showNotification(
        paste0(
          "Merge '", new_name, "' not applied. Missing: ", sample_missing,
          ". View: ", view_filter,
          if (nzchar(examples)) paste0(". Closest RAG: ", examples) else ""
        ),
        type = "warning", duration = 8)
    }
    attr(res, "merge_status") <- merge_status(
      applied = FALSE,
      missing = resolved$missing,
      ambiguous = resolved$ambiguous,
      examples = if (length(unresolved)) closest_split_examples(unresolved[1], resolved$candidates) else character(0),
      matched = rag_cols
    )
    return(res)
  }

  modeled_role <- normalize_model_metric(
    res$modeled_role %||% cfg$modeled_role %||% merge_metric
  )
  paired_rag_cols <- character(0)
  if (nrow(manifest) &&
      all(c("Role", "VariableSplit", "PairedVariableSplit") %in% names(manifest))) {
    paired_rag_cols <- manifest %>%
      dplyr::filter(.data$Role == "modeled",
                    .data$VariableSplit %in% rag_cols) %>%
      dplyr::pull(.data$PairedVariableSplit) %>%
      unique()
    paired_rag_cols <- paired_rag_cols[!is.na(paired_rag_cols) & nzchar(paired_rag_cols)]
  }

  derived_pair_cands <- if (identical(modeled_role, "spend")) {
    stringr::str_replace_all(
      rag_cols, stringr::regex(spend_kw, ignore_case = TRUE), act_kw
    )
  } else {
    stringr::str_replace_all(
      rag_cols, stringr::regex(act_kw, ignore_case = TRUE), spend_kw
    )
  }
  paired_rag_cols <- unique(c(
    paired_rag_cols,
    intersect(derived_pair_cands, rag_split_names)
  ))
  paired_rag_cols <- setdiff(paired_rag_cols, rag_cols)

  new_paired_name <- if (identical(modeled_role, "spend")) {
    stringr::str_replace_all(
      new_name, stringr::regex(spend_kw, ignore_case = TRUE), act_kw
    )
  } else {
    stringr::str_replace_all(
      new_name, stringr::regex(act_kw, ignore_case = TRUE), spend_kw
    )
  }
  if (identical(new_paired_name, new_name)) {
    pair_kw <- if (identical(modeled_role, "spend")) act_kw else spend_kw
    new_paired_name <- paste0(new_name, "_", pair_kw)
  }

  activity_splits <- if (identical(modeled_role, "activity")) rag_cols else paired_rag_cols
  spend_splits <- if (identical(modeled_role, "spend")) rag_cols else paired_rag_cols
  new_activity_name <- if (identical(modeled_role, "activity")) new_name else new_paired_name
  new_spend_name <- if (identical(modeled_role, "spend")) new_name else new_paired_name

 # RAG modeled variable
  selected_splits <- rag_cols
  new_rag             <- res$rag
  new_rag[[new_name]] <- rowSums(new_rag[, rag_cols, drop = FALSE], na.rm = TRUE)
  new_rag             <- new_rag[, setdiff(names(new_rag), setdiff(rag_cols, new_name))]

 # RAG ForIndices variable
  paired_rag_cols <- intersect(paired_rag_cols, names(new_rag))
  if (length(paired_rag_cols) > 0) {
    new_rag[[new_paired_name]] <- rowSums(
      new_rag[, paired_rag_cols, drop = FALSE], na.rm = TRUE)
    new_rag <- new_rag[, setdiff(
      names(new_rag),
      setdiff(paired_rag_cols, new_paired_name)
    )]
  }

  # act_diagnoses
  selected_diag <- res$act_diagnoses %>%
    dplyr::filter(VariableSplit %in% activity_splits, period == view_filter)
  if (nrow(selected_diag) == 0 && nrow(res$act_diagnoses) > 0)
    selected_diag <- res$act_diagnoses %>%
    dplyr::filter(VariableSplit %in% activity_splits)
  if (nrow(selected_diag) == 0) {
    rag_vals <- if (new_activity_name %in% names(new_rag))
      as.data.frame(new_rag)[[new_activity_name]] else numeric(0)
    non_zero <- rag_vals[!is.na(rag_vals) & rag_vals > 0]
    if (length(non_zero) > 0)
      selected_diag <- tibble::tibble(
        VariableSplit = new_activity_name, total_activity = sum(non_zero),
        pct_total_activity = NA_real_, num_weeks_activity = length(non_zero),
        max_index = NA_real_, min_consecutive_weeks = NA_real_,
        sd = if (length(non_zero) > 1) sd(non_zero) else 0,
        min = min(non_zero), quartile_1 = as.numeric(quantile(non_zero, 0.25)),
        median = as.numeric(quantile(non_zero, 0.50)),
        quartile_3 = as.numeric(quantile(non_zero, 0.75)),
        max_no_outlier = as.numeric(quantile(non_zero, 0.95)),
        max = max(non_zero), period = view_filter, seg = 1L,
        model_var = cfg$model_variable %||% "")
  }

  finite_or <- function(x, fun, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (!length(x)) return(default)
    fun(x)
  }

  merged_act <- if (nrow(selected_diag) > 0) {
    tibble::tibble(
      VariableSplit         = new_activity_name,
      total_activity        = sum(selected_diag$total_activity,        na.rm = TRUE),
      pct_total_activity    = NA_real_,
      num_weeks_activity    = finite_or(selected_diag$num_weeks_activity, max, 0),
      max_index             = NA_real_,
      min_consecutive_weeks = finite_or(selected_diag$min_consecutive_weeks, max),
      sd             = NA_real_,
      min            = finite_or(selected_diag$min, min),
      quartile_1     = finite_or(selected_diag$quartile_1, mean),
      median         = finite_or(selected_diag$median, mean),
      quartile_3     = finite_or(selected_diag$quartile_3, mean),
      max_no_outlier = finite_or(selected_diag$max_no_outlier, max),
      max            = finite_or(selected_diag$max, max)
    ) %>% dplyr::bind_cols(
      selected_diag %>% dplyr::slice(1) %>%
        dplyr::select(dplyr::any_of(c("seg", "period", "model_var"))))
  } else NULL

  new_act_diag <- res$act_diagnoses %>%
    dplyr::filter(!VariableSplit %in% activity_splits)
  if (!is.null(merged_act))
    new_act_diag <- new_act_diag %>%
    dplyr::bind_rows(merged_act) %>%
    dplyr::group_by(period) %>%
    dplyr::mutate(
      grand_p            = sum(total_activity, na.rm = TRUE),
      pct_total_activity = round(total_activity / pmax(grand_p, 1) * 100, 4)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grand_p)

  merged_sm <- res$side_mapping %>%
    dplyr::filter(VariableSplit %in% selected_splits) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(VariableSplit = new_name)
  new_side_map <- res$side_mapping %>%
    dplyr::filter(!VariableSplit %in% selected_splits) %>%
    dplyr::bind_rows(merged_sm)

  stored_spend <- unlist(merge_entry$spend_merged %||% character(0))
  matching_cost <- intersect(
    unique(c(spend_splits, stored_spend)),
    res$cost_diagnoses$VariableSplit
  )

  new_cost_diag <- tryCatch({
    if (!length(matching_cost)) {
      res$cost_diagnoses
    } else {
      sel_cost <- res$cost_diagnoses %>%
        dplyr::filter(VariableSplit %in% matching_cost)
      mc_row <- tibble::tibble(
        VariableSplit         = new_spend_name,
        total_spend           = sum(sel_cost$total_spend,          na.rm = TRUE),
        pct_total_spend       = NA_real_,
        num_weeks_spend       = finite_or(sel_cost$num_weeks_spend, max, 0),
        min_consecutive_weeks = finite_or(sel_cost$min_consecutive_weeks, max),
        sd             = NA_real_,
        min            = finite_or(sel_cost$min, min),
        quartile_1     = finite_or(sel_cost$quartile_1, mean),
        median         = finite_or(sel_cost$median, mean),
        quartile_3     = finite_or(sel_cost$quartile_3, mean),
        max_no_outlier = finite_or(sel_cost$max_no_outlier, max),
        max            = finite_or(sel_cost$max, max),
        max_index      = NA_real_,
        period    = sel_cost$period[1]    %||% "focus",
        seg       = sel_cost$seg[1]       %||% NA_integer_,
        model_var = sel_cost$model_var[1] %||% NA_character_)
      cc <- res$cost_diagnoses %>%
        dplyr::filter(!VariableSplit %in% matching_cost) %>%
        dplyr::bind_rows(mc_row)
      if ("period" %in% names(cc))
        cc %>% dplyr::group_by(period) %>%
        dplyr::mutate(gp = sum(total_spend, na.rm = TRUE),
                      pct_total_spend = round(total_spend / pmax(gp, 1) * 100, 4)) %>%
        dplyr::ungroup() %>% dplyr::select(-gp)
      else {
        gt <- sum(cc$total_spend, na.rm = TRUE)
        cc %>% dplyr::mutate(
          pct_total_spend = round(total_spend / pmax(gt, 1) * 100, 4))
      }
    }
  }, error = \(e) res$cost_diagnoses)
  new_cost_diag <- normalize_cost_diagnoses(new_cost_diag)

  new_manifest <- manifest
  if (nrow(new_manifest) && "VariableSplit" %in% names(new_manifest)) {
    remove_splits <- unique(c(rag_cols, paired_rag_cols))
    templates <- new_manifest %>%
      dplyr::filter(.data$VariableSplit %in% remove_splits)
    new_manifest <- new_manifest %>%
      dplyr::filter(!.data$VariableSplit %in% remove_splits)

    granularity_cols <- setdiff(
      names(new_manifest),
      split_manifest_core_columns()
    )

    modeled_template <- templates %>%
      dplyr::filter(.data$Role == "modeled")
    modeled_template <- collapse_manifest_granularity(
      modeled_template,
      granularity_cols
    )
    if (!nrow(modeled_template)) {
      modeled_template <- tibble::tibble(
        Role = "modeled", MetricRole = modeled_role,
        SourceVariableName = cfg$modeled_variable %||% cfg$model_variable %||% "",
        PairKey = paste0("merge||", tolower(new_name)), SplitKey = new_name,
        PeriodScope = view_filter, SplitName = new_name,
        VariableSplit = new_name, PairedVariableSplit = "",
        PairStatus = if (length(paired_rag_cols)) "Matched" else "Missing"
      )
    } else {
      modeled_template$SplitName <- new_name
      modeled_template$VariableSplit <- new_name
      modeled_template$SplitKey <- new_name
      modeled_template$PairKey <- paste0("merge||", tolower(new_name))
      modeled_template$MetricRole <- modeled_role
      modeled_template$PairedVariableSplit <- if (length(paired_rag_cols)) new_paired_name else ""
      modeled_template$PairStatus <- if (length(paired_rag_cols)) "Matched" else "Missing"
    }
    add_rows <- modeled_template

    if (length(paired_rag_cols)) {
      for_indices_template <- templates %>%
        dplyr::filter(.data$Role == "for_indices")
      for_indices_template <- collapse_manifest_granularity(
        for_indices_template,
        granularity_cols
      )
      if (!nrow(for_indices_template)) for_indices_template <- modeled_template
      for_indices_template$Role <- "for_indices"
      for_indices_template$MetricRole <- if (identical(modeled_role, "spend")) "activity" else "spend"
      for_indices_template$SplitName <- new_paired_name
      for_indices_template$VariableSplit <- new_paired_name
      for_indices_template$SplitKey <- new_paired_name
      for_indices_template$PairKey <- paste0("merge||", tolower(new_name))
      for_indices_template$PairedVariableSplit <- new_name
      for_indices_template$PairStatus <- "Counterpart"
      add_rows <- dplyr::bind_rows(add_rows, for_indices_template)
    }
    new_manifest <- dplyr::bind_rows(new_manifest, add_rows)
  }

  model_metric <- modeled_role
  model_diagnoses <- if (identical(model_metric, "spend")) new_cost_diag else new_act_diag
  modeled_names <- if (nrow(new_manifest) &&
                       all(c("Role", "VariableSplit") %in% names(new_manifest))) {
    unique(new_manifest$VariableSplit[new_manifest$Role == "modeled"])
  } else character(0)
  if (length(modeled_names) && "VariableSplit" %in% names(model_diagnoses)) {
    model_diagnoses <- model_diagnoses %>%
      dplyr::filter(.data$VariableSplit %in% modeled_names)
  }
  model_side_map <- build_side_mapping_from_manifest(
    new_manifest,
    cfg$model_variable %||% ""
  )
  if (!nrow(model_side_map)) model_side_map <- build_side_mapping(model_diagnoses)
  new_act_spend <- build_modeled_for_indices_totals(
    new_act_diag, new_cost_diag, cfg, new_manifest
  )

  out <- list(rag            = new_rag,
              cross_cols     = res$cross_cols,
              ref_cross      = res$ref_cross,
              activity_spend = new_act_spend,
              side_mapping   = model_side_map,
              act_diagnoses  = new_act_diag,
              cost_diagnoses = new_cost_diag,
              model_diagnoses = model_diagnoses,
              model_metric = model_metric,
              modeled_role = model_metric,
              role_pair_status = res$role_pair_status %||% cfg$role_pair_status %||% "Missing",
              split_manifest = new_manifest)
  attr(out, "merge_status") <- merge_status(applied = TRUE, matched = rag_cols)
  out
}

# =============================================================================
# process_channel
# =============================================================================
# Synchronous processing entry point. It builds both metric roles, applies
# channel dimensions and saved merges, then returns results and a role manifest.
process_channel <- function(all_rags,
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
                            schema_metadata   = NULL,
                            progress_cb       = NULL) {

  pb <- function(detail, value = NULL) {
    if (!is.null(progress_cb)) progress_cb(detail, value)
  }

  pb("Preparing data...", 0.05)

  all_rags   <- as.data.frame(all_rags)
  analytical <- as.data.frame(analytical)
  dates_df   <- as.data.frame(dates_df)

  if (is.null(all_rags)) stop("All RAGs data not uploaded.")

 # Pre-convert all date params once
  min_p   <- if (!is.null(min_period))
    tryCatch(as.Date(min_period), error = \(e) as.Date(NA)) else as.Date(NA)
  max_p   <- if (!is.null(max_period))
    tryCatch(as.Date(max_period), error = \(e) as.Date(NA)) else as.Date(NA)
  start_d <- as.Date(start_report_date)
  end_d   <- as.Date(end_report_date)

  an_min_date <- as.Date(NA)
  an_max_date <- as.Date(NA)
  if (nrow(dates_df) > 0) {
    analytical_periods <- parse_period_robust(dates_df$Period)
    analytical_periods <- analytical_periods[!is.na(analytical_periods)]
    if (length(analytical_periods)) {
      an_min_date <- min(analytical_periods)
      an_max_date <- max(analytical_periods)
    }
  }
  effective_min <- suppressWarnings(max(c(min_p, an_min_date), na.rm = TRUE))
  effective_max <- suppressWarnings(min(c(max_p, an_max_date), na.rm = TRUE))
  if (!is.finite(as.numeric(effective_min))) effective_min <- as.Date(NA)
  if (!is.finite(as.numeric(effective_max))) effective_max <- as.Date(NA)

  pb("Filtering source data...", 0.10)
  effective_rae <- filter_effective_channel_rae(
    all_rags = all_rags,
    cfg = cfg,
    min_period = effective_min,
    max_period = effective_max,
    schema_metadata = schema_metadata,
    segment_overrides = segment_overrides,
    role = "all"
  )
  if (nzchar(effective_rae$failure_stage %||% "")) {
    stop(effective_rae$reason %||% "No data remained after channel filters.")
  }
  source_data <- effective_rae$date_data
  d_prefilt <- data.table::as.data.table(effective_rae$data)

  cross_id   <- c(cross_cols, "Period")
  join_key   <- cross_id
  id_protect <- cross_id

 # rag_base via data.table
  rag_base_dt <- unique(
    data.table::as.data.table(source_data)[, cross_id, with = FALSE])
  data.table::setorderv(rag_base_dt, "Period")
  rag_base <- as.data.frame(rag_base_dt)

 # ref_cross_key from rag_base (smaller)
  cross_data_rb <- rag_base[, cross_cols, drop = FALSE]
  cross_key_rb  <- do.call(paste, c(as.list(cross_data_rb), list(sep = " / ")))
  ref_cross_key <- sort(unique(cross_key_rb))[1]

  model_var <- cfg$model_variable %||% ""
  s_beg     <- c(as.Date(NA_character_))
  s_end     <- c(end_d)

  rag_joins <- list()
  act_rows  <- list()
  cost_rows <- list()
  split_manifest <- tibble::tibble()

 # Segment loop
  pb("Building splits...", 0.20)

  d <- data.table::copy(d_prefilt)

  role_vars <- resolve_role_rae_variables(
    unique(d$VariableName %||% character(0)),
    cfg
  )
  d[, `.__role` := dplyr::case_when(
    tolower(trimws(VariableName)) %in% tolower(role_vars$modeled) ~ "modeled",
    tolower(trimws(VariableName)) %in% tolower(role_vars$for_indices) ~ "for_indices",
    TRUE ~ "other"
  )]

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
    split_dims <- setdiff(intersect(split_cols_technical, names(d)), "VariableName")
    d[, `.__split_key` := if (length(split_dims))
      build_split_name_from_columns(d, split_dims, fallback_col = ".__missing")
      else "__all__"]
    d[, `.__pair_key` := paste(
      metric_base_name(VariableName),
      `.__split_key`,
      sep = "||"
    )]

    modeled_pair_keys <- unique(d[`.__role` == "modeled", `.__pair_key`])
    if ((cfg$role_pair_status %||% "") %in% c("Matched", "Partial") &&
        length(modeled_pair_keys)) {
      d <- d[`.__role` != "for_indices" | `.__pair_key` %in% modeled_pair_keys]
    }
    d[, SplitName := build_split_name_from_columns(d, split_cols_technical)]

    split_manifest <- build_role_split_manifest(
      d,
      cfg = cfg,
      update_label = update_label,
      start_date = start_d,
      end_date = end_d
    )

 # Pivot wide
    lhs    <- paste(cross_id, collapse = " + ")
    d_wide <- data.table::dcast(d,
                                as.formula(paste(lhs, "~ SplitName")),
                                value.var = "VariableValue",
                                fun.aggregate = sum, fill = 0)
    d_wide <- merge(rag_base_dt, d_wide, by = cross_id, all.x = TRUE)

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

 # Assemble RAG
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
  model_metric <- normalize_model_metric(
    cfg$modeled_role %||% cfg$model_metric %||% "activity"
  )
  model_diagnoses <- if (identical(model_metric, "spend")) cost_all else act_all
  modeled_splits <- if (nrow(split_manifest) &&
                        all(c("Role", "VariableSplit") %in% names(split_manifest))) {
    unique(split_manifest$VariableSplit[split_manifest$Role == "modeled"])
  } else character(0)
  if (length(modeled_splits) && "VariableSplit" %in% names(model_diagnoses)) {
    model_diagnoses <- model_diagnoses %>%
      dplyr::filter(.data$VariableSplit %in% modeled_splits)
  }

  list(
    rag            = rag,
    cross_cols     = cross_cols,
    ref_cross      = ref_cross_key,
    activity_spend = build_modeled_for_indices_totals(
      act_all, cost_all, cfg, split_manifest
    ),
    side_mapping   = {
      sm <- build_side_mapping_from_manifest(split_manifest, model_var)
      if (nrow(sm)) sm else build_side_mapping(model_diagnoses)
    },
    act_diagnoses  = act_all,
    cost_diagnoses = cost_all,
    model_diagnoses = model_diagnoses,
    model_metric = model_metric,
    modeled_role = model_metric,
    role_pair_status = cfg$role_pair_status %||% "Missing",
    split_manifest = split_manifest
  )
}




