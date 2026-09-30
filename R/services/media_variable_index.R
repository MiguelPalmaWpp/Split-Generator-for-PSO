# Build the Media Variable Index from explicit source data and metadata.

build_media_index <- function(main_data, analytical, vof_df, model_details,
                              channels_rois = NULL, cross_cols = "Geography",
                              keyword_dict = MEDIA_KEYWORD_DICT,
                              schema_metadata = NULL) {
  main_data <- clean_data_columns(main_data)
  analytical <- clean_data_columns(analytical)
  vof_df <- clean_data_columns(vof_df)
  model_details <- clean_data_columns(model_details)
  channels_rois <- clean_data_columns(channels_rois)

  channels <- list()
  connection_rows <- list()
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

 # Detect VOF geo column (Geography or Geographies)
  detect_geo_col <- function(df) {
    if ("Geography"   %in% names(df)) return("Geography")
    if ("Geographies" %in% names(df)) return("Geographies")
    return(NULL)
  }

 # Step 1: IN/FIXED variables
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
  details_all_vars <- if (!is.null(model_details) &&
                          "VariableName" %in% names(model_details)) {
    vals <- unique(trimws(as.character(model_details$VariableName)))
    vals[!is.na(vals) & nzchar(vals)]
  } else character(0)

 # Step 2: validate + filter VOF
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
  details_all_norm <- normalize_model_var(details_all_vars)
  inactive_vof_main_norm <- setdiff(
    intersect(normalize_model_var(vof_df$MainModelVariableName), details_all_norm),
    in_model_norm
  )
  inactive_vof_analytical_norm <- normalize_model_var(
    vof_df$AnalyticalVariableName[
      normalize_model_var(vof_df$MainModelVariableName) %in% inactive_vof_main_norm
    ]
  )
  details_type_lookup <- if (!is.null(model_details) &&
                             all(c("Type", "VariableName") %in% names(model_details))) {
    details_types <- split(
      trimws(as.character(model_details$Type)),
      normalize_model_var(model_details$VariableName)
    )
    vapply(details_types, function(x) {
      x <- unique(x[!is.na(x) & nzchar(x)])
      paste(x, collapse = " | ")
    }, character(1))
  } else setNames(character(0), character(0))
  details_active_lookup <- if (length(details_type_lookup)) {
    setNames(
      vapply(strsplit(details_type_lookup, " \\| ", fixed = FALSE), function(x) {
        any(stringr::str_detect(
          stringr::str_to_lower(trimws(x)),
          "\\b(in|fixed)\\b"
        ))
      }, logical(1)),
      names(details_type_lookup)
    )
  } else setNames(logical(0), character(0))
  analytical_model_cols <- if (!is.null(analytical)) {
    cross_id_cols <- c(cross_cols, "Period", "BP_Year")
    setdiff(names(analytical)[sapply(analytical, is.numeric)], cross_id_cols)
  } else character(0)
  analytical_model_norm <- normalize_model_var(analytical_model_cols)
  vof_filtered <- vof_df %>%
    dplyr::mutate(
      .details_has_main = .mv_norm %in% details_all_norm,
      .details_main_active = .mv_norm %in% in_model_norm,
      .model_details_type = unname(details_type_lookup[.mv_norm]),
      .model_details_type = ifelse(is.na(.model_details_type), "", .model_details_type),
      .model_details_active = unname(details_active_lookup[.mv_norm]),
      .model_details_active = ifelse(is.na(.model_details_active), FALSE, .model_details_active),
      .analytical_norm = normalize_model_var(AnalyticalVariableName),
      .details_analytical_active = .analytical_norm %in% in_model_norm,
      .analytical_exists = AnalyticalVariableName %in% analytical_model_cols |
        .analytical_norm %in% analytical_model_norm
    ) %>%
    dplyr::filter(
      (.details_has_main & .details_main_active) |
        (!.details_has_main & (.details_analytical_active | .analytical_exists))
    )
  if (!nrow(vof_filtered))
    stop("No VOF rows matched ModelDetails Type='IN'/'FIXED'.")

  role_map <- resolve_modeled_for_indices_roles(
    vof_df = vof_df,
    model_details = model_details,
    modeled_names = unique(vof_filtered$MainModelVariableName),
    keyword_dict = keyword_dict
  )

 # Step 3: coverage info
  vk_info <- build_var_key(main_data,
                           unique(vof_filtered$AnalyticalVariableName))

 # Step 4: ROI lookup
  roi_lookup <- NULL
  if (!is.null(channels_rois) &&
      "MainModelVariableName" %in% names(channels_rois)) {
    roi_meta <- c("MainModelVariableName", "Channel", "Geography",
                  "Sourced VariableName", "VariableSplit", "SplitOrder")
    roi_num <- names(channels_rois)[
      stringr::str_detect(names(channels_rois), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE)) |
        vapply(channels_rois, is.numeric, logical(1))
    ]
    roi_num <- setdiff(unique(roi_num), roi_meta)
    for (roi_col in roi_num) {
      if (!is.numeric(channels_rois[[roi_col]])) {
        channels_rois[[roi_col]] <- suppressWarnings(as.numeric(
          gsub("%", "", gsub(",", "", as.character(channels_rois[[roi_col]])))
        ))
      }
    }
    if (length(roi_num))
      roi_lookup <- channels_rois %>%
        dplyr::select(MainModelVariableName, dplyr::all_of(roi_num)) %>%
        dplyr::distinct(MainModelVariableName, .keep_all = TRUE)
  }

# =============================================================================
 # PRE-COMPUTATIONS run ONCE before the loop
# =============================================================================

 # OPT-2: Parse ALL VOF dates FIRST must happen before split so that
  # .min_parsed / .max_parsed are present in every element of vof_by_mv
  vof_period_tokens_for_dates <- stringr::str_match(
    trimws(as.character(vof_filtered$MainModelVariableName %||% "")),
    stringr::regex("\\s+--p\\s+(-?\\d{8}-?)", ignore_case = TRUE)
  )[, 2]
  vof_period_tokens_for_dates[is.na(vof_period_tokens_for_dates)] <- ""

  vof_min_dates <- tryCatch(
    as.Date(parse_vof_period_context(
      vof_filtered$MinPeriod,
      vof_period_tokens_for_dates,
      role = "min"
    ), origin = "1970-01-01"),
    error = \(e) rep(as.Date(NA), nrow(vof_filtered))
  )
  vof_max_dates <- tryCatch(
    as.Date(parse_vof_period_context(
      vof_filtered$MaxPeriod,
      vof_period_tokens_for_dates,
      role = "max"
    ), origin = "1970-01-01"),
    error = \(e) rep(as.Date(NA), nrow(vof_filtered))
  )
  vof_filtered$.min_parsed <- vof_min_dates
  vof_filtered$.max_parsed <- vof_max_dates
  vof_contract <- vof_filtered %>%
    dplyr::transmute(
      MainModelVariableName,
      AnalyticalVariableName,
      ModelDetailsType = .model_details_type,
      IsModelled = ifelse(.details_has_main, .model_details_active, TRUE),
      PeriodToken = stringr::str_match(
        MainModelVariableName,
        stringr::regex("\\s+--p\\s+(-?\\d{8}-?)", ignore_case = TRUE)
      )[, 2],
      EffectiveMinPeriod = .min_parsed,
      EffectiveMaxPeriod = .max_parsed,
      GeographySignature = if (has_geo) {
        trimws(as.character(.data[[detect_geo_col(vof_filtered)]]))
      } else ""
    )

 # OPT-1: Split AFTER adding parsed date columns O(1) lookup per variable
  vof_by_mv <- split(vof_filtered, vof_filtered$MainModelVariableName)

  # OPT-3: Pre-compute unique VariableNames from main_data ONCE
  all_main_vn <- if (!is.null(main_data) && "VariableName" %in% names(main_data))
    unique(trimws(as.character(main_data$VariableName))) else character(0)
  all_main_vn <- all_main_vn[!is.na(all_main_vn) & nzchar(all_main_vn)]

  # Build a light RAE view for connection auditing. The full RAE can be large,
  # so channel-level diagnostics should start from indexed VariableName rows.
  audit_cols <- unique(c(
    "VariableName", "Period", "Geography",
    cross_cols,
    if (!is.null(schema_metadata)) schema_metadata$useful_long else character(0)
  ))
  audit_cols <- intersect(audit_cols, names(main_data))
  main_audit_data <- if (!is.null(main_data) &&
                         "VariableName" %in% names(main_data) &&
                         length(audit_cols)) {
    main_data[, audit_cols, drop = FALSE]
  } else {
    data.frame()
  }
  if (nrow(main_audit_data)) {
    main_audit_data$.__vn_key <- tolower(trimws(as.character(main_audit_data$VariableName)))
    if ("Period" %in% names(main_audit_data)) {
      main_audit_data$.__period <- if (inherits(main_audit_data$Period, "Date")) {
        main_audit_data$Period
      } else {
        parse_period_robust(main_audit_data$Period)
      }
    }
    main_rows_by_vn <- split(seq_len(nrow(main_audit_data)), main_audit_data$.__vn_key)
  } else {
    main_rows_by_vn <- list()
  }

 # OPT-4: Pre-index schema name_lookup as named list O(1) lookup
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
    matching <- all_main_vn[
      tolower(trimws(all_main_vn)) %in% tolower(trimws(varname_include))
    ]
    for (kw in keyword_dict$spend)
      if (any(grepl(kw, matching, ignore.case = TRUE))) return(kw)

    include_base <- unique(metric_base_name(varname_include, keyword_dict))
    include_base <- include_base[!is.na(include_base) & nzchar(include_base)]
    if (length(include_base)) {
      spend_match <- Reduce(`|`, lapply(keyword_dict$spend, function(kw) {
        grepl(kw, all_main_vn, ignore.case = TRUE)
      }))
      spend_candidates <- all_main_vn[spend_match]
      if (length(spend_candidates)) {
        spend_base <- metric_base_name(spend_candidates, keyword_dict)
        paired <- spend_candidates[spend_base %in% include_base]
        if (length(paired)) {
          for (kw in keyword_dict$spend)
            if (any(grepl(kw, paired, ignore.case = TRUE))) return(kw)
        }
      }
    }
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

  role_from_analytical_name <- function(x) {
    source_name <- stringr::str_split_fixed(as.character(x %||% ""), "_", 2)[, 1]
    has_spend <- vapply(keyword_dict$spend, function(kw) {
      grepl(kw, source_name, ignore.case = TRUE)
    }, logical(1))
    if (any(has_spend)) return("spend")

    activity_terms <- setdiff(keyword_dict$activity, keyword_dict$spend)
    has_activity <- vapply(activity_terms, function(kw) {
      grepl(kw, source_name, ignore.case = TRUE)
    }, logical(1))
    if (any(has_activity)) return("activity")
    NA_character_
  }

  extract_vof_period_token <- function(x) {
    x <- trimws(as.character(x %||% ""))
    m <- stringr::str_match(
      x,
      stringr::regex("\\s+--p\\s+(-?\\d{8}-?)", ignore_case = TRUE)
    )
    token <- m[, 2]
    ifelse(is.na(token), "", trimws(token))
  }

  vof_period_family <- function(x) {
    x <- trimws(as.character(x %||% ""))
    x <- stringr::str_remove(
      x,
      stringr::regex("\\s+--p\\s+[^\\s]+", ignore_case = TRUE)
    )
    x <- stringr::str_remove(
      x,
      stringr::regex("\\s+--g\\s+.*$", ignore_case = TRUE)
    )
    x <- stringr::str_remove(
      x,
      stringr::regex("-+\\s*(Spend|Cost|Investment|Budget)\\s*$", ignore_case = TRUE)
    )
    x <- stringr::str_squish(x)
    trimws(x)
  }

  extract_vof_geo_token <- function(x) {
    x <- trimws(as.character(x %||% ""))
    x <- stringr::str_remove(
      x,
      stringr::regex("-+\\s*(Spend|Cost|Investment|Budget)\\s*$", ignore_case = TRUE)
    )
    m <- stringr::str_match(
      x,
      stringr::regex("\\s+--g\\s+(.+?)(?=\\s+--[a-z]\\s+|$)", ignore_case = TRUE)
    )
    token <- m[, 2]
    ifelse(is.na(token), "", stringr::str_squish(token))
  }

  vof_geo_family <- function(x) {
    vof_period_family(x)
  }

  vof_geo_signature <- function(geo_token = "", geo_values = character(0),
                                segment_overrides = list()) {
    geo_token <- stringr::str_squish(as.character(geo_token %||% ""))
    if (nzchar(geo_token)) return(paste0("token:", tolower(geo_token)))

    vals <- unique(stringr::str_squish(unlist(
      strsplit(as.character(geo_values %||% character(0)), ","),
      use.names = FALSE
    )))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    vals <- vals[!toupper(vals) %in% c("ALL", "TOTAL", "NATIONAL")]
    if (length(vals)) return(paste0("include:", paste(sort(tolower(vals)), collapse = "|")))

    geo_exc <- unique(unlist(lapply(segment_overrides %||% list(), function(o) {
      o$geography_exclude %||% character(0)
    }), use.names = FALSE))
    geo_exc <- stringr::str_squish(as.character(geo_exc))
    geo_exc <- geo_exc[!is.na(geo_exc) & nzchar(geo_exc)]
    if (length(geo_exc)) return(paste0("exclude:", paste(sort(tolower(geo_exc)), collapse = "|")))

    "all"
  }

  vof_period_boundary <- function(token, min_raw, max_raw) {
    token <- trimws(as.character(token %||% ""))
    token_date <- parse_period_token_date(token)
    if (!is.na(token_date)) {
      if (startsWith(token, "-")) return(token_date)
      if (endsWith(token, "-")) return(token_date - 1)
      return(token_date)
    }
    min_raw <- tryCatch(as.Date(min_raw), error = function(e) as.Date(NA))
    max_raw <- tryCatch(as.Date(max_raw), error = function(e) as.Date(NA))
    if (!is.na(min_raw) && is.na(max_raw)) return(min_raw - 1)
    if (is.na(min_raw) && !is.na(max_raw)) return(max_raw)
    as.Date(NA_character_)
  }

  analytical_exists <- function(x) {
    x <- trimws(as.character(x %||% character(0)))
    if (!length(x) || !length(analytical_model_cols)) return(rep(FALSE, length(x)))
    x %in% analytical_model_cols |
      normalize_model_var(x) %in% analytical_model_norm
  }

  source_base_from_analytical <- function(anal_var_names) {
    anal_var_names <- unique(trimws(as.character(anal_var_names %||% character(0))))
    anal_var_names <- anal_var_names[!is.na(anal_var_names) & nzchar(anal_var_names)]
    if (!length(anal_var_names)) return(character(0))
    if (!is.null(schema_lookup_by_orig)) {
      rows <- dplyr::bind_rows(
        schema_lookup_by_orig[intersect(anal_var_names, names(schema_lookup_by_orig))]
      )
      if (nrow(rows) > 0 && "VariableName" %in% names(rows)) {
        vals <- unique(trimws(as.character(rows$VariableName)))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals)) return(vals)
      }
    }
    unique(trimws(stringr::str_remove(anal_var_names, "_Total(_Total)*$")))
  }

  useful_longitudinal_filters <- function(anal_var_names) {
    if (is.null(schema_metadata) ||
        is.null(schema_metadata$name_lookup) ||
        !length(schema_metadata$useful_long %||% character(0))) {
      return(list(text = "", filters = list()))
    }
    lookup <- schema_metadata$name_lookup
    filters <- list()
    text <- character(0)
    for (dim in schema_metadata$useful_long %||% character(0)) {
      vals <- get_useful_long_values(anal_var_names, lookup, dim)
      vals <- unique(trimws(as.character(vals)))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (!length(vals)) next
      filters[[dim]] <- vals
      text <- c(text, paste0(dim, ": ", paste(vals, collapse = ", ")))
    }
    list(text = paste(text, collapse = " | "), filters = filters)
  }

  filter_effective_rae_for_channel <- function(cfg, min_p = NULL, max_p = NULL,
                                               segment_overrides = list()) {
    if (!nrow(main_audit_data) || !"VariableName" %in% names(main_audit_data)) {
      return(data.frame())
    }
    vi <- as.character(cfg$varname_include %||% character(0))
    vi <- vi[!is.na(vi) & nzchar(trimws(vi))]
    if (length(vi)) {
      vi <- expand_varname_include_with_spend(
        all_main_vn,
        vi,
        cfg$spend_keyword %||% NULL,
        keyword_dict
      )
      vi <- expand_analytical_keys_to_variable_names(all_main_vn, vi)
      match_mode <- cfg$varname_match_mode %||%
        if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
      if (identical(match_mode, "exact")) {
        keys <- intersect(tolower(trimws(vi)), names(main_rows_by_vn))
        idx <- if (length(keys)) unlist(main_rows_by_vn[keys], use.names = FALSE) else integer(0)
        d <- main_audit_data[idx, , drop = FALSE]
      } else {
        vn <- trimws(as.character(main_audit_data$VariableName))
        pattern <- paste(paste0("^", stringr::str_replace_all(vi, "([\\W])", "\\\\\\1")), collapse = "|")
        d <- main_audit_data[grepl(pattern, vn, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
      }
    } else {
      d <- main_audit_data
    }
    d <- filter_to_analytical_varkey_combinations(d, cfg, schema_metadata)
    if ("Period" %in% names(d)) {
      p <- if (".__period" %in% names(d)) d$.__period else if (inherits(d$Period, "Date")) d$Period else parse_period_robust(d$Period)
      keep <- rep(TRUE, nrow(d))
      if (!is.null(min_p) && !is.na(min_p)) keep <- keep & p >= as.Date(min_p)
      if (!is.null(max_p) && !is.na(max_p)) keep <- keep & p <= as.Date(max_p)
      keep[is.na(keep)] <- FALSE
      d <- d[keep, , drop = FALSE]
    }
    geo_exc <- character(0)
    if (length(segment_overrides) > 0) {
      geo_exc <- unique(unlist(lapply(segment_overrides, function(o) {
        o$geography_exclude %||% character(0)
      }), use.names = FALSE))
    }
    if (length(geo_exc) && "Geography" %in% names(d)) {
      for (g in geo_exc[nzchar(geo_exc)]) {
        d <- d[!grepl(g, d$Geography, ignore.case = TRUE), , drop = FALSE]
      }
    }
    d[, setdiff(names(d), c(".__vn_key", ".__period")), drop = FALSE]
  }

  metric_rows_from_scope <- function(scope, activity_keyword, spend_keyword) {
    vn <- trimws(as.character(scope$VariableName %||% character(0)))
    list(
      activity_rows = if (nzchar(activity_keyword %||% "")) sum(grepl(activity_keyword, vn, ignore.case = TRUE), na.rm = TRUE) else 0L,
      spend_rows = if (nzchar(spend_keyword %||% "")) sum(grepl(spend_keyword, vn, ignore.case = TRUE), na.rm = TRUE) else 0L,
      activity_vars = if (nzchar(activity_keyword %||% "")) unique(vn[grepl(activity_keyword, vn, ignore.case = TRUE)]) else character(0),
      spend_vars = if (nzchar(spend_keyword %||% "")) unique(vn[grepl(spend_keyword, vn, ignore.case = TRUE)]) else character(0)
    )
  }

  add_connection_row <- function(...) {
    connection_rows[[length(connection_rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
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

# =============================================================================
  # MAIN LOOP
# =============================================================================

  for (mv in unique(vof_filtered$MainModelVariableName)) {

    vof_rows <- vof_by_mv[[mv]]
    if (is.null(vof_rows) || !nrow(vof_rows)) next

    role_info <- role_map[[mv]] %||% list()
    modeled_anal_var_names <- unique(
      role_info$modeled_analytical_variables %||% vof_rows$AnalyticalVariableName
    )
    for_indices_anal_var_names <- unique(
      role_info$for_indices_analytical_variables %||% character(0)
    )
    model_metric <- normalize_model_metric(
      role_info$modeled_role %||% infer_metric_role(
        if ("Metric" %in% names(vof_rows)) vof_rows$Metric else NULL,
        modeled_anal_var_names,
        keyword_dict
      ) %||% "activity"
    )
    modeled_ch_cfg <- derive_ch_config_fast(modeled_anal_var_names)
    for_indices_ch_cfg <- derive_ch_config_fast(for_indices_anal_var_names)
    pair_status <- role_info$pair_status %||% "Missing"
    pair_reason <- role_info$pair_reason %||% ""
    pair_source <- if (identical(pair_status, "Missing")) "Not found" else "VOF / ModelDetails"
    pair_coverage <- if (identical(pair_status, "Matched")) 1 else 0
    pair_missing_sources <- character(0)
    pair_candidates <- role_info$pair_candidates %||% character(0)
    pair_source_pairs <- role_info$source_pairs %||% data.frame()
    for_indices_role <- role_info$for_indices_role %||% ""

    if (identical(pair_status, "Matched") && nrow(pair_source_pairs) &&
        all(c("PairStatus", "ModeledAnalyticalVariableName") %in%
            names(pair_source_pairs))) {
      source_matched <- pair_source_pairs$PairStatus %in% "Matched"
      pair_coverage <- sum(source_matched, na.rm = TRUE) / nrow(pair_source_pairs)
      pair_missing_sources <- pair_source_pairs$ModeledAnalyticalVariableName[
        !source_matched
      ]
      if (pair_coverage < 1) {
        pair_status <- "Partial"
        pair_reason <- paste0(
          pair_reason,
          " Some modeled source variables do not have an exact ForIndices counterpart."
        )
      }
    }

    if (identical(pair_status, "Missing")) {
      rae_fallback <- resolve_rae_for_indices_fallback(
        all_variable_names = all_main_vn,
        modeled_variable_names = modeled_ch_cfg$varname_include,
        modeled_role = model_metric,
        keyword_dict = keyword_dict
      )
      if (!identical(rae_fallback$status, "Missing")) {
        for_indices_ch_cfg <- list(
          varname_include = rae_fallback$candidates,
          split_columns = modeled_ch_cfg$split_columns
        )
        for_indices_role <- rae_fallback$role
        pair_status <- rae_fallback$status
        pair_source <- "RAE fallback"
        pair_coverage <- rae_fallback$coverage
        pair_missing_sources <- rae_fallback$missing_sources
        pair_candidates <- rae_fallback$candidates
        pair_source_pairs <- rae_fallback$source_pairs
        pair_reason <- if (identical(pair_status, "Matched")) {
          "ForIndices was resolved from compatible opposite-metric variables in RAE."
        } else {
          "ForIndices was partially resolved from RAE; some modeled source variables have no compatible opposite-metric variable."
        }
      }
    }

    anal_var_names <- unique(c(modeled_anal_var_names, for_indices_anal_var_names))
    source_metric_roles <- vapply(
      anal_var_names,
      role_from_analytical_name,
      character(1)
    )
    source_metric_roles[is.na(source_metric_roles) &
                          anal_var_names %in% modeled_anal_var_names] <- model_metric
    if (!is.na(for_indices_role) && nzchar(for_indices_role)) {
      source_metric_roles[is.na(source_metric_roles) &
                            anal_var_names %in% for_indices_anal_var_names] <- for_indices_role
    }
    activity_vars <- unique(anal_var_names[source_metric_roles == "activity"])
    spend_vars <- unique(anal_var_names[source_metric_roles == "spend"])
    if (!length(activity_vars) && !length(spend_vars)) {
      if (identical(model_metric, "spend")) spend_vars <- modeled_anal_var_names
      else activity_vars <- modeled_anal_var_names
    }
    if (length(for_indices_ch_cfg$varname_include %||% character(0))) {
      if (identical(for_indices_role, "spend")) {
        spend_vars <- unique(c(spend_vars, for_indices_ch_cfg$varname_include))
      } else if (identical(for_indices_role, "activity")) {
        activity_vars <- unique(c(activity_vars, for_indices_ch_cfg$varname_include))
      }
    }
    ch_cfg <- list(
      varname_include = unique(c(
        modeled_ch_cfg$varname_include,
        for_indices_ch_cfg$varname_include
      )),
      split_columns = unique(c(
        modeled_ch_cfg$split_columns,
        for_indices_ch_cfg$split_columns
      ))
    )

    act_kw <- if (length(activity_vars)) {
      detect_activity_keyword(
        stringr::str_remove(activity_vars, "_Total_Total_Total$"),
        keyword_dict)
    } else {
      "Activity"
    }

    varname_include <- unique(ch_cfg$varname_include)
    varname_include <- varname_include[nzchar(varname_include)]

    spend_kw <- if (length(spend_vars)) {
      detected <- detect_spend_keyword(
        data.frame(VariableName = stringr::str_remove(spend_vars, "_Total(_Total)*$")),
        stringr::str_remove(spend_vars, "_Total(_Total)*$"),
        keyword_dict)
      metric_kw <- if ("Metric" %in% names(vof_rows))
        keyword_from_metric(
          vof_rows$Metric[vof_rows$AnalyticalVariableName %in% spend_vars],
          "spend"
        )
      else NA_character_
      if (!is.na(detected) && nzchar(detected)) detected
      else if (!is.na(metric_kw) && nzchar(metric_kw)) metric_kw
      else detect_spend_kw_fast(varname_include)
    } else {
      detect_spend_kw_fast(varname_include)
    }

 # Dates read from pre-parsed columns (now always Date class)
    vof_min_raw <- tryCatch({
      d <- vof_rows$.min_parsed
      d <- d[!is.na(d)]
      if (length(d)) min(d) else NA
    }, error = \(e) NA)
    vof_max_raw <- tryCatch({
      d <- vof_rows$.max_parsed
      d <- d[!is.na(d)]
      if (length(d)) max(d) else NA
    }, error = \(e) NA)

    if (is.na(vof_min_raw) || !is.finite(as.numeric(vof_min_raw)))
      vof_min_raw <- as.Date(NA_character_)
    if (is.na(vof_max_raw) || !is.finite(as.numeric(vof_max_raw)))
      vof_max_raw <- as.Date(NA_character_)

    vof_period_token_val <- extract_vof_period_token(mv)
    vof_period_family_val <- vof_period_family(mv)
    vof_period_boundary_val <- vof_period_boundary(
      vof_period_token_val,
      vof_min_raw,
      vof_max_raw
    )
    vof_geo_token_val <- extract_vof_geo_token(mv)
    vof_geo_family_val <- vof_geo_family(mv)

    min_p <- vof_min_raw
    max_p <- vof_max_raw
    if (is.na(min_p) || !is.finite(as.numeric(min_p))) min_p <- an_min_date
    if (is.na(max_p) || !is.finite(as.numeric(max_p))) max_p <- an_max_date

    # Geography exclusions
    geo_col_vof       <- detect_geo_col(vof_rows)
    geo_values_raw <- if (!is.null(geo_col_vof)) {
      vof_rows[[geo_col_vof]]
    } else {
      character(0)
    }
    segment_overrides <- if (!is.null(geo_col_vof) && length(an_geos) > 0) {
      geo_strs <- geo_values_raw
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
    vof_geo_signature_val <- vof_geo_signature(
      geo_token = vof_geo_token_val,
      geo_values = geo_values_raw,
      segment_overrides = segment_overrides
    )

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

 # ROI O(1) lookup
    roi_val <- NA_real_
    ri      <- roi_by_mv[[mv]]
    if (!is.null(ri) && nrow(ri) > 0) {
      r_num <- setdiff(names(ri), "MainModelVariableName")
      if (length(r_num)) roi_val <- mean(ri[[r_num[1]]], na.rm = TRUE)
    }

    tmp_cfg <- list(
      channel_name = mv,
      model_variable = mv,
      varname_include = varname_include,
      varname_match_mode = "exact",
      analytical_varkeys = anal_var_names,
      source = "vof",
      activity_keyword = act_kw,
      spend_keyword = spend_kw
    )
    long_filters <- useful_longitudinal_filters(modeled_anal_var_names)
    effective_scope <- filter_effective_rae_for_channel(
      tmp_cfg,
      min_p = min_p,
      max_p = max_p,
      segment_overrides = segment_overrides
    )
    metric_counts <- metric_rows_from_scope(effective_scope, act_kw, spend_kw)
    paired_spend_candidates <- expand_varname_include_with_spend(
      all_main_vn,
      varname_include,
      spend_kw,
      keyword_dict
    )
    paired_spend_exists <- length(setdiff(
      tolower(trimws(paired_spend_candidates)),
      tolower(trimws(varname_include))
    )) > 0
    primary_rows <- if (identical(normalize_model_metric(model_metric), "spend")) {
      metric_counts$spend_rows
    } else {
      metric_counts$activity_rows
    }
    analytical_ok <- any(analytical_exists(modeled_anal_var_names))
    connection_status <- if (!isTRUE(analytical_ok)) {
      "Missing Analytical"
    } else if (!nrow(effective_scope)) {
      "Missing RAE"
    } else if (primary_rows == 0L ||
               (metric_counts$spend_rows == 0L &&
                isTRUE(paired_spend_exists))) {
      "Partial"
    } else {
      "Matched"
    }
    source_rae_vars <- unique(trimws(as.character(effective_scope$VariableName %||% character(0))))
    source_rae_vars <- source_rae_vars[!is.na(source_rae_vars) & nzchar(source_rae_vars)]
    source_base_vars <- source_base_from_analytical(anal_var_names)
    analytical_contract_vars <- unique(modeled_anal_var_names[
      modeled_anal_var_names %in% names(analytical)
    ])
    canonical_model_variable <- if (mv %in% names(analytical)) {
      mv
    } else if (length(analytical_contract_vars) == 1L) {
      analytical_contract_vars[[1]]
    } else {
      mv
    }

    channels[[mv]] <- list(
      channel_name      = mv,
      model_variable    = canonical_model_variable,
      varname_include   = varname_include,
      modeled_variable  = role_info$modeled_variable %||% mv,
      modeled_type      = role_info$modeled_type %||% "",
      modeled_role      = model_metric,
      modeled_analytical_variables = modeled_anal_var_names,
      modeled_varname_include = unique(modeled_ch_cfg$varname_include),
      for_indices_variable = role_info$for_indices_variable %||% "",
      for_indices_type = role_info$for_indices_type %||% "",
      for_indices_role = if (!is.na(for_indices_role)) for_indices_role else "",
      for_indices_analytical_variables = for_indices_anal_var_names,
      for_indices_varname_include = unique(for_indices_ch_cfg$varname_include),
      role_pair_status = pair_status,
      role_pair_reason = pair_reason,
      role_pair_source = pair_source,
      role_pair_coverage = pair_coverage,
      role_pair_missing_sources = pair_missing_sources,
      role_pair_candidates = pair_candidates,
      role_source_pairs = pair_source_pairs,
      varname_match_mode = "exact",
      analytical_varkeys = modeled_anal_var_names,
      min_period        = min_p,
      max_period        = max_p,
      vof_min_raw       = vof_min_raw,
      vof_max_raw       = vof_max_raw,
      vof_period_token  = vof_period_token_val,
      vof_period_family = vof_period_family_val,
      vof_period_boundary = vof_period_boundary_val,
      vof_geo_token     = vof_geo_token_val,
      vof_geo_family    = vof_geo_family_val,
      vof_geo_signature = vof_geo_signature_val,
      geo_label         = "",
      vof_geo_group     = "",
      vof_geo_order     = NA_integer_,
      vof_geo_group_size = 0L,
      segment_overrides = segment_overrides,
      model_metric      = model_metric,
      activity_keyword  = act_kw,
      spend_keyword     = spend_kw,
      split_columns     = ch_cfg$split_columns,
      saved_merges      = list(),
      dimension_breaks  = list(),
      roi               = roi_val,
      source            = "vof",
      media_channel     = media_channel,
      sub_channel       = sub_channel,
      effect            = effect,
      connection_source = "VOF",
      connection_status = connection_status,
      model_details_type = unique(vof_contract$ModelDetailsType[
        vof_contract$MainModelVariableName == mv &
          nzchar(vof_contract$ModelDetailsType)
      ])[1] %||% "",
      model_details_active = TRUE,
      vof_rows_detected = sum(vof_df$MainModelVariableName == mv),
      vof_rows_active = sum(vof_filtered$MainModelVariableName == mv),
      vof_rows_discarded = sum(vof_df$MainModelVariableName == mv) -
        sum(vof_filtered$MainModelVariableName == mv),
      matched_analytical_cols = anal_var_names,
      source_variable_names_rae = source_rae_vars,
      source_base_variables = source_base_vars,
      activity_source_variables_rae = metric_counts$activity_vars,
      spend_source_variables_rae = metric_counts$spend_vars,
      activity_rows = metric_counts$activity_rows,
      spend_rows = metric_counts$spend_rows,
      useful_longitudinal_filters = long_filters$filters,
      useful_longitudinal_filters_text = long_filters$text,
      time_break_source = "VOF + ModelDetails",
      time_break_partition_count = 0L
    )

    add_connection_row(
      MainModelVariableName = mv,
      AnalyticalVariableName = paste(anal_var_names, collapse = " | "),
      ModelDetailsVariableName = if (normalize_model_var(mv) %in% in_model_norm) mv else "",
      SourceVariableNameRAE = paste(source_rae_vars, collapse = " | "),
      MediaChannel = media_channel,
      SubChannel = sub_channel,
      Effect = effect,
      Metric = if ("Metric" %in% names(vof_rows)) paste(unique(trimws(as.character(vof_rows$Metric))), collapse = " | ") else "",
      ModelMetric = model_metric,
      MinPeriod = if (!is.na(min_p)) as.character(min_p) else "",
      MaxPeriod = if (!is.na(max_p)) as.character(max_p) else "",
      GeographyRule = if (length(segment_overrides)) "VOF geography include/exclude" else "All",
      UsefulLongitudinalFilters = long_filters$text,
      ConnectionSource = "VOF",
      ConnectionStatus = connection_status,
      ModeledVariableName = role_info$modeled_variable %||% mv,
      ForIndicesVariableName = if (nzchar(role_info$for_indices_variable %||% ""))
        role_info$for_indices_variable else paste(pair_candidates, collapse = " | "),
      RolePairStatus = pair_status,
      RolePairSource = pair_source,
      RolePairCoverage = pair_coverage
    )

    role_updates <- list(
      for_indices_role = if (!is.na(for_indices_role)) for_indices_role else "",
      for_indices_varname_include = unique(for_indices_ch_cfg$varname_include),
      pair_status = pair_status,
      pair_reason = pair_reason,
      pair_source = pair_source,
      pair_coverage = pair_coverage,
      pair_missing_sources = pair_missing_sources,
      pair_candidates = pair_candidates,
      source_pairs = pair_source_pairs
    )
    # source_pairs is a data frame whose row count legitimately changes when
    # RAE fallback replaces VOF evidence. A recursive modifyList() attempts to
    # merge its columns and fails when the old and new evidence have different
    # row counts, so update the role contract one top-level field at a time.
    role_info[names(role_updates)] <- role_updates
    role_map[[mv]] <- role_info
  }

 # Step 6: keyword fallback channels
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
    non_vof_cols <- non_vof_cols[
      !(normalize_model_var(non_vof_cols) %in% details_all_norm &
          !normalize_model_var(non_vof_cols) %in% in_model_norm)
    ]
    non_vof_cols <- non_vof_cols[
      !normalize_model_var(non_vof_cols) %in% inactive_vof_analytical_norm
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
      rae_fallback <- resolve_rae_for_indices_fallback(
        all_variable_names = all_main_vn,
        modeled_variable_names = ch_cfg$varname_include,
        modeled_role = "activity",
        keyword_dict = keyword_dict
      )
      for_indices_vars <- rae_fallback$candidates
      if (length(for_indices_vars)) {
        varname_include <- unique(c(varname_include, for_indices_vars))
        spend_kw <- detect_spend_kw_fast(for_indices_vars)
      }

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
        modeled_variable  = col,
        modeled_type      = if (normalize_model_var(col) %in% in_model_norm) "IN/FIXED" else "Fallback",
        modeled_role      = "activity",
        modeled_analytical_variables = col,
        modeled_varname_include = unique(ch_cfg$varname_include),
        for_indices_variable = "",
        for_indices_type = "",
        for_indices_role = rae_fallback$role,
        for_indices_analytical_variables = character(0),
        for_indices_varname_include = for_indices_vars,
        role_pair_status = rae_fallback$status,
        role_pair_reason = if (!identical(rae_fallback$status, "Missing"))
          "ForIndices was resolved from compatible opposite-metric variables in RAE."
        else "No explicit or RAE ForIndices relationship is available for this fallback channel.",
        role_pair_source = if (!identical(rae_fallback$status, "Missing"))
          "RAE fallback" else "Not found",
        role_pair_coverage = rae_fallback$coverage,
        role_pair_missing_sources = rae_fallback$missing_sources,
        role_pair_candidates = for_indices_vars,
        role_source_pairs = rae_fallback$source_pairs,
        varname_match_mode = "prefix",
        analytical_varkeys = col,
        min_period        = an_min_date,
        max_period        = an_max_date,
        segment_overrides = list(),
        geo_label         = "",
        vof_geo_group     = "",
        vof_geo_order     = NA_integer_,
        vof_geo_group_size = 0L,
        vof_geo_signature = "all",
        model_metric      = "activity",
        activity_keyword  = act_kw,
        spend_keyword     = spend_kw,
        split_columns     = ch_cfg$split_columns,
        saved_merges      = list(),
        dimension_breaks  = list(),
        roi               = roi_val,
        source            = "keyword_fallback",
        media_channel     = "",
        sub_channel       = "",
        effect            = "",
        connection_source = "Fallback",
        connection_status = "Matched",
        matched_analytical_cols = col,
        source_variable_names_rae = varname_include,
        source_base_variables = source_base_from_analytical(col),
        activity_source_variables_rae = varname_include[grepl(act_kw, varname_include, ignore.case = TRUE)],
        spend_source_variables_rae = for_indices_vars,
        activity_rows = NA_integer_,
        spend_rows = NA_integer_,
        useful_longitudinal_filters = list(),
        useful_longitudinal_filters_text = ""
      )

      add_connection_row(
        MainModelVariableName = col,
        AnalyticalVariableName = col,
        ModelDetailsVariableName = if (normalize_model_var(col) %in% in_model_norm) col else "",
        SourceVariableNameRAE = paste(varname_include, collapse = " | "),
        MediaChannel = "",
        SubChannel = "",
        Effect = "",
        Metric = act_kw,
        ModelMetric = "activity",
        MinPeriod = if (!is.na(an_min_date)) as.character(an_min_date) else "",
        MaxPeriod = if (!is.na(an_max_date)) as.character(an_max_date) else "",
        GeographyRule = "All",
        UsefulLongitudinalFilters = "",
        ConnectionSource = "Fallback",
        ConnectionStatus = "Matched"
      )
    }
  }

 # Step 7: time_break_labels
  for (nm in names(channels)) {
    if (!identical(channels[[nm]]$source, "vof")) next
    channels[[nm]]$time_break_label <- ""
    channels[[nm]]$vof_time_break_group <- ""
    channels[[nm]]$vof_time_break_order <- NA_integer_
    channels[[nm]]$vof_time_break_group_size <- 0L
  }

  vof_sigs <- vapply(names(channels), function(nm) {
    ch <- channels[[nm]]
    if (!identical(ch$source, "vof")) return(NA_character_)
    if (!nzchar(ch$vof_period_token %||% "")) return(NA_character_)
    raw_boundary <- tryCatch(as.Date(ch$vof_period_boundary), error = function(e) as.Date(NA))
    if (is.na(raw_boundary)) return(NA_character_)
    family_key <- ch$vof_period_family %||% vof_period_family(ch$channel_name %||% nm)
      paste(
      paste(sort(unique(ch$analytical_varkeys)), collapse = "||"),
      ch$media_channel %||% "",
      ch$effect %||% "",
      ch$model_metric %||% "activity",
      paste(sort(unique(vapply(names(ch$useful_longitudinal_filters %||% list()), function(dim) {
        vals <- ch$useful_longitudinal_filters[[dim]]
        paste(dim, paste(sort(unique(trimws(as.character(vals)))), collapse = "|"), sep = "=")
      }, character(1)))), collapse = "||"),
      family_key,
      sep = "::::"
    )
  }, character(1))

  vof_sigs <- vof_sigs[!is.na(vof_sigs)]
  dup_sigs <- names(which(table(vof_sigs) > 1))

  for (sig in dup_sigs) {
    group_nms <- names(vof_sigs[vof_sigs == sig])
    range_df <- data.frame(
      nm = group_nms,
      boundary = as.Date(vapply(group_nms, function(nm) {
        bd <- channels[[nm]]$vof_period_boundary
        if (!is.null(bd) && !is.na(bd)) as.character(as.Date(bd)) else NA_character_
      }, character(1))),
      min_date = as.Date(vapply(group_nms, function(nm) {
        mp <- channels[[nm]]$vof_min_raw
        if (!is.null(mp) && !is.na(mp)) as.character(as.Date(mp)) else NA_character_
      }, character(1))),
      max_date = as.Date(vapply(group_nms, function(nm) {
        mp <- channels[[nm]]$vof_max_raw
        if (!is.null(mp) && !is.na(mp)) as.character(as.Date(mp)) else NA_character_
      }, character(1))),
      stringsAsFactors = FALSE
    )
    range_df$range_key <- paste(range_df$min_date, range_df$max_date, sep = "|")
    unique_ranges <- range_df[!duplicated(range_df$range_key), , drop = FALSE]
    range_order <- unique_ranges$boundary
    range_order[is.na(range_order)] <- unique_ranges$min_date[is.na(range_order)]
    range_order[is.na(range_order)] <- unique_ranges$max_date[is.na(range_order)]
    range_order[is.na(range_order)] <- as.Date("2999-12-31")
    min_order <- unique_ranges$min_date
    min_order[is.na(min_order)] <- as.Date("1900-01-01")
    max_order <- unique_ranges$max_date
    max_order[is.na(max_order)] <- as.Date("2999-12-31")
    unique_ranges <- unique_ranges[order(range_order, min_order, max_order,
                                         unique_ranges$nm), , drop = FALSE]

    # The signature is already strict enough to identify real VOF siblings:
    # same period family, analytical variable set, media channel and effect.
    # Do not split siblings into extra clusters by date gaps; weekly data can
    # legitimately have gaps and still must receive First/Second labels.
    if (nrow(unique_ranges) <= 1) next

    range_labels <- setNames(
      paste0(vapply(seq_len(nrow(unique_ranges)), ordinal_tag, character(1)), "TimeBreak"),
      unique_ranges$range_key
    )
    range_orders <- setNames(seq_len(nrow(unique_ranges)), unique_ranges$range_key)

    for (nm in range_df$nm) {
      key <- range_df$range_key[range_df$nm == nm][1]
      channels[[nm]]$time_break_label <- range_labels[[key]] %||% ""
      channels[[nm]]$vof_time_break_group <- sig
      channels[[nm]]$vof_time_break_order <- range_orders[[key]] %||% NA_integer_
      channels[[nm]]$vof_time_break_group_size <- nrow(unique_ranges)
      channels[[nm]]$time_break_partition_count <- nrow(unique_ranges)
    }
  }

 # Step 8: geo_labels
  for (nm in names(channels)) {
    channels[[nm]]$geo_label <- channels[[nm]]$geo_label %||% ""
    channels[[nm]]$vof_geo_group <- channels[[nm]]$vof_geo_group %||% ""
    channels[[nm]]$vof_geo_order <- channels[[nm]]$vof_geo_order %||% NA_integer_
    channels[[nm]]$vof_geo_group_size <- channels[[nm]]$vof_geo_group_size %||% 0L
    channels[[nm]]$vof_geo_signature <- channels[[nm]]$vof_geo_signature %||% "all"
  }

  vof_geo_groups <- vapply(names(channels), function(nm) {
    ch <- channels[[nm]]
    if (!identical(ch$source, "vof")) return(NA_character_)
    if (nzchar(ch$time_break_label %||% "")) return(NA_character_)
    family_key <- ch$vof_geo_family %||% vof_geo_family(ch$channel_name %||% nm)
    date_key <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[1])) return("")
      out <- tryCatch(as.Date(x[1]), error = function(e) as.Date(NA))
      if (is.na(out)) "" else as.character(out)
    }
    period_key <- ch$vof_period_token %||% ""
    if (!nzchar(period_key)) {
      min_key <- date_key(ch$vof_min_raw)
      max_key <- date_key(ch$vof_max_raw)
      period_key <- paste(min_key, max_key, sep = "|")
    }
    long_filters <- ch$useful_longitudinal_filters %||% list()
    long_sig <- if (length(long_filters)) {
      paste(vapply(names(long_filters), function(dim) {
        vals <- sort(unique(trimws(as.character(long_filters[[dim]]))))
        paste0(dim, "=", paste(vals, collapse = ","))
      }, character(1)), collapse = "||")
    } else {
      ""
    }
    paste(
      paste(sort(unique(ch$analytical_varkeys)), collapse = "||"),
      ch$media_channel %||% "",
      ch$effect %||% "",
      normalize_model_metric(ch$model_metric %||% "activity"),
      family_key,
      period_key,
      long_sig,
      sep = "::::"
    )
  }, character(1))

  vof_geo_groups <- vof_geo_groups[!is.na(vof_geo_groups)]
  for (sig in unique(vof_geo_groups)) {
    group_nms <- names(vof_geo_groups[vof_geo_groups == sig])
    geo_signatures <- vapply(group_nms, function(nm) {
      channels[[nm]]$vof_geo_signature %||% "all"
    }, character(1))
    unique_geo <- unique(geo_signatures)
    if (length(unique_geo) <= 1L) next

    label_map <- setNames(paste0("GeoLabel", seq_along(unique_geo)), unique_geo)
    order_map <- setNames(seq_along(unique_geo), unique_geo)
    for (nm in group_nms) {
      geo_sig <- channels[[nm]]$vof_geo_signature %||% "all"
      channels[[nm]]$geo_label <- label_map[[geo_sig]] %||% ""
      channels[[nm]]$vof_geo_group <- sig
      channels[[nm]]$vof_geo_order <- order_map[[geo_sig]] %||% NA_integer_
      channels[[nm]]$vof_geo_group_size <- length(unique_geo)
    }
  }

  # TimeBreak already separates sibling variables by their effective period.
  # A GeoLabel on the same channel would add a second, redundant identity.
  for (nm in names(channels)) {
    if (!nzchar(channels[[nm]]$time_break_label %||% "")) next
    channels[[nm]]$geo_label <- ""
    channels[[nm]]$vof_geo_group <- ""
    channels[[nm]]$vof_geo_order <- NA_integer_
    channels[[nm]]$vof_geo_group_size <- 0L
  }

  n_vof      <- sum(sapply(channels, \(c) identical(c$source, "vof")))
  n_fallback <- sum(sapply(channels, \(c) identical(c$source, "keyword_fallback")))
  n_with_roi <- sum(sapply(channels, \(c) !is.na(c$roi %||% NA_real_)))
  connection_map <- if (length(connection_rows)) {
    dplyr::bind_rows(connection_rows)
  } else {
    data.frame(
      MainModelVariableName = character(0),
      AnalyticalVariableName = character(0),
      ModelDetailsVariableName = character(0),
      SourceVariableNameRAE = character(0),
      MediaChannel = character(0),
      SubChannel = character(0),
      Effect = character(0),
      Metric = character(0),
      ModelMetric = character(0),
      MinPeriod = character(0),
      MaxPeriod = character(0),
      GeographyRule = character(0),
      UsefulLongitudinalFilters = character(0),
      ConnectionSource = character(0),
      ConnectionStatus = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    channels        = channels,
    connection_map  = connection_map,
    role_map        = role_map,
    var_key_info    = vk_info,
    schema_metadata = schema_metadata,
    summary = list(
      total_channels = length(channels),
      from_vof       = n_vof,
      from_fallback  = n_fallback,
      with_roi       = n_with_roi,
      connection_matched = sum(connection_map$ConnectionStatus == "Matched", na.rm = TRUE),
      connection_partial = sum(connection_map$ConnectionStatus == "Partial", na.rm = TRUE),
      connection_missing_rae = sum(connection_map$ConnectionStatus == "Missing RAE", na.rm = TRUE),
      connection_missing_analytical = sum(connection_map$ConnectionStatus == "Missing Analytical", na.rm = TRUE),
      var_key_type   = vk_info$type,
      vof_coverage   = round(vk_info$coverage * 100, 1),
      xs_dims        = if (!is.null(schema_metadata))
        schema_metadata$xs_dims else character(0),
      useful_long    = if (!is.null(schema_metadata))
        schema_metadata$useful_long else character(0),
      discarded_long = if (!is.null(schema_metadata))
        schema_metadata$discarded_long else character(0),
      vof_rows_detected = nrow(vof_df),
      vof_rows_active = nrow(vof_filtered),
      vof_rows_discarded = nrow(vof_df) - nrow(vof_filtered),
      details_rows_total = if (!is.null(model_details)) nrow(model_details) else 0L,
      details_modelled_variables = length(unique(in_model_norm)),
      details_inactive_variables = length(setdiff(details_all_norm, in_model_norm)),
      time_break_channels = sum(vapply(channels, function(ch) {
        nzchar(ch$time_break_label %||% "")
      }, logical(1)))
    ),
    vof_contract = vof_contract
  )
}

# Timeline HTML builder pure function, no reactive dependencies
# Used by mod_setup output$file_comparison for Time Scope warnings.
