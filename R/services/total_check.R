# Total Check reconciles the Analytical model total, effective RAE and processed splits.

total_check_values_match <- function(left, right, absolute_tolerance = 0.01,
                                     relative_tolerance = 1e-10) {
  left <- suppressWarnings(as.numeric(left))
  right <- suppressWarnings(as.numeric(right))
  scale <- pmax(abs(left), abs(right), 1, na.rm = TRUE)
  abs(left - right) <= pmax(absolute_tolerance, relative_tolerance * scale)
collapse_total_check_source <- function(df, key_cols, value_col, output_col,
                                        tolerance = 0.01) {
  if (is.null(df) || !nrow(df) || !value_col %in% names(df)) {
    return(list(data = tibble::tibble(), duplicate_keys = 0L,
                conflicting_keys = 0L))
  }
  key_cols <- intersect(key_cols, names(df))
  if (!length(key_cols)) {
    return(list(data = tibble::tibble(), duplicate_keys = 0L,
                conflicting_keys = 0L))
  }
  grouped <- df %>%
    dplyr::select(dplyr::all_of(c(key_cols, value_col))) %>%
    dplyr::rename(.__value = dplyr::all_of(value_col)) %>%
    dplyr::mutate(.__value = suppressWarnings(as.numeric(.__value))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(
      .__rows = dplyr::n(),
      .__distinct = dplyr::n_distinct(.__value[!is.na(.__value)]),
      .__total = {
        values <- .__value[!is.na(.__value)]
        if (!length(values)) {
          0
        } else if ((max(values) - min(values)) <= tolerance) {
          values[[1]]
        } else {
          sum(values, na.rm = TRUE)
        }
      },
      .groups = "drop"
    )
  duplicate_keys <- sum(grouped$.__rows > 1L)
  conflicting_keys <- sum(grouped$.__rows > 1L & grouped$.__distinct > 1L)
  data <- grouped %>%
    dplyr::select(-.__rows, -.__distinct) %>%
    dplyr::rename(!!output_col := .__total)
  list(
    data = data,
    duplicate_keys = duplicate_keys,
    conflicting_keys = conflicting_keys
  )
}

filter_to_analytical_key_domain <- function(data, analytical_keys, key_cols) {
  if (is.null(data) || !is.data.frame(data) || !nrow(data) ||
      is.null(analytical_keys) || !is.data.frame(analytical_keys) ||
      !nrow(analytical_keys) || !length(key_cols)) {
    return(list(data = data, excluded_rows = 0L, excluded_keys = data.frame()))
  }

  missing_cols <- setdiff(key_cols, intersect(names(data), names(analytical_keys)))
  if (length(missing_cols)) {
    stop(
      "Cannot apply the Analytical key domain because these columns are missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  normalize_key <- function(x) {
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    value <- trimws(as.character(x))
    value[is.na(x)] <- NA_character_
    tolower(value)
  }

  normalized_names <- paste0(".__analytical_key_", seq_along(key_cols))
  source <- as.data.frame(data)
  domain <- unique(as.data.frame(analytical_keys[, key_cols, drop = FALSE]))
  for (i in seq_along(key_cols)) {
    source[[normalized_names[[i]]]] <- normalize_key(source[[key_cols[[i]]]])
    domain[[normalized_names[[i]]]] <- normalize_key(domain[[key_cols[[i]]]])
  }

  valid_domain <- unique(domain[, normalized_names, drop = FALSE]) %>%
    dplyr::mutate(.__in_analytical_domain = TRUE)
  tagged <- source %>%
    dplyr::left_join(valid_domain, by = normalized_names) %>%
    dplyr::mutate(
      .__in_analytical_domain = tidyr::replace_na(.__in_analytical_domain, FALSE)
    )
  excluded <- tagged[!tagged$.__in_analytical_domain, , drop = FALSE]
  kept <- tagged[tagged$.__in_analytical_domain, , drop = FALSE]

  list(
    data = kept %>%
      dplyr::select(-dplyr::all_of(c(normalized_names, ".__in_analytical_domain"))),
    excluded_rows = nrow(excluded),
    excluded_keys = excluded %>%
      dplyr::select(dplyr::all_of(key_cols)) %>%
      dplyr::distinct()
  )
}

# Compare exact Analytical keys with filtered RAE and the final modeled splits.
# Analytical defines both the period range and the valid cross-section keys.
build_canonical_total_check <- function(analytical, all_rags, result, cfg,
                                        cross_cols,
                                        schema_metadata = NULL,
                                        tolerance = 0.01) {
  empty_result <- function(status, message, diagnostics = list(),
                           stage_counts = data.frame(),
                           applied_filters = character(0),
                           warnings = character(0)) {
    list(
      status = status,
      summary = list(
        status = status,
        message = message,
        model_variable = cfg$model_variable %||% "",
        modeled_metric = normalize_model_metric(
          cfg$modeled_role %||% cfg$model_metric %||% "activity"
        ),
        min_period = as.Date(NA),
        max_period = as.Date(NA),
        keys = 0L,
        mismatches = 0L,
        comparison_level = ""
      ),
      detail = tibble::tibble(),
      diagnostics = diagnostics,
      stage_counts = stage_counts,
      applied_filters = applied_filters,
      warnings = warnings
    )
  }

  if (is.null(analytical) || !is.data.frame(analytical) ||
      !"Period" %in% names(analytical)) {
    return(empty_result("error", "Analytical is unavailable or is missing Period."))
  }
  model_contract_candidates <- unique(trimws(as.character(c(
    cfg$model_variable %||% character(0),
    cfg$modeled_analytical_variables %||% character(0),
    cfg$analytical_varkeys %||% character(0)
  ))))
  model_contract_candidates <- model_contract_candidates[
    !is.na(model_contract_candidates) & nzchar(model_contract_candidates)
  ]
  exact_model_candidates <- model_contract_candidates[
    model_contract_candidates %in% names(analytical)
  ]
  configured_model <- cfg$model_variable %||% ""
  model_variable <- if (configured_model %in% names(analytical)) {
    configured_model
  } else if (length(exact_model_candidates) == 1L) {
    exact_model_candidates[[1]]
  } else {
    ""
  }
  if (!nzchar(model_variable)) {
    candidate_text <- if (length(exact_model_candidates)) {
      paste(exact_model_candidates, collapse = ", ")
    } else {
      "none"
    }
    return(empty_result(
      "error",
      paste0(
        "The channel does not resolve to one exact Analytical variable. ",
        "Explicit candidates found: ", candidate_text, "."
      ),
      diagnostics = list(model_variable_candidates = exact_model_candidates)
    ))
  }
  cfg$model_variable <- model_variable
  if (is.null(result) || is.null(result$rag)) {
    return(empty_result("error", "The channel has no processed split result."))
  }

  analytical_df <- as.data.frame(analytical)
  analytical_df$Period <- if (inherits(analytical_df$Period, "Date")) {
    analytical_df$Period
  } else {
    parse_period_robust(analytical_df$Period)
  }
  analytical_df$.__model_value <- suppressWarnings(as.numeric(
    as.character(analytical_df[[model_variable]])
  ))
  observed <- !is.na(analytical_df$Period) & !is.na(analytical_df$.__model_value)
  if (!any(observed)) {
    return(empty_result(
      "error",
      "The Analytical modeled variable has no dated numeric observations."
    ))
  }

  observed_periods <- analytical_df$Period[observed]
  min_p <- min(observed_periods)
  max_p <- max(observed_periods)
  cfg_min <- tryCatch(as.Date(cfg$min_period), error = function(e) as.Date(NA))
  cfg_max <- tryCatch(as.Date(cfg$max_period), error = function(e) as.Date(NA))
  if (length(cfg_min) && !is.na(cfg_min)) min_p <- max(min_p, cfg_min)
  if (length(cfg_max) && !is.na(cfg_max)) max_p <- min(max_p, cfg_max)
  if (is.na(min_p) || is.na(max_p) || min_p > max_p) {
    return(empty_result(
      "error",
      "The Analytical observations do not overlap the channel effective date range."
    ))
  }

  analytical_scoped <- analytical_df[
    observed & analytical_df$Period >= min_p & analytical_df$Period <= max_p,
    , drop = FALSE
  ]
  exact_periods <- sort(unique(analytical_scoped$Period))
  available_cross_cols <- intersect(cross_cols, names(analytical_scoped))
  if (length(setdiff(cross_cols, names(analytical_scoped)))) {
    missing_cross <- setdiff(cross_cols, names(analytical_scoped))
    return(empty_result(
      "error",
      paste0("Analytical is missing Total Check key columns: ",
             paste(missing_cross, collapse = ", "), ".")
    ))
  }
  # Cross-sectional dimensions are always part of the reconciliation key.
  # Repeated Analytical totals do not make distinct entities interchangeable.
  replicated_cross_cols <- character(0)
  comparison_cross_cols <- available_cross_cols
  key_cols <- unique(c(comparison_cross_cols, "Period"))

  model_result <- collapse_total_check_source(
    analytical_scoped, key_cols, ".__model_value", "ModelTotal", tolerance
  )
  model_side <- model_result$data %>% dplyr::mutate(.__model_present = TRUE)
  analytical_key_domain <- model_side[, key_cols, drop = FALSE]

  effective_rae <- filter_effective_channel_rae(
    all_rags = all_rags,
    cfg = cfg,
    min_period = min_p,
    max_period = max_p,
    exact_periods = exact_periods,
    schema_metadata = schema_metadata,
    segment_overrides = cfg$segment_overrides %||% list(),
    role = "modeled"
  )
  rae_warnings <- effective_rae$warnings %||% character(0)
  if (model_result$conflicting_keys > 0L) {
    rae_warnings <- c(
      rae_warnings,
      paste(model_result$conflicting_keys,
            "Analytical keys contained different replicated values and were summed.")
    )
  }

  rae_side <- model_side[0, key_cols, drop = FALSE] %>%
    dplyr::mutate(
      RAEEffectiveTotal = numeric(0),
      .__rae_present = logical(0)
    )
  if (!nzchar(effective_rae$failure_stage %||% "")) {
    rae_data <- effective_rae$data
    missing_keys <- setdiff(key_cols, names(rae_data))
    if (length(missing_keys)) {
      return(empty_result(
        "error",
        paste0("RAE is missing Total Check key columns: ",
               paste(missing_keys, collapse = ", "), "."),
        diagnostics = list(failure_stage = "rae_keys"),
        stage_counts = effective_rae$stage_counts,
        applied_filters = effective_rae$applied_filters,
        warnings = rae_warnings
      ))
    }
    domain_result <- filter_to_analytical_key_domain(
      rae_data, analytical_key_domain, key_cols
    )
    rae_data <- domain_result$data
    effective_rae$stage_counts <- dplyr::bind_rows(
      effective_rae$stage_counts,
      data.frame(Stage = "analytical_keys", Rows = nrow(rae_data))
    )
    effective_rae$applied_filters <- unique(c(
      effective_rae$applied_filters,
      paste0("Exact Analytical key domain: ", paste(key_cols, collapse = " x "))
    ))
    if (domain_result$excluded_rows > 0L) {
      excluded_preview <- apply(
        utils::head(domain_result$excluded_keys, 5L), 1L,
        function(x) paste(x, collapse = " / ")
      )
      rae_warnings <- c(
        rae_warnings,
        paste0(
          format(domain_result$excluded_rows, big.mark = ","),
          " RAE rows outside the exact Analytical key domain were excluded. Extra keys: ",
          paste(excluded_preview, collapse = "; "), "."
        )
      )
    }
    rae_data$VariableValue <- suppressWarnings(as.numeric(as.character(rae_data$VariableValue)))
    rae_data$VariableValue[is.na(rae_data$VariableValue)] <- 0
    rae_source_keys <- unique(c(
      intersect(available_cross_cols, names(rae_data)), "Period"
    ))
    rae_by_cross <- data.table::as.data.table(rae_data)[
      , .(.__rae_cross_total = sum(VariableValue, na.rm = TRUE)),
      by = rae_source_keys
    ] %>%
      as.data.frame()
    rae_side <- collapse_total_check_source(
      rae_by_cross,
      key_cols,
      ".__rae_cross_total",
      "RAEEffectiveTotal",
      tolerance
    )$data %>%
      dplyr::mutate(.__rae_present = TRUE)
  }

  manifest <- result$split_manifest %||% tibble::tibble()
  modeled_splits <- if (nrow(manifest) &&
                        all(c("Role", "VariableSplit") %in% names(manifest))) {
    unique(as.character(manifest$VariableSplit[manifest$Role == "modeled"]))
  } else {
    character(0)
  }
  processed <- as.data.frame(result$rag)
  processed$Period <- if (inherits(processed$Period, "Date")) {
    processed$Period
  } else {
    parse_period_robust(processed$Period)
  }
  processed <- processed[processed$Period %in% exact_periods, , drop = FALSE]
  split_cols <- intersect(modeled_splits, names(processed))
  missing_split_cols <- setdiff(modeled_splits, names(processed))
  if (length(missing_split_cols)) {
    rae_warnings <- c(
      rae_warnings,
      paste0("Modeled split columns missing from the processed result: ",
             paste(missing_split_cols, collapse = ", "))
    )
  }
  processed_side <- model_side[0, key_cols, drop = FALSE] %>%
    dplyr::mutate(
      SplitTotal = numeric(0),
      .__split_present = logical(0)
    )
  if (nrow(processed) && length(split_cols) && all(key_cols %in% names(processed))) {
    processed$.__split_total <- rowSums(
      as.data.frame(lapply(processed[split_cols], function(x) {
        value <- suppressWarnings(as.numeric(as.character(x)))
        value[is.na(value)] <- 0
        value
      })),
      na.rm = TRUE
    )
    processed_domain <- filter_to_analytical_key_domain(
      processed, analytical_key_domain, key_cols
    )
    processed <- processed_domain$data
    processed_source_keys <- unique(c(
      intersect(available_cross_cols, names(processed)), "Period"
    ))
    processed_by_cross <- data.table::as.data.table(processed)[
      , .(.__processed_cross_total = sum(.__split_total, na.rm = TRUE)),
      by = processed_source_keys
    ] %>%
      as.data.frame()
    processed_side <- collapse_total_check_source(
      processed_by_cross,
      key_cols,
      ".__processed_cross_total",
      "SplitTotal",
      tolerance
    )$data %>%
      dplyr::mutate(.__split_present = TRUE)
  }

  joined <- model_side %>%
    dplyr::full_join(rae_side, by = key_cols) %>%
    dplyr::full_join(processed_side, by = key_cols) %>%
    dplyr::mutate(
      .__model_present = tidyr::replace_na(.__model_present, FALSE),
      .__rae_present = tidyr::replace_na(.__rae_present, FALSE),
      .__split_present = tidyr::replace_na(.__split_present, FALSE),
      ModelTotal = tidyr::replace_na(ModelTotal, 0),
      RAEEffectiveTotal = tidyr::replace_na(RAEEffectiveTotal, 0),
      SplitTotal = tidyr::replace_na(SplitTotal, 0),
      ModelVsRAEDiff = ModelTotal - RAEEffectiveTotal,
      RAEVsSplitDiff = RAEEffectiveTotal - SplitTotal,
      ModelVsSplitDiff = ModelTotal - SplitTotal,
      .__model_rae_ok = total_check_values_match(ModelTotal, RAEEffectiveTotal, tolerance),
      .__rae_split_ok = total_check_values_match(RAEEffectiveTotal, SplitTotal, tolerance),
      Status = dplyr::case_when(
        !.__model_present ~ "Missing key",
        (!.__rae_present | !.__split_present) &
          (abs(ModelTotal) > tolerance | abs(RAEEffectiveTotal) > tolerance |
             abs(SplitTotal) > tolerance) ~ "Missing key",
        !.__model_rae_ok ~ "Source/Filter mismatch",
        !.__rae_split_ok ~ "Processing mismatch",
        TRUE ~ "OK"
      )
    ) %>%
    dplyr::filter(
      .__model_present |
        abs(RAEEffectiveTotal) > tolerance |
        abs(SplitTotal) > tolerance
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))

  detail <- joined %>%
    dplyr::select(
      dplyr::all_of(key_cols), ModelTotal, RAEEffectiveTotal, SplitTotal,
      ModelVsRAEDiff, RAEVsSplitDiff, ModelVsSplitDiff, Status
    )
  mismatch_count <- sum(detail$Status != "OK", na.rm = TRUE)
  overall_status <- if (mismatch_count == 0L) "ok" else "mismatch"
  message <- if (identical(overall_status, "ok")) {
    paste(format(nrow(detail), big.mark = ","), "keys match across Analytical, effective RAE and processed splits.")
  } else {
    paste(mismatch_count, "of", nrow(detail), "keys require review.")
  }

  list(
    status = overall_status,
    summary = list(
      status = overall_status,
      message = message,
      model_variable = model_variable,
      modeled_metric = normalize_model_metric(
        cfg$modeled_role %||% cfg$model_metric %||% "activity"
      ),
      min_period = min_p,
      max_period = max_p,
      keys = nrow(detail),
      mismatches = mismatch_count,
      comparison_level = paste(key_cols, collapse = " × "),
      source_mismatches = sum(detail$Status == "Source/Filter mismatch"),
      processing_mismatches = sum(detail$Status == "Processing mismatch"),
      missing_keys = sum(detail$Status == "Missing key")
    ),
    detail = detail,
    diagnostics = list(
      exact_periods = exact_periods,
      modeled_splits = modeled_splits,
      missing_split_columns = missing_split_cols,
      analytical_duplicate_keys = model_result$duplicate_keys,
      analytical_conflicting_keys = model_result$conflicting_keys,
      comparison_cross_cols = comparison_cross_cols,
      replicated_cross_cols = replicated_cross_cols,
      rae_failure_stage = effective_rae$failure_stage %||% "",
      rae_failure_reason = effective_rae$reason %||% ""
    ),
    stage_counts = effective_rae$stage_counts,
    applied_filters = effective_rae$applied_filters,
    warnings = unique(rae_warnings)
  )
}
