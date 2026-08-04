# =============================================================================
# R/processing.R
# =============================================================================

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

apply_dimension_breaks <- function(d, dimension_breaks, channel_name = NULL) {
  if (!length(dimension_breaks)) return(d)
  for (brk in dimension_breaks) {
    col <- brk$column; sep <- brk$separator; n <- brk$n_parts
    missing_part_value <- brk$missing_part_value %||% "Total"
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
           Weight    = 1, MinWeight = 0.5, MaxWeight = 2,
           rank      = NA_real_) %>%
    select(-model_var)
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

 # RAG activity
  selected_splits <- rag_cols
  new_rag             <- res$rag
  new_rag[[new_name]] <- rowSums(new_rag[, rag_cols, drop = FALSE], na.rm = TRUE)
  new_rag             <- new_rag[, setdiff(names(new_rag), setdiff(rag_cols, new_name))]

 # RAG spend
  spend_rag_cands  <- stringr::str_replace_all(
    rag_cols, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
  spend_rag_cands  <- spend_rag_cands[!identical(act_kw, spend_kw) &
                                        spend_rag_cands != rag_cols]
  spend_rag_cols   <- intersect(spend_rag_cands, names(new_rag))
  new_spend_rag_nm <- stringr::str_replace_all(
    new_name, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
  if (new_spend_rag_nm == new_name)
    new_spend_rag_nm <- paste0(new_name, "_", spend_kw)
  if (length(spend_rag_cols) > 0) {
    new_rag[[new_spend_rag_nm]] <- rowSums(
      new_rag[, spend_rag_cols, drop = FALSE], na.rm = TRUE)
    new_rag <- new_rag[, setdiff(names(new_rag), setdiff(spend_rag_cols, new_spend_rag_nm))]
  }

  # act_diagnoses
  selected_diag <- res$act_diagnoses %>%
    dplyr::filter(VariableSplit %in% selected_splits, period == view_filter)
  if (nrow(selected_diag) == 0 && nrow(res$act_diagnoses) > 0)
    selected_diag <- res$act_diagnoses %>%
    dplyr::filter(VariableSplit %in% selected_splits)
  if (nrow(selected_diag) == 0) {
    rag_vals <- as.data.frame(new_rag)[[new_name]]
    non_zero <- rag_vals[!is.na(rag_vals) & rag_vals > 0]
    if (length(non_zero) > 0)
      selected_diag <- tibble::tibble(
        VariableSplit = new_name, total_activity = sum(non_zero),
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
      VariableSplit         = new_name,
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
    dplyr::filter(!VariableSplit %in% selected_splits)
  if (!is.null(merged_act))
    new_act_diag <- new_act_diag %>%
    dplyr::bind_rows(merged_act) %>%
    dplyr::group_by(period) %>%
    dplyr::mutate(
      grand_p            = sum(total_activity, na.rm = TRUE),
      pct_total_activity = round(total_activity / pmax(grand_p, 1) * 100, 4)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grand_p)

  merged_as <- res$activity_spend %>%
    dplyr::filter(VariableSplit %in% selected_splits) %>%
    dplyr::summarise(
      VariableSplit         = new_name,
      total_activity        = sum(total_activity, na.rm = TRUE),
      total_spend           = sum(total_spend,    na.rm = TRUE),
      Channel               = dplyr::first(Channel),
      MainModelVariableName = dplyr::first(MainModelVariableName))
  new_act_spend <- res$activity_spend %>%
    dplyr::filter(!VariableSplit %in% selected_splits) %>%
    dplyr::bind_rows(merged_as)

  merged_sm <- res$side_mapping %>%
    dplyr::filter(VariableSplit %in% selected_splits) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(VariableSplit = new_name)
  new_side_map <- res$side_mapping %>%
    dplyr::filter(!VariableSplit %in% selected_splits) %>%
    dplyr::bind_rows(merged_sm)

  stored_spend    <- unlist(merge_entry$spend_merged %||% character(0))
  derived_spend   <- stringr::str_replace_all(
    rag_cols, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
  all_spend_cands <- unique(c(stored_spend, derived_spend))
  all_spend_cands <- all_spend_cands[nzchar(all_spend_cands)]
  new_spend_name  <- if (nzchar(merge_entry$new_spend_name %||% ""))
    merge_entry$new_spend_name else new_spend_rag_nm
  matching_cost   <- intersect(all_spend_cands, res$cost_diagnoses$VariableSplit)

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

  model_metric <- normalize_model_metric(merge_metric %||% cfg$model_metric %||% "activity")
  model_diagnoses <- if (identical(model_metric, "spend")) new_cost_diag else new_act_diag
  model_side_map <- if (identical(model_metric, "spend")) {
    build_side_mapping(new_cost_diag)
  } else {
    new_side_map
  }

  out <- list(rag            = new_rag,
              cross_cols     = res$cross_cols,
              ref_cross      = res$ref_cross,
              activity_spend = new_act_spend,
              side_mapping   = model_side_map,
              act_diagnoses  = new_act_diag,
              cost_diagnoses = new_cost_diag,
              model_diagnoses = model_diagnoses,
              model_metric = model_metric)
  attr(out, "merge_status") <- merge_status(applied = TRUE, matched = rag_cols)
  out
}

# =============================================================================
# process_channel
# =============================================================================
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

  source_data <- all_rags
  if (is.null(source_data)) stop("All RAGs data not uploaded.")

 # Pre-convert all date params once
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
         min_period, " -> ", max_period, ").")

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

  if (!is.null(schema_metadata) &&
      !is.null(schema_metadata$name_lookup) &&
      length(cfg$analytical_varkeys %||% character(0)) > 0) {
    d_prefilt <- data.table::as.data.table(
      filter_to_analytical_varkey_combinations(
        as.data.frame(d_prefilt), cfg, schema_metadata
      )
    )
  }

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

    num_cols_w <- names(d_wide)[sapply(d_wide, is.numeric)]
    if (length(num_cols_w) > 0)
      data.table::setnafill(d_wide, fill = 0, cols = num_cols_w)

    d_wide <- as.data.frame(d_wide)
    d_wide <- d_wide[order(d_wide$Period), ]

 # Non-focus suffix
    nf_sfx <- {
      tbr <- cfg$time_break_label %||% ""
      if (nzchar(tbr)) paste0("Before ", update_label, "|", tbr)
      else             paste0("Before ", update_label)
    }

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




