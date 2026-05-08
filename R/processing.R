# ════════════════════════════════════════════════════════════════
# R/processing.R
# ════════════════════════════════════════════════════════════════

ordinal_tag <- function(i) {
  c("First", "Second", "Third", "Fourth", "Fifth")[min(i, 5)]
}

keep_nonzero_cols <- function(df) {
  df   <- as.data.frame(df)
  keep <- vapply(df, function(col) {
    if (!is.numeric(col)) return(TRUE)
    sum(col, na.rm = TRUE) != 0
  }, logical(1))
  df[, keep, drop = FALSE]
}

apply_dimension_breaks <- function(d, dimension_breaks,
                                   channel_name = NULL) {
  if (!length(dimension_breaks)) return(d)
  for (brk in dimension_breaks) {
    col <- brk$column; sep <- brk$separator; n <- brk$n_parts
    if (!col %in% names(d)) next
    parts   <- strsplit(as.character(d[[col]]), sep, fixed = TRUE)
    n_short <- sum(sapply(parts, length) < n)
    if (n_short > 0)
      warning(sprintf(
        "apply_dimension_breaks: %d value(s) in '%s' have fewer than %d parts.",
        n_short, col, n))
    for (i in seq_len(n)) {
      d[[brk$names[i]]] <- sapply(parts, function(p) {
        if (length(p) < i) p[length(p)]
        else if (i == n)   paste(p[i:length(p)], collapse = sep)
        else               p[i]
      })
    }
  }
  d
}

# is_rag = TRUE  → all_rags  (geographic: filter to first cross-section)
# is_rag = FALSE → all_transformed (national: return as-is)
get_diag_df <- function(df, is_rag, cross_cols, ref_cross_key) {
  if (!is_rag || nrow(df) == 0) return(df)
  cross_data <- df[, cross_cols, drop = FALSE]
  cross_key  <- do.call(paste, c(as.list(cross_data), list(sep = " / ")))
  df[cross_key == ref_cross_key, ]
}

build_model_total <- function(analytical, cross_id,
                              model_variables, break_dates) {
  n_vars        <- length(model_variables)
  break_dates_d <- as.Date(break_dates %||% character(0))
  base <- analytical %>% select(all_of(cross_id)) %>% mutate(ModelTotal = 0)
  for (i in seq_len(n_vars)) {
    mv <- model_variables[i]
    if (!mv %in% names(analytical)) {
      warning(sprintf(
        "build_model_total: variable '%s' not found in AnalyticalDataset. Segment %d will have ModelTotal=0.",
        mv, i))
      next
    }
    seg_start <- if (i == 1)      as.Date("1900-01-01")
    else             break_dates_d[i - 1] + 1
    seg_end   <- if (i == n_vars) as.Date("2999-12-31")
    else             break_dates_d[i]
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

build_activity_spend <- function(act_all, cost_all, cfg) {
  if (nrow(act_all) == 0) return(tibble())
  cost_key <- if (nrow(cost_all) > 0)
    cost_all %>%
    select(VariableSplit_c = VariableSplit, total_spend) %>%
    mutate(key = str_remove_all(VariableSplit_c,
                                regex(cfg$spend_keyword, ignore_case = TRUE)))
  else
    tibble(key = character(), total_spend = numeric(),
           VariableSplit_c = character())
  act_all %>%
    select(VariableSplit, total_activity, model_var) %>%
    mutate(key = str_remove_all(VariableSplit,
                                regex(cfg$activity_keyword, ignore_case = TRUE))) %>%
    left_join(cost_key, by = "key") %>%
    mutate(Channel = cfg$channel_name, MainModelVariableName = model_var) %>%
    select(VariableSplit, total_activity, total_spend,
           Channel, MainModelVariableName)
}

build_side_mapping <- function(act_all) {
  if (nrow(act_all) == 0) return(tibble())
  act_all %>%
    select(VariableSplit, model_var) %>%
    mutate(MainModelVariableName = model_var, Weight = 1,
           MinWeight = 0.5, MaxWeight = 2, rank = NA_real_) %>%
    select(-model_var)
}

# ── Main processing function ───────────────────────────────────────
process_channel <- function(all_transformeds, all_rags,
                            analytical, dates_df,
                            cfg, cross_cols,
                            source_type       = "all_transformed",
                            start_report_date,
                            end_report_date,
                            update_label,
                            dimension_breaks  = list(),
                            segment_overrides = list()) {
  
  if (!is.null(all_transformeds)) all_transformeds <- as.data.frame(all_transformeds)
  if (!is.null(all_rags))         all_rags         <- as.data.frame(all_rags)
  analytical <- as.data.frame(analytical)
  dates_df   <- as.data.frame(dates_df)
  
  # is_rag = TRUE  → all_rags  (geographic)
  # is_rag = FALSE → all_transformed (national)
  is_rag      <- source_type == "all_rags"
  source_data <- if (is_rag) all_rags else all_transformeds
  if (is.null(source_data))
    stop("Data source '", source_type, "' not uploaded.")
  
  ref_cross_key <- if (is_rag && length(cross_cols) > 0) {
    cross_data <- source_data[, cross_cols, drop = FALSE]
    cross_key  <- do.call(paste, c(as.list(cross_data), list(sep = " / ")))
    sort(unique(cross_key))[1]
  } else { "national (all_transformed)" }
  
  cross_id   <- c(cross_cols, "Period")
  
  # National: join by Period only → rag_base from analytical is fine
  # Geographic: join by full cross_id → rag_base MUST come from source_data
  #   to guarantee cross_id values match d_wide (same file, same naming)
  join_key   <- if (!is_rag) "Period" else cross_id
  id_protect <- if (!is_rag) "Period" else cross_id
  
  rag_base <- if (is_rag) {
    # KEY FIX: use source_data so Geography/Product/Period values
    # always match d_wide in the cross_id merge
    unique(source_data[, cross_id, drop = FALSE])
  } else {
    unique(analytical[, cross_id, drop = FALSE])
  }
  rag_base <- rag_base[order(rag_base$Period), ]
  
  n     <- length(cfg$model_variables)
  bks   <- as.Date(cfg$break_dates)
  s_beg <- c(as.Date(NA_character_), bks)
  s_end <- c(bks, as.Date(end_report_date))
  
  rag_joins  <- list()
  act_rows   <- list()
  cost_rows  <- list()
  
  has_geo_overrides <- length(segment_overrides) > 0 &&
    any(sapply(segment_overrides,
               \(o) length(o$geography_exclude %||% character(0)) > 0))
  
  # Pre-filter: channel-level filters applied ONCE
  d_prefilt <- source_data
  
  vi <- cfg$varname_include[nchar(cfg$varname_include %||% "") > 0]
  if (length(vi) > 0) {
    keep      <- Reduce("|", lapply(vi, function(p)
      grepl(p, d_prefilt$VariableName, ignore.case = TRUE)))
    d_prefilt <- d_prefilt[keep, ]
  }
  for (p in cfg$varname_exclude)
    if (nchar(p %||% "") > 0)
      d_prefilt <- d_prefilt[!grepl(p, d_prefilt$VariableName,
                                    ignore.case = TRUE), ]
  
  if (!has_geo_overrides) {
    for (p in cfg$geography_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d_prefilt <- d_prefilt[!grepl(p, d_prefilt$Geography,
                                      ignore.case = TRUE), ]
  }
  for (p in cfg$campaign_exclude)
    if (nchar(p %||% "") > 0)
      d_prefilt <- d_prefilt[!grepl(p, d_prefilt$Campaign,
                                    ignore.case = TRUE), ]
  for (p in cfg$outlet_exclude %||% character(0))
    if (nchar(p %||% "") > 0)
      d_prefilt <- d_prefilt[!grepl(p, d_prefilt$Outlet,
                                    ignore.case = TRUE), ]
  for (p in cfg$creative_exclude %||% character(0))
    if (nchar(p %||% "") > 0)
      d_prefilt <- d_prefilt[!grepl(p, d_prefilt$Creative,
                                    ignore.case = TRUE), ]
  
  for (i in seq_len(n)) {
    d <- d_prefilt
    
    if (has_geo_overrides) {
      seg_ovr <- Filter(\(o) isTRUE(o$seg == i), segment_overrides)
      geo_exc <- if (length(seg_ovr) > 0)
        seg_ovr[[1]]$geography_exclude %||% character(0)
      else cfg$geography_exclude %||% character(0)
      for (p in geo_exc)
        if (nchar(p %||% "") > 0)
          d <- d[!grepl(p, d$Geography, ignore.case = TRUE), ]
    }
    
    if (!is.na(s_beg[i])) d <- d[d$Period >= s_beg[i], ]
    d <- d[d$Period <= s_end[i], ]
    d <- as.data.frame(d)
    if (nrow(d) == 0) next
    
    d$VariableValue <- suppressWarnings(
      as.numeric(as.character(d$VariableValue)))
    d$VariableValue[is.na(d$VariableValue)] <- 0
    
    d <- apply_dimension_breaks(d, dimension_breaks,
                                channel_name = cfg$channel_name)
    
    d$SplitName <- apply(d[, cfg$split_columns, drop = FALSE],
                         1, paste, collapse = "_")
    
    # Pivot wide
    # National  (!is_rag): id_cols = Period, merge with dates_df
    # Geographic (is_rag): id_cols = cross_id, merge with rag_base
    if (!is_rag) {
      d_wide <- as_tibble(d) %>%
        pivot_wider(id_cols = Period, names_from = SplitName,
                    values_from = VariableValue,
                    values_fn = first, values_fill = 0) %>%
        as.data.frame()
      d_wide <- merge(dates_df, d_wide, by = "Period", all.x = TRUE)
    } else {
      d_wide <- as_tibble(d) %>%
        pivot_wider(id_cols = all_of(cross_id), names_from = SplitName,
                    values_from = VariableValue,
                    values_fn = sum, values_fill = 0) %>%
        as.data.frame()
      # rag_base is from source_data → cross_id values guaranteed to match
      d_wide <- merge(rag_base, d_wide, by = cross_id, all.x = TRUE)
    }
    
    d_wide[is.na(d_wide)] <- 0
    d_wide <- d_wide[order(d_wide$Period), ]
    
    nf_sfx <- if (n == 1) paste0("Before_", update_label)
    else paste0("Before_", update_label, "|", ordinal_tag(i), "TimeBreak")
    
    # Non-focus slice
    nf_raw <- as.data.frame(d_wide[d_wide$Period < as.Date(start_report_date), ])
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
          splits_summary(get_diag_df(nf_act, is_rag, cross_cols, ref_cross_key),
                         "activity") %>%
            mutate(period = "nonfocus", seg = i, model_var = cfg$model_variables[i])
        ))
      }
      cost_col_names <- grep(cfg$spend_keyword, names(nf),
                             ignore.case = TRUE, value = TRUE)
      if (length(cost_col_names) > 0) {
        nf_cost   <- nf[, c(id_protect, cost_col_names), drop = FALSE]
        cost_rows <- c(cost_rows, list(
          splits_summary(get_diag_df(nf_cost, is_rag, cross_cols, ref_cross_key),
                         "spend") %>%
            mutate(period = "nonfocus", seg = i, model_var = cfg$model_variables[i])
        ))
      }
    }
    
    # Focus slice
    fc_raw <- as.data.frame(
      d_wide[d_wide$Period >= as.Date(start_report_date) &
               d_wide$Period <= as.Date(end_report_date), ])
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
          splits_summary(get_diag_df(fc_act, is_rag, cross_cols, ref_cross_key),
                         "activity") %>%
            mutate(period = "focus", seg = i, model_var = cfg$model_variables[i])
        ))
      }
      cost_col_fc <- grep(cfg$spend_keyword, names(fc),
                          ignore.case = TRUE, value = TRUE)
      if (length(cost_col_fc) > 0) {
        fc_cost   <- fc[, c(id_protect, cost_col_fc), drop = FALSE]
        cost_rows <- c(cost_rows, list(
          splits_summary(get_diag_df(fc_cost, is_rag, cross_cols, ref_cross_key),
                         "spend") %>%
            mutate(period = "focus", seg = i, model_var = cfg$model_variables[i])
        ))
      }
    }
  }
  
  rag <- rag_base
  for (j in rag_joins) {
    rag <- merge(rag, j$df, by = j$key, all.x = TRUE)
    rag[is.na(rag)] <- 0
  }
  
  act_all  <- bind_rows(act_rows)
  cost_all <- bind_rows(cost_rows)
  
  list(
    rag            = rag,
    cross_cols     = cross_cols,
    is_rag         = is_rag,
    ref_cross      = ref_cross_key,
    activity_spend = build_activity_spend(act_all, cost_all, cfg),
    side_mapping   = build_side_mapping(act_all),
    act_diagnoses  = act_all,
    cost_diagnoses = cost_all
  )
}