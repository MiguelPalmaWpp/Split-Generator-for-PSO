# ── Helpers ───────────────────────────────────────────────────

ordinal_tag <- function(i) {
  c("First", "Second", "Third", "Fourth", "Fifth")[min(i, 5)]
}

keep_nonzero_cols <- function(df) {
  df     <- as.data.frame(df)
  is_num <- sapply(df, is.numeric)
  keep   <- sapply(names(df), function(col) {
    if (!is_num[[col]]) return(TRUE)
    sum(df[[col]], na.rm = TRUE) != 0
  })
  df[, keep, drop = FALSE]
}

# ── Diagnosis slice helper ────────────────────────────────────
#   all_transformed (is_rag=TRUE):  1 row/period  → return as-is
#   all_RAGs        (is_rag=FALSE): N geos × M products/period
#                                   → filter to FIRST cross-section
#
# "First" = first alphabetical combination of cross_cols
# (e.g. "Alexandria LA / Convenience" if Geography × Product)
#
# The user can validate by filtering raw data to this same combination.
# The ref_cross_key (returned separately) tells them which one was used.

get_diag_df <- function(df, is_rag, cross_cols, ref_cross_key) {
  if (is_rag || nrow(df) == 0) return(df)
  
  cross_data <- df[, cross_cols, drop = FALSE]
  cross_key  <- do.call(paste, c(as.list(cross_data), list(sep = " / ")))
  df[cross_key == ref_cross_key, ]
}

# ── Private assemblers ────────────────────────────────────────

build_activity_spend <- function(act_all, cost_all, cfg) {
  if (nrow(act_all) == 0) return(tibble())
  
  cost_key <- if (nrow(cost_all) > 0)
    cost_all %>%
    select(VariableSplit_c = VariableSplit, total_spend) %>%
    mutate(key = str_remove_all(
      VariableSplit_c,
      regex(cfg$spend_keyword, ignore_case = TRUE)
    ))
  else
    tibble(key = character(), total_spend = numeric(),
           VariableSplit_c = character())
  
  act_all %>%
    select(VariableSplit, total_activity, model_var) %>%
    mutate(key = str_remove_all(
      VariableSplit,
      regex(cfg$activity_keyword, ignore_case = TRUE)
    )) %>%
    left_join(cost_key, by = "key") %>%
    mutate(
      Channel               = cfg$channel_name,
      MainModelVariableName = model_var
    ) %>%
    select(VariableSplit, total_activity, total_spend,
           Channel, MainModelVariableName)
}

build_side_mapping <- function(act_all) {
  if (nrow(act_all) == 0) return(tibble())
  
  act_all %>%
    select(VariableSplit, model_var) %>%
    mutate(
      MainModelVariableName = model_var,
      Weight    = 1,
      MinWeight = 0.5,
      MaxWeight = 2,
      rank      = NA_real_
    ) %>%
    select(-model_var)
}

# ── Main processing function ──────────────────────────────────
# Source determines strategy — no detect_rag():
#   cfg$data_source == "all_transformed" → is_rag = TRUE
#   cfg$data_source == "all_rags"        → is_rag = FALSE
#
# RAG output (res$rag): always built with FULL cross-section data
# Diagnosis stats:      built with FIRST cross-section (for all_RAGs)
#   → user validates by filtering raw data to res$ref_cross

process_channel <- function(all_transformeds, all_rags, analytical, dates_df,
                            cfg, cross_cols,
                            start_report_date, end_report_date, update_label) {
  
  # ── Strip all grouping ────────────────────────────────────
  if (!is.null(all_transformeds)) all_transformeds <- as.data.frame(all_transformeds)
  if (!is.null(all_rags))         all_rags         <- as.data.frame(all_rags)
  analytical <- as.data.frame(analytical)
  dates_df   <- as.data.frame(dates_df)
  
  # ── Strategy from data_source ─────────────────────────────
  is_rag      <- (cfg$data_source %||% "all_transformed") != "all_rags"
  source_data <- if (is_rag) all_transformeds else all_rags
  
  if (is.null(source_data))
    stop("Data source '", cfg$data_source, "' not uploaded.")
  
  # ── Reference cross-section for diagnosis ─────────────────
  # all_transformed: single geo, no filtering needed
  # all_RAGs: determine first cross-section alphabetically
  # Stored in result so mod_process can display it to the user
  ref_cross_key <- if (!is_rag && length(cross_cols) > 0) {
    cross_data <- source_data[, cross_cols, drop = FALSE]
    cross_key  <- do.call(paste, c(as.list(cross_data), list(sep = " / ")))
    sort(unique(cross_key))[1]
  } else {
    "national (all_transformed)"
  }
  
  # ── Build base grid ───────────────────────────────────────
  cross_id   <- c(cross_cols, "Period")
  join_key   <- if (is_rag) "Period" else cross_id
  id_protect <- if (is_rag) "Period" else cross_id
  
  rag_base <- unique(analytical[, cross_id, drop = FALSE])
  rag_base <- rag_base[order(rag_base$Period), ]
  
  n     <- length(cfg$model_variables)
  bks   <- as.Date(cfg$break_dates)
  s_beg <- c(as.Date(NA_character_), bks)
  s_end <- c(bks, as.Date(end_report_date))
  
  rag_joins <- list()
  act_rows  <- list()
  cost_rows <- list()
  
  for (i in seq_len(n)) {
    
    # 1. Segment date window
    d <- source_data
    if (!is.na(s_beg[i])) d <- d[d$Period >= s_beg[i], ]
    d <- d[d$Period <= s_end[i], ]
    d <- as.data.frame(d)
    
    # 2. Channel-specific filters
    vi <- cfg$varname_include[nchar(cfg$varname_include %||% "") > 0]
    if (length(vi) > 0) {
      keep <- Reduce("|", lapply(vi, function(p)
        grepl(p, d$VariableName, ignore.case = TRUE)))
      d <- d[keep, ]
    }
    
    for (p in cfg$varname_exclude)
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, d$VariableName, ignore.case = TRUE), ]
    
    for (p in cfg$geography_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, d$Geography, ignore.case = TRUE), ]
    
    for (p in cfg$campaign_exclude)
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, d$Campaign, ignore.case = TRUE), ]
    
    for (p in cfg$outlet_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, d$Outlet, ignore.case = TRUE), ]
    
    for (p in cfg$creative_exclude %||% character(0))
      if (nchar(p %||% "") > 0)
        d <- d[!grepl(p, d$Creative, ignore.case = TRUE), ]
    
    
    if (nrow(d) == 0) next
    
    # Defensive: ensure VariableValue is numeric
    d$VariableValue <- suppressWarnings(as.numeric(as.character(d$VariableValue)))
    d$VariableValue[is.na(d$VariableValue)] <- 0
    
    # 3. Build SplitName
    d$SplitName <- apply(
      d[, cfg$split_columns, drop = FALSE], 1, paste, collapse = "_"
    )
    
    # 4. Pivot wide
    if (is_rag) {
      d_wide <- as_tibble(d) %>%
        pivot_wider(
          id_cols     = Period,
          names_from  = SplitName,
          values_from = VariableValue,
          values_fn   = first,
          values_fill = 0
        ) %>%
        as.data.frame()
      d_wide <- merge(dates_df, d_wide, by = "Period", all.x = TRUE)
      
    } else {
      d_wide <- as_tibble(d) %>%
        pivot_wider(
          id_cols     = all_of(cross_id),
          names_from  = SplitName,
          values_from = VariableValue,
          values_fn   = sum,
          values_fill = 0
        ) %>%
        as.data.frame()
      d_wide <- merge(rag_base, d_wide, by = cross_id, all.x = TRUE)
    }
    
    d_wide[is.na(d_wide)] <- 0
    d_wide <- d_wide[order(d_wide$Period), ]
    
    nf_sfx <- if (n == 1)
      paste0("Before_", update_label)
    else
      paste0("Before_", update_label, "|", ordinal_tag(i), "TimeBreak")
    
    # 5. Non-focus slice
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
        nf_act <- nf[, c(id_protect, act_col_names), drop = FALSE]
        
        # RAG output: full cross-section data
        rag_joins <- c(rag_joins, list(list(df = nf_act, key = join_key)))
        
        # Diagnosis: first cross-section only (no replication for all_RAGs)
        act_rows <- c(act_rows, list(
          splits_summary(
            get_diag_df(nf_act, is_rag, cross_cols, ref_cross_key),
            "activity"
          ) %>%
            mutate(period = "nonfocus", seg = i,
                   model_var = cfg$model_variables[i])
        ))
      }
      
      cost_col_names <- grep(cfg$spend_keyword, names(nf),
                             ignore.case = TRUE, value = TRUE)
      if (length(cost_col_names) > 0) {
        nf_cost <- nf[, c(id_protect, cost_col_names), drop = FALSE]
        cost_rows <- c(cost_rows, list(
          splits_summary(
            get_diag_df(nf_cost, is_rag, cross_cols, ref_cross_key),
            "spend"
          ) %>%
            mutate(period = "nonfocus", seg = i,
                   model_var = cfg$model_variables[i])
        ))
      }
    }
    
    # 6. Focus slice
    fc_raw <- as.data.frame(d_wide[
      d_wide$Period >= as.Date(start_report_date) &
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
        fc_act <- fc[, c(id_protect, act_col_fc), drop = FALSE]
        
        # RAG output: full cross-section data
        rag_joins <- c(rag_joins, list(list(df = fc_act, key = join_key)))
        
        # Diagnosis: first cross-section only
        act_rows <- c(act_rows, list(
          splits_summary(
            get_diag_df(fc_act, is_rag, cross_cols, ref_cross_key),
            "activity"
          ) %>%
            mutate(period = "focus", seg = i,
                   model_var = cfg$model_variables[i])
        ))
      }
      
      cost_col_fc <- grep(cfg$spend_keyword, names(fc),
                          ignore.case = TRUE, value = TRUE)
      if (length(cost_col_fc) > 0) {
        fc_cost <- fc[, c(id_protect, cost_col_fc), drop = FALSE]
        cost_rows <- c(cost_rows, list(
          splits_summary(
            get_diag_df(fc_cost, is_rag, cross_cols, ref_cross_key),
            "spend"
          ) %>%
            mutate(period = "focus", seg = i,
                   model_var = cfg$model_variables[i])
        ))
      }
    }
  }
  
  # 7. Assemble RAG (full data — all cross-sections)
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
    ref_cross      = ref_cross_key,   # ← which cross-section was used for diagnosis
    activity_spend = build_activity_spend(act_all, cost_all, cfg),
    side_mapping   = build_side_mapping(act_all),
    act_diagnoses  = act_all,
    cost_diagnoses = cost_all
  )
}