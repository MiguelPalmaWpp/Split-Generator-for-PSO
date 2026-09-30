# Compare original component splits with final SAP aggregates.
build_split_composition_data <- function(export_data, channels_list = list(),
                                          channel_labels = list(), roi_data = NULL,
                                          roi_key_cols = character(0)) {
  if (!length(export_data)) return(NULL)

  normalize_split <- function(x) trimws(as.character(x))
  split_without_time <- function(x) stringr::str_remove(
    as.character(x), "(_Before(\\s+|_).*$|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$"
  )
  split_key_variants <- function(x) {
    x <- normalize_split(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) return(character(0))
    squish <- function(value) tolower(stringr::str_squish(as.character(value)))
    unique(c(x, split_without_time(x), squish(x), squish(split_without_time(x))))
  }
  is_focus <- function(x) {
    x <- normalize_split(x)
    !is.na(x) & nzchar(x) & !grepl("_Before(\\s+|_)", x, ignore.case = TRUE)
  }
  metric_variants <- function(split_name, cfg) {
    split_name <- normalize_split(split_name)
    split_name <- split_name[!is.na(split_name) & nzchar(split_name)]
    if (!length(split_name)) return(character(0))
    activity_keyword <- cfg$activity_keyword %||% ""
    spend_keyword <- cfg$spend_keyword %||% ""
    variants <- split_name
    if (nzchar(activity_keyword) && nzchar(spend_keyword)) {
      variants <- c(
        variants,
        stringr::str_replace(split_name, stringr::regex(activity_keyword, ignore_case = TRUE), spend_keyword),
        stringr::str_replace(split_name, stringr::regex(spend_keyword, ignore_case = TRUE), activity_keyword)
      )
    }
    unique(normalize_split(variants))
  }
  make_metric_lookup <- function(df, metric, cfg) {
    if (is.null(df) || !nrow(df) ||
        !all(c("VariableSplit", metric) %in% names(df))) {
      return(function(split_name) NA_real_)
    }
    values <- suppressWarnings(as.numeric(df[[metric]]))
    key_index <- new.env(parent = emptyenv(), hash = TRUE)
    for (i in seq_len(nrow(df))) {
      keys <- unique(unlist(lapply(
        metric_variants(df$VariableSplit[[i]], cfg), split_key_variants
      ), use.names = FALSE))
      for (key in keys[!is.na(keys) & nzchar(keys)]) {
        current <- get0(key, envir = key_index, ifnotfound = integer(), inherits = FALSE)
        assign(key, unique(c(current, i)), envir = key_index)
      }
    }
    function(split_name) {
      keys <- unique(unlist(lapply(metric_variants(split_name, cfg), split_key_variants),
                            use.names = FALSE))
      keys <- keys[!is.na(keys) & nzchar(keys)]
      if (!length(keys)) return(NA_real_)
      idx <- unique(unlist(mget(keys, envir = key_index,
                                ifnotfound = list(integer()), inherits = FALSE),
                           use.names = FALSE))
      found <- values[idx]
      found <- found[!is.na(found)]
      if (length(found)) sum(found, na.rm = TRUE) else NA_real_
    }
  }
  ensure_metric_column <- function(df, metric) {
    if (is.null(df)) df <- tibble::tibble()
    if (!"VariableSplit" %in% names(df)) df$VariableSplit <- character(nrow(df))
    if (!metric %in% names(df)) df[[metric]] <- numeric(nrow(df))
    df
  }
  normalize_roi_text_local <- function(x) {
    tolower(stringr::str_squish(trimws(as.character(x %||% ""))))
  }
  normalize_roi_mv_local <- function(x) {
    normalize_roi_text_local(stringr::str_remove(
      as.character(x %||% ""), stringr::regex("(_Total)+$", ignore_case = TRUE)
    ))
  }
  make_component_channel_lookup <- function(component_metrics, model_variable,
                                            fallback_channel) {
    rois <- roi_data
    if (is.null(rois) || !nrow(rois) ||
        !all(c("MainModelVariableName", "Channel") %in% names(rois)) ||
        is.null(component_metrics) || !nrow(component_metrics) ||
        !"VariableSplit" %in% names(component_metrics)) {
      return(function(split_name) fallback_channel)
    }
    keys <- setdiff(roi_key_cols, "Geography")
    meta_cols <- intersect(c("Sourced VariableName", keys), names(component_metrics))
    if (!length(meta_cols)) return(function(split_name) fallback_channel)

    rois$.mv_norm <- normalize_roi_mv_local(rois$MainModelVariableName)
    rois <- rois[rois$.mv_norm == normalize_roi_mv_local(model_variable), , drop = FALSE]
    if (!nrow(rois)) return(function(split_name) fallback_channel)
    if ("Sourced VariableName" %in% names(rois)) {
      rois$.source_norm <- normalize_roi_text_local(rois[["Sourced VariableName"]])
    }
    for (key in keys) {
      if (key %in% names(rois)) {
        rois[[paste0(".key_", key)]] <- normalize_roi_text_local(rois[[key]])
      }
    }

    channel_by_split <- new.env(parent = emptyenv(), hash = TRUE)
    meta <- dplyr::distinct(dplyr::select(
      component_metrics, "VariableSplit", dplyr::any_of(meta_cols)
    ))
    for (i in seq_len(nrow(meta))) {
      candidates <- rois
      if ("Sourced VariableName" %in% meta_cols &&
          "Sourced VariableName" %in% names(meta) && ".source_norm" %in% names(candidates)) {
        source_value <- normalize_roi_text_local(meta[["Sourced VariableName"]][[i]])
        if (nzchar(source_value)) {
          candidates <- candidates[
            nzchar(candidates$.source_norm) & candidates$.source_norm == source_value,
            , drop = FALSE
          ]
        }
      }
      for (key in keys) {
        meta_key <- paste0(".key_", key)
        if (!key %in% names(meta) || !meta_key %in% names(candidates)) next
        key_value <- normalize_roi_text_local(meta[[key]][[i]])
        if (!nzchar(key_value)) next
        candidates <- candidates[
          nzchar(candidates[[meta_key]]) & candidates[[meta_key]] == key_value,
          , drop = FALSE
        ]
      }
      channel_values <- unique(trimws(as.character(candidates$Channel)))
      channel_values <- channel_values[!is.na(channel_values) & nzchar(channel_values)]
      if (length(channel_values) == 1L) {
        for (key in split_key_variants(meta$VariableSplit[[i]])) {
          assign(key, channel_values[[1]], envir = channel_by_split)
        }
      }
    }
    function(split_name) {
      keys <- split_key_variants(split_name)
      hits <- unique(unlist(mget(keys, envir = channel_by_split,
                                 ifnotfound = list(character()), inherits = FALSE),
                           use.names = FALSE))
      hits <- hits[!is.na(hits) & nzchar(hits)]
      if (length(hits) == 1L) hits[[1]] else fallback_channel
    }
  }

  rows <- Filter(Negate(is.null), lapply(names(export_data), function(channel_name) {
    item <- export_data[[channel_name]]
    if (is.null(item) || is.null(item$res)) return(NULL)
    res <- item$res
    cfg <- channels_list[[channel_name]] %||% item$cfg %||% list()
    final <- item$final %||% tibble::tibble()
    if (!nrow(final)) return(NULL)
    pre_activity <- item$pre_act %||% final
    if (!nrow(pre_activity)) pre_activity <- final
    pre_spend <- item$pre_cost %||% tibble::tibble()
    resolved <- item$merge_resolved %||% list(
      map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
    )
    lineage <- resolved$map %||% tibble::tibble(
      MergedSplitName = character(), ComponentSplit = character()
    )
    channel_label <- channel_labels[[channel_name]] %||%
      cfg$media_channel %||% cfg$channel_name %||% channel_name
    model_variable <- cfg$model_variable %||% channel_name
    canonical <- item$canonical_totals

    has_canonical <- !is.null(canonical) &&
      nrow(canonical$component_focus_totals %||% tibble::tibble()) > 0 &&
      nrow(canonical$final_focus_totals %||% tibble::tibble()) > 0
    if (has_canonical) {
      lineage <- canonical$merge_map %||% lineage
      lineage <- lineage %>% dplyr::filter(
        is_focus(.data$MergedSplitName), is_focus(.data$ComponentSplit)
      )
      if (!nrow(lineage)) return(NULL)
      component_metrics <- canonical$component_focus_totals %>% dplyr::rename(
        total_activity = Component_Activity,
        total_spend = Component_Spend
      )
      final_metrics <- canonical$final_focus_totals %>% dplyr::rename(
        total_activity = Activity,
        total_spend = Spend
      )
    } else {
      lineage <- lineage %>% dplyr::filter(
        is_focus(.data$MergedSplitName), is_focus(.data$ComponentSplit)
      )
      if (!nrow(lineage)) return(NULL)
      rae_totals <- item$rae_totals %||% list()
      metric_pre_activity <- ensure_metric_column(
        rae_totals$activity %||% pre_activity, "total_activity"
      )
      metric_pre_spend <- ensure_metric_column(
        rae_totals$spend %||% pre_spend, "total_spend"
      )
      component_metrics <- dplyr::full_join(
        dplyr::select(metric_pre_activity, "VariableSplit", "total_activity"),
        dplyr::select(metric_pre_spend, "VariableSplit", "total_spend"),
        by = "VariableSplit"
      ) %>% dplyr::filter(is_focus(.data$VariableSplit))
      final_metrics <- component_metrics
    }

    component_metrics <- component_metrics %>% dplyr::filter(is_focus(.data$VariableSplit))
    final_metrics <- final_metrics %>% dplyr::filter(is_focus(.data$VariableSplit))
    lookup_channel <- make_component_channel_lookup(component_metrics, model_variable, channel_label)
    lookup_component_activity <- make_metric_lookup(component_metrics, "total_activity", cfg)
    lookup_component_spend <- make_metric_lookup(component_metrics, "total_spend", cfg)
    lookup_merged_activity <- make_metric_lookup(final_metrics, "total_activity", cfg)
    lookup_merged_spend <- make_metric_lookup(final_metrics, "total_spend", cfg)
    model_metric <- normalize_model_metric(
      res$modeled_role %||% cfg$modeled_role %||%
        res$model_metric %||% cfg$model_metric %||% "activity"
    )

    lineage %>% dplyr::mutate(
      Channel = vapply(.data$ComponentSplit, lookup_channel, character(1)),
      MainModelVariableName = model_variable,
      Component_Activity = vapply(.data$ComponentSplit, lookup_component_activity, numeric(1)),
      Component_Spend = vapply(.data$ComponentSplit, lookup_component_spend, numeric(1)),
      Merged_Activity = vapply(.data$MergedSplitName, lookup_merged_activity, numeric(1)),
      Merged_Spend = vapply(.data$MergedSplitName, lookup_merged_spend, numeric(1))
    ) %>% dplyr::mutate(
      Component_Activity = dplyr::coalesce(.data$Component_Activity, 0),
      Component_Spend = dplyr::coalesce(.data$Component_Spend, 0),
      Merged_Activity = dplyr::if_else(
        is.na(.data$Merged_Activity),
        ave(.data$Component_Activity, .data$MergedSplitName, FUN = sum),
        .data$Merged_Activity
      ),
      Merged_Spend = dplyr::if_else(
        is.na(.data$Merged_Spend),
        ave(.data$Component_Spend, .data$MergedSplitName, FUN = sum),
        .data$Merged_Spend
      ),
      Component_Pct = dplyr::if_else(
        if (identical(model_metric, "spend")) .data$Merged_Spend > 0 else .data$Merged_Activity > 0,
        if (identical(model_metric, "spend"))
          round(.data$Component_Spend / .data$Merged_Spend * 100, 2)
        else round(.data$Component_Activity / .data$Merged_Activity * 100, 2),
        NA_real_
      )
    )
  }))
  if (!length(rows)) return(NULL)
  result <- dplyr::bind_rows(rows)
  if (!nrow(result)) return(NULL)
  result %>%
    dplyr::arrange(
      .data$Channel, .data$MainModelVariableName, .data$MergedSplitName,
      dplyr::desc(.data$Component_Activity)
    ) %>%
    dplyr::select(
        "Channel", "MainModelVariableName", "MergedSplitName", "ComponentSplit",
        "Component_Activity", "Component_Pct", "Component_Spend",
        "Merged_Activity", "Merged_Spend"
      ) %>%
    dplyr::rename(
      `Total Activity` = Merged_Activity,
      `Total Spend` = Merged_Spend
    )
}
