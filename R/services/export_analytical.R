# Build Analytical Extended from source columns and prepared channel snapshots.
build_analytical_extended_data <- function(d, res_list, channels_list, gcfg,
                                           schema_metadata = NULL,
                                           export_data = NULL) {
  if (is.null(d$analytical)) return(NULL)
  cross_cols <- gcfg$cross_cols %||% "Geography"
  cross_id <- c(cross_cols, "Period")

  in_fixed_mv <- if (!is.null(d$details) &&
                     all(c("Type", "VariableName") %in% names(d$details))) {
    d$details %>%
      dplyr::filter(!stringr::str_detect(
        stringr::str_to_lower(trimws(.data$Type)), "none")) %>%
      dplyr::pull(.data$VariableName) %>% unique()
  } else {
    model_vars <- unique(vapply(
      channels_list, function(channel) channel$model_variable %||% "", character(1)
    ))
    model_vars[nzchar(model_vars)]
  }

  model_cols_an <- if (!is.null(schema_metadata) &&
                       !is.null(schema_metadata$name_lookup) &&
                       nrow(schema_metadata$name_lookup) > 0) {
    lookup <- schema_metadata$name_lookup
    direct <- intersect(in_fixed_mv, names(d$analytical))
    via_lookup <- lookup$OriginalName[
      lookup$VariableName %in% in_fixed_mv & !is.na(lookup$OriginalName)
    ]
    unique(c(direct, via_lookup))
  } else {
    intersect(in_fixed_mv, names(d$analytical))
  }
  model_cols_an <- intersect(model_cols_an, names(d$analytical))

  id_cols_an <- intersect(c(cross_cols, "Period", "BP_Year"), names(d$analytical))
  keep_an_cols <- union(id_cols_an, model_cols_an)
  selected_weight <- trimws(as.character(gcfg$weight_variable_name %||% ""))
  weight_col <- if (nzchar(selected_weight) && selected_weight %in% names(d$analytical)) {
    selected_weight
  } else {
    fallback_weight <- intersect("Weight Variable MMM", names(d$analytical))
    if (length(fallback_weight)) fallback_weight[[1]] else ""
  }
  if (nzchar(weight_col)) keep_an_cols <- union(keep_an_cols, weight_col)

  nonfocus_map <- d$side_mapping_nonfocus
  if (!is.null(nonfocus_map) && nrow(nonfocus_map) > 0 &&
      "VariableSplit" %in% names(nonfocus_map)) {
    nonfocus_cols <- intersect(nonfocus_map$VariableSplit, names(d$analytical))
    keep_an_cols <- union(keep_an_cols, nonfocus_cols)
  }
  result <- as.data.frame(d$analytical) %>%
    dplyr::select(dplyr::all_of(keep_an_cols))

  join_split_columns <- function(target, rag, split_cols, channel_name) {
    if (is.null(rag)) return(target)
    rag <- as.data.frame(rag)
    join_key <- intersect(cross_id, names(rag))
    valid_keys <- intersect(join_key, intersect(names(rag), names(target)))
    if (!length(valid_keys)) return(target)

    split_cols <- unique(as.character(split_cols %||% character(0)))
    split_cols <- split_cols[!is.na(split_cols) & nzchar(split_cols)]
    split_cols <- intersect(split_cols, names(rag))
    if (!length(split_cols)) return(target)

    rag_subset <- rag[, c(valid_keys, split_cols), drop = FALSE]
    conflicts <- intersect(split_cols, names(target))
    if (length(conflicts)) {
      names(rag_subset)[names(rag_subset) %in% conflicts] <-
        paste0(names(rag_subset)[names(rag_subset) %in% conflicts], "_", channel_name)
    }
    dplyr::left_join(target, rag_subset, by = valid_keys)
  }

  for (channel_name in names(res_list)) {
    result_item <- res_list[[channel_name]]
    if (is.null(result_item)) next
    channel_snapshot <- export_data[[channel_name]] %||% list()
    final_cols <- channel_snapshot$final$VariableSplit %||% character(0)
    pre_cols <- channel_snapshot$pre_act$VariableSplit %||% character(0)
    final_cols <- unique(as.character(final_cols))
    pre_cols <- unique(as.character(pre_cols))
    final_cols <- final_cols[!is.na(final_cols) & nzchar(final_cols)]
    pre_cols <- pre_cols[!is.na(pre_cols) & nzchar(pre_cols)]
    if (!length(pre_cols)) pre_cols <- final_cols

    clean_rag <- channel_snapshot$clean$rag %||% result_item$rag
    result <- join_split_columns(result, clean_rag, pre_cols, channel_name)
    result <- join_split_columns(
      result, result_item$rag, setdiff(final_cols, pre_cols), channel_name
    )
  }

  split_cols_out <- setdiff(names(result), keep_an_cols)
  numeric_split_cols <- split_cols_out[
    vapply(result[split_cols_out], is.numeric, logical(1))
  ]
  if (length(numeric_split_cols)) {
    result[numeric_split_cols] <- lapply(result[numeric_split_cols], function(x) {
      x[is.na(x)] <- 0
      x
    })
  }
  result
}
