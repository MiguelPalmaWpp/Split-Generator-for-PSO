# Build Side Model Mapping using processed split metadata and the export snapshot.
build_side_mapping_export_data <- function(res_list, channels_list = list(),
                                           side_mapping_nonfocus = NULL,
                                           export_data = NULL) {
  standard_cols <- c(
    "VariableSplit", "MainModelVariableName", "Weight", "MinWeight", "MaxWeight"
  )

  normalize_mapping <- function(df, channel_name, cfg = list()) {
    if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df)) return(NULL)
    out <- as.data.frame(df)
    out$VariableSplit <- trimws(as.character(out$VariableSplit))
    out <- out[!is.na(out$VariableSplit) & nzchar(out$VariableSplit), , drop = FALSE]
    if (!nrow(out)) return(NULL)

    if (!"MainModelVariableName" %in% names(out)) {
      out$MainModelVariableName <- if ("model_var" %in% names(out)) {
        out$model_var
      } else {
        cfg$model_variable %||% cfg$channel_name %||% channel_name
      }
    }
    out$MainModelVariableName <- trimws(as.character(out$MainModelVariableName))
    if (!"Weight" %in% names(out)) out$Weight <- 1
    if (!"MinWeight" %in% names(out)) out$MinWeight <- 0.5
    if (!"MaxWeight" %in% names(out)) out$MaxWeight <- 2
    out$Weight <- suppressWarnings(as.numeric(out$Weight))
    out$MinWeight <- suppressWarnings(as.numeric(out$MinWeight))
    out$MaxWeight <- suppressWarnings(as.numeric(out$MaxWeight))

    dplyr::distinct(
      dplyr::select(tibble::as_tibble(out), dplyr::any_of(standard_cols)),
      .data$VariableSplit, .data$MainModelVariableName, .keep_all = TRUE
    )
  }

  rows <- Filter(Negate(is.null), lapply(names(res_list), function(channel_name) {
    res <- res_list[[channel_name]]
    cfg <- channels_list[[channel_name]] %||% list()
    snapshot_item <- export_data[[channel_name]] %||% list()
    final <- snapshot_item$final %||% NULL

    if (is.null(final)) {
      final <- res$side_mapping %||% NULL
      if ((is.null(final) || !nrow(final)) &&
          !is.null(res$split_manifest) && nrow(res$split_manifest) &&
          all(c("Role", "VariableSplit") %in% names(res$split_manifest))) {
        final <- res$split_manifest %>%
          dplyr::filter(.data$Role == "modeled") %>%
          dplyr::transmute(
            VariableSplit = .data$VariableSplit,
            MainModelVariableName = cfg$model_variable %||% channel_name
          ) %>%
          dplyr::distinct(.data$VariableSplit, .keep_all = TRUE)
      }
    }
    final <- normalize_mapping(final, channel_name, cfg)
    if (is.null(final) || !nrow(final)) return(NULL)

    out <- final %>%
      dplyr::select("VariableSplit", "MainModelVariableName") %>%
      dplyr::mutate(Weight = 1, MinWeight = 0.5, MaxWeight = 2)
    side_meta <- normalize_mapping(res$side_mapping, channel_name, cfg)
    if (!is.null(side_meta) && nrow(side_meta)) {
      side_meta <- side_meta %>%
        dplyr::filter(.data$VariableSplit %in% out$VariableSplit) %>%
        dplyr::transmute(
          VariableSplit = .data$VariableSplit,
          Weight.side = .data$Weight,
          MinWeight.side = .data$MinWeight,
          MaxWeight.side = .data$MaxWeight
        ) %>%
        dplyr::distinct(.data$VariableSplit, .keep_all = TRUE)
      out <- out %>%
        dplyr::left_join(side_meta, by = "VariableSplit") %>%
        dplyr::mutate(
          Weight = dplyr::coalesce(.data$Weight.side, .data$Weight),
          MinWeight = dplyr::coalesce(.data$MinWeight.side, .data$MinWeight),
          MaxWeight = dplyr::coalesce(.data$MaxWeight.side, .data$MaxWeight)
        ) %>%
        dplyr::select(dplyr::all_of(standard_cols))
    }
    dplyr::select(out, dplyr::all_of(standard_cols))
  }))
  result <- if (length(rows)) dplyr::bind_rows(rows) else NULL

  if (!is.null(side_mapping_nonfocus) && nrow(side_mapping_nonfocus) > 0) {
    nonfocus <- normalize_mapping(
      side_mapping_nonfocus, "nonfocus", list(model_variable = NA_character_)
    )
    if (!is.null(nonfocus) && nrow(nonfocus)) {
      result <- if (is.null(result)) nonfocus else dplyr::bind_rows(result, nonfocus)
    }
  }
  if (is.null(result) || !nrow(result)) return(NULL)

  result %>%
    dplyr::arrange(.data$VariableSplit, .data$MainModelVariableName) %>%
    dplyr::distinct(.data$VariableSplit, .data$MainModelVariableName, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(standard_cols))
}
