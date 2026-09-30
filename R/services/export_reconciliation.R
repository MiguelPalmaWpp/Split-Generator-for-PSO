# Resolve saved SAP references and derive canonical export totals.
export_split_text <- function(x) trimws(as.character(x))

export_split_key_variants <- function(x) {
  x <- export_split_text(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  without_time <- stringr::str_remove(
    x, "(_Before .*|_Before_.*|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$"
  )
  squish <- function(value) tolower(stringr::str_squish(as.character(value)))
  unique(c(x, without_time, squish(x), squish(without_time)))
}

export_split_signature <- function(x, cfg = list()) {
  x <- export_split_text(x)
  base <- stringr::str_remove(
    x, "(_Before .*|_Before_.*|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$"
  )
  suffix <- tolower(trimws(stringr::str_remove(as.character(x), stringr::fixed(base))))
  vars <- unique(export_split_text(cfg$varname_include %||% character(0)))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  vars <- vars[order(nchar(vars), decreasing = TRUE)]
  matched_var <- ""
  remainder <- base
  for (variable in vars) {
    if (identical(tolower(base), tolower(variable))) {
      matched_var <- variable
      remainder <- ""
      break
    }
    prefix <- paste0(variable, "_")
    if (startsWith(tolower(base), tolower(prefix))) {
      matched_var <- variable
      remainder <- substring(base, nchar(prefix) + 1L)
      break
    }
  }
  if (!nzchar(matched_var)) {
    pieces <- strsplit(base, "_", fixed = TRUE)[[1]]
    matched_var <- pieces[1] %||% ""
    remainder <- if (length(pieces) > 1) paste(pieces[-1], collapse = "_") else ""
  }
  parts <- export_split_text(strsplit(remainder, "_", fixed = TRUE)[[1]])
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(tolower(export_split_text(matched_var)), suffix,
        paste(sort(tolower(parts)), collapse = "|"), sep = "||")
}

extract_export_merges <- function(cfg) {
  first_non_empty <- function(x) {
    x <- export_split_text(unlist(x, use.names = FALSE))
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) x[[1]] else NA_character_
  }
  merges <- cfg$saved_merges %||% list()
  if (is.data.frame(merges)) merges <- split(merges, seq_len(nrow(merges)))
  rows <- lapply(merges, function(merge) {
    if (is.null(merge) || !is.list(merge)) return(NULL)
    active <- isTRUE(merge$active) || isTRUE(merge$enabled) || isTRUE(merge$checked)
    if (!active) return(NULL)
    merge_metric <- normalize_model_metric(merge$metric %||% cfg$model_metric %||% "activity")
    channel_metric <- normalize_model_metric(cfg$modeled_role %||% cfg$model_metric %||% "activity")
    if (!is.null(merge$metric) && !identical(merge_metric, channel_metric)) return(NULL)
    merged_name <- first_non_empty(c(
      merge$new_name, merge$name, merge$merged_split, merge$MergeName, merge$merge_name
    ))
    if (is.na(merged_name) || !nzchar(merged_name)) return(NULL)
    components <- merge$merged %||% merge$merged_splits %||% merge$components %||%
      merge$component_splits %||% merge$merged_items
    components <- export_split_text(unlist(components, use.names = FALSE))
    components <- unique(components[!is.na(components) & nzchar(components)])
    if (!length(components)) return(NULL)
    list(MergedSplitName = merged_name, Components = components)
  })
  Filter(Negate(is.null), rows)
}

resolve_export_split_name <- function(name, candidates, cfg = list()) {
  name <- export_split_text(name)
  candidates <- export_split_text(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (!length(candidates) || is.na(name) || !nzchar(name)) return(NA_character_)
  exact <- candidates[candidates == name]
  if (length(exact)) return(exact[[1]])
  for (key in export_split_key_variants(name)) {
    hits <- candidates[vapply(candidates, function(candidate) {
      key %in% export_split_key_variants(candidate)
    }, logical(1))]
    hits <- hits[!is.na(hits) & nzchar(hits)]
    if (length(hits) == 1L) return(hits[[1]])
  }
  signature <- export_split_signature(name, cfg)
  signatures <- vapply(candidates, export_split_signature, character(1), cfg = cfg)
  hits <- candidates[signatures == signature]
  hits <- hits[!is.na(hits) & nzchar(hits)]
  if (length(hits) == 1L) hits[[1]] else NA_character_
}

resolve_export_merge_map <- function(cfg, final_splits, component_splits) {
  merges <- extract_export_merges(cfg)
  final_names <- final_splits$VariableSplit %||% character(0)
  component_names <- component_splits$VariableSplit %||% character(0)
  issues <- character(0)
  empty_map <- tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
  rows <- lapply(merges, function(merge) {
    resolved_merge <- resolve_export_split_name(merge$MergedSplitName, final_names, cfg)
    if (is.na(resolved_merge) || !nzchar(resolved_merge)) {
      issues <<- c(issues, paste0("Missing merged split: ", merge$MergedSplitName))
      return(NULL)
    }
    components <- vapply(
      merge$Components, resolve_export_split_name, character(1),
      candidates = component_names, cfg = cfg
    )
    missing <- merge$Components[is.na(components) | !nzchar(components)]
    if (length(missing)) {
      issues <<- c(issues, paste0(
        resolved_merge, " missing component(s): ",
        paste(utils::head(missing, 3), collapse = " | "),
        if (length(missing) > 3) paste0(" +", length(missing) - 3, " more") else ""
      ))
    }
    components <- unique(components[!is.na(components) & nzchar(components)])
    if (!length(components)) return(NULL)
    tibble::tibble(MergedSplitName = resolved_merge, ComponentSplit = components)
  })
  valid_rows <- Filter(Negate(is.null), rows)
  list(
    map = if (length(valid_rows)) dplyr::bind_rows(valid_rows) else empty_map,
    issues = unique(issues)
  )
}

empty_export_metric_totals <- function() {
  list(
    activity = tibble::tibble(VariableSplit = character(), total_activity = numeric()),
    spend = tibble::tibble(VariableSplit = character(), total_spend = numeric()),
    seed = tibble::tibble(
      VariableSplit = character(), Geography = character(),
      total_activity = numeric(), total_spend = numeric()
    )
  )
}

build_canonical_export_totals <- function(rae_totals, merge_resolved,
                                          model_metric = "activity") {
  empty_component <- tibble::tibble(
    VariableSplit = character(), Component_Activity = numeric(), Component_Spend = numeric()
  )
  empty_final <- tibble::tibble(
    VariableSplit = character(), Activity = numeric(), Spend = numeric()
  )
  empty_seed <- tibble::tibble(
    VariableSplit = character(), Geography = character(),
    total_activity = numeric(), total_spend = numeric()
  )
  empty <- list(
    component_focus_totals = empty_component,
    final_focus_totals = empty_final,
    seed_focus_totals = empty_seed,
    merge_map = tibble::tibble(MergedSplitName = character(), ComponentSplit = character())
  )

  component_seed <- rae_totals$seed %||% NULL
  if (is.null(component_seed) || !nrow(component_seed) ||
      !"VariableSplit" %in% names(component_seed)) return(empty)
  component_seed <- as.data.frame(component_seed)
  if (!"Geography" %in% names(component_seed)) component_seed$Geography <- NA_character_
  if (!"total_activity" %in% names(component_seed)) component_seed$total_activity <- 0
  if (!"total_spend" %in% names(component_seed)) component_seed$total_spend <- 0
  component_seed$VariableSplit <- export_split_text(component_seed$VariableSplit)
  component_seed$Geography <- as.character(component_seed$Geography)
  component_seed$total_activity <- suppressWarnings(as.numeric(component_seed$total_activity))
  component_seed$total_spend <- suppressWarnings(as.numeric(component_seed$total_spend))
  component_seed$total_activity[is.na(component_seed$total_activity)] <- 0
  component_seed$total_spend[is.na(component_seed$total_spend)] <- 0
  seed_meta_cols <- setdiff(
    names(component_seed), c("VariableSplit", "Geography", "total_activity", "total_spend")
  )
  stable_seed_meta <- function(x) {
    values <- unique(trimws(as.character(x)))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 1L) values[[1]] else NA_character_
  }
  component_seed <- component_seed[
    !is.na(component_seed$VariableSplit) & nzchar(component_seed$VariableSplit) &
      !grepl("_Before(\\s+|_)", component_seed$VariableSplit, ignore.case = TRUE),
    , drop = FALSE
  ]
  if (!nrow(component_seed)) return(empty)

  component_focus <- component_seed %>%
    dplyr::group_by(.data$VariableSplit) %>%
    dplyr::summarise(
      Component_Activity = sum(.data$total_activity, na.rm = TRUE),
      Component_Spend = sum(.data$total_spend, na.rm = TRUE),
      dplyr::across(dplyr::all_of(seed_meta_cols), stable_seed_meta),
      .groups = "drop"
    )
  merge_map <- merge_resolved$map %||% empty$merge_map
  if (!is.null(merge_map) && nrow(merge_map)) {
    merge_map <- as.data.frame(merge_map)
    merge_map$MergedSplitName <- export_split_text(merge_map$MergedSplitName)
    merge_map$ComponentSplit <- export_split_text(merge_map$ComponentSplit)
    merge_map <- merge_map[
      !is.na(merge_map$MergedSplitName) & nzchar(merge_map$MergedSplitName) &
        !grepl("_Before(\\s+|_)", merge_map$MergedSplitName, ignore.case = TRUE) &
        merge_map$ComponentSplit %in% component_seed$VariableSplit,
      c("MergedSplitName", "ComponentSplit"), drop = FALSE
    ]
    merge_map <- dplyr::distinct(
      tibble::as_tibble(merge_map), .data$ComponentSplit, .keep_all = TRUE
    )
  } else {
    merge_map <- empty$merge_map
  }

  final_seed <- component_seed %>%
    dplyr::left_join(merge_map, by = c("VariableSplit" = "ComponentSplit")) %>%
    dplyr::mutate(VariableSplit = dplyr::coalesce(.data$MergedSplitName, .data$VariableSplit)) %>%
    dplyr::select(-dplyr::any_of("MergedSplitName")) %>%
    dplyr::group_by(.data$VariableSplit) %>%
    dplyr::summarise(
      Geography = stable_seed_meta(.data$Geography),
      total_activity = sum(.data$total_activity, na.rm = TRUE),
      total_spend = sum(.data$total_spend, na.rm = TRUE),
      dplyr::across(dplyr::all_of(seed_meta_cols), stable_seed_meta),
      .groups = "drop"
    ) %>%
    dplyr::filter(if (identical(normalize_model_metric(model_metric), "spend"))
      .data$total_spend > 0 else .data$total_activity > 0)
  final_focus <- final_seed %>%
    dplyr::group_by(.data$VariableSplit) %>%
    dplyr::summarise(
      Activity = sum(.data$total_activity, na.rm = TRUE),
      Spend = sum(.data$total_spend, na.rm = TRUE),
      .groups = "drop"
    )
  list(
    component_focus_totals = component_focus,
    final_focus_totals = final_focus,
    seed_focus_totals = final_seed,
    merge_map = merge_map
  )
}
