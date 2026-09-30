library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/services/export_reconciliation.R")
source("R/services/export_file_dimensions.R")

export_module_source <- readLines("R/mod_export.R", warn = FALSE)
stopifnot(
  sum(grepl("build_file_dims <- function", export_module_source, fixed = TRUE)) == 1L,
  sum(grepl("build_export_file_dimensions(", export_module_source, fixed = TRUE)) == 1L,
  !any(grepl("build_file_dims_legacy", export_module_source, fixed = TRUE)),
  any(grepl('"R/services/export_file_dimensions.R"',
            readLines("global.R", warn = FALSE), fixed = TRUE))
)

export_data <- list(ChannelA = list(
  final = tibble::tibble(VariableSplit = c("A", "Merged")),
  pre_act = tibble::tibble(VariableSplit = c("A", "B")),
  cfg = list(saved_merges = list(list(
    active = TRUE, new_name = "Merged", merged = c("A", "B")
  )))
))
details <- tibble::tibble(
  VariableName = c("ModelA", "Ignored"), Type = c("IN", "NONE")
)
analytical <- data.frame(
  Geography = "East", Product = "Widget", Period = as.Date("2024-01-01"),
  BP_Year = 2024, ModelA = 1
)
nonfocus <- data.frame(VariableSplit = "A_Before Previous")
roi_data <- data.frame(
  MainModelVariableName = "ModelA", Channel = "ChannelA", Geography = "East",
  ROI = 1.2
)

dims <- build_export_file_dimensions(
  export_data, details, analytical, nonfocus,
  channels = list(ChannelA = list(model_variable = "ModelA")), roi_data
)
stopifnot(
  identical(dims$analytical, list(rows = 1L, cols = 9L)),
  identical(dims$side_map, list(rows = 3L, cols = 6L)),
  identical(dims$activity, list(rows = 2L, cols = 8L)),
  identical(dims$composition, list(rows = 2L, cols = 10L)),
  identical(dims$config, list(rows = 1L, cols = NULL))
)

empty <- build_export_file_dimensions(
  list(), NULL, NULL, NULL, channels = list(), roi_data = NULL
)
stopifnot(is.null(empty$analytical), is.null(empty$side_map),
          is.null(empty$activity), is.null(empty$composition),
          is.null(empty$config))

cat("EXPORT_FILE_DIMENSIONS_SERVICE_OK\n")
