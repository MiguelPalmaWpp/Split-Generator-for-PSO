library(shiny)
library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/utils/processing.R")
source("R/contracts/data_contracts.R")
source("R/services/splits_metadata.R")

cfg <- list(
  model_variable = "Model_Total",
  split_columns = c("VariableName", "Action", "Partner"),
  activity_keyword = "Impressions", spend_keyword = "Spend",
  model_metric = "activity", varname_include = "Impressions",
  min_period = as.Date("2023-01-01"), max_period = as.Date("2023-12-31"),
  dimension_breaks = list(list(
    column = "Campaign", separator = " - ", n_parts = 3L,
    names = c("Action", "Brand", "Performance")
  )),
  dimension_aliases = list(list(source = "Outlet", alias = "Partner")),
  saved_merges = list(list(
    active = TRUE, metric = "activity", new_name = "Merged_A",
    merged = list("Split_A", "Split_B")
  ))
)
metadata <- export_splits_metadata_csv(list(Social = cfg), list(update_label = "Update"))
stopifnot(
  identical(metadata$RecordType, c("Channel", "Break", "Rename", "SAP", "SAP")),
  identical(metadata$BreakSeparator[[2]], " - "),
  identical(metadata$VariableSplit[4:5], c("Split_A", "Split_B")),
  !any(c("Type", "Name", "Splits") %in% names(metadata))
)

file <- tempfile(fileext = ".csv")
write.csv(metadata, file, row.names = FALSE, na = "")
imported <- normalize_splits_metadata_rows(read.csv(
  file, check.names = FALSE, na.strings = "NA", stringsAsFactors = FALSE
))
imported_from_file <- read_channel_config_file(file)
stopifnot(
  nrow(imported) == 4L,
  identical(imported_from_file$Channel, imported$Channel),
  identical(imported_from_file$RecordType, imported$RecordType),
  identical(imported_from_file$Type, imported$Type),
  identical(imported_from_file$Name, imported$Name),
  identical(imported_from_file$Splits, imported$Splits),
  identical(imported$Type, c("Config", "Break", "Rename", "Merge")),
  identical(imported$Splits[[2]], " - |3"),
  identical(imported$Splits[[4]], "Split_A ||| Split_B")
)
tab_file <- tempfile(fileext = ".tsv")
write.table(metadata, tab_file, sep = "\t", row.names = FALSE, quote = TRUE, na = "")
imported_from_tab <- read_channel_config_file(tab_file)
stopifnot(
  identical(imported_from_tab$Channel, imported$Channel),
  identical(imported_from_tab$RecordType, imported$RecordType),
  identical(imported_from_tab$Type, imported$Type),
  identical(imported_from_tab$Name, imported$Name),
  identical(imported_from_tab$Splits, imported$Splits)
)
legacy <- data.frame(Channel = "Social", Type = "Config",
                     Name = "", Splits = "", SplitOrder = "VariableName")
stopifnot(identical(normalize_splits_metadata_rows(legacy), legacy))

cat("METADATA_ROUNDTRIP_OK\n")
