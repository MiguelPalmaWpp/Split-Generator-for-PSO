library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/services/export_split_composition.R")

merge_map <- tibble::tibble(
  MergedSplitName = c("Merge_AB", "Merge_AB", "Merge_AB_Before Previous"),
  ComponentSplit = c("Split_A", "Split_B", "Old_Before Previous")
)
canonical <- list(
  component_focus_totals = tibble::tibble(
    VariableSplit = c("Split_A", "Split_B"),
    Component_Activity = c(20, 30),
    Component_Spend = c(2, 3)
  ),
  final_focus_totals = tibble::tibble(
    VariableSplit = "Merge_AB", Activity = 50, Spend = 5
  ),
  merge_map = merge_map
)
export_data <- list(Social = list(
  res = list(modeled_role = "spend"),
  cfg = list(model_variable = "Model_Total"),
  final = tibble::tibble(VariableSplit = "Merge_AB"),
  pre_act = tibble::tibble(VariableSplit = c("Split_A", "Split_B")),
  pre_cost = tibble::tibble(VariableSplit = c("Split_A", "Split_B")),
  merge_resolved = list(map = merge_map),
  canonical_totals = canonical
))

out <- build_split_composition_data(
  export_data,
  channels_list = list(Social = list(
    model_variable = "Model_Total", modeled_role = "spend"
  )),
  channel_labels = list(Social = "Paid Social")
)
stopifnot(
  nrow(out) == 2L,
  all(out$Channel == "Paid Social"),
  identical(out$MainModelVariableName, c("Model_Total", "Model_Total")),
  identical(out$MergedSplitName, c("Merge_AB", "Merge_AB")),
  identical(unname(out$ComponentSplit), c("Split_B", "Split_A")),
  identical(unname(out$Component_Activity), c(30, 20)),
  identical(unname(out$Component_Pct), c(60, 40)),
  identical(unname(out$Component_Spend), c(3, 2)),
  identical(unname(out$`Total Activity`), c(50, 50)),
  identical(unname(out$`Total Spend`), c(5, 5))
)

cat("EXPORT_SPLIT_COMPOSITION_SERVICE_OK\n")
