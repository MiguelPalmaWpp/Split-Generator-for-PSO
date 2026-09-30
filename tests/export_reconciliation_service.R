library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/services/export_reconciliation.R")

cfg <- list(
  model_metric = "activity",
  varname_include = "Impressions",
  saved_merges = list(
    list(active = TRUE, metric = "activity", new_name = "Merge_AB",
         merged = list("Split_A", "Split_B")),
    list(active = FALSE, metric = "activity", new_name = "Disabled",
         merged = list("Split_A")),
    list(active = TRUE, metric = "spend", new_name = "Spend_Merge",
         merged = list("Split_A", "Split_B"))
  )
)
final <- tibble::tibble(VariableSplit = c("Merge_AB", "Split_C"))
components <- tibble::tibble(VariableSplit = c("Split_A", "Split_B", "Split_C"))
resolved <- resolve_export_merge_map(cfg, final, components)
stopifnot(
  nrow(resolved$map) == 2L,
  identical(unique(resolved$map$MergedSplitName), "Merge_AB"),
  length(resolved$issues) == 0L
)

missing_cfg <- cfg
missing_cfg$saved_merges <- list(list(
  active = TRUE, metric = "activity", new_name = "Missing_Merge",
  merged = list("Split_A", "Not_Available")
))
missing <- resolve_export_merge_map(
  missing_cfg,
  tibble::tibble(VariableSplit = "Missing_Merge"),
  components
)
stopifnot(nrow(missing$map) == 1L, length(missing$issues) == 1L)

seed <- tibble::tibble(
  VariableSplit = c("Split_A", "Split_A", "Split_B", "Split_C",
                    "Split_A_Before FirstTimeBreak"),
  Geography = c("North", "South", "North", "North", "North"),
  total_activity = c(10, 15, 25, 8, 1000),
  total_spend = c(1, 1.5, 2.5, 0.8, 100),
  Campaign = c("A", "A", "B", "C", "A")
)
canonical <- build_canonical_export_totals(
  list(seed = seed), resolved, model_metric = "activity"
)
merged <- canonical$seed_focus_totals[
  canonical$seed_focus_totals$VariableSplit == "Merge_AB", , drop = FALSE
]
stopifnot(
  nrow(canonical$component_focus_totals) == 3L,
  nrow(canonical$seed_focus_totals) == 2L,
  nrow(merged) == 1L,
  merged$total_activity == 50,
  merged$total_spend == 5,
  is.na(merged$Geography),
  is.na(merged$Campaign)
)

cat("EXPORT_RECONCILIATION_SERVICE_OK\n")
