library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/utils/processing.R")
source("R/services/sap.R")

cfg <- list(split_columns = c("VariableName", "Campaign", "Outlet", "Creative"))
res <- list(split_manifest = tibble::tibble(
  Role = c("modeled", "modeled", "for_indices"),
  PeriodScope = c("focus", "focus", "focus"),
  VariableSplit = c("Activity_A", "Activity_B", "Spend_A"),
  Campaign = c("Action", "Action", "Action"),
  Outlet = c("Brand", "Brand", "Brand"),
  Creative = c("Performance", "Unknown", "Performance")
))
df <- tibble::tibble(
  VariableSplit = c("Activity_A", "Activity_B"),
  total_activity = c(20, 5),
  pct_total_activity = c(80, 20),
  max_index = c(0.8, 0.2)
)
sap <- build_splits_aggregation_plan(df, res, cfg, "focus", "activity")
stopifnot(
  identical(names(sap), c("VariableSplit", "Campaign", "Outlet", "Creative",
                          "MergeName", "Total Activity", "Pct Total Activity", "Max Index")),
  identical(sap$Creative, c("Performance", "Unknown")),
  identical(sap$MergeName, c(NA_character_, NA_character_))
)

spend <- build_splits_aggregation_plan(
  transform(df, total_spend = c(200, 50), pct_total_spend = c(80, 20)),
  res, cfg, "focus", "spend"
)
stopifnot("Total Spend" %in% names(spend), "Pct Total Spend" %in% names(spend))

csv_plan <- read_sap_plan_content(
  "VariableSplit,Campaign,MergeName\nActivity_A,Action,Branding"
)
stopifnot(identical(csv_plan$VariableSplit, "Activity_A"))
legacy <- read_sap_plan_content("Split,MergeName\nA,Branding")
hydrated <- hydrate_sap_variable_split(
  legacy,
  tibble::tibble(VariableSplit = c("Model_A", "Model_B"))
)
stopifnot(identical(hydrated$VariableSplit, "Model_A"))

cat("SAP_SERVICE_OK\n")
