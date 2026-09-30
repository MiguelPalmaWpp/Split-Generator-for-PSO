library(dplyr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/services/export_side_mapping.R")

res <- list(Social = list(
  side_mapping = tibble::tibble(
    VariableSplit = c("Merge_AB", "Not_Final"),
    MainModelVariableName = c("Model_Total", "Model_Total"),
    Weight = c(1.2, 1.5),
    MinWeight = c(0.8, 0.5),
    MaxWeight = c(1.8, 2.0)
  ),
  split_manifest = tibble::tibble(
    Role = "modeled", VariableSplit = "Manifest_Fallback"
  )
))
channels <- list(Social = list(model_variable = "Model_Total"))
export_data <- list(Social = list(final = tibble::tibble(
  VariableSplit = "Merge_AB",
  MainModelVariableName = "Model_Total",
  total_activity = 25
)))
nonfocus <- tibble::tibble(
  VariableSplit = "Past_Split",
  MainModelVariableName = "Past_Model"
)

out <- build_side_mapping_export_data(
  res, channels, nonfocus, export_data
)
stopifnot(
  identical(names(out), c("VariableSplit", "MainModelVariableName", "Weight",
                          "MinWeight", "MaxWeight")),
  identical(out$VariableSplit, c("Merge_AB", "Past_Split")),
  identical(out$Weight[[1]], 1.2),
  identical(out$MinWeight[[1]], 0.8),
  identical(out$MaxWeight[[1]], 1.8),
  identical(out$Weight[[2]], 1),
  identical(out$MinWeight[[2]], 0.5),
  identical(out$MaxWeight[[2]], 2)
)

fallback <- build_side_mapping_export_data(
  list(Social = list(split_manifest = res$Social$split_manifest)),
  channels
)
stopifnot(
  identical(fallback$VariableSplit, "Manifest_Fallback"),
  identical(fallback$MainModelVariableName, "Model_Total")
)

cat("EXPORT_SIDE_MAPPING_SERVICE_OK\n")
