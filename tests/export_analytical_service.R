# Verify Analytical Extended includes base splits and final SAP aggregates once.
# Intermediate aggregates should not leak into the exported columns.
library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/services/export_analytical.R")

d <- list(
  analytical = tibble::tibble(
    Geography = c("North", "South"),
    Period = as.Date(c("2026-01-05", "2026-01-12")),
    BP_Year = c(2026L, 2026L),
    Model_Total = c(15, 23),
    `Weight Variable MMM` = c(1, 1)
  ),
  details = tibble::tibble(
    Type = c("IN", "NONE"),
    VariableName = c("Model_Total", "Unused_Model")
  )
)
channels <- list(Social = list(model_variable = "Model_Total"))
results <- list(Social = list(rag = tibble::tibble(
  Geography = c("North", "South"),
  Period = as.Date(c("2026-01-05", "2026-01-12")),
  Merge_AB = c(5, 9),
  Split_C = c(10, 14),
  Paired_Merge_AB = c(50, 90),
  Paired_Split_C = c(100, 140),
  Intermediate_AB = c(4, 8)
)))
export_data <- list(Social = list(
  clean = list(rag = tibble::tibble(
    Geography = c("North", "South"),
    Period = as.Date(c("2026-01-05", "2026-01-12")),
    Split_A = c(2, 3),
    Split_B = c(3, 6),
    Split_C = c(10, 14),
    Paired_A = c(20, 30),
    Paired_B = c(30, 60),
    Paired_C = c(100, 140)
  )),
  pre_act = tibble::tibble(VariableSplit = c("Split_A", "Split_B", "Split_C")),
  final = tibble::tibble(VariableSplit = c("Merge_AB", "Split_C")),
  pre_for_indices = tibble::tibble(
    VariableSplit = c("Paired_A", "Paired_B", "Paired_C")
  ),
  final_for_indices = tibble::tibble(
    VariableSplit = c("Paired_Merge_AB", "Paired_Split_C")
  )
))

out <- build_analytical_extended_data(
  d, results, channels, list(cross_cols = "Geography"),
  export_data = export_data
)
stopifnot(
  all(c(
    "Model_Total", "Split_A", "Split_B", "Split_C", "Merge_AB",
    "Paired_A", "Paired_B", "Paired_C", "Paired_Merge_AB", "Paired_Split_C"
  ) %in% names(out)),
  !"Intermediate_AB" %in% names(out),
  identical(out$Merge_AB, c(5, 9)),
  identical(out$Split_A, c(2, 3)),
  identical(out$Paired_A, c(20, 30)),
  identical(out$Paired_B, c(30, 60)),
  identical(out$Paired_Merge_AB, c(50, 90)),
  identical(out$Paired_Split_C, c(100, 140)),
  nrow(out) == 2L
)

cat("EXPORT_ANALYTICAL_SERVICE_OK\n")
