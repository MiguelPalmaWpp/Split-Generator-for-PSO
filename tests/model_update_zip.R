library(dplyr)
library(stringr)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/utils/functions.R")
source("R/utils/processing.R")
source("R/contracts/data_contracts.R")
source("R/services/splits_metadata.R")
source("R/services/model_update_zip.R")

cfg <- list(
  model_variable = "Model____Update14",
  split_columns = c("VariableName", "Campaign"),
  model_metric = "activity",
  saved_merges = list(list(
    active = TRUE, metric = "activity", new_name = "Merged_2026Q2",
    merged = list("Split_A_2026Q2", "Split_B_2026Q2")
  ))
)
metadata <- export_splits_metadata_csv(list(ChannelA = cfg),
                                       list(update_label = "2026Q2"))
side_mapping <- data.frame(
  VariableSplit = c("Split_A_2026Q2", "Split_B_2026Q2", "Merged_2026Q2"),
  MainModelVariableName = rep("Model____Update14", 3),
  Weight = 1, MinWeight = 0.5, MaxWeight = 2
)
analytical <- data.frame(
  Geography = c("Phoenix", "Phoenix"),
  Period = c("2026-01-05", "2026-01-12"),
  Split_A_2026Q2 = c(2, 4),
  Split_B_2026Q2 = c(3, 5),
  Merged_2026Q2 = c(5, 9),
  check.names = FALSE
)

root <- tempfile("model_update_zip_")
dir.create(root)
write.csv(analytical, file.path(root, "Analytical Splits Extended.csv"),
          row.names = FALSE)
write.csv(side_mapping, file.path(root, "Side Model Mapping.csv"), row.names = FALSE)
write.csv(metadata, file.path(root, "Splits Metadata.csv"), row.names = FALSE)
zip_path <- file.path(root, "deep_dives_splits_test.zip")
zip::zipr(zip_path, c("Analytical Splits Extended.csv", "Side Model Mapping.csv",
                      "Splits Metadata.csv"), root = root)

pkg <- read_model_update_zip(zip_path)
sap <- model_update_sap_by_channel(pkg$metadata)
remapped_sap <- remap_model_update_sap(
  sap, pkg$metadata,
  data.frame(Update14 = "Model____Update14", Update15 = "Model____Update15"),
  "Update14", "Update15",
  list(ChannelB = list(model_variable = "Model____Update15"))
)
stopifnot(
  nrow(pkg$analytical) == 2L,
  inherits(pkg$analytical$Period, "Date"),
  identical(pkg$previous_label, "2026Q2"),
  identical(infer_previous_update_id(pkg$side_mapping,
    data.frame(Update14 = "Model", Update15 = "Other")), "Update14"),
  identical(model_update_history_name(
    c("Split_A_2026Q2", "Merged_Before 2026Q2", "Split_C_2026Q2|FirstTimeBreak", "Plain"),
    "2026Q2", "2026Q3"),
    c("Split_A_Before 2026Q3", "Merged_Before 2026Q3",
      "Split_C_Before 2026Q3|FirstTimeBreak", "Plain_Before 2026Q3")),
  identical(model_update_history_name("Split_A_Before 2026Q2|GeoLabel1", "", "2026Q3"),
            "Split_A_Before 2026Q3|GeoLabel1"),
  identical(model_update_focus_name(
    c("Split_A_2026Q2", "Merged_Before 2026Q2", "Plain"), "2026Q2", "2026Q3"),
    c("Split_A_2026Q3", "Merged_2026Q3", "Plain")),
  identical(names(model_update_sap_by_channel(pkg$metadata)), "ChannelA"),
  length(model_update_sap_by_channel(pkg$metadata)$ChannelA) == 1L,
  identical(relabel_model_update_sap(
    model_update_sap_by_channel(pkg$metadata), "2026Q2", "2026Q3"
  )$ChannelA[[1]]$merged, list("Split_A_2026Q3", "Split_B_2026Q3")),
  identical(relabel_model_update_sap(
    model_update_sap_by_channel(pkg$metadata), "2026Q2", "2026Q3"
  )$ChannelA[[1]]$new_name, "Merged_2026Q3"),
  identical(names(remapped_sap), "ChannelB")
)

bad_path <- file.path(root, "incomplete.zip")
zip::zipr(bad_path, "Side Model Mapping.csv", root = root)
stopifnot(inherits(try(read_model_update_zip(bad_path), silent = TRUE), "try-error"))
stopifnot(inherits(try(read_model_update_zip("missing.zip"), silent = TRUE), "try-error"))
cat("MODEL_UPDATE_ZIP_OK\n")
