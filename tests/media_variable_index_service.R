source("global.R")
on.exit(shutdown_pso_async(), add = TRUE)

period <- as.Date(c("2024-01-01", "2024-01-08"))
active_name <- "Social Impressions --Direct"
inactive_name <- "Ghost Impressions --Direct"
active_key <- "Social Impressions_Total_Total_Total_Total"
inactive_key <- "Ghost Impressions_Total_Total_Total_Total"

main_data <- data.frame(
  Geography = rep("US", 4L),
  Product = rep("Total", 4L),
  VariableName = rep(c("Social Impressions", "Ghost Impressions"), each = 2L),
  Period = rep(period, 2L),
  Campaign = rep("Total", 4L),
  Outlet = rep("Total", 4L),
  Creative = rep("Total", 4L),
  VariableValue = c(10, 12, 2, 3)
)
analytical <- data.frame(
  Geography = rep("US", 2L),
  Period = period,
  setNames(list(c(10, 12)), active_key),
  setNames(list(c(2, 3)), inactive_key),
  check.names = FALSE
)
vof <- data.frame(
  MainModelVariableName = c(active_name, inactive_name),
  AnalyticalVariableName = c(active_key, inactive_key),
  MinPeriod = c("2024-01-01", "2024-01-01"),
  MaxPeriod = c("2024-01-08", "2024-01-08"),
  Geography = c("US", "US"),
  MediaChannel = c("Social", "Ghost"),
  Effect = "Direct",
  Metric = "Activity",
  stringsAsFactors = FALSE
)
details <- data.frame(
  VariableName = c(active_name, inactive_name),
  Type = c("IN", "NONE"),
  stringsAsFactors = FALSE
)

index <- build_media_index(
  main_data = main_data,
  analytical = analytical,
  vof_df = vof,
  model_details = details,
  cross_cols = "Geography"
)
stopifnot(
  identical(names(index$channels), active_name),
  index$summary$vof_rows_detected == 2L,
  index$summary$vof_rows_active == 1L,
  index$summary$vof_rows_discarded == 1L,
  identical(index$channels[[active_name]]$source, "vof"),
  identical(index$vof_contract$IsModelled, TRUE)
)

cat("MEDIA_VARIABLE_INDEX_SERVICE_OK\n")
