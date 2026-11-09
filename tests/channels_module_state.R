# Exercise Channels module state with a small VOF, ModelDetails, and RAE fixture.
# Covers channel availability and the effects of add, import, and remove actions.
source("global.R")
on.exit(shutdown_pso_async(), add = TRUE)

period <- as.Date(c("2024-01-01", "2024-01-08"))
channel_name <- "Social Impressions --Direct"
model_name <- "Social Impressions_Total_Total_Total_Total"
rae <- data.frame(
  Geography = "US", Product = "Total", VariableName = "Social Impressions",
  Period = period, Campaign = "Brand", Outlet = "Total", Creative = "Total",
  VariableValue = c(10, 12)
)
analytical <- data.frame(
  Geography = "US", Period = period,
  setNames(list(c(10, 12)), model_name), check.names = FALSE
)
vof <- data.frame(
  MainModelVariableName = channel_name, AnalyticalVariableName = model_name,
  MinPeriod = "2024-01-01", MaxPeriod = "2024-01-08", Geography = "US",
  MediaChannel = "Social", Effect = "Direct", Metric = "Activity"
)
details <- data.frame(VariableName = channel_name, Type = "IN")
index <- build_media_index(rae, analytical, vof, details, cross_cols = "Geography")
data_bundle <- list(
  all_rags = rae, all_rags_indexed = build_indexed_rae(rae),
  analytical = analytical, vof_data = vof, details_raw = details,
  dates_df = data.frame(Period = period), app_mode = "build"
)

shiny::testServer(mod_channels_server, args = list(
  data = shiny::reactive(data_bundle), media_index = shiny::reactive(index),
  config = shiny::reactive(list(
    start_report_date = min(period), end_report_date = max(period),
    update_label = "Q1"
  )), performance_cache = new_performance_cache()
), {
  session$flushReact()
  returned <- session$getReturned()
  stopifnot(length(returned$channels()) == 0L)

  session$setInputs(ch_mgr_add = channel_name)
  session$flushReact()
  channels <- returned$channels()
  stopifnot(identical(names(channels), channel_name))

  metadata <- export_splits_metadata_csv(channels, list(update_label = "Q1"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(metadata, path, row.names = FALSE, na = "")
  parsed <- returned$read_splits_metadata_file(path)
  preview <- returned$preview_splits_metadata_import(parsed)
  stopifnot(preview$total == 1L, preview$updated == 1L)
  returned$apply_splits_metadata_import(parsed)
  session$flushReact()
  stopifnot(identical(names(returned$channels()), channel_name))

  session$setInputs(ch_mgr_remove = channel_name)
  session$flushReact()
  stopifnot(length(returned$channels()) == 0L)
})

cat("CHANNELS_MODULE_STATE_OK\n")
