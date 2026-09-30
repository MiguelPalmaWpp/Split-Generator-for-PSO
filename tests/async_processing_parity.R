source("global.R")
on.exit(shutdown_pso_async(), add = TRUE)

periods <- as.Date(c("2024-01-01", "2024-01-08"))
rae <- data.frame(
  Geography = rep("US", 4), Product = rep("Total", 4),
  Campaign = rep("Brand", 4), Outlet = rep("Total", 4),
  Creative = rep("Total", 4),
  VariableName = rep(c("Social Impressions", "Social Spend"), each = 2),
  Period = rep(periods, 2), VariableValue = c(100, 120, 10, 12)
)
analytical <- data.frame(
  Geography = "US", Period = periods,
  `Social Impressions_Total_Total_Total_Total` = c(100, 120),
  check.names = FALSE
)
cfg <- list(
  channel_name = "Social",
  model_variable = "Social Impressions_Total_Total_Total_Total",
  varname_include = c("Social Impressions", "Social Spend"),
  modeled_varname_include = "Social Impressions",
  for_indices_varname_include = "Social Spend",
  modeled_role = "activity", model_metric = "activity",
  for_indices_role = "spend", role_pair_status = "Matched",
  activity_keyword = "Impressions", spend_keyword = "Spend",
  split_columns = c("VariableName", "Campaign"),
  dimension_breaks = list(), dimension_aliases = list(),
  segment_overrides = list(), saved_merges = list(),
  min_period = min(periods), max_period = max(periods),
  time_break_label = "", geo_label = ""
)
payload <- build_async_channel_payload(
  "Social", cfg, rae, analytical, data.frame(Period = periods),
  list(
    cross_cols = "Geography", start_report_date = min(periods),
    end_report_date = max(periods), update_label = "Q1"
  ),
  operation_id = "parity", data_signature_value = "data",
  config_signature_value = "config", result_version = 0L
)
stopifnot(is.finite(payload$payload_size_estimate_bytes),
          payload$payload_size_estimate_bytes > 0)

local_result <- process_channel_pipeline(payload)
remote_call <- mirai::mirai(
  process_channel_pipeline(payload), payload = payload,
  .compute = pso_async_compute
)
remote_result <- remote_call[]

stopifnot(local_result$ok, remote_result$ok)
stopifnot(identical(names(local_result$final$rag), names(remote_result$final$rag)))
stopifnot(isTRUE(all.equal(
  local_result$final$rag, remote_result$final$rag,
  check.attributes = FALSE
)))

data_state <- list(
  all_rags = rae,
  all_rags_indexed = build_indexed_rae(rae),
  analytical = analytical,
  dates_df = data.frame(Period = periods),
  schema_metadata = NULL,
  data_signature = data_signature(rae)
)
global_state <- list(
  cross_cols = "Geography", start_report_date = min(periods),
  end_report_date = max(periods), update_label = "Q1"
)

shiny::testServer(
  mod_process_server,
  args = list(
    data = shiny::reactive(data_state),
    config = shiny::reactive(global_state),
    channels = shiny::reactive(list(Social = cfg)),
    performance_cache = new_performance_cache()
  ),
  {
    session$setInputs(channel_select = "Social", btn_one = 1)
    deadline <- Sys.time() + 10
    repeat {
      later::run_now(0.1)
      Sys.sleep(0.05)
      session$flushReact()
      qa <- session$getReturned()$qa_status()
      if (qa$processed == 1L || Sys.time() > deadline) break
    }
    qa <- session$getReturned()$qa_status()
    stopifnot(qa$processed == 1L, qa$failed == 0L, !qa$batch_running)
  }
)

review_cfg <- cfg
review_cfg$saved_merges <- list(list(
  active = TRUE,
  merged = "Missing split",
  new_name = "Needs review",
  view = "focus"
))
batch_channels <- list(
  First = cfg,
  Review = review_cfg,
  Last = cfg
)
shiny::testServer(
  mod_process_server,
  args = list(
    data = shiny::reactive(data_state),
    config = shiny::reactive(global_state),
    channels = shiny::reactive(batch_channels),
    performance_cache = new_performance_cache()
  ),
  {
    session$setInputs(channel_select = "First", btn_all = 1)
    deadline <- Sys.time() + 30
    repeat {
      later::run_now(0.1)
      Sys.sleep(0.05)
      session$flushReact()
      qa <- session$getReturned()$qa_status()
      if (qa$processed == 3L || Sys.time() > deadline) break
    }
    qa <- session$getReturned()$qa_status()
    stopifnot(
      qa$processed == 3L,
      qa$failed == 0L,
      qa$merge_review >= 1L,
      !qa$batch_running
    )
  }
)

options(pso.async.enabled = FALSE)
shiny::testServer(
  mod_process_server,
  args = list(
    data = shiny::reactive(data_state),
    config = shiny::reactive(global_state),
    channels = shiny::reactive(list(Social = cfg)),
    performance_cache = new_performance_cache()
  ),
  {
    session$setInputs(channel_select = "Social", btn_one = 1)
    qa <- session$getReturned()$qa_status()
    sync_results <- session$getReturned()$results()
    sync_clean <- session$getReturned()$clean_results()
    stopifnot(qa$processed == 1L, qa$failed == 0L)
    stopifnot(isTRUE(all.equal(
      sync_results$Social$rag, local_result$final$rag,
      check.attributes = FALSE
    )))
    stopifnot(isTRUE(all.equal(
      sync_clean$Social$rag, local_result$clean$rag,
      check.attributes = FALSE
    )))
  }
)

cat("ASYNC_PROCESSING_PARITY_OK\n")
