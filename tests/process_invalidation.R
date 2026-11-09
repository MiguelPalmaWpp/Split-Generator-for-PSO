# Verify that completed results become stale when any processing input changes.
# Exercise actual Process All/Changed execution and late-worker rejection.
options(pso.async.enabled = FALSE)
source("global.R")

periods <- as.Date(c("2024-01-01", "2024-01-08"))
rae <- data.frame(
  Geography = "US", Product = "Total", Campaign = "Brand",
  Outlet = "Total", Creative = "Total", VariableName = "Social Impressions",
  Period = periods, VariableValue = c(10, 12)
)
analytical <- data.frame(Geography = "US", Period = periods, Model = c(10, 12))
cfg <- list(
  model_variable = "Model", varname_include = "Social Impressions",
  modeled_varname_include = "Social Impressions", modeled_role = "activity",
  model_metric = "activity", activity_keyword = "Impressions",
  spend_keyword = "Spend", split_columns = c("VariableName", "Campaign"),
  min_period = min(periods), max_period = max(periods)
)
bundle <- list(
  all_rags = rae, all_rags_indexed = build_indexed_rae(rae),
  analytical = analytical, dates_df = data.frame(Period = periods),
  data_signature = data_signature(rae)
)
settings <- list(
  cross_cols = "Geography", start_report_date = min(periods),
  end_report_date = max(periods), update_label = "Q1"
)
data_state <- shiny::reactiveVal(bundle)
config_state <- shiny::reactiveVal(settings)
channel_state <- shiny::reactiveVal(list(Social = cfg))

shiny::testServer(
  mod_process_server,
  args = list(
    data = data_state, config = config_state, channels = channel_state,
    update_merges = function(nm, merges) {
      updated <- shiny::isolate(channel_state())
      updated[[nm]]$saved_merges <- merges
      channel_state(updated)
    }
  ),
  {
    session$setInputs(channel_select = "Social", btn_all = 1)
    qa_status <- session$getReturned()$qa_status()
    stopifnot(qa_status$processed == 1L, qa_status$stale == 0L)
    version <- result_versions[["Social"]]

    # Navigation and export-only settings must not invalidate processing.
    session$setInputs(period_filter = "nonfocus", metric_tabs = "total_check")
    unchanged <- settings
    unchanged$weight_variable_name <- "Model"
    config_state(unchanged)
    session$flushReact()
    stopifnot(length(stale_names()) == 0L)
    session$setInputs(btn_all = 2)
    stopifnot(identical(result_versions[["Social"]], version))

    changed <- settings
    changed$start_report_date <- periods[[2]]
    changed$update_label <- "Q2"
    config_state(changed)
    session$flushReact()
    stopifnot(identical(stale_names(), "Social"))
    session$setInputs(btn_all = 3)
    stopifnot(result_versions[["Social"]] > version, length(stale_names()) == 0L)
    stopifnot(any(grepl("Q2", names(results_store[["Social"]]$rag), fixed = TRUE)))
    version <- result_versions[["Social"]]

    changed_data <- bundle
    changed_data$all_rags$VariableValue <- c(100, 120)
    changed_data$all_rags_indexed <- build_indexed_rae(changed_data$all_rags)
    changed_data$data_signature <- data_signature(changed_data$all_rags)
    data_state(changed_data)
    session$flushReact()
    stopifnot(identical(stale_names(), "Social"))
    session$setInputs(btn_all = 4)
    stopifnot(result_versions[["Social"]] > version, length(stale_names()) == 0L)
    modeled <- unique(results_store[["Social"]]$split_manifest$VariableSplit[
      results_store[["Social"]]$split_manifest$Role == "modeled"
    ])
    stopifnot(sum(as.matrix(results_store[["Social"]]$rag[, modeled, drop = FALSE])) == 220)
    version <- result_versions[["Social"]]

    # Analytical replacement must invalidate even if the RAE signature is stable.
    changed_data$analytical$Model <- c(100, 120)
    data_state(changed_data)
    session$flushReact()
    stopifnot(identical(stale_names(), "Social"))
    session$setInputs(btn_changed = 1)
    stopifnot(result_versions[["Social"]] > version, length(stale_names()) == 0L)

    edits <- list(
      geography_exclude = "Elsewhere",
      varname_exclude = "Other variable",
      campaign_exclude = "Other campaign",
      dimension_aliases = list(list(source = "Outlet", alias = "Partner")),
      segment_overrides = list(list(geography_exclude = "Elsewhere")),
      useful_longitudinal_filters = list(Product = "Jewelry"),
      geo_label = "GeoLabel1",
      varname_match_mode = "exact",
      saved_merges = list(list(active = TRUE, merged = "A", new_name = "B", view = "focus"))
    )
    for (field in names(edits)) {
      edited <- cfg
      edited[[field]] <- edits[[field]]
      channel_state(list(Social = edited))
      session$flushReact()
      stopifnot(identical(stale_names(), "Social"))
    }
    channel_state(list(Social = cfg))
    session$flushReact()
    stopifnot(length(stale_names()) == 0L)
    focus_merge <- edits$saved_merges
    nonfocus_merge <- focus_merge
    nonfocus_merge[[1]]$view <- "nonfocus"
    stopifnot(!identical(
      channel_signature(c(cfg, list(saved_merges = focus_merge))),
      channel_signature(c(cfg, list(saved_merges = nonfocus_merge)))
    ))

    # SAP bookkeeping cannot make results current after an unrelated input edit.
    config_state(settings)
    session$flushReact()
    mark_result_current("Social", cfg, list())
    stopifnot(identical(stale_names(), "Social"))
    session$setInputs(btn_changed = 2)
    stopifnot(length(stale_names()) == 0L)

    # A real pipeline result captured before a settings change must be discarded.
    job <- run_sync_channel_pipeline("Social", data_state(), cfg, config_state(),
                                     data_state()$all_rags)
    stopifnot(job$ok)
    async_state$active <- TRUE
    async_state$id <- job$operation_id
    async_state$items <- list(Social = operation_item("Social", "Processing"))
    version <- result_versions[["Social"]]
    config_state(changed)
    session$flushReact()
    stopifnot(identical(integrate_async_result(job), "discarded"),
              identical(result_versions[["Social"]], version))
    config_state(settings)
    changed_rules <- cfg
    changed_rules$geography_exclude <- "Elsewhere"
    channel_state(list(Social = changed_rules))
    session$flushReact()
    stopifnot(identical(integrate_async_result(job), "discarded"),
              identical(result_versions[["Social"]], version))
    channel_state(list(Social = cfg))
    changed_data$analytical$Model <- c(200, 240)
    data_state(changed_data)
    session$flushReact()
    stopifnot(identical(integrate_async_result(job), "discarded"),
              identical(result_versions[["Social"]], version))
    async_state$active <- FALSE

    # An actual SAP applied to a current result must not request a full rerun.
    session$setInputs(btn_changed = 3, period_filter = "focus")
    stopifnot(length(stale_names()) == 0L)
    plan <- data.frame(
      VariableSplit = current_model_data()$VariableSplit, MergeName = "Brand"
    )
    apply_splits_aggregation_plan(plan)
    session$flushReact()
    stopifnot(length(stale_names()) == 0L, "Brand" %in% names(results_store[["Social"]]$rag))
  }
)

cat("PROCESS_INVALIDATION_OK\n")
