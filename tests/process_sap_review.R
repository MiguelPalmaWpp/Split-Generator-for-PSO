# Keep sidebar review status and Total Check synchronized across SAP edits.
# Use real processing and aggregation with two modeled components per period.
options(pso.async.enabled = FALSE)
source("global.R")

periods <- as.Date(c("2024-01-01", "2024-01-08"))
rae <- data.frame(
  Geography = "US", Product = "Total", Campaign = rep(c("A", "B"), each = 2),
  Outlet = "Total", Creative = "Total", VariableName = "Social Impressions",
  Period = rep(periods, 2), VariableValue = c(5, 6, 5, 6)
)
cfg <- list(
  model_variable = "Model", varname_include = "Social Impressions",
  modeled_varname_include = "Social Impressions", modeled_role = "activity",
  model_metric = "activity", activity_keyword = "Impressions",
  spend_keyword = "Spend", split_columns = c("VariableName", "Campaign"),
  min_period = min(periods), max_period = max(periods)
)
bundle <- list(
  all_rags = rae, all_rags_indexed = build_indexed_rae(rae),
  analytical = data.frame(Geography = "US", Period = periods, Model = c(10, 12)),
  dates_df = data.frame(Period = periods), data_signature = data_signature(rae)
)
settings <- list(
  cross_cols = "Geography", start_report_date = min(periods),
  end_report_date = max(periods), update_label = "Q1"
)
data_state <- shiny::reactiveVal(bundle)
channel_state <- shiny::reactiveVal(list(Social = cfg))

shiny::testServer(
  mod_process_server,
  args = list(
    data = data_state, config = shiny::reactiveVal(settings), channels = channel_state,
    update_merges = function(nm, merges) {
      updated <- shiny::isolate(channel_state())
      updated[[nm]]$saved_merges <- merges
      channel_state(updated)
    }
  ),
  {
    session$setInputs(channel_select = "Social", period_filter = "focus", btn_one = 1)
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    base_result <- results_store[["Social"]]
    plan <- data.frame(VariableSplit = current_model_data()$VariableSplit, MergeName = "Brand")
    stopifnot(nrow(plan) == 2L)

    apply_splits_aggregation_plan(plan)
    session$flushReact()
    # The sidebar must be current even before visiting the Total Check tab.
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(length(channel_review_messages("Social")) == 0L)
    stopifnot(identical(current_total_check("Social"), total_check_data()))
    stopifnot(grepl(">Processed<", output$status$html, fixed = TRUE))
    stopifnot("Brand" %in% names(results_store[["Social"]]$rag))

    session$setInputs(btn_undo = 1)
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(length(channel_review_messages("Social")) == 0L)
    stopifnot(identical(results_store[["Social"]]$rag, base_result$rag))

    apply_splits_aggregation_plan(plan)
    session$flushReact()
    session$setInputs(btn_confirm_reset = 1)
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(length(channel_review_messages("Social")) == 0L)
    stopifnot(identical(results_store[["Social"]]$rag, base_result$rag))

    # Genuine differences must still produce Review after an aggregation.
    changed <- bundle
    changed$analytical$Model <- c(20, 24)
    data_state(changed)
    session$flushReact()
    session$setInputs(btn_one = 2)
    apply_splits_aggregation_plan(plan)
    session$flushReact()
    stopifnot(identical(current_total_check("Social")$status, "mismatch"))
    stopifnot(length(channel_review_messages("Social")) > 0L)
    stopifnot(identical(current_total_check("Social"), total_check_data()))
    stopifnot(grepl(">Review<", output$status$html, fixed = TRUE))

    # Instrument only this module session, not the global service or workers.
    calls <- new.env(parent = emptyenv())
    calls$count <- 0L
    calls$fail <- FALSE
    service <- build_canonical_total_check
    assign("build_canonical_total_check", function(...) {
      calls$count <- calls$count + 1L
      if (calls$fail) stop("Synthetic reconciliation failure")
      service(...)
    }, envir = environment(current_total_check))

    set_res("Social", results_store[["Social"]])
    stopifnot(identical(current_total_check("Social")$status, "mismatch"))
    stopifnot(calls$count == 1L)
    stopifnot(identical(current_total_check("Social"), total_check_data()))
    channel_review_messages("Social")
    stopifnot(calls$count == 1L)

    # Replacing Analytical invalidates both views, without needing Process.
    data_state(bundle)
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(calls$count == 2L)
    stopifnot(identical(current_total_check("Social"), total_check_data()))
    stopifnot(calls$count == 2L)

    # Identical config/version in different channels must not share a check.
    identical_cfg <- channel_state()[["Social"]]
    channel_state(list(Social = identical_cfg, Other = identical_cfg))
    other_result <- results_store[["Social"]]
    other_result$rag$Brand <- other_result$rag$Brand * 2
    set_res("Other", other_result)
    result_versions[["Other"]] <- result_versions[["Social"]]
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(identical(current_total_check("Other")$status, "mismatch"))
    stopifnot(calls$count == 3L)

    # Unexpected reconciliation failures remain visible, not green or fatal.
    calls$fail <- TRUE
    set_res("Social", results_store[["Social"]])
    check <- current_total_check("Social")
    stopifnot(identical(check$status, "error"))
    stopifnot(length(channel_review_messages("Social")) > 0L)
    stopifnot(identical(check, total_check_data()))
    stopifnot(!grepl("Synthetic", check$summary$message, fixed = TRUE))
    stopifnot(calls$count == 4L)
    calls$fail <- FALSE
    set_res("Social", results_store[["Social"]])
    stopifnot(identical(current_total_check("Social")$status, "ok"))
    stopifnot(length(channel_review_messages("Social")) == 0L)
    stopifnot(calls$count == 5L)
  }
)
cat("PROCESS_SAP_REVIEW_OK\n")
