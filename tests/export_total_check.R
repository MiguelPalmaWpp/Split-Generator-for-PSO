# Export must report the same canonical reconciliation outcome as Process.
# Cover zeros, exact dates/geographies, modeled roles, errors, and cache refresh.
options(pso.async.enabled = FALSE)
source("global.R")

scope <- new.env(parent = .GlobalEnv)
register_export_split_payload(list(scope = scope))
periods <- as.Date(c("2024-01-01", "2024-01-08"))
analytical <- data.frame(Geography = "Phoenix Prescott", Period = periods,
                         Model = c(10, 0))
rae <- data.frame(
  Geography = "Phoenix Prescott", Period = periods,
  VariableName = "Social Impressions", VariableValue = c(10, 7)
)
cfg <- list(
  model_variable = "Model", modeled_role = "activity",
  modeled_varname_include = "Social Impressions",
  varname_include = "Social Impressions", activity_keyword = "Impressions",
  spend_keyword = "Spend", min_period = min(periods), max_period = max(periods)
)
result <- list(
  rag = data.frame(Geography = "Phoenix Prescott", Period = periods,
                   Brand = c(10, 7), Partner = c(999, 999)),
  cross_cols = "Geography",
  split_manifest = data.frame(
    VariableSplit = c("Brand", "Partner"), Role = c("modeled", "for_indices"),
    MetricRole = c("activity", "spend")
  )
)
settings <- list(cross_cols = "Geography")
final <- data.frame(VariableSplit = "Brand")

check_export <- function(an = analytical, raw = rae, res = result, channel = cfg,
                         expected = "mismatch") {
  canonical <- build_canonical_total_check(an, raw, res, channel, "Geography")
  summary <- scope$build_channel_summary_item(
    "Social", res, channel, final, list(analytical = an, all_rags = raw), settings
  )
  stopifnot(identical(canonical$status, expected),
            summary$processed,
            identical(summary$tc_mismatch, !identical(canonical$status, "ok")))
  invisible(summary)
}

check_export()
modeled_only <- result
modeled_only$rag$Partner <- NULL
modeled_only$split_manifest <- modeled_only$split_manifest[1, ]
check_export(res = modeled_only)
matching <- analytical
matching$Model <- c(10, 7)
check_export(an = matching, expected = "ok")
shifted <- result
shifted$rag$Period <- shifted$rag$Period + 1
check_export(an = matching, res = shifted)
different_geo <- result
different_geo$rag$Geography <- "Phoenix"
check_export(an = matching, res = different_geo)
check_export(an = NULL, expected = "error")
check_export(res = within(result, rag <- NULL), expected = "error")
zero_model <- analytical
zero_model$Model <- 0
check_export(an = zero_model, raw = NULL, expected = "error")
spend <- cfg
spend$modeled_role <- "spend"
spend$modeled_varname_include <- spend$varname_include <- "Social Spend"
spend_raw <- rae
spend_raw$VariableName <- "Social Spend"
spend_result <- result
spend_result$split_manifest$MetricRole <- c("spend", "activity")
check_export(an = matching, raw = spend_raw, res = spend_result, channel = spend,
             expected = "ok")

# An unexpected validation error must produce a warning, never a green status.
scope$build_canonical_total_check <- function(...) stop("synthetic failure")
failed <- suppressMessages(scope$build_channel_summary_item(
  "Social", result, cfg, final, list(analytical = analytical, all_rags = rae), settings
))
stopifnot(is.list(failed), failed$tc_mismatch, failed$has_warning)
rm("build_canonical_total_check", envir = scope)

# Replacing Analytical alone must invalidate the memoized Export snapshot.
bundle <- list(analytical = matching, all_rags = rae, data_signature = "stable-rae")
data_state <- shiny::reactiveVal(bundle)
shiny::testServer(
  mod_export_server,
  args = list(
    results = shiny::reactive(list(Social = result)), data = data_state,
    config = shiny::reactive(settings), channels = shiny::reactive(list(Social = cfg)),
    result_versions = shiny::reactive(list(Social = 1L))
  ),
  {
    stopifnot(!channel_summary()[[1]]$tc_mismatch)
    updated <- bundle
    updated$analytical <- analytical
    data_state(updated)
    session$flushReact()
    stopifnot(channel_summary()[[1]]$tc_mismatch)
  }
)

cat("EXPORT_TOTAL_CHECK_OK\n")
