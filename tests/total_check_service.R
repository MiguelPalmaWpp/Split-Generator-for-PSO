source("global.R")
on.exit(shutdown_pso_async(), add = TRUE)

period <- as.Date(c("2024-01-01", "2024-01-08"))
analytical <- data.frame(
  Geography = c("Phoenix", "Phoenix Prescott", "Phoenix", "Phoenix Prescott"),
  Period = rep(period, each = 2L),
  ModelTotal = c(10, 20, 0, 30),
  check.names = FALSE
)
rae <- data.frame(
  Geography = c("Phoenix", "Phoenix Prescott", "Phoenix", "Phoenix Prescott", "Phoenix"),
  VariableName = rep("Social Impressions", 5L),
  Period = c(rep(period, each = 2L), as.Date("2024-01-15")),
  VariableValue = c(10, 20, 0, 30, 999),
  check.names = FALSE
)
cfg <- list(
  model_variable = "ModelTotal",
  modeled_analytical_variables = "ModelTotal",
  modeled_role = "activity",
  model_metric = "activity",
  modeled_varname_include = "Social Impressions",
  varname_include = "Social Impressions",
  varname_match_mode = "exact",
  activity_keyword = "Impressions",
  spend_keyword = "Spend",
  min_period = min(period),
  max_period = max(period),
  dimension_breaks = list(),
  segment_overrides = list()
)
result <- list(
  rag = data.frame(
    Geography = analytical$Geography,
    Period = analytical$Period,
    ModeledSplit = analytical$ModelTotal,
    EfficiencySplit = 500,
    check.names = FALSE
  ),
  split_manifest = data.frame(
    Role = c("modeled", "efficiency"),
    VariableSplit = c("ModeledSplit", "EfficiencySplit")
  ),
  cross_cols = "Geography"
)

check <- build_canonical_total_check(
  analytical, rae, result, cfg, "Geography", tolerance = 0.01
)
stopifnot(
  identical(check$status, "ok"),
  nrow(check$detail) == 4L,
  all(c("Phoenix", "Phoenix Prescott") %in% check$detail$Geography),
  check$summary$keys == 4L,
  all(check$detail$ModelTotal == check$detail$SplitTotal)
)

# Identical Analytical values must not collapse different cross-sections.
replicated_analytical <- analytical
replicated_analytical$ModelTotal <- c(10, 10, 0, 0)
replicated_result <- result
replicated_result$rag$ModeledSplit <- replicated_analytical$ModelTotal
replicated_check <- build_canonical_total_check(
  replicated_analytical, rae, replicated_result, cfg, "Geography", tolerance = 0.01
)
stopifnot(
  nrow(replicated_check$detail) == 4L,
  length(unique(replicated_check$detail$Geography)) == 2L,
  setequal(replicated_check$detail$Geography, c("Phoenix", "Phoenix Prescott")),
  identical(replicated_check$diagnostics$replicated_cross_cols, character(0))
)

bad_result <- result
bad_result$rag$ModeledSplit[[2L]] <- 21
bad_check <- build_canonical_total_check(
  analytical, rae, bad_result, cfg, "Geography", tolerance = 0.01
)
stopifnot(
  identical(bad_check$status, "mismatch"),
  any(bad_check$detail$Status == "Processing mismatch")
)

cat("TOTAL_CHECK_SERVICE_OK\n")
