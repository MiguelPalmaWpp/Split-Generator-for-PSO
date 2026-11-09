# Exercise Setup upload state and Media Index creation through testServer.
# Temporary files keep this test independent of private client datasets.
source("global.R")
on.exit(shutdown_pso_async(), add = TRUE)

run_setup_upload_fixture <- function() {
  root <- tempfile("setup-module-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  period <- as.Date(c("2024-01-01", "2024-01-08"))
  rae <- data.frame(
    Geography = "US", Product = "Total",
    VariableName = "Social Impressions", Period = period,
    Campaign = "Brand", Outlet = "Total", Creative = "Total",
    VariableValue = c(10, 12)
  )
  model_name <- "Social Impressions_Total_Total_Total_Total"
  analytical <- data.frame(
    Geography = "US", Period = period,
    setNames(list(c(10, 12)), model_name), check.names = FALSE
  )
  vof <- data.frame(
    MainModelVariableName = "Social Impressions --Direct",
    AnalyticalVariableName = model_name,
    MinPeriod = "2024-01-01", MaxPeriod = "2024-01-08",
    Geography = "US", MediaChannel = "Social", Effect = "Direct",
    Metric = "Activity"
  )
  details <- data.frame(
    VariableName = "Social Impressions --Direct", Type = "IN",
    FunctionForm = "", Normalization = "", MinMaxAdjustment = ""
  )

  paths <- file.path(root, c("rae.csv", "analytical.RData", "vof.csv", "details.csv"))
  utils::write.csv(rae, paths[[1L]], row.names = FALSE)
  save(analytical, file = paths[[2L]])
  utils::write.csv(vof, paths[[3L]], row.names = FALSE)
  utils::write.csv(details, paths[[4L]], row.names = FALSE)
  upload <- function(path) data.frame(
    name = basename(path), size = file.info(path)$size,
    type = "application/octet-stream", datapath = path
  )

  shiny::testServer(mod_setup_server, args = list(
    performance_cache = new_performance_cache()
  ), {
    session$setInputs(file_main = upload(paths[[1L]]))
    session$setInputs(file_analytical = upload(paths[[2L]]))
    session$setInputs(file_vof = upload(paths[[3L]]))
    session$setInputs(file_details = upload(paths[[4L]]))
    session$flushReact()
    returned <- session$getReturned()
    qa <- returned$qa_status()
    index <- returned$media_index()
    stopifnot(
      qa$files_ready, qa$media_ready,
      identical(names(index$channels), "Social Impressions --Direct"),
      index$summary$vof_rows_active == 1L,
      identical(returned$data()$details_raw$Type, "IN")
    )
  })
}

run_setup_upload_fixture()
cat("SETUP_MODULE_UPLOAD_OK\n")
