# Ensure operation failures do not expose private technical paths in the UI.
# This uses a fake session so no browser or running Shiny app is required.
source("R/utils/operation_status.R")

run_operation_status_safety_test <- function() {
  old_debug <- getOption("pso.debug.errors")
  on.exit(options(pso.debug.errors = old_debug), add = TRUE)
  options(pso.debug.errors = FALSE)
  messages <- list()
  session <- list(
    sendCustomMessage = function(type, payload) {
      messages[[length(messages) + 1L]] <<- payload
    },
    flushOutput = function() invisible(NULL)
  )
  status <- new_operation_status(session)
  status$start("test", "Test")
  status$fail("Operation failed", "C:/private/file.csv")
  stopifnot(identical(tail(messages, 1L)[[1L]]$technicalDetail, ""))
  stopifnot(identical(
    suppressMessages(pso_public_error(simpleError("C:/private/file.csv"),
                                      "test", "Please retry")),
    "Please retry"
  ))
}

run_operation_status_safety_test()

cat("OPERATION_STATUS_SAFETY_OK\n")
