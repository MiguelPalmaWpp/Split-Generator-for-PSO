# Validate ZIP package rules for required and optional outputs.
# Missing, empty, and duplicate files must prevent a successful package.
source("R/services/export_package.R")

run_export_package_test <- function() {
  directory <- tempfile("export-package-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  required <- c(analytical = "Analytical.csv", mapping = "Mapping.csv")
  files <- file.path(directory, unname(required))
  writeLines("data", files[[1L]])
  writeLines("data", files[[2L]])

  stopifnot(validate_export_package(files, required, directory)$ok)
  stopifnot(validate_export_package(files, as.list(required), directory)$ok)
  optional <- c(composition = "Composition.csv")
  stopifnot(validate_export_package(files, required, directory, optional)$ok)
  stopifnot(!validate_export_package(files, c(required, optional), directory)$ok)
  composition_file <- file.path(directory, optional)
  writeLines("merge lineage", composition_file)
  stopifnot(validate_export_package(c(files, composition_file), required,
                                    directory, optional)$ok)
  stopifnot(validate_export_package(c(files, composition_file),
                                    c(required, optional), directory)$ok)
  unlink(composition_file)
  stopifnot(!validate_export_package(files[[1L]], required, directory)$ok)
  stopifnot(!validate_export_package(c(files, files[[1L]]), required, directory)$ok)
  writeLines(character(0), files[[2L]])
  stopifnot(!validate_export_package(files, required, directory)$ok)
  unlink(files[[2L]])
  stopifnot(!validate_export_package(files, required, directory)$ok)
}

run_export_package_test()

cat("EXPORT_PACKAGE_SERVICE_OK\n")
