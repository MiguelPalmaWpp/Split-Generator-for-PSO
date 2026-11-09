# Reject missing, duplicate, empty, or unexpected outputs before ZIP creation.
# Optional files may be absent, but any optional file that was written must
# still exist and be nonempty. The caller decides when composition is required.
validate_export_package <- function(written, required_files, directory,
                                    optional_files = character(0)) {
  expected <- unname(as.character(required_files))
  allowed <- c(expected, unname(as.character(optional_files)))
  actual <- basename(written)
  missing <- setdiff(expected, actual)
  unexpected <- setdiff(actual, allowed)
  duplicate <- unique(actual[duplicated(actual)])
  checked <- unique(c(expected, actual))
  unavailable <- checked[!file.exists(file.path(directory, checked))]
  empty <- written[file.exists(written) & file.info(written)$size <= 0]
  list(
    ok = !length(missing) && !length(unexpected) && !length(duplicate) &&
      !length(unavailable) && !length(empty) &&
      length(written) >= length(expected) && length(written) <= length(allowed),
    missing = missing,
    unexpected = unexpected,
    duplicate = duplicate,
    unavailable = unavailable,
    empty = basename(empty)
  )
}
