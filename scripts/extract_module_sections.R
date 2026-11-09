# One-time modularization aid. Given expression ranges from a module AST,
# move them into new register_* controllers and rewrite the module in place.
# Run only with a clean backup/worktree; this script writes source files.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: extract_module_sections.R module name:first-last ...")
}

module_path <- args[[1L]]
module_lines <- readLines(module_path, warn = FALSE, encoding = "UTF-8")
module_ast <- parse(module_path, keep.source = TRUE)
server_body <- module_ast[[2L]][[3L]][[3L]][[2L]][[3L]][[3L]]
refs <- attr(server_body, "srcref")

# Resolve expression ranges through source references, not raw line guesses.
segments <- lapply(args[-1L], function(spec) {
  match <- regexec("^([a-z_]+):([0-9]+)-([0-9]+)$", spec)
  parts <- regmatches(spec, match)[[1L]]
  if (length(parts) != 4L) stop("Invalid section: ", spec)
  first <- as.integer(parts[[3L]])
  last <- as.integer(parts[[4L]])
  if (first < 2L || last >= length(refs) || first > last) {
    stop("Invalid expression range: ", spec)
  }
  list(
    name = parts[[2L]],
    first = as.integer(refs[[first]])[[1L]],
    last = as.integer(refs[[last]])[[3L]]
  )
})

starts <- vapply(segments, `[[`, integer(1), "first")
ends <- vapply(segments, `[[`, integer(1), "last")
if (anyDuplicated(vapply(segments, `[[`, character(1), "name")) ||
    any(starts[order(starts)][-1L] <= ends[order(starts)][-length(ends)])) {
  stop("Controller sections overlap or have duplicate names.")
}

dir.create("R/controllers", recursive = TRUE, showWarnings = FALSE)
for (segment in segments[order(starts, decreasing = TRUE)]) {
  name <- segment$name
  target <- file.path("R/controllers", paste0(name, ".R"))
  if (file.exists(target)) stop("Controller already exists: ", target)
  body <- module_lines[segment$first:segment$last]
  controller <- c(
    paste0("register_", name, " <- function(ctx) {"),
    "  stopifnot(is.list(ctx), is.environment(ctx$scope))",
    "  evalq({",
    body,
    "  }, envir = ctx$scope)",
    "  invisible(NULL)",
    "}"
  )
  writeLines(controller, target, useBytes = TRUE)
  module_lines <- c(
    if (segment$first > 1L) module_lines[seq_len(segment$first - 1L)],
    paste0("    register_", name, "(controller_context)"),
    if (segment$last < length(module_lines))
      module_lines[seq.int(segment$last + 1L, length(module_lines))]
  )
  cat(name, segment$first, segment$last, "\n")
}
writeLines(module_lines, module_path, useBytes = TRUE)
