args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: check_controller_parity.R baseline-ref R/mod_name.R")
}

module_path <- args[[2L]]
baseline <- system2("git", c("show", paste0(args[[1L]], ":", module_path)),
                    stdout = TRUE, stderr = TRUE)
if (!is.null(attr(baseline, "status"))) stop(paste(baseline, collapse = "\n"))

old <- parse(text = baseline)
current <- parse(module_path)
server_expressions <- function(parsed) {
  as.list(parsed[[2L]][[3L]][[3L]][[2L]][[3L]][[3L]])[-1L]
}
render <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n")
if (!identical(render(old[[1L]]), render(current[[1L]])) ||
    !identical(render(old[[2L]][[3L]][[2L]]),
               render(current[[2L]][[3L]][[2L]]))) {
  stop("Module UI or public server arguments changed: ", module_path)
}

expanded <- unlist(lapply(server_expressions(current), function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("controller_context"))) return(list())
  if (is.call(expr) && grepl("^register_", as.character(expr[[1L]]))) {
    name <- sub("^register_", "", as.character(expr[[1L]]))
    controller <- parse(file.path("R/controllers", paste0(name, ".R")))[[1L]]
    block <- controller[[3L]][[3L]][[3L]][[2L]]
    return(as.list(block)[-1L])
  }
  list(expr)
}), recursive = FALSE)

before <- vapply(server_expressions(old), render, character(1))
after <- vapply(expanded, render, character(1))
if (!identical(before, after)) {
  at <- which(before[seq_len(min(length(before), length(after)))] !=
                after[seq_len(min(length(before), length(after)))])
  stop("Server expression mismatch at ", if (length(at)) at[[1L]] else "length",
       " in ", module_path)
}
cat("CONTROLLER_SOURCE_PARITY_OK", module_path, length(before), "expressions\n")
