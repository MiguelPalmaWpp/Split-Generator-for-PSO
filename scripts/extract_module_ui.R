# One-time modularization aid. Move the first mod_*_ui definition into a
# view builder and replace it with a delegating wrapper. This rewrites files;
# it is not part of app startup or routine regression testing.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: extract_module_ui.R R/mod_name.R view_name")

module_path <- args[[1L]]
view_name <- args[[2L]]
target <- file.path("R/views", paste0(view_name, ".R"))
if (file.exists(target)) stop("View already exists: ", target)

lines <- readLines(module_path, warn = FALSE, encoding = "UTF-8")
parsed <- parse(module_path, keep.source = TRUE)
ui <- parsed[[1L]]
ui_name <- as.character(ui[[2L]])
if (!grepl("^mod_.*_ui$", ui_name)) stop("First expression is not module UI.")
ref <- as.integer(attr(parsed, "srcref")[[1L]])
first <- ref[[1L]]
last <- ref[[3L]]
# Keep the original UI body and IDs while changing only its function name.
view_lines <- lines[first:last]
builder <- paste0("build_", view_name)
view_lines[[1L]] <- sub(paste0("^", ui_name), builder, view_lines[[1L]])

dir.create("R/views", recursive = TRUE, showWarnings = FALSE)
writeLines(view_lines, target, useBytes = TRUE)
lines <- c(
  if (first > 1L) lines[seq_len(first - 1L)],
  paste0(ui_name, " <- function(id) ", builder, "(id)"),
  if (last < length(lines)) lines[seq.int(last + 1L, length(lines))]
)
writeLines(lines, module_path, useBytes = TRUE)
cat(module_path, "->", target, "\n")
