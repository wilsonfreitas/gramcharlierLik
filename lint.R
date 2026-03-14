# lint.R
# Run lintr on all R files in the project.
# Usage:
#   Rscript lint.R              # lint all R files
#   Rscript lint.R file1.R ...  # lint specific files

args <- commandArgs(trailingOnly = TRUE)

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("lintr is not installed. Run: Rscript setup.R")
}

if (length(args) > 0) {
  results <- do.call(c, lapply(args, lintr::lint))
} else {
  results <- lintr::lint_dir(".")
}

if (length(results) > 0) {
  print(results)
  quit(status = 1)
} else {
  cat("No linting issues found.\n")
}
