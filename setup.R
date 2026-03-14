# setup.R
# Installs all R packages required by the project.
# Usage: Rscript setup.R

packages <- c(
  "boot",
  "cowplot",
  "DEoptim",
  "fGarch",
  "ggplot2",
  "kableExtra",
  "knitr",
  "nloptr",
  "PDQutils",
  "PerformanceAnalytics",
  "profvis",
  "quarto",
  "rbcb",
  "rb3",
  "rmarkdown",
  "tidyverse",
  "timeDate",
  "xts",
  "zoo",
  "bindrcpp",
  "lintr"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All packages are already installed.\n")
}
