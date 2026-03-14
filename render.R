# render.R
# Renders all Quarto (.qmd) and R Markdown (.Rmd) files in the project.
# Usage: Rscript render.R [pdf|html|all]
#   pdf  - render PDF output only (default)
#   html - render HTML output only
#   all  - render both PDF and HTML

args <- commandArgs(trailingOnly = TRUE)
format <- if (length(args) > 0) args[1] else "pdf"

# Quarto files (rendered with quarto)
qmd_files <- c(
  "estimadores-assimetria-curtose/estimadores-assimetria-curtose.qmd"
)

# Render Quarto files
qmd_formats <- switch(format,
  pdf  = "pdf",
  html = "html",
  all  = c("pdf", "html"),
  stop("Unknown format: ", format, ". Use pdf, html, or all.")
)

for (qmd in qmd_files) {
  if (!file.exists(qmd)) {
    cat("Skipping (not found):", qmd, "\n")
    next
  }
  for (fmt in qmd_formats) {
    cat("Rendering", qmd, "->", fmt, "\n")
    quarto::quarto_render(qmd, output_format = fmt, quiet = FALSE)
  }
}

cat("Done.\n")
