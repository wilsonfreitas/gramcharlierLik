# render.R
# Renders Quarto (.qmd) files in the project.
# Usage: Rscript render.R [format] [file]
#   format: pdf (default), html, or all
#   file:   estimadores, pricing, or all (default)
#
# Examples:
#   Rscript render.R              # render all files as PDF
#   Rscript render.R pdf pricing  # render pricing paper as PDF
#   Rscript render.R html all     # render all files as HTML
#   Rscript render.R all estimadores  # render estimadores as PDF and HTML

args <- commandArgs(trailingOnly = TRUE)
format <- if (length(args) > 0) args[1] else "pdf"
file_arg <- if (length(args) > 1) args[2] else "all"

# Available Quarto files
all_qmd_files <- list(
  estimadores = "estimadores-assimetria-curtose/estimadores-assimetria-curtose.qmd",
  pricing = "pricing-options-garch-gram-charlier/pricing-options-garch-gram-charlier.qmd"
)

# Select files to render
qmd_files <- switch(file_arg,
  estimadores = all_qmd_files["estimadores"],
  pricing     = all_qmd_files["pricing"],
  all         = all_qmd_files,
  stop("Unknown file: ", file_arg, ". Use estimadores, pricing, or all.")
)

# Select output formats
qmd_formats <- switch(format,
  pdf  = "pdf",
  html = "html",
  all  = c("pdf", "html"),
  stop("Unknown format: ", format, ". Use pdf, html, or all.")
)

# Render selected files
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
