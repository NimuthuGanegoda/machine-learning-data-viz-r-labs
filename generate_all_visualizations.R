#!/usr/bin/env Rscript
# Auto-generate all visualizations from R scripts in the repository

# Install required packages if not available
required_packages <- c(
  "vegan", "tidyverse", "scales", "scatterplot3d",
  "mlbench", "mvabund", "ggpubr", "factoextra",
  "car", "corrplot", "cowplot", "forecast"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("=== Starting Visualization Generation ===\n\n")

# Function to safely run R scripts and capture plots
run_and_visualize <- function(script_path, output_dir) {
  cat(paste0("Processing: ", script_path, "\n"))

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Get script name without extension
  script_name <- tools::file_path_sans_ext(basename(script_path))

  # Redirect all plots to PDF
  pdf_file <- file.path(output_dir, paste0(script_name, "_plots.pdf"))
  pdf(pdf_file, width = 12, height = 8)

  tryCatch({
    # Source the script
    source(script_path, local = new.env())
    cat(paste0("  ✓ Generated: ", pdf_file, "\n"))
  }, error = function(e) {
    cat(paste0("  ✗ Error: ", e$message, "\n"))
  }, finally = {
    dev.off()
  })
}

# Find all R script files (excluding renv and system files)
r_scripts <- c(
  "Workshops/Week 1/Introduction to R (Part 1)- Code Only-1-1.r",
  "Workshops/Week 1/Introduction to R (Part 2)- Code Only-1.r",
  "Workshops/Week 2/Data Visualisation with GGPlot - Code Only-2.R",
  "Workshops/Week 3/PCA and PCoA - Code Only-1-1.R",
  "Workshops/Week 4/Hierarchical Clustering Analysis - Code Only-1-2.R",
  "Workshops/Week 3/viz_demo.R",
  "Workshops/Week 5/viz_demo.R",
  "Workshops/Week 6/viz_demo.R",
  "Workshops/Week 7/viz_demo.R",
  "Workshops/Week 8/viz_demo.R"
)

# Process each script
for (script in r_scripts) {
  if (file.exists(script)) {
    output_dir <- file.path(dirname(script), "visualizations_output")
    run_and_visualize(script, output_dir)
  }
}

cat("\n=== Visualization Generation Complete ===\n")
cat("All plots have been saved as PDF files in 'visualizations_output' folders\n")
