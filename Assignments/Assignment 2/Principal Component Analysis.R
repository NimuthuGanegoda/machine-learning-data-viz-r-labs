# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 2 - Principal Component Analysis
# ==============================================================================

# Setup and Configuration -----------------------------------------------------
# Determine file paths for different systems and set output directory
file_path_nimuthu <- "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 2/WACY-COM.csv"
file_path_ashani <- "/home/ashani/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 2/WACY-COM.csv"

if (file.exists(file_path_nimuthu)) {
  file_path <- file_path_nimuthu
  output_dir <- "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 2"
} else if (file.exists(file_path_ashani)) {
  file_path <- file_path_ashani
  output_dir <- "/home/ashani/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 2"
} else {
  stop("WACY-COM.csv not found in either location!")
}

cat("Reading data from:", file_path, "\n")
cat("Output will be saved to:", output_dir, "\n\n")

# (i) Import the dataset into R Studio ----------------------------------------
dat <- read.csv(file_path, na.strings = NA, stringsAsFactors = TRUE)
cat("Dataset loaded successfully.\n")
cat("Original dataset dimensions:", nrow(dat), "rows x", ncol(dat), "columns\n\n")

# (ii) Generate sub-sample and extract features -------------------------------
# Set seed to student ID for reproducibility
set.seed(10695889)

# Randomly select 400 rows without replacement
selected.rows <- sample(1:nrow(dat), size = 400, replace = FALSE)

# Create sub-sample of 400 observations
mydata <- dat[selected.rows, ]
cat("Sub-sample created: 400 rows\n")
cat("Dimensions check:", dim(mydata), "\n\n")

# Extract numeric features AND the APT target variable
# Note: We include APT for later visualization but not for PCA itself
my_extracted_data <- mydata[, c(
  "Hits",
  "Average.Request.Size.Bytes",
  "Attack.Window.Seconds",
  "Average.Attacker.Payload.Entropy.Bits",
  "Average.ping.to.attacking.IP.milliseconds",
  "Average.ping.variability",
  "Individual.URLs.requested",
  "APT"
)]
cat("Extracted features:", ncol(my_extracted_data) - 1, "numeric + 1 target (APT)\n\n")

# (iii) Data Cleaning Based on Assignment 1 Feedback -------------------------
cat("=== Data Cleaning ===\n")

# Remove invalid APTAIP outliers (99999 represents missing/invalid pings)
# These were identified as data quality issues in Assignment 1
rows_before <- nrow(my_extracted_data)
clean_data <- my_extracted_data[my_extracted_data$Average.ping.to.attacking.IP.milliseconds != 99999, ]
rows_removed <- rows_before - nrow(clean_data)
cat("Removed", rows_removed, "rows with APTAIP = 99999\n")

# Apply log transformations to address skewness
# APV (Average Ping Variability) and APTAIP were highly right-skewed in Assignment 1
# Log transformation makes distributions more normal, which is better for PCA
# Adding 1 to handle zero values before log transformation
clean_data$log_APV <- log(clean_data$Average.ping.variability + 1)
clean_data$log_APTAIP <- log(clean_data$Average.ping.to.attacking.IP.milliseconds + 1)
cat("Applied log transformations to APV and APTAIP\n\n")

# (iv) Remove Incomplete Cases ------------------------------------------------
# Remove rows with any missing values to prepare data for PCA
# PCA requires complete data for all observations
pca_ready_data <- na.omit(clean_data)
cat("Removed incomplete cases:", rows_before - nrow(pca_ready_data), "rows\n")
cat("Final dataset for PCA:", nrow(pca_ready_data), "rows (~",
    round(nrow(pca_ready_data) / 400 * 100, 1), "% of original 400)\n\n")

# (v) Perform PCA -------------------------------------------------------------
cat("=== Performing Principal Component Analysis ===\n")

# Scaling Decision:
# We MUST scale the data (scale=TRUE) because:
# 1. Features have vastly different units and ranges:
#    - Hits can be in millions
#    - AAPE (entropy) is typically between 0-8 bits
#    - Attack Window can be seconds to hours
# 2. Without scaling, variables with larger ranges would dominate the PCA
# 3. Scaling standardizes all features to mean=0 and sd=1, giving equal weight

# Select only the cleaned numeric features for PCA
# Exclude: original APV and APTAIP (using log versions instead)
# Exclude: APT (target variable, not a predictor)
pca_features <- c(
  "Hits",
  "Average.Request.Size.Bytes",
  "Attack.Window.Seconds",
  "Average.Attacker.Payload.Entropy.Bits",
  "Individual.URLs.requested",
  "log_APV",
  "log_APTAIP"
)

pca_final <- prcomp(
  pca_ready_data[, pca_features],
  scale = TRUE  # Standardize features before PCA
)

cat("PCA completed successfully with", length(pca_features), "features\n\n")

# Display PCA Summary ---------------------------------------------------------
cat("=== PCA Summary ===\n")
summary(pca_final)
cat("\n")

# (v) Display variance explained (3 decimal places) --------------------------
cat("=== Variance Explained by Each Principal Component ===\n")

# Extract variance information
pca_variance <- pca_final$sdev^2  # Variance = standard deviation squared
total_variance <- sum(pca_variance)

# Individual proportion of variance for each PC
individual_var <- pca_variance / total_variance

# Cumulative proportion of variance
cumulative_var <- cumsum(individual_var)

# Create formatted table
variance_table <- data.frame(
  PC = paste0("PC", 1:length(pca_variance)),
  Individual = sprintf("%.3f", individual_var),  # 3 decimal places
  Cumulative = sprintf("%.3f", cumulative_var)   # 3 decimal places
)

print(variance_table)
cat("\n")

# Determine number of PCs for 50% variance
pcs_for_50 <- which(cumulative_var >= 0.50)[1]
cat("Number of PCs needed to explain at least 50% variance:", pcs_for_50, "\n")
cat("Cumulative variance with", pcs_for_50, "PC(s):",
    sprintf("%.3f", cumulative_var[pcs_for_50]), "\n\n")

# (v) Display loadings for PC1, PC2, PC3 (3 decimal places) ------------------
cat("=== Loadings (Coefficients) for PC1, PC2, and PC3 ===\n")

# Extract rotation matrix (loadings)
loadings <- pca_final$rotation

# Display loadings for first 3 PCs with 3 decimal places
loadings_table <- data.frame(
  Feature = rownames(loadings),
  PC1 = sprintf("%.3f", loadings[, 1]),
  PC2 = sprintf("%.3f", loadings[, 2]),
  PC3 = sprintf("%.3f", loadings[, 3])
)

print(loadings_table, row.names = FALSE)
cat("\n")

# Interpretation of key drivers (absolute loading > 0.3)
cat("Key drivers for each PC (|loading| > 0.3):\n")
for (pc_num in 1:3) {
  pc_name <- paste0("PC", pc_num)
  strong_loadings <- abs(loadings[, pc_num]) > 0.3
  if (any(strong_loadings)) {
    cat(pc_name, ":", paste(rownames(loadings)[strong_loadings], collapse = ", "), "\n")
  }
}
cat("\n")

# (vi) Create Biplot: PC1 vs PC2 colored by APT -------------------------------
cat("=== Creating Biplot: PC1 vs PC2 ===\n")

# Extract PC scores (transformed data)
pca_scores <- pca_final$x

# Create biplot with APT color coding
png(file.path(output_dir, "Figure3_PCA_Biplot.png"), width = 1000, height = 800)
par(mar = c(5, 5, 4, 2))

# Plot points colored by APT
colors <- ifelse(pca_ready_data$APT == "Known", "red", "blue")
plot(pca_scores[, 1], pca_scores[, 2],
     col = colors,
     pch = 19,
     cex = 1.2,
     main = "PCA Biplot: PC1 vs PC2 (Colored by APT Activity)",
     xlab = paste0("PC1 (", sprintf("%.1f", individual_var[1] * 100), "% variance)"),
     ylab = paste0("PC2 (", sprintf("%.1f", individual_var[2] * 100), "% variance)"),
     cex.lab = 1.2,
     cex.main = 1.3)

# Add legend
legend("topright",
       legend = c("Known APT", "Unknown APT"),
       col = c("red", "blue"),
       pch = 19,
       cex = 1.1)

# Add loading vectors (scaled for visibility)
scaling_factor <- 3
arrows(0, 0,
       loadings[, 1] * scaling_factor,
       loadings[, 2] * scaling_factor,
       length = 0.1,
       col = "darkgray",
       lwd = 2)

# Add loading labels
text(loadings[, 1] * scaling_factor * 1.15,
     loadings[, 2] * scaling_factor * 1.15,
     labels = rownames(loadings),
     col = "darkgreen",
     cex = 0.9,
     font = 2)

dev.off()
cat("Biplot saved to:", file.path(output_dir, "Figure3_PCA_Biplot.png"), "\n\n")

# (vii) Analyze PC1 vs PC2 for APT Classification ----------------------------
cat("=== Analysis: Which PC Better Classifies APT? ===\n")

# Extract PC1 and PC2 scores
pc1_scores <- pca_scores[, 1]
pc2_scores <- pca_scores[, 2]

# Separate by APT status
pc1_known <- pc1_scores[pca_ready_data$APT == "Known"]
pc1_unknown <- pc1_scores[pca_ready_data$APT == "Unknown"]
pc2_known <- pc2_scores[pca_ready_data$APT == "Known"]
pc2_unknown <- pc2_scores[pca_ready_data$APT == "Unknown"]

# Calculate separation metrics
cat("PC1 Separation:\n")
cat("  Known APT - Mean:", sprintf("%.3f", mean(pc1_known)),
    ", SD:", sprintf("%.3f", sd(pc1_known)), "\n")
cat("  Unknown APT - Mean:", sprintf("%.3f", mean(pc1_unknown)),
    ", SD:", sprintf("%.3f", sd(pc1_unknown)), "\n")
cat("  Difference in means:", sprintf("%.3f", abs(mean(pc1_known) - mean(pc1_unknown))), "\n\n")

cat("PC2 Separation:\n")
cat("  Known APT - Mean:", sprintf("%.3f", mean(pc2_known)),
    ", SD:", sprintf("%.3f", sd(pc2_known)), "\n")
cat("  Unknown APT - Mean:", sprintf("%.3f", mean(pc2_unknown)),
    ", SD:", sprintf("%.3f", sd(pc2_unknown)), "\n")
cat("  Difference in means:", sprintf("%.3f", abs(mean(pc2_known) - mean(pc2_unknown))), "\n\n")

# Determine which PC has better separation
pc1_separation <- abs(mean(pc1_known) - mean(pc1_unknown))
pc2_separation <- abs(mean(pc2_known) - mean(pc2_unknown))

if (pc1_separation > pc2_separation) {
  cat("CONCLUSION: PC1 better assists in classifying APT\n")
  cat("Reason: Greater separation between Known and Unknown APT (",
      sprintf("%.3f", pc1_separation), " vs ", sprintf("%.3f", pc2_separation), ")\n\n")
  chosen_pc <- "PC1"
} else {
  cat("CONCLUSION: PC2 better assists in classifying APT\n")
  cat("Reason: Greater separation between Known and Unknown APT (",
      sprintf("%.3f", pc2_separation), " vs ", sprintf("%.3f", pc1_separation), ")\n\n")
  chosen_pc <- "PC2"
}

# Identify key features driving the chosen PC
cat("Key features driving", chosen_pc, "(|loading| > 0.3):\n")
chosen_pc_num <- ifelse(chosen_pc == "PC1", 1, 2)
strong_features <- abs(loadings[, chosen_pc_num]) > 0.3
for (feat in rownames(loadings)[strong_features]) {
  loading_val <- loadings[feat, chosen_pc_num]
  cat("  -", feat, ":", sprintf("%.3f", loading_val), "\n")
}

cat("\n=== PCA Analysis Complete ===\n")
cat("All outputs saved to:", output_dir, "\n")
