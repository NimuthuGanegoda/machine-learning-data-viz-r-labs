# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 2 - Principal Component Analysis
# ==============================================================================

# ==============================================================================
# PART (i): Import the dataset into R Studio
# ==============================================================================
# You may need to change/include the path of your working directory
# Import the dataset into R Studio.
dat <- read.csv("WACY-COM.csv", na.strings = NA, stringsAsFactors = TRUE)

set.seed(10695889) # Enter your student ID here

# Randomly select 400 rows
selected.rows <- sample(1:nrow(dat), size = 400, replace = FALSE)

# Your sub-sample of 400 observations
mydata <- dat[selected.rows, ]

dim(mydata) # check the dimension of your sub-sample

# Set output directory for saving plots
output_dir <- getwd() # Use current working directory

# ==============================================================================
# PART (ii): Extract only the numeric features and the APT feature
# ==============================================================================
# Extract the 7 continuous numeric features from Assignment 1
# PLUS the APT target variable (needed for visualization later)
my_extracted_data <- mydata[, c(
  "Hits", # Number of requests
  "Average.Request.Size.Bytes", # Size of payloads
  "Attack.Window.Seconds", # Duration of attack
  "Average.Attacker.Payload.Entropy.Bits", # Complexity/randomness
  "Average.ping.to.attacking.IP.milliseconds", # Network latency
  "Average.ping.variability", # Network stability
  "Individual.URLs.requested", # Diversity of requests
  "APT" # Target: Yes/No APT activity
)]

# ==============================================================================
# PART (iii): Clean the extracted data based on Assignment 1 feedback
# ==============================================================================
# Step 1: Remove outliers/invalid data
# APTAIP value of 99999 = missing/invalid ping data (identified in Assignment 1)
rows_before <- nrow(my_extracted_data)
clean_data <- my_extracted_data[my_extracted_data$Average.ping.to.attacking.IP.milliseconds != 99999, ]
rows_removed <- rows_before - nrow(clean_data)

# Step 2: Apply log transformations to fix skewness
# APV and APTAIP were highly right-skewed (long tail to the right) in Assignment 1
# Log transformation makes them more normally distributed, which improves PCA
# We add +1 before taking log to handle any zero values (log(0) is undefined)
clean_data$log_APV <- log(clean_data$Average.ping.variability + 1)
clean_data$log_APTAIP <- log(clean_data$Average.ping.to.attacking.IP.milliseconds + 1)

# ==============================================================================
# PART (iv): Remove incomplete cases
# ==============================================================================
# PCA requires complete data - no missing values allowed
# na.omit() removes any rows that have NA (missing) values in any column
pca_ready_data <- na.omit(clean_data)

# ==============================================================================
# PART (v): Perform PCA using prcomp() - only on numeric features
# ==============================================================================
# ---------------
# SCALING DECISION: WHY we MUST scale (scale=TRUE)
# ---------------
# Our features have vastly different units and ranges:
#   - Hits: millions of requests
#   - Entropy: 0-8 bits
#   - Attack Window: seconds to hours
#   - Request Size: bytes
#
# WITHOUT scaling: Variables with larger ranges dominate PCA
# WITH scaling: All features standardized to mean=0, sd=1 → equal weight
#
# ANSWER: YES, we should scale to ensure fair contribution from all features

# Select the 7 numeric features for PCA
pca_features <- c(
  "Hits",
  "Average.Request.Size.Bytes",
  "Attack.Window.Seconds",
  "Average.Attacker.Payload.Entropy.Bits",
  "Individual.URLs.requested",
  "log_APV", # Using log-transformed version
  "log_APTAIP" # Using log-transformed version
)

# Run PCA with scaling
pca_final <- prcomp(
  pca_ready_data[, pca_features],
  scale = TRUE # Standardize features before PCA
)

# Display PCA Summary
cat("=== PCA Summary ===\n")
summary(pca_final)
cat("\n")

# ------------------------------------------------------------------------------
# QUESTION: Display variance explained (3 decimal places) - Individual & Cumulative
# ------------------------------------------------------------------------------
cat("=== Variance Explained by Each Principal Component ===\n")

# Calculate variance from standard deviations
pca_variance <- pca_final$sdev^2 # Variance = (standard deviation)²
total_variance <- sum(pca_variance)

# Individual proportion: how much variance EACH PC explains
individual_var <- pca_variance / total_variance

# Cumulative proportion: TOTAL variance explained by PCs so far
cumulative_var <- cumsum(individual_var)

# Create table with 3 decimal places (as required)
variance_table <- data.frame(
  PC = paste0("PC", 1:length(pca_variance)),
  Individual = sprintf("%.3f", individual_var),
  Cumulative = sprintf("%.3f", cumulative_var)
)

print(variance_table)
cat("\n")

# ---------------
# QUESTION: How many PCs needed to explain at least 50% of variability?
# ---------------
pcs_for_50 <- which(cumulative_var >= 0.50)[1]
cat("ANSWER:", pcs_for_50, "principal components are adequate\n")
cat(
  "They explain", sprintf("%.3f", cumulative_var[pcs_for_50]), "(",
  sprintf("%.1f%%", cumulative_var[pcs_for_50] * 100), ") of total variance\n\n"
)

# ------------------------------------------------------------------------------
# QUESTION: Display & interpret loadings for PC1, PC2, PC3 (3 decimal places)
# ------------------------------------------------------------------------------
cat("=== Loadings (Coefficients) for PC1, PC2, and PC3 ===\n")

# Loadings = how each original feature contributes to each PC
loadings <- pca_final$rotation

# Display first 3 PCs with 3 decimal places (as required)
loadings_table <- data.frame(
  Feature = rownames(loadings),
  PC1 = sprintf("%.3f", loadings[, 1]),
  PC2 = sprintf("%.3f", loadings[, 2]),
  PC3 = sprintf("%.3f", loadings[, 3])
)

print(loadings_table, row.names = FALSE)
cat("\n")

# ---------------
# QUESTION: Which features are key drivers for PC1, PC2, PC3?
# ---------------
# Key driver = |loading| > 0.3 (strong contribution to that PC)
cat("Key drivers for each PC (based on |loading| > 0.3):\n\n")
for (pc_num in 1:3) {
  pc_name <- paste0("PC", pc_num)
  strong_loadings <- abs(loadings[, pc_num]) > 0.3

  if (any(strong_loadings)) {
    cat(pc_name, "key drivers:\n")
    key_features <- rownames(loadings)[strong_loadings]
    for (feat in key_features) {
      cat("  -", feat, "(", sprintf("%.3f", loadings[feat, pc_num]), ")\n")
    }
    cat("\n")
  }
}
cat("\n")

# ==============================================================================
# PART (vi): Create biplot to visualize PCA results (PC1 vs PC2)
# ==============================================================================
cat("=== Creating Biplot: PC1 vs PC2 ===\n")

# Extract PC scores (the transformed/projected data points)
pca_scores <- pca_final$x

# Create high-quality biplot visualization
png(file.path(output_dir, "Figure3_PCA_Biplot.png"), width = 1000, height = 800)
par(mar = c(5, 5, 4, 2))

# Color code points by APT status: Red = APT (Yes), Blue = No APT (No)
colors <- ifelse(pca_ready_data$APT == "Yes", "#E74C3C", "#3498DB") # Red for Known, Blue for Unknown

# Plot the observations in PC1-PC2 space
plot(pca_scores[, 1], pca_scores[, 2],
  col = colors,
  pch = 19, # Solid circles
  cex = 1.2, # Point size
  main = "PCA Biplot: PC1 vs PC2 (Colored by APT Activity)",
  xlab = paste0("PC1 (", sprintf("%.1f", individual_var[1] * 100), "% variance)"),
  ylab = paste0("PC2 (", sprintf("%.1f", individual_var[2] * 100), "% variance)"),
  cex.lab = 1.3,
  cex.main = 1.4,
  cex.axis = 1.1
)

# Add grid for readability
grid(col = "gray90", lty = 1)

# Add legend to explain colors
legend("topright",
  legend = c("APT = Yes", "APT = No"),
  col = c("#E74C3C", "#3498DB"),
  pch = 19,
  cex = 1.2,
  bg = "white"
)

# Add loading vectors (arrows) to show feature contributions
# Arrows point in direction of increasing feature values
# Arrow length = strength of feature contribution to PC1 and PC2
scaling_factor <- 4 # Scale arrows to make them visible
arrows(0, 0,
  loadings[, 1] * scaling_factor,
  loadings[, 2] * scaling_factor,
  length = 0.12, # Arrow head size
  col = "#2C3E50", # Dark gray arrows
  lwd = 2.5
) # Arrow thickness

# Label each arrow with the feature name
text(loadings[, 1] * scaling_factor * 1.2,
  loadings[, 2] * scaling_factor * 1.2,
  labels = rownames(loadings),
  col = "#27AE60", # Green text
  cex = 0.95,
  font = 2
) # Bold font

dev.off()
cat("Biplot saved to:", file.path(output_dir, "Figure3_PCA_Biplot.png"), "\n\n")

# ==============================================================================
# PART (vii): Determine which PC (PC1 or PC2) best classifies APT activity
# ==============================================================================
cat("=== PC1 vs PC2 for APT Classification ===\n\n")

# Extract PC1 and PC2 scores for all observations
pc1_scores <- pca_scores[, 1]
pc2_scores <- pca_scores[, 2]

# Separate scores by APT status
# This allows us to compare where APT=Yes vs APT=No fall on each axis
pc1_apt_yes <- pc1_scores[pca_ready_data$APT == "Yes"] # APT attacks on PC1
pc1_apt_no <- pc1_scores[pca_ready_data$APT == "No"] # Non-APT on PC1
pc2_apt_yes <- pc2_scores[pca_ready_data$APT == "Yes"] # APT attacks on PC2
pc2_apt_no <- pc2_scores[pca_ready_data$APT == "No"] # Non-APT on PC2

# Analyze PC1 axis projection
# "Projecting onto PC1" means looking at points along the PC1 axis only
cat("PC1 axis projection:\n")
cat("  APT=Yes:  Mean =", sprintf("%.3f", mean(pc1_apt_yes)), "\n")
cat("  APT=No:   Mean =", sprintf("%.3f", mean(pc1_apt_no)), "\n")
cat("  Separation:", sprintf("%.3f", abs(mean(pc1_apt_yes) - mean(pc1_apt_no))), "\n\n")

# Analyze PC2 axis projection
# "Projecting onto PC2" means looking at points along the PC2 axis only
cat("PC2 axis projection:\n")
cat("  APT=Yes:  Mean =", sprintf("%.3f", mean(pc2_apt_yes)), "\n")
cat("  APT=No:   Mean =", sprintf("%.3f", mean(pc2_apt_no)), "\n")
cat("  Separation:", sprintf("%.3f", abs(mean(pc2_apt_yes) - mean(pc2_apt_no))), "\n\n")

# Make the classification decision
# Whichever PC has greater separation between APT=Yes and APT=No is better
pc1_sep <- abs(mean(pc1_apt_yes) - mean(pc1_apt_no))
pc2_sep <- abs(mean(pc2_apt_yes) - mean(pc2_apt_no))

if (pc1_sep > pc2_sep) {
  cat("ANSWER: PC1 better assists in classifying APT\n")
  cat("Reason: Greater separation (", sprintf("%.3f", pc1_sep), " vs ", sprintf("%.3f", pc2_sep), ")\n\n")
  chosen_pc_num <- 1
} else {
  cat("ANSWER: PC2 better assists in classifying APT\n")
  cat("Reason: Greater separation (", sprintf("%.3f", pc2_sep), " vs ", sprintf("%.3f", pc1_sep), ")\n\n")
  chosen_pc_num <- 2
}

# Identify key features that drive the chosen PC
# These are the features we should focus on for APT classification
cat("Key features (|loading| > 0.3) for PC", chosen_pc_num, ":\n")
strong_features <- abs(loadings[, chosen_pc_num]) > 0.3
for (feat in rownames(loadings)[strong_features]) {
  cat("  -", feat, ":", sprintf("%.3f", loadings[feat, chosen_pc_num]), "\n")
}

cat("\n=== Analysis Complete ===\n")
