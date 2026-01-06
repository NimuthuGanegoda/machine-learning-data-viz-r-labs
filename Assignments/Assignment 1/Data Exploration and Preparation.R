# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 1 - Data Exploration and Preparation
# ==============================================================================

# Setup & loading -------------------------------------------------------
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(e1071)

# Try both file paths to work on different systems
file_path_nimuthu <- "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/WACY-COM.csv"
file_path_ashani <- "/home/ashani/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/WACY-COM.csv"

if (file.exists(file_path_nimuthu)) {
  file_path <- file_path_nimuthu
  output_dir <- "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1"
} else if (file.exists(file_path_ashani)) {
  file_path <- file_path_ashani
  output_dir <- "/home/ashani/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1"
} else {
  stop("WACY-COM.csv not found in either location!")
}

cat("Reading data from:", file_path, "\n")
dat <- read.csv(file_path, na.strings = "NA", stringsAsFactors = TRUE)

# Display column names for debugging
cat("\nColumn names in dataset:\n")
print(names(dat))
cat("\n")

set.seed(10695889)
selected.rows <- sample(1:nrow(dat), size = 400, replace = FALSE)
mydata <- dat[selected.rows, ]

write.csv(mydata, file.path(output_dir, "mydata.csv"), row.names = FALSE)

# Categorical analysis (Table 1) ---------------------------------------
cat("\n=== TABLE 1: Summary of categorical features ===\n")

cat_vars <- c("Port", "Protocol", "Target.Honeypot.Server.OS", "Source.OS.Detected",
              "Source.IP.Type.Detected", "APT")

table1_list <- list()

for (var in cat_vars) {
  # Special handling for Source.OS.Detected - filter out "???"
  if (var == "Source.OS.Detected") {
    temp_data <- mydata[[var]][mydata[[var]] != "???" & !is.na(mydata[[var]])]
  } else {
    temp_data <- mydata[[var]]
  }

  freq_table <- table(temp_data, useNA = "ifany")

  # Skip if no data
  if (length(freq_table) == 0) {
    next
  }

  pct <- prop.table(freq_table) * 100

  result_df <- data.frame(
    "Categorical Feature" = var,
    "Level" = names(freq_table),
    "N (%)" = paste0(as.vector(freq_table), " (", round(pct, 1), "%)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  table1_list[[var]] <- result_df
}

table1 <- do.call(rbind, table1_list)
rownames(table1) <- NULL

print(table1)
write.csv(table1, file.path(output_dir, "table1_categorical.csv"), row.names = FALSE)
cat("Table 1 saved to:", file.path(output_dir, "table1_categorical.csv"), "\n")

# Continuous analysis (Table 2) ---------------------------------------
cat("\n=== TABLE 2: Summary of numeric features ===\n")

# Define continuous variables (excluding ASIPA and IPRTS per data quality issues)
cont_vars_full <- c(
  "Hits",
  "Average.Request.Size.Bytes",
  "Attack.Window.Seconds",
  "Average.Attacker.Payload.Entropy.Bits",
  "Average.ping.to.attacking.IP.milliseconds",
  "Average.ping.variability",
  "Individual.URLs.requested"
)

cont_abbrev <- c(
  "Hits" = "Hits",
  "Average.Request.Size.Bytes" = "ARS",
  "Attack.Window.Seconds" = "AW",
  "Average.Attacker.Payload.Entropy.Bits" = "AAPE",
  "Average.ping.to.attacking.IP.milliseconds" = "APTAIP",
  "Average.ping.variability" = "APV",
  "Individual.URLs.requested" = "IUR"
)

table2_list <- list()

for (var in cont_vars_full) {
  # Check if variable exists in dataset
  if (!var %in% names(mydata)) {
    cat("WARNING: Variable not found:", var, "\n")
    next
  }

  var_data <- mydata[[var]]

  # Check if variable is numeric
  if (!is.numeric(var_data)) {
    cat("WARNING: Variable is not numeric:", var, "\n")
    next
  }

  n_missing <- sum(is.na(var_data))
  pct_missing <- round((n_missing / length(var_data)) * 100, 0)

  result_df <- data.frame(
    "Continuous" = cont_abbrev[var],
    "N" = n_missing,
    "(%)" = paste0(pct_missing, "%"),
    "Min" = round(min(var_data, na.rm = TRUE), 1),
    "Max" = round(max(var_data, na.rm = TRUE), 1),
    "Mean" = round(mean(var_data, na.rm = TRUE), 1),
    "Median" = round(median(var_data, na.rm = TRUE), 1),
    "Skewness" = round(skewness(var_data, na.rm = TRUE), 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  table2_list[[var]] <- result_df
}

table2 <- do.call(rbind, table2_list)
rownames(table2) <- NULL

print(table2)
write.csv(table2, file.path(output_dir, "table2_continuous.csv"), row.names = FALSE)
cat("Table 2 saved to:", file.path(output_dir, "table2_continuous.csv"), "\n")

# Visualizations for data quality issues --------------------------------
cat("\n=== Generating Figures ===\n")

# Figure 1: APTAIP histograms (pre and post removal of 99999)
aptaip <- mydata$"Average.ping.to.attacking.IP.milliseconds"

# Figure 1a - Pre-removal
png(file.path(output_dir, "Figure1a_APTAIP_pre_removal.png"), width = 800, height = 600)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(aptaip,
     main = "Pre-removal of 99999",
     xlab = "Average ping to attacking IP (milliseconds)",
     ylab = "Count",
     col = "steelblue",
     border = "white",
     breaks = 30)
dev.off()
cat("Figure 1a saved\n")

# Figure 1b - Post-removal
aptaip_clean <- aptaip[!is.na(aptaip) & aptaip != 99999]
png(file.path(output_dir, "Figure1b_APTAIP_post_removal.png"), width = 800, height = 600)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(aptaip_clean,
     main = "Post-removal of 99999",
     xlab = "Average ping to attacking IP (milliseconds)",
     ylab = "Count",
     col = "steelblue",
     border = "white",
     breaks = 30)
dev.off()
cat("Figure 1b saved\n")

# Figure 2: APV histogram
apv <- mydata$"Average.ping.variability"
png(file.path(output_dir, "Figure2_APV_histogram.png"), width = 800, height = 600)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(apv,
  main = "Average ping variability",
  xlab = "Average ping variability (st. dev.)",
  ylab = "Count",
  col = "steelblue",
  border = "white",
  breaks = 30)
dev.off()
cat("Figure 2 saved\n")

cat("\n=== Analysis Complete ===\n")
cat("All outputs saved to:", output_dir, "\n")
