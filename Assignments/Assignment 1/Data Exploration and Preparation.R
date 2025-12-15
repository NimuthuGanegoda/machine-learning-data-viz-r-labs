# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 1 - Data Exploration and Preparation
# ==============================================================================

# --- STARTER CODE (modified per assignment) ---
# dat <- read.csv("WACY-COM.csv", na.strings = "NA", stringsAsFactors = TRUE)
# set.seed(Enter your student ID here)
# selected.rows <- sample(1:nrow(dat), size = 400, replace = FALSE)
# mydata <- dat[selected.rows, ]
# --- END STARTER CODE ---

# Setup & loading -------------------------------------------------------
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(e1071)

file_path <- "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/WACY-COM.csv"
dat <- read.csv(file_path, na.strings = "NA", stringsAsFactors = TRUE)

set.seed(10695889)
selected.rows <- sample(1:nrow(dat), size = 400, replace = FALSE)
mydata <- dat[selected.rows, ]

write.csv(mydata, "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/mydata.csv", row.names = FALSE)

# Categorical analysis (Table 1) ---------------------------------------
cat_cols <- c(
  "Port",
  "Protocol",
  "Target.Honeypot.Server.OS",
  "Source.OS..Detected.",
  "Source.Port.Range",
  "Source.IP.Type..Detected.",
  "APT"
)

cat("\n=== TABLE 1: STATISTICS (CATEGORICAL) ===\n")
cat_results <- list()
for (col in cat_cols) {
  if (col %in% names(mydata)) {
    counts <- table(mydata[[col]], useNA = "always")
    percents <- round(prop.table(counts) * 100, 1)
    out <- data.frame(Category = names(counts), Count = as.integer(counts), Percentage = percents, row.names = NULL)
    cat_results[[col]] <- out
    cat("\n", col, "\n", sep = "")
    print(out)
  } else {
    cat("\nWARNING: Column not found ->", col, "\n")
  }
}

if (length(cat_results) > 0) {
  cat_table <- do.call(rbind, lapply(names(cat_results), function(nm) cbind(Variable = nm, cat_results[[nm]])))
  write.csv(cat_table, "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/table1_categorical.csv", row.names = FALSE)
}

# Continuous analysis (Table 2) ---------------------------------------
num_cols <- c(
  "Hits",
  "Average.Request.Size..Bytes.",
  "Attack.Window..Seconds.",
  "Average.Attacker.Payload.Entropy..Bits.",
  "Attack.Source.IP.Address.Count",
  "Average.ping.to.attacking.IP..milliseconds.",
  "Average.ping.variability..st.dev.",
  "Individual.URLs.requested",
  "IP.Range.Trust.Score"
)

cat("\n=== TABLE 2: STATISTICS (CONTINUOUS) ===\n")
cont_results <- list()
for (col in num_cols) {
  if (col %in% names(mydata)) {
    vals <- mydata[[col]]
    n_missing <- sum(is.na(vals))
    pct_missing <- round((n_missing / length(vals)) * 100, 1)
    min_val <- round(min(vals, na.rm = TRUE), 1)
    max_val <- round(max(vals, na.rm = TRUE), 1)
    mean_val <- round(mean(vals, na.rm = TRUE), 1)
    med_val <- round(median(vals, na.rm = TRUE), 1)
    skew_val <- round(skewness(vals, na.rm = TRUE), 1)

    out <- data.frame(
      Variable = col,
      Missing_N = n_missing,
      Missing_Pct = pct_missing,
      Min = min_val,
      Max = max_val,
      Mean = mean_val,
      Median = med_val,
      Skewness = skew_val
    )
    cont_results[[col]] <- out

    cat("\n", col, "\n", sep = "")
    cat(" Missing:", n_missing, "(", pct_missing, "%)\n", sep = "")
    cat(" Min:", min_val, " Max:", max_val, "\n", sep = "")
    cat(" Mean:", mean_val, " Median:", med_val, "\n", sep = "")
    cat(" Skewness:", skew_val, "\n", sep = "")
  } else {
    cat("\nWARNING: Column not found ->", col, "\n")
  }
}

if (length(cont_results) > 0) {
  cont_table <- do.call(rbind, cont_results)
  write.csv(cont_table, "/home/nimuthu/Repo/machine-learning-data-viz-r-labs/Assignments/Assignment 1/table2_continuous.csv", row.names = FALSE)
}
