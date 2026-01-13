# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 3 - Supervised Learning Modelling
# ==============================================================================

# 1. DEFINE FILE LOCATION
# ------------------------------------------------------------------------------
# Robustly detect script directory without changing the working directory
detect_script_dir <- function() {
    # 1) Rscript invocation
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
        return(dirname(normalizePath(sub("^--file=", "", file_arg))))
    }

    # 2) RStudio interactive
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("getActiveDocumentContext")) {
        ctx <- rstudioapi::getActiveDocumentContext()
        if (!is.null(ctx$path) && nzchar(ctx$path)) {
            return(dirname(normalizePath(ctx$path)))
        }
    }

    # 3) source() interactive
    if (!is.null(sys.frame(1)) && !is.null(sys.frame(1)$ofile)) {
        return(dirname(normalizePath(sys.frame(1)$ofile)))
    }

    # 4) Fallback to current working directory
    return(getwd())
}

script_dir <- detect_script_dir()
cat("Script directory detected as:", script_dir, "\n")
# ------------------------------------------------------------------------------

# 2. SETUP & LIBRARIES
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(caret)) install.packages("caret")
if (!require(glmnet)) install.packages("glmnet")
if (!require(rpart)) install.packages("rpart")
if (!require(ranger)) install.packages("ranger")
if (!require(forcats)) install.packages("forcats")

library(tidyverse)
library(caret)
library(glmnet)
library(rpart)
library(ranger)
library(forcats)

# Set seed for reproducibility using Student ID
MY_SEED <- 10695889
set.seed(MY_SEED)

# ==============================================================================
# PART 1: DATA LOADING & CLEANING
# ==============================================================================

# Verification Check with detailed diagnostics (without setwd)
csv_filename <- "WACY-COM.csv"
csv_path <- file.path(script_dir, csv_filename)

cat("\n=== FILE DETECTION ===\n")
cat("Primary lookup:", csv_path, "\n")
cat("Exists:", file.exists(csv_path), "\n")

if (!file.exists(csv_path)) {
    cat("\nPrimary location missing. Script dir contents:\n")
    print(list.files(script_dir))

    # Check parent directory
    parent_csv <- file.path(dirname(script_dir), csv_filename)
    cat("Checking parent:", parent_csv, " (exists:", file.exists(parent_csv), ")\n")

    if (file.exists(parent_csv)) {
        csv_path <- parent_csv
    } else {
        stop(paste0("\nFATAL: ", csv_filename, " not found in:\n  - ", script_dir, "\n  - ", dirname(script_dir)))
    }
}

cat("Reading CSV from:", csv_path, "\n\n")
dat <- read.csv(csv_path, na.strings = c("NA", ""), stringsAsFactors = TRUE)
cat("Successfully loaded", nrow(dat), "rows and", ncol(dat), "columns\n")

# i. Cleaning based on Assignment 1 & 3 requirements
dat <- dat %>% filter(Source.OS.Detected != "???")
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL
dat <- dat %>% filter(Average.ping.to.attacking.IP.milliseconds != 99999)
dat <- dat %>% filter(Attack.Source.IP.Address.Count != -1)

# ii. Merging Categories using fct_collapse
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008")
)

dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_DeskServ = c("Windows (Desktops)", "Windows (Servers)"),
    MacOS_Linux = c("Linux", "MacOS (All)")
)

# iii. Log and Sqrt Transformations
dat$Average.ping.variability <- log(dat$Average.ping.variability + 1)
dat$Hits <- sqrt(dat$Hits)
dat$Attack.Source.IP.Address.Count <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$Average.ping.to.attacking.IP.milliseconds <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$Individual.URLs.requested <- sqrt(dat$Individual.URLs.requested)

# iv. Final Clean Dataset
WACY_COM_cleaned <- na.omit(dat)

# c) Partition Data (30% Training / 70% Testing)
set.seed(MY_SEED)
trainIndex <- createDataPartition(WACY_COM_cleaned$APT, p = 0.30, list = FALSE)
train_data <- WACY_COM_cleaned[trainIndex, ]
test_data <- WACY_COM_cleaned[-trainIndex, ]

# ==============================================================================
# PART 2: MODELLING & SIMULTANEOUS TUNING
# ==============================================================================

# Setup Training Control: 5-Fold Repeated CV (2 repeats)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

# --- MODEL 1: LOGISTIC RIDGE REGRESSION ---
set.seed(MY_SEED)
grid_ridge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 10))
model_ridge <- train(APT ~ .,
    data = train_data, method = "glmnet",
    trControl = fitControl, tuneGrid = grid_ridge
)

# --- MODEL 2: CLASSIFICATION TREE ---
set.seed(MY_SEED)
grid_tree <- expand.grid(cp = seq(0.001, 0.05, length = 10))
model_tree <- train(APT ~ .,
    data = train_data, method = "rpart",
    trControl = fitControl, tuneGrid = grid_tree
)

# --- MODEL 3: RANDOM FOREST (SIMULTANEOUS TUNING) ---
set.seed(MY_SEED)
rf_results <- list()
ntree_vals <- c(100, 300, 500)
rf_grid <- expand.grid(mtry = c(2, 4, 6), splitrule = "gini", min.node.size = c(1, 5, 10))

for (nt in ntree_vals) {
    res_name <- paste0("ntree_", nt)
    rf_results[[res_name]] <- train(APT ~ .,
        data = train_data, method = "ranger",
        trControl = fitControl, tuneGrid = rf_grid, num.trees = nt
    )
}

# Select best RF model
best_idx <- which.max(sapply(rf_results, function(x) max(x$results$Accuracy)))
model_rf <- rf_results[[best_idx]]

# ==============================================================================
# PART 3: RESULTS & METRICS
# ==============================================================================

# Calculate Predictions for all models
pred_ridge <- predict(model_ridge, newdata = test_data)
pred_tree <- predict(model_tree, newdata = test_data)
pred_rf <- predict(model_rf, newdata = test_data)

# Confusion Matrices
cm_ridge <- confusionMatrix(pred_ridge, test_data$APT, positive = "Yes")
cm_tree <- confusionMatrix(pred_tree, test_data$APT, positive = "Yes")
cm_rf <- confusionMatrix(pred_rf, test_data$APT, positive = "Yes")

# Table for Report
performance_table <- data.frame(
    Metric = c("Accuracy", "Sensitivity (APT)", "Specificity"),
    Logistic_Ridge = c(round(cm_ridge$overall["Accuracy"], 4), round(cm_ridge$byClass["Sensitivity"], 4), round(cm_ridge$byClass["Specificity"], 4)),
    Classification_Tree = c(round(cm_tree$overall["Accuracy"], 4), round(cm_tree$byClass["Sensitivity"], 4), round(cm_tree$byClass["Specificity"], 4)),
    Random_Forest = c(round(cm_rf$overall["Accuracy"], 4), round(cm_rf$byClass["Sensitivity"], 4), round(cm_rf$byClass["Specificity"], 4))
)

cat("\n--- FINAL RESULTS TABLE ---\n")
print(performance_table)

cat("\n--- OPTIMAL HYPERPARAMETERS ---\n")
cat("Ridge Lambda:", model_ridge$bestTune$lambda, "\n")
cat("Tree CP:", model_tree$bestTune$cp, "\n")
cat("RF (mtry/node/ntree):", model_rf$bestTune$mtry, "/", model_rf$bestTune$min.node.size, "/", model_rf$finalModel$num.trees, "\n")
