# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 3 - Supervised Learning Modelling (SIMPLIFIED)
# ==============================================================================

# 1. SETUP & LIBRARIES
if(!require(glmnet)) install.packages("glmnet", repos="https://cloud.r-project.org/")
if(!require(rpart)) install.packages("rpart", repos="https://cloud.r-project.org/")
if(!require(ipred)) install.packages("ipred", repos="https://cloud.r-project.org/")
if(!require(randomForest)) install.packages("randomForest", repos="https://cloud.r-project.org/")
if(!require(forcats)) install.packages("forcats", repos="https://cloud.r-project.org/")

library(glmnet)
library(rpart)
library(ipred)
library(randomForest)
library(forcats)

# Set seed for reproducibility (Student ID)
MY_SEED <- 10695889

# ==============================================================================
# PART 1: DATA PREPARATION & CLEANING
# ==============================================================================

# a) Import Data
cat("Loading data...\n")
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)
cat("Initial dimensions:", dim(dat), "\n")

# b) Cleaning & Feature Engineering

# --- Step i: General Cleaning ---
# Remove invalid "???" in Source.OS.Detected
dat <- dat[dat$Source.OS.Detected != "???", ]
cat("After removing ???:", nrow(dat), "rows\n")

# Remove high-missing columns: Source.Port.Range and IPRTS
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL

# Remove invalid rows: APTAIP == 99999 (Outliers)
dat <- dat[dat$Average.ping.to.attacking.IP.milliseconds != 99999, ]
cat("After filtering APTAIP:", nrow(dat), "rows\n")

# Remove invalid rows: ASIPA == -1 (Impossible value)
dat <- dat[dat$Attack.Source.IP.Address.Count != -1, ]
cat("After filtering ASIPA:", nrow(dat), "rows\n")

# --- Step ii: Merging Categories ---
# Merge Windows versions in Source.OS.Detected
cat("\nCollapsing factor levels...\n")
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)
cat("Source.OS.Detected levels:", paste(levels(dat$Source.OS.Detected), collapse=", "), "\n")

# Merge categories in Target.Honeypot.Server.OS
dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)
cat("Target.Honeypot.Server.OS levels:", paste(levels(dat$Target.Honeypot.Server.OS), collapse=", "), "\n")

# --- Step iii: Transformations ---
cat("\nApplying transformations...\n")
# Log-transform APV (Average.ping.variability)
dat$log_APV <- log(dat$Average.ping.variability + 1)
dat$Average.ping.variability <- NULL

# Square-root transformations for count/magnitude data
dat$sqrt_Hits <- sqrt(dat$Hits)
dat$Hits <- NULL

dat$sqrt_ASIPA <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$Attack.Source.IP.Address.Count <- NULL

dat$sqrt_APTAIP <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$Average.ping.to.attacking.IP.milliseconds <- NULL

dat$sqrt_IUR <- sqrt(dat$Individual.URLs.requested)
dat$Individual.URLs.requested <- NULL

# --- Step iv: Remove incomplete cases ---
dat_cleaned <- na.omit(dat)

# Ensure APT is a factor
dat_cleaned$APT <- factor(dat_cleaned$APT, levels = c("No", "Yes"))

cat("\nCleaned Data Summary:\n")
cat("- Dimensions:", dim(dat_cleaned), "\n")
cat("- APT distribution: No =", sum(dat_cleaned$APT == "No"), ", Yes =", sum(dat_cleaned$APT == "Yes"), "\n")

# c) Partition Data (30% Train / 70% Test)
set.seed(MY_SEED)
trainIndex <- sample(1:nrow(dat_cleaned), size = floor(0.30 * nrow(dat_cleaned)))
train_data <- dat_cleaned[trainIndex, ]
test_data  <- dat_cleaned[-trainIndex, ]

cat("\nTrain/Test Split:\n")
cat("- Training Set Size:", nrow(train_data), "\n")
cat("- Test Set Size:", nrow(test_data), "\n")

# ==============================================================================
# PART 2: MODEL TRAINING & EVALUATION
# ==============================================================================

cat("\n" , strrep("=", 70), "\n", sep="")
cat("MODEL SELECTION & TRAINING\n")
cat(strrep("=", 70), "\n", sep="")

# Determine 3 random models
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest")

set.seed(MY_SEED)
myModels <- c(sample(models.list1, size=1), sample(models.list2, size=2))
cat("\nYour selected models are:\n")
for(i in 1:length(myModels)) cat(i, ".", myModels[i], "\n", sep="")

# Convert APT to numeric for glmnet (0/1)
train_data_glmnet <- train_data
train_data_glmnet$APT <- as.numeric(train_data$APT) - 1

test_data_glmnet <- test_data
test_data_glmnet$APT <- as.numeric(test_data$APT) - 1

# Prepare model matrix
X_train <- model.matrix(APT ~ ., data = train_data_glmnet)[, -1]
y_train <- train_data_glmnet$APT

X_test <- model.matrix(APT ~ ., data = test_data_glmnet)[, -1]
y_test <- test_data_glmnet$APT

# Define confusion matrix function
calc_confusion_matrix <- function(pred_class, actual_class) {
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  actual_class <- factor(actual_class, levels = c("No", "Yes"))
  
  cm <- table(Predicted = pred_class, Actual = actual_class)
  
  TP <- cm["Yes", "Yes"]
  TN <- cm["No", "No"]
  FP <- cm["Yes", "No"]
  FN <- cm["No", "Yes"]
  
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  
  return(list(
    confusion_matrix = cm,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision
  ))
}

# ==============================================================================
# MODEL 1: LOGISTIC REGRESSION
# ==============================================================================
if (any(grepl("Logistic", myModels))) {
  logit_type <- myModels[grep("Logistic", myModels)]
  cat("\n", strrep("-", 70), "\n", sep="")
  cat("Training:", logit_type, "\n")
  cat(strrep("-", 70), "\n", sep="")
  
  set.seed(MY_SEED)
  
  if (logit_type == "Logistic Ridge Regression") {
    model_logit <- glmnet(X_train, y_train, family = "binomial", alpha = 0)
  } else if (logit_type == "Logistic LASSO Regression") {
    model_logit <- glmnet(X_train, y_train, family = "binomial", alpha = 1)
  } else {
    model_logit <- glmnet(X_train, y_train, family = "binomial", alpha = 0.5)
  }
  
  # Use cross-validated lambda
  set.seed(MY_SEED)
  cv_logit <- cv.glmnet(X_train, y_train, family = "binomial", 
                        alpha = ifelse(logit_type == "Logistic Ridge Regression", 0,
                               ifelse(logit_type == "Logistic LASSO Regression", 1, 0.5)))
  
  cat("✓ Best lambda:", cv_logit$lambda.min, "\n")
  
  # Predictions
  logit_pred_prob <- predict(model_logit, newx = X_test, s = cv_logit$lambda.min, type = "response")
  logit_pred_class <- ifelse(logit_pred_prob > 0.5, "Yes", "No")
  
  cm_logit <- calc_confusion_matrix(logit_pred_class, test_data$APT)
  
  cat("\nConfusion Matrix:\n")
  print(cm_logit$confusion_matrix)
  cat("\nMetrics:\n")
  cat("- Accuracy:   ", round(cm_logit$accuracy, 4), "\n")
  cat("- Sensitivity:", round(cm_logit$sensitivity, 4), "\n")
  cat("- Specificity:", round(cm_logit$specificity, 4), "\n")
  cat("- Precision:  ", round(cm_logit$precision, 4), "\n")
}

# ==============================================================================
# MODEL 2: CLASSIFICATION TREE
# ==============================================================================
if ("Classification Tree" %in% myModels) {
  cat("\n", strrep("-", 70), "\n", sep="")
  cat("Training: Classification Tree\n")
  cat(strrep("-", 70), "\n", sep="")
  
  set.seed(MY_SEED)
  model_tree <- rpart(APT ~ ., data = train_data, method = "class",
                      control = rpart.control(cp = 0.01, minsplit = 20))
  
  cat("✓ Tree trained with", length(unique(model_tree$where)), "nodes\n")
  
  # Predictions
  tree_pred <- predict(model_tree, newdata = test_data, type = "class")
  
  cm_tree <- calc_confusion_matrix(tree_pred, test_data$APT)
  
  cat("\nConfusion Matrix:\n")
  print(cm_tree$confusion_matrix)
  cat("\nMetrics:\n")
  cat("- Accuracy:   ", round(cm_tree$accuracy, 4), "\n")
  cat("- Sensitivity:", round(cm_tree$sensitivity, 4), "\n")
  cat("- Specificity:", round(cm_tree$specificity, 4), "\n")
  cat("- Precision:  ", round(cm_tree$precision, 4), "\n")
}

# ==============================================================================
# MODEL 3: BAGGING TREE
# ==============================================================================
if ("Bagging Tree" %in% myModels) {
  cat("\n", strrep("-", 70), "\n", sep="")
  cat("Training: Bagging Tree\n")
  cat(strrep("-", 70), "\n", sep="")
  
  set.seed(MY_SEED)
  model_bagging <- bagging(APT ~ ., data = train_data, nbagg = 50,
                          control = rpart.control(minsplit = 2, cp = 0))
  
  cat("✓ Bagging model trained with 50 bags\n")
  
  # Predictions
  bag_pred <- predict(model_bagging, newdata = test_data, type = "class")
  
  cm_bag <- calc_confusion_matrix(bag_pred, test_data$APT)
  
  cat("\nConfusion Matrix:\n")
  print(cm_bag$confusion_matrix)
  cat("\nMetrics:\n")
  cat("- Accuracy:   ", round(cm_bag$accuracy, 4), "\n")
  cat("- Sensitivity:", round(cm_bag$sensitivity, 4), "\n")
  cat("- Specificity:", round(cm_bag$specificity, 4), "\n")
  cat("- Precision:  ", round(cm_bag$precision, 4), "\n")
}

# ==============================================================================
# MODEL 4: RANDOM FOREST
# ==============================================================================
if ("Random Forest" %in% myModels) {
  cat("\n", strrep("-", 70), "\n", sep="")
  cat("Training: Random Forest\n")
  cat(strrep("-", 70), "\n", sep="")
  
  set.seed(MY_SEED)
  model_rf <- randomForest(APT ~ ., data = train_data, ntree = 200,
                          mtry = sqrt(ncol(train_data) - 1))
  
  cat("✓ Random Forest trained with 200 trees\n")
  cat("- Out-of-Bag Error Rate:", round(model_rf$err.rate[200, "OOB"], 4), "\n")
  
  # Predictions
  rf_pred <- predict(model_rf, newdata = test_data, type = "class")
  
  cm_rf <- calc_confusion_matrix(rf_pred, test_data$APT)
  
  cat("\nConfusion Matrix:\n")
  print(cm_rf$confusion_matrix)
  cat("\nMetrics:\n")
  cat("- Accuracy:   ", round(cm_rf$accuracy, 4), "\n")
  cat("- Sensitivity:", round(cm_rf$sensitivity, 4), "\n")
  cat("- Specificity:", round(cm_rf$specificity, 4), "\n")
  cat("- Precision:  ", round(cm_rf$precision, 4), "\n")
  
  # Feature importance
  cat("\nTop 10 Important Features:\n")
  importance <- importance(model_rf)
  top_features <- head(importance[order(-importance[, 1]), , drop = FALSE], 10)
  print(top_features)
}

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n", sep="")
