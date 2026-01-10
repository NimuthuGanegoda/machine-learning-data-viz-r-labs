# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 3 - Supervised Learning Modelling
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
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

# b) Cleaning & Feature Engineering

# --- Step i: General Cleaning ---
# Remove invalid "???" in Source.OS.Detected
dat <- dat[dat$Source.OS.Detected != "???", ]

# Remove high-missing columns: Source.Port.Range and IPRTS
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL

# Remove invalid rows: APTAIP == 99999 (Outliers)
dat <- dat[dat$Average.ping.to.attacking.IP.milliseconds != 99999, ]

# Remove invalid rows: ASIPA == -1 (Impossible value)
dat <- dat[dat$Attack.Source.IP.Address.Count != -1, ]

# --- Step ii: Merging Categories ---
# Merge Windows versions in Source.OS.Detected
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)

# Merge categories in Target.Honeypot.Server.OS
dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)

# --- Step iii: Transformations ---
# Log-transform APV (Average.ping.variability)
# Note: Adding +1 to avoid log(0) if any values are 0
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

cat("Dimensions after cleaning:", dim(dat_cleaned), "\n")
cat("APT distribution:", table(dat_cleaned$APT), "\n")

# c) Partition Data (30% Train / 70% Test)
set.seed(MY_SEED)
trainIndex <- sample(1:nrow(dat_cleaned), size = floor(0.30 * nrow(dat_cleaned)))
train_data <- dat_cleaned[trainIndex, ]
test_data  <- dat_cleaned[-trainIndex, ]

cat("Training Set Size:", nrow(train_data), "\n")
cat("Test Set Size:", nrow(test_data), "\n")

# ==============================================================================
# PART 2: MODEL SELECTION & TUNING
# ==============================================================================

# a) Determine Your 3 Models
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest")

set.seed(MY_SEED)
myModels <- c(sample(models.list1, size=1), sample(models.list2, size=2))
cat("\nYour selected models are:\n")
print(myModels)

# Prepare Training Control (5-Fold CV repeated 2 times)
fitControl <- list(method = "repeatedcv", number = 5, repeats = 2)

# Define a function to evaluate models on Test Set
evaluate_model <- function(model_obj, test_set, model_name) {
  preds <- predict(model_obj, newdata = test_set)
  
  # Calculate confusion matrix manually
  preds_factor <- factor(preds, levels = c("No", "Yes"))
  actual <- factor(test_set$APT, levels = c("No", "Yes"))
  cm <- table(preds_factor, actual)
  
  cat("\n=========================================\n")
  cat("RESULTS FOR:", model_name, "\n")
  cat("=========================================\n")
  print(cm)
  
  # Calculate metrics
  TP <- cm["Yes", "Yes"]
  TN <- cm["No", "No"]
  FP <- cm["Yes", "No"]
  FN <- cm["No", "Yes"]
  
  accuracy <- (TP + TN) / sum(cm)
  cat("Accuracy:", round(accuracy, 4), "\n")
  
  return(list(cm = cm, accuracy = accuracy))
}

# ------------------------------------------------------------------------------
# LOGISTIC REGRESSION BLOCK
# ------------------------------------------------------------------------------
if (any(grepl("Logistic", myModels))) {

  logit_type <- myModels[grep("Logistic", myModels)]
  cat("\nTraining", logit_type, "...\n")

  # Prepare data for glmnet
  X_train <- model.matrix(APT ~ ., data = train_data)[, -1]
  y_train <- as.numeric(train_data$APT) - 1
  
  X_test <- model.matrix(APT ~ ., data = test_data)[, -1]
  y_test <- as.numeric(test_data$APT) - 1

  # Determine alpha based on model type
  if (logit_type == "Logistic Ridge Regression") {
    alpha_val <- 0
  } else if (logit_type == "Logistic LASSO Regression") {
    alpha_val <- 1
  } else {
    alpha_val <- 0.5  # Elastic-Net
  }

  set.seed(MY_SEED)
  model_logit <- cv.glmnet(X_train, y_train, family = "binomial", alpha = alpha_val, nfolds = 5)

  cat("Best lambda:", model_logit$lambda.min, "\n")
  
  # Predictions
  logit_pred_prob <- predict(model_logit, newx = X_test, s = "lambda.min", type = "response")
  logit_pred_class <- ifelse(logit_pred_prob > 0.5, "Yes", "No")
  
  evaluate_model(model_logit, test_data, logit_type)
}

# ------------------------------------------------------------------------------
# CLASSIFICATION TREE
# ------------------------------------------------------------------------------
if ("Classification Tree" %in% myModels) {
  cat("\nTraining Classification Tree...\n")

  set.seed(MY_SEED)
  model_tree <- rpart(APT ~ ., data = train_data, method = "class",
                      control = rpart.control(cp = 0.01, minsplit = 20))

  cat("Tree trained successfully\n")
  
  # Predictions
  tree_pred <- predict(model_tree, newdata = test_data, type = "class")
  evaluate_model(model_tree, test_data, "Classification Tree")
}

# ------------------------------------------------------------------------------
# BAGGING TREE
# ------------------------------------------------------------------------------
if ("Bagging Tree" %in% myModels) {
  cat("\nTraining Bagging Tree (Manual Grid Search)...\n")

  bag_grid <- expand.grid(
    nbagg = c(25, 50, 100),
    cp = c(0.001, 0.01, 0.05),
    minsplit = c(2, 5, 10)
  )

  results_bag <- data.frame()

  for(i in 1:nrow(bag_grid)) {
    params <- bag_grid[i, ]
    set.seed(MY_SEED)
    mod <- bagging(APT ~ ., data = train_data,
                   nbagg = as.numeric(params$nbagg),
                   control = rpart.control(cp = as.numeric(params$cp),
                                          minsplit = as.numeric(params$minsplit)),
                   coob = TRUE)

    results_bag <- rbind(results_bag, cbind(params, Error = mod$err))
  }

  best_bag_params <- results_bag[which.min(results_bag$Error), ]
  cat("\nBest Bagging Parameters:\n")
  print(best_bag_params)

  set.seed(MY_SEED)
  final_bag <- bagging(APT ~ ., data = train_data,
                       nbagg = as.numeric(best_bag_params$nbagg),
                       control = rpart.control(cp = as.numeric(best_bag_params$cp),
                                               minsplit = as.numeric(best_bag_params$minsplit)))

  bag_preds <- predict(final_bag, newdata = test_data, type = "class")
  cm_bag <- confusionMatrix(bag_preds, test_data$APT)
  print(cm_bag)
}

# ------------------------------------------------------------------------------
# RANDOM FOREST
# ------------------------------------------------------------------------------
if ("Random Forest" %in% myModels) {
  cat("\nTraining Random Forest (Manual Grid Search)...\n")

  rf_grid <- expand.grid(
    ntree = c(100, 300, 500),
    mtry = c(2, 4, 6),
    nodesize = c(1, 5, 10)
  )

  results_rf <- data.frame()

  for(i in 1:nrow(rf_grid)) {
    params <- rf_grid[i, ]
    set.seed(MY_SEED)
    mod <- randomForest(APT ~ ., data = train_data,
                        ntree = as.numeric(params$ntree),
                        mtry = as.numeric(params$mtry),
                        nodesize = as.numeric(params$nodesize))

    oob_err <- mod$err.rate[as.numeric(params$ntree), "OOB"]
    results_rf <- rbind(results_rf, cbind(params, Error = oob_err))
  }

  best_rf_params <- results_rf[which.min(results_rf$Error), ]
  cat("\nBest Random Forest Parameters:\n")
  print(best_rf_params)

  set.seed(MY_SEED)
  final_rf <- randomForest(APT ~ ., data = train_data,
                           ntree = as.numeric(best_rf_params$ntree),
                           mtry = as.numeric(best_rf_params$mtry),
                           nodesize = as.numeric(best_rf_params$nodesize))

  rf_preds <- predict(final_rf, newdata = test_data)
  cm_rf <- confusionMatrix(rf_preds, test_data$APT)
  print(cm_rf)
}
