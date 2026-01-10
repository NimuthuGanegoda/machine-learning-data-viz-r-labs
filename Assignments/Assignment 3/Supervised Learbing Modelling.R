# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 3 - Supervised Learning Modelling
# ==============================================================================

# 1. SETUP & LIBRARIES
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(glmnet)) install.packages("glmnet")
if(!require(rpart)) install.packages("rpart")
if(!require(ipred)) install.packages("ipred")
if(!require(randomForest)) install.packages("randomForest")
if(!require(forcats)) install.packages("forcats")

library(tidyverse)
library(caret)
library(glmnet)
library(rpart)
library(ipred)
library(randomForest)

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
dat <- dat %>% filter(Source.OS.Detected != "???")

# Remove high-missing columns: Source.Port.Range and IPRTS
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL

# Remove invalid rows: APTAIP == 99999 (Outliers)
dat <- dat %>% filter(Average.ping.to.attacking.IP.milliseconds != 99999)

# Remove invalid rows: ASIPA == -1 (Impossible value)
dat <- dat %>% filter(Attack.Source.IP.Address.Count != -1)

# --- Step ii: Merging Categories ---
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008")
)

dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_DeskServ = c("Windows (Desktops)", "Windows (Servers)"),
    MacOS_Linux = c("Linux", "MacOS (All)")
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
cat("Dimensions after cleaning:", dim(dat_cleaned), "\n")

# c) Partition Data (30% Train / 70% Test)
set.seed(MY_SEED)
trainIndex <- createDataPartition(dat_cleaned$APT, p = 0.30, list = FALSE)
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
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

# Define a function to evaluate models on Test Set
evaluate_model <- function(model_obj, test_set, model_name) {
  preds <- predict(model_obj, newdata = test_set)
  cm <- confusionMatrix(preds, test_set$APT)
  cat("\n=========================================\n")
  cat("RESULTS FOR:", model_name, "\n")
  cat("=========================================\n")
  print(cm)
  return(cm)
}

# ------------------------------------------------------------------------------
# LOGISTIC REGRESSION BLOCK
# ------------------------------------------------------------------------------
if (any(grepl("Logistic", myModels))) {

  logit_type <- myModels[grep("Logistic", myModels)]
  cat("\nTraining", logit_type, "...\n")

  lambda_grid <- 10^seq(-4, 1, length = 20)

  if (logit_type == "Logistic Ridge Regression") {
    tune_grid <- expand.grid(alpha = 0, lambda = lambda_grid)
  } else if (logit_type == "Logistic LASSO Regression") {
    tune_grid <- expand.grid(alpha = 1, lambda = lambda_grid)
  } else {
    tune_grid <- expand.grid(alpha = seq(0.1, 0.9, length = 5), lambda = lambda_grid)
  }

  set.seed(MY_SEED)
  model_logit <- train(APT ~ ., data = train_data,
                       method = "glmnet",
                       trControl = fitControl,
                       tuneGrid = tune_grid,
                       family = "binomial")

  print(model_logit)
  plot(model_logit)
  evaluate_model(model_logit, test_data, logit_type)
}

# ------------------------------------------------------------------------------
# CLASSIFICATION TREE
# ------------------------------------------------------------------------------
if ("Classification Tree" %in% myModels) {
  cat("\nTraining Classification Tree...\n")

  dt_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))

  set.seed(MY_SEED)
  model_tree <- train(APT ~ ., data = train_data,
                      method = "rpart",
                      trControl = fitControl,
                      tuneGrid = dt_grid)

  print(model_tree)
  plot(model_tree)
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
                   nbagg = params$nbagg,
                   control = rpart.control(cp = params$cp, minsplit = params$minsplit),
                   coob = TRUE)

    results_bag <- rbind(results_bag, cbind(params, Error = mod$err))
  }

  best_bag_params <- results_bag[which.min(results_bag$Error), ]
  print(best_bag_params)

  set.seed(MY_SEED)
  final_bag <- bagging(APT ~ ., data = train_data,
                       nbagg = best_bag_params$nbagg,
                       control = rpart.control(cp = best_bag_params$cp,
                                               minsplit = best_bag_params$minsplit))

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
                        ntree = params$ntree,
                        mtry = params$mtry,
                        nodesize = params$nodesize)

    oob_err <- mod$err.rate[params$ntree, "OOB"]
    results_rf <- rbind(results_rf, cbind(params, Error = oob_err))
  }

  best_rf_params <- results_rf[which.min(results_rf$Error), ]
  print(best_rf_params)

  set.seed(MY_SEED)
  final_rf <- randomForest(APT ~ ., data = train_data,
                           ntree = best_rf_params$ntree,
                           mtry = best_rf_params$mtry,
                           nodesize = best_rf_params$nodesize)

  rf_preds <- predict(final_rf, newdata = test_data)
  cm_rf <- confusionMatrix(rf_preds, test_data$APT)
  print(cm_rf)
}
