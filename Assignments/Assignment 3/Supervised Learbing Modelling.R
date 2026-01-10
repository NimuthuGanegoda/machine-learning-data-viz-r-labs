# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   Assignment 3 - Supervised Learning Modelling
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. LIBRARIES & SETUP
# ------------------------------------------------------------------------------
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

# Set Student ID as seed for reproducibility
MY_SEED <- 10695889

# ==============================================================================
# PART 1: DATA PREPARATION
# ==============================================================================

# a) Import Data
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

# b) Data Cleaning & Feature Engineering

# (i) General cleaning based on Assignment 1 feedback
# Remove invalid OS entries
dat <- dat %>% filter(Source.OS.Detected != "???")

# Remove features with excessive missing values (>40%)
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL

# Remove invalid outliers (APTAIP = 99999) and impossible values (ASIPA = -1)
dat <- dat %>% filter(Average.ping.to.attacking.IP.milliseconds != 99999)
dat <- dat %>% filter(Attack.Source.IP.Address.Count != -1)

# (ii) Merging Categorical Levels to reduce sparsity
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008")
)

dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_DeskServ = c("Windows (Desktops)", "Windows (Servers)"),
    MacOS_Linux = c("Linux", "MacOS (All)")
)

# (iii) Transformations
# Log-transform highly skewed Ping Variability
dat$log_APV <- log(dat$Average.ping.variability + 1)
dat$Average.ping.variability <- NULL

# Sqrt-transform count/magnitude features
dat$sqrt_Hits <- sqrt(dat$Hits)
dat$Hits <- NULL

dat$sqrt_ASIPA <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$Attack.Source.IP.Address.Count <- NULL

dat$sqrt_APTAIP <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$Average.ping.to.attacking.IP.milliseconds <- NULL

dat$sqrt_IUR <- sqrt(dat$Individual.URLs.requested)
dat$Individual.URLs.requested <- NULL

# (iv) Remove incomplete cases
dat_cleaned <- na.omit(dat)

# c) Partition Data (30% Training / 70% Test)
set.seed(MY_SEED)
trainIndex <- createDataPartition(dat_cleaned$APT, p = 0.30, list = FALSE)
train_data <- dat_cleaned[trainIndex, ]
test_data  <- dat_cleaned[-trainIndex, ]

# Export for verification if required
write.csv(train_data, "WACY-COM_train.csv", row.names = FALSE)
write.csv(test_data, "WACY-COM_test.csv", row.names = FALSE)

# ==============================================================================
# PART 2: MODELLING
# ==============================================================================

# a) Select Models using Student ID
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest")

set.seed(MY_SEED)
myModels <- c(sample(models.list1, size=1), sample(models.list2, size=2))
cat("\nSELECTED MODELS FOR ID 10695889:\n")
print(myModels)

# Training Controls
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

# Helper function for evaluation
print_results <- function(model, test_set, name) {
  preds <- predict(model, newdata = test_set, type = "raw") # Ensure raw class prediction
  # Handle different predict outputs (some packages return class, some probs)
  if(is.list(preds)) preds <- preds$class
  if(is.numeric(preds)) preds <- ifelse(preds > 0.5, "Yes", "No") # fallback for glmnet specifics

  # Ensure factors levels match
  preds <- factor(preds, levels = levels(test_set$APT))

  cm <- confusionMatrix(preds, test_set$APT)
  cat("\n--- RESULTS:", name, "---\n")
  print(cm)
}

# --- MODEL 1: PENALISED LOGISTIC REGRESSION ---
if (any(grepl("Logistic", myModels))) {
  logit_type <- myModels[grep("Logistic", myModels)]
  cat("\nRunning:", logit_type, "...\n")

  # Setup grid based on type
  lambda_seq <- 10^seq(-4, 1, length = 20)
  if (logit_type == "Logistic Ridge Regression") alpha_val <- 0
  if (logit_type == "Logistic LASSO Regression") alpha_val <- 1
  if (grepl("Elastic", logit_type)) alpha_val <- seq(0.1, 0.9, length = 5)

  tune_grid <- expand.grid(alpha = alpha_val, lambda = lambda_seq)

  set.seed(MY_SEED)
  mod_logit <- train(APT ~ ., data = train_data, method = "glmnet",
                     trControl = fitControl, tuneGrid = tune_grid, family = "binomial")

  print(mod_logit)
  plot(mod_logit)

  # For caret/glmnet, predict returns class directly if trained as factor
  preds_log <- predict(mod_logit, newdata = test_data)
  confusionMatrix(preds_log, test_data$APT)
}

# --- MODEL 2/3: TREE MODELS ---

# 1. Classification Tree
if ("Classification Tree" %in% myModels) {
  cat("\nRunning: Classification Tree...\n")
  set.seed(MY_SEED)
  mod_tree <- train(APT ~ ., data = train_data, method = "rpart",
                    trControl = fitControl, tuneGrid = expand.grid(cp = seq(0.001, 0.1, 0.005)))

  print(mod_tree)
  plot(mod_tree)
  confusionMatrix(predict(mod_tree, test_data), test_data$APT)
}

# 2. Bagging (Custom Loop for Tuning Requirement)
if ("Bagging Tree" %in% myModels) {
  cat("\nRunning: Bagging Tree...\n")
  bag_grid <- expand.grid(nbagg = c(25, 50, 100), cp = c(0.001, 0.01, 0.05), minsplit = c(2, 5, 10))

  res_bag <- data.frame()
  for(i in 1:nrow(bag_grid)) {
    set.seed(MY_SEED)
    m <- bagging(APT ~ ., data = train_data, nbagg = bag_grid$nbagg[i],
                 control = rpart.control(cp = bag_grid$cp[i], minsplit = bag_grid$minsplit[i]), coob=TRUE)
    res_bag <- rbind(res_bag, cbind(bag_grid[i,], Error=m$err))
  }

  best_bag <- res_bag[which.min(res_bag$Error),]
  print(best_bag)

  # Final Bagging Model
  final_bag <- bagging(APT ~ ., data = train_data, nbagg = best_bag$nbagg,
                       control = rpart.control(cp = best_bag$cp, minsplit = best_bag$minsplit))

  preds_bag <- predict(final_bag, newdata = test_data, type = "class")
  print(confusionMatrix(as.factor(preds_bag), test_data$APT))
}

# 3. Random Forest (Custom Loop for Tuning Requirement)
if ("Random Forest" %in% myModels) {
  cat("\nRunning: Random Forest...\n")
  rf_grid <- expand.grid(ntree = c(100, 300, 500), mtry = c(2, 4, 6), nodesize = c(1, 5, 10))

  res_rf <- data.frame()
  for(i in 1:nrow(rf_grid)) {
    set.seed(MY_SEED)
    m <- randomForest(APT ~ ., data = train_data, ntree = rf_grid$ntree[i],
                      mtry = rf_grid$mtry[i], nodesize = rf_grid$nodesize[i])
    res_rf <- rbind(res_rf, cbind(rf_grid[i,], Error=m$err.rate[rf_grid$ntree[i], "OOB"]))
  }

  best_rf <- res_rf[which.min(res_rf$Error),]
  print(best_rf)

  # Final RF Model
  final_rf <- randomForest(APT ~ ., data = train_data, ntree = best_rf$ntree,
                           mtry = best_rf$mtry, nodesize = best_rf$nodesize)

  preds_rf <- predict(final_rf, newdata = test_data)
  print(confusionMatrix(preds_rf, test_data$APT))
}
