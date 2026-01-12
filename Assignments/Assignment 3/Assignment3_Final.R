# ==============================================================================
# ASSIGNMENT 3: SUPERVISED LEARNING MODELLING
# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# ==============================================================================

# Load required libraries (install if missing)
required_pkgs <- c("Rcpp", "RcppEigen", "glmnet", "rpart", "randomForest")

# Clean up any leftover install locks (common on interrupted installs)
lock_paths <- list.files(.libPaths()[1], pattern = "^00LOCK", full.names = TRUE)
if (length(lock_paths)) {
    unlink(lock_paths, recursive = TRUE, force = TRUE)
}

for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, repos = "https://cloud.r-project.org")
    }
}

library(glmnet)
library(rpart)
library(randomForest)

# Set seed for reproducibility
set.seed(10695889)

# ==============================================================================
# PART 1: DATA LOADING & CLEANING
# ==============================================================================

# Load dataset
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

# Remove invalid values
dat <- dat[dat$Source.OS.Detected != "???", ]

# Remove high-missing columns
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL

# Remove outliers
dat <- dat[dat$Average.ping.to.attacking.IP.milliseconds != 99999, ]
dat <- dat[dat$Attack.Source.IP.Address.Count != -1, ]

# Merge factor categories
dat$Source.OS.Detected <- factor(dat$Source.OS.Detected, 
    levels = c("Windows 10", "Windows Server 2008", "Linux (>4.0)", "Linux (2.6.3)", "MacOS (Unknown)"),
    labels = c("Windows", "Windows", "Linux", "Linux", "MacOS"))

dat$Target.Honeypot.Server.OS <- factor(dat$Target.Honeypot.Server.OS,
    levels = c("Windows (Desktops)", "Windows (Servers)", "Linux", "MacOS (All)"),
    labels = c("Windows", "Windows", "Unix", "Unix"))

# Feature transformations
dat$log_APV <- log(dat$Average.ping.variability + 1)
dat$sqrt_Hits <- sqrt(dat$Hits)
dat$sqrt_ASIPA <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$sqrt_APTAIP <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$sqrt_IUR <- sqrt(dat$Individual.URLs.requested)

# Remove original columns
dat$Average.ping.variability <- NULL
dat$Hits <- NULL
dat$Attack.Source.IP.Address.Count <- NULL
dat$Average.ping.to.attacking.IP.milliseconds <- NULL
dat$Individual.URLs.requested <- NULL

# Remove missing values
dat <- na.omit(dat)

# Ensure APT is a factor
dat$APT <- factor(dat$APT, levels = c("No", "Yes"))

cat("Data cleaning complete:", nrow(dat), "rows\n")

# ==============================================================================
# PART 2: DATA PARTITIONING
# ==============================================================================

# Split into 30% training, 70% testing
set.seed(10695889)
n <- nrow(dat)
train_idx <- sample(1:n, size = floor(0.3 * n))
train_data <- dat[train_idx, ]
test_data <- dat[-train_idx, ]

cat("Training set:", nrow(train_data), "rows\n")
cat("Testing set:", nrow(test_data), "rows\n")

# ==============================================================================
# PART 3: MODEL TRAINING
# ==============================================================================

# Prepare data matrices for glmnet
x_train <- model.matrix(APT ~ . - 1, data = train_data)
y_train <- train_data$APT
x_test <- model.matrix(APT ~ . - 1, data = test_data)
y_test <- test_data$APT

# ------------------------------------------------------------------------------
# MODEL 1: LOGISTIC LASSO REGRESSION
# ------------------------------------------------------------------------------
cat("\nTraining Logistic LASSO Regression...\n")

# Cross-validation to find best lambda
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 5)
best_lambda <- cv_lasso$lambda.min

# Train final model
model_lasso <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# Predictions
pred_lasso <- predict(model_lasso, x_test, type = "class")
acc_lasso <- mean(pred_lasso == y_test)

cat("LASSO Accuracy:", round(acc_lasso * 100, 2), "%\n")

# Confusion matrix
cm_lasso <- table(Predicted = pred_lasso, Actual = y_test)
print(cm_lasso)

# ------------------------------------------------------------------------------
# MODEL 2: CLASSIFICATION TREE
# ------------------------------------------------------------------------------
cat("\nTraining Classification Tree...\n")

# Train decision tree
model_tree <- rpart(APT ~ ., data = train_data, method = "class", 
                    control = rpart.control(cp = 0.001))

# Predictions
pred_tree <- predict(model_tree, test_data, type = "class")
acc_tree <- mean(pred_tree == test_data$APT)

cat("Tree Accuracy:", round(acc_tree * 100, 2), "%\n")

# Confusion matrix
cm_tree <- table(Predicted = pred_tree, Actual = test_data$APT)
print(cm_tree)

# ------------------------------------------------------------------------------
# MODEL 3: RANDOM FOREST
# ------------------------------------------------------------------------------
cat("\nTraining Random Forest...\n")

# Train random forest
model_rf <- randomForest(APT ~ ., data = train_data, ntree = 100, mtry = 3)

# Predictions
pred_rf <- predict(model_rf, test_data)
acc_rf <- mean(pred_rf == test_data$APT)

cat("Random Forest Accuracy:", round(acc_rf * 100, 2), "%\n")

# Confusion matrix
cm_rf <- table(Predicted = pred_rf, Actual = test_data$APT)
print(cm_rf)

# ==============================================================================
# PART 4: MODEL COMPARISON
# ==============================================================================

cat("\n==============================================================================\n")
cat("FINAL MODEL COMPARISON\n")
cat("==============================================================================\n")
cat("Logistic LASSO:     ", round(acc_lasso * 100, 2), "%\n")
cat("Classification Tree:", round(acc_tree * 100, 2), "%\n")
cat("Random Forest:      ", round(acc_rf * 100, 2), "%\n")
cat("==============================================================================\n")

# Identify best model
accuracies <- c(acc_lasso, acc_tree, acc_rf)
model_names <- c("Logistic LASSO", "Classification Tree", "Random Forest")
best_model <- model_names[which.max(accuracies)]

cat("\nBest Model:", best_model, "\n")
