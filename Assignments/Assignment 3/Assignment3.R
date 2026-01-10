# ==============================================================================
# SUPERVISED LEARNING MODELLING - ASSIGNMENT 3
# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   3 - Supervised Learning Modelling
# ==============================================================================

# Load required libraries
library(glmnet)      # For logistic regression with LASSO regularization
library(rpart)       # For decision tree classification
library(randomForest) # For random forest classification
library(forcats)     # For categorical variable manipulation

# Set random seed for reproducibility using student ID
set.seed(10695889)
MY_SEED <- 10695889

# ============================================================================= #
# PART 1: DATA LOADING & PREPARATION
# ============================================================================= #

cat("================================================================================\n")
cat("LOADING AND PREPARING DATA\n")
cat("================================================================================\n\n")

# Load the cybersecurity attack dataset
# This dataset contains features of network attacks that may or may not be
# Advanced Persistent Threats (APT)
cat("Reading WACY-COM.csv...\n")
data <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)
cat("Initial dataset: ", nrow(data), "rows and", ncol(data), "columns\n\n")

# DATA CLEANING STEP 1: Remove rows with invalid "???" OS values
cat("Data Cleaning:\n")
cat("1. Removing invalid OS values (???)...\n")
data <- data[data$Source.OS.Detected != "???", ]

# DATA CLEANING STEP 2: Remove columns with too many missing values
# These columns don't have enough data to be useful
cat("2. Removing columns with excessive missing values...\n")
data$Source.Port.Range <- NULL
data$IP.Range.Trust.Score <- NULL

# DATA CLEANING STEP 3: Remove rows with impossible values
# 99999 milliseconds for ping is unrealistic, and -1 for IP count is impossible
cat("3. Removing outlier/impossible values...\n")
data <- data[data$Average.ping.to.attacking.IP.milliseconds != 99999, ]
data <- data[data$Attack.Source.IP.Address.Count != -1, ]
cat("Cleaned dataset: ", nrow(data), "rows and", ncol(data), "columns\n\n")

# FEATURE ENGINEERING STEP 1: Consolidate categorical variables
# Merging similar categories reduces model complexity
cat("Feature Engineering:\n")
cat("1. Consolidating categorical variables...\n")

# Merge similar Windows versions in Source Operating System
data$Source.OS.Detected <- fct_collapse(data$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)

# Merge similar Target Operating Systems
data$Target.Honeypot.Server.OS <- fct_collapse(data$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)

# FEATURE ENGINEERING STEP 2: Transform numerical variables
# These transformations reduce skewness and stabilize variance
cat("2. Applying mathematical transformations:\n")
cat("   - Log transformation: Average Ping Variability\n")
cat("   - Square root transformation: Hits, IP Address Count, Ping Time, URLs\n")

data$log_APV <- log(data$Average.ping.variability + 1)
data$Average.ping.variability <- NULL

data$sqrt_Hits <- sqrt(data$Hits)
data$Hits <- NULL

data$sqrt_ASIPA <- sqrt(data$Attack.Source.IP.Address.Count)
data$Attack.Source.IP.Address.Count <- NULL

data$sqrt_APTAIP <- sqrt(data$Average.ping.to.attacking.IP.milliseconds)
data$Average.ping.to.attacking.IP.milliseconds <- NULL

data$sqrt_IUR <- sqrt(data$Individual.URLs.requested)
data$Individual.URLs.requested <- NULL

# Remove rows with any remaining missing values
cat("3. Removing rows with missing values...\n")
data_final <- na.omit(data)

# Ensure target variable is properly formatted as a factor
data_final$APT <- factor(data_final$APT, levels = c("No", "Yes"))

cat("Final prepared dataset: ", nrow(data_final), "rows and", ncol(data_final), "columns\n\n")

# Show distribution of target variable
cat("Target variable (APT) distribution:\n")
apt_table <- table(data_final$APT)
print(apt_table)
cat("Proportion of APT: ", round(apt_table["Yes"]/sum(apt_table)*100, 2), "%\n\n")

# PARTITION DATA: 70% training, 30% testing
cat("Partitioning data: 70% training, 30% testing...\n")
set.seed(MY_SEED)
train_idx <- sample(1:nrow(data_final), size = floor(0.70 * nrow(data_final)))
train_df <- data_final[train_idx, ]
test_df <- data_final[-train_idx, ]

cat("Training set: ", nrow(train_df), "observations\n")
cat("Testing set: ", nrow(test_df), "observations\n\n")


# ============================================================================= #
# PART 2: MODEL BUILDING & EVALUATION
# ============================================================================= #

cat("================================================================================\n")
cat("BUILDING AND EVALUATING THREE MODELS\n")
cat("================================================================================\n\n")

# Prepare feature matrices for logistic regression
X_train <- model.matrix(APT ~ ., data = train_df)[, -1]
y_train <- as.numeric(train_df$APT) - 1
X_test <- model.matrix(APT ~ ., data = test_df)[, -1]

# ============================================================================= #
# MODEL 1: LOGISTIC REGRESSION WITH LASSO
# ============================================================================= #

cat("MODEL 1: LOGISTIC REGRESSION WITH LASSO\n")
cat("-----------------------------------------\n")
cat("Logistic regression predicts probability of APT attack (0 to 1).\n")
cat("LASSO regularization (L1 penalty) performs automatic feature selection.\n")
cat("This model is interpretable and shows which features matter most.\n\n")

set.seed(MY_SEED)
cat("Training model with 5-fold cross-validation...\n")
lasso_cv <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, nfolds = 5)

cat("Making predictions on test set...\n")
lasso_pred <- predict(lasso_cv, X_test, s = "lambda.min", type = "class")
lasso_pred <- factor(lasso_pred, levels = c("0", "1"), labels = c("No", "Yes"))

# Calculate accuracy and confusion matrix
lasso_correct <- sum(lasso_pred == test_df$APT)
lasso_acc <- lasso_correct / nrow(test_df)

cat("Results:\n")
cat("Accuracy: ", round(lasso_acc, 4), " (", round(lasso_acc*100, 2), "%)\n")
cat("Correct predictions: ", lasso_correct, " out of ", nrow(test_df), "\n\n")
cat("Confusion Matrix:\n")
print(table(Predicted = lasso_pred, Actual = test_df$APT))
cat("\n")

# ============================================================================= #
# MODEL 2: DECISION TREE
# ============================================================================= #

cat("MODEL 2: DECISION TREE (CART)\n")
cat("------------------------------\n")
cat("Decision trees create interpretable if-then rules for classification.\n")
cat("They capture non-linear relationships and are easy to explain to stakeholders.\n\n")

set.seed(MY_SEED)
cat("Training decision tree...\n")
tree_model <- rpart(APT ~ ., data = train_df, method = "class",
                    control = rpart.control(cp = 0.01, minsplit = 20))

cat("Making predictions on test set...\n")
tree_pred <- predict(tree_model, test_df, type = "class")

# Calculate accuracy and confusion matrix
tree_correct <- sum(tree_pred == test_df$APT)
tree_acc <- tree_correct / nrow(test_df)

cat("Results:\n")
cat("Accuracy: ", round(tree_acc, 4), " (", round(tree_acc*100, 2), "%)\n")
cat("Correct predictions: ", tree_correct, " out of ", nrow(test_df), "\n\n")
cat("Confusion Matrix:\n")
print(table(Predicted = tree_pred, Actual = test_df$APT))
cat("\n")

# ============================================================================= #
# MODEL 3: RANDOM FOREST
# ============================================================================= #

cat("MODEL 3: RANDOM FOREST\n")
cat("----------------------\n")
cat("Random Forest is an ensemble method that builds multiple trees.\n")
cat("It reduces overfitting and typically provides the best accuracy.\n")
cat("Feature importance scores show which attributes matter most.\n\n")

set.seed(MY_SEED)
cat("Training random forest with 100 trees...\n")
rf_model <- randomForest(APT ~ ., data = train_df, ntree = 100, mtry = 4,
                         nodesize = 5, importance = TRUE)

cat("Making predictions on test set...\n")
rf_pred <- predict(rf_model, test_df, type = "class")

# Calculate accuracy and confusion matrix
rf_correct <- sum(rf_pred == test_df$APT)
rf_acc <- rf_correct / nrow(test_df)

cat("Results:\n")
cat("Accuracy: ", round(rf_acc, 4), " (", round(rf_acc*100, 2), "%)\n")
cat("Correct predictions: ", rf_correct, " out of ", nrow(test_df), "\n\n")
cat("Confusion Matrix:\n")
print(table(Predicted = rf_pred, Actual = test_df$APT))

cat("\nTop 10 Most Important Features:\n")
imp_scores <- importance(rf_model)
imp_sorted <- imp_scores[order(-imp_scores[, 1]), ]
print(head(imp_sorted, 10))
cat("\n")


# ============================================================================= #
# PART 3: MODEL COMPARISON & CONCLUSION
# ============================================================================= #

cat("================================================================================\n")
cat("MODEL COMPARISON & CONCLUSIONS\n")
cat("================================================================================\n\n")

# Create comparison table
comparison <- data.frame(
    Model = c("Logistic Regression (LASSO)", "Decision Tree", "Random Forest"),
    Accuracy = c(round(lasso_acc, 4), round(tree_acc, 4), round(rf_acc, 4)),
    Type = c("Linear with Regularization", "Tree-based", "Ensemble Tree-based"),
    Interpretability = c("Good", "Excellent", "Good")
)

cat("Summary of Model Performance:\n")
print(comparison)
cat("\n")

# Identify best model
best_idx <- which.max(comparison$Accuracy)
best_model_name <- comparison$Model[best_idx]
best_accuracy <- comparison$Accuracy[best_idx]

cat("Best Model: ", best_model_name, "\n")
cat("Best Accuracy: ", round(best_accuracy*100, 2), "%\n\n")

# Analysis and recommendations
cat("Key Findings & Recommendations:\n")
cat("=============================\n\n")

cat("1. LOGISTIC REGRESSION (LASSO):\n")
cat("   Strengths: Provides probability estimates, interpretable, automatic feature selection\n")
cat("   Weaknesses: Assumes linear decision boundary, may miss complex patterns\n\n")

cat("2. DECISION TREE:\n")
cat("   Strengths: Highly interpretable rules, captures non-linear relationships\n")
cat("   Weaknesses: Prone to overfitting, unstable to small data changes\n\n")

cat("3. RANDOM FOREST:\n")
cat("   Strengths: High accuracy, robust to overfitting, feature importance\n")
cat("   Weaknesses: Less interpretable than single tree, more computationally expensive\n\n")

cat("RECOMMENDATION:\n")
cat("For cybersecurity threat detection, ", best_model_name, " is recommended because:\n")
cat("- Higher accuracy reduces missed threats\n")
cat("- Robustness prevents adversarial attacks\n")
cat("- Feature importance reveals attack patterns\n")
cat("- Ensemble approach generalizes well to new data\n\n")

cat("================================================================================\n")
cat("END OF ANALYSIS\n")
cat("================================================================================\n")
