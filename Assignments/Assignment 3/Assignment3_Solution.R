# ==============================================================================
# ASSIGNMENT 3: SUPERVISED LEARNING MODELLING
# ==============================================================================
# Student Name: Nimuthu Ganegoda
# Student ID:   10695889
# Unit:         Machine Learning and Data Visualisation
# Assignment:   3 - Supervised Learning Modelling
# Date:         January 2026
# ==============================================================================

# ASSIGNMENT OBJECTIVE:
# Build and compare three different supervised learning models to predict
# whether a cybersecurity attack is an Advanced Persistent Threat (APT).
# The analysis includes data cleaning, feature engineering, model training,
# and performance evaluation.

# KEY STEPS:
# 1. Load and explore the dataset
# 2. Clean and preprocess the data
# 3. Engineer features for better model performance
# 4. Split data into training and testing sets
# 5. Build three different models
# 6. Evaluate and compare model performance


# ==============================================================================
# STEP 1: LIBRARY SETUP & SEED
# ==============================================================================

# The following libraries are required for this assignment:
# glmnet: For logistic regression with LASSO regularization
# rpart: For decision tree (CART) classification
# randomForest: For Random Forest ensemble classification
# forcats: For manipulating categorical factor variables

cat("Loading required libraries...\n\n")
library(glmnet)      # Logistic regression with L1/L2 regularization
library(rpart)       # Classification and Regression Trees (CART)
library(randomForest) # Random Forest classifier
library(forcats)     # Factor manipulation utilities

# Set random seed for reproducibility
# Using the student ID ensures consistent random splits and model training
set.seed(10695889)
MY_SEED <- 10695889


# ==============================================================================
# STEP 2: DATA LOADING & EXPLORATION
# ==============================================================================

cat("==============================================================================\n")
cat("STEP 2: DATA LOADING\n")
cat("==============================================================================\n\n")

# Load the cybersecurity dataset
# Dataset contains 200,000 network attacks with various features
# Target variable: APT (Advanced Persistent Threat attack - Yes/No)

cat("Loading WACY-COM.csv dataset...\n")
dataset <- read.csv("WACY-COM.csv", 
                    na.strings = c("NA", ""),    # Treat "NA" and blank as missing
                    stringsAsFactors = TRUE)      # Convert strings to factors

cat("Dataset loaded successfully!\n")
cat("Dataset dimensions:", nrow(dataset), "observations and", ncol(dataset), "variables\n\n")

# Display first few observations
cat("First 3 rows of data:\n")
print(head(dataset, 3))


# ==============================================================================
# STEP 3: DATA CLEANING
# ==============================================================================

cat("\n==============================================================================\n")
cat("STEP 3: DATA CLEANING\n")
cat("==============================================================================\n\n")

# Step 3.1: Remove invalid data values
# Some entries have "???" which indicates unknown/invalid operating system
cat("Removing invalid operating system values (???)...\n")
initial_n <- nrow(dataset)
dataset <- dataset[dataset$Source.OS.Detected != "???", ]
removed <- initial_n - nrow(dataset)
cat("Removed", removed, "rows. Remaining rows:", nrow(dataset), "\n\n")

# Step 3.2: Remove columns with excessive missing values
cat("Removing columns with insufficient data...\n")
dataset$Source.Port.Range <- NULL        # Column has too many missing values
dataset$IP.Range.Trust.Score <- NULL     # Column has too many missing values
cat("Removed 2 columns with excessive missing data\n")
cat("Remaining columns:", ncol(dataset), "\n\n")

# Step 3.3: Remove rows with impossible/unrealistic values
cat("Removing unrealistic outlier values...\n")
initial_n <- nrow(dataset)
dataset <- dataset[dataset$Average.ping.to.attacking.IP.milliseconds != 99999, ]
dataset <- dataset[dataset$Attack.Source.IP.Address.Count != -1, ]
removed <- initial_n - nrow(dataset)
cat("Removed", removed, "rows with impossible values\n")
cat("Rows remaining:", nrow(dataset), "\n\n")


# ==============================================================================
# STEP 4: FEATURE ENGINEERING
# ==============================================================================

cat("==============================================================================\n")
cat("STEP 4: FEATURE ENGINEERING\n")
cat("==============================================================================\n\n")

# Feature engineering creates new variables and transforms existing ones

cat("Consolidating categorical variables...\n\n")

# SOURCE OPERATING SYSTEM: Merge different Windows versions
cat("Merging Source OS categories:\n")
cat("  - Windows 10 + Windows Server 2008 -> Windows_All\n")
cat("  - Linux (>4.0) + Linux (2.6.3) -> Linux_All\n")
dataset$Source.OS.Detected <- fct_collapse(dataset$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)
cat("Source.OS categories reduced to:", nlevels(dataset$Source.OS.Detected), "levels\n\n")

# TARGET HONEYPOT SERVER OS: Merge similar operating systems
cat("Merging Target OS categories:\n")
cat("  - Windows (Desktops) + Windows (Servers) -> Windows_All\n")
cat("  - Linux + MacOS (All) -> Unix_Like\n")
dataset$Target.Honeypot.Server.OS <- fct_collapse(dataset$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)
cat("Target OS categories reduced to:", nlevels(dataset$Target.Honeypot.Server.OS), "levels\n\n")

# Mathematical transformations on numerical variables
cat("Applying mathematical transformations to numerical features:\n\n")

cat("1. Log-transformation of Average Ping Variability\n")
dataset$log_APV <- log(dataset$Average.ping.variability + 1)
dataset$Average.ping.variability <- NULL

cat("2. Square-root transformation of Hits\n")
dataset$sqrt_Hits <- sqrt(dataset$Hits)
dataset$Hits <- NULL

cat("3. Square-root transformation of Attack Source IP Count\n")
dataset$sqrt_ASIPA <- sqrt(dataset$Attack.Source.IP.Address.Count)
dataset$Attack.Source.IP.Address.Count <- NULL

cat("4. Square-root transformation of Average Ping Time\n")
dataset$sqrt_APTAIP <- sqrt(dataset$Average.ping.to.attacking.IP.milliseconds)
dataset$Average.ping.to.attacking.IP.milliseconds <- NULL

cat("5. Square-root transformation of URLs Requested\n")
dataset$sqrt_IUR <- sqrt(dataset$Individual.URLs.requested)
dataset$Individual.URLs.requested <- NULL

cat("\nAll transformations complete. Dataset now has", ncol(dataset), "columns\n\n")


# ==============================================================================
# SECTION 3: DATA CLEANING & PREPROCESSING
# ==============================================================================

cat("\n--- Starting Data Cleaning ---\n")

# Step 1: Remove rows with invalid "???" values in operating system detection
cat("Removing invalid OS values (???)...\n")
data <- data[data$Source.OS.Detected != "???", ]
cat("Rows remaining:", nrow(data), "\n")

# Step 2: Remove columns with too many missing values
# These columns have insufficient information for modeling
cat("Removing columns with excessive missing values...\n")
data$Source.Port.Range <- NULL        # Column: Source.Port.Range
data$IP.Range.Trust.Score <- NULL    # Column: IP.Range.Trust.Score
cat("Columns removed. Now have:", ncol(data), "columns\n")

# Step 3: Remove rows with impossible/outlier values
# Average ping of 99999ms is unrealistic
cat("Removing outlier values...\n")
initial_rows <- nrow(data)
data <- data[data$Average.ping.to.attacking.IP.milliseconds != 99999, ]
data <- data[data$Attack.Source.IP.Address.Count != -1, ]
cat("Outliers removed:", initial_rows - nrow(data), "rows\n")
cat("Rows remaining:", nrow(data), "\n")


# ==============================================================================
# SECTION 4: FEATURE ENGINEERING
# ==============================================================================

cat("\n--- Feature Engineering ---\n")

# Step 1: Consolidate categorical variables to reduce categories
# This helps simplify the model and improves training

cat("Consolidating categorical variables...\n")

# Merge different Windows versions into a single "Windows_All" category
# Also merge Linux versions into "Linux_All" category
data$Source.OS.Detected <- fct_collapse(data$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008"),
    Linux_All = c("Linux (>4.0)", "Linux (2.6.3)")
)

# Merge desktop and server Windows into "Windows_All"
# Merge Linux and MacOS into "Unix_Like"
data$Target.Honeypot.Server.OS <- fct_collapse(data$Target.Honeypot.Server.OS,
    Windows_All = c("Windows (Desktops)", "Windows (Servers)"),
    Unix_Like = c("Linux", "MacOS (All)")
)

cat("Factor levels consolidated\n")

# Step 2: Apply transformations to numerical variables
# Log transformation: reduces skewness in count data
# Square root transformation: stabilizes variance

cat("Applying mathematical transformations to numerical features...\n")

# Log-transform Average Ping Variability (adds 1 to avoid log(0))
data$log_APV <- log(data$Average.ping.variability + 1)
data$Average.ping.variability <- NULL

# Square root transformation for Hits (count data)
data$sqrt_Hits <- sqrt(data$Hits)
data$Hits <- NULL

# Square root transformation for Attack.Source.IP.Address.Count
data$sqrt_ASIPA <- sqrt(data$Attack.Source.IP.Address.Count)
data$Attack.Source.IP.Address.Count <- NULL

# Square root transformation for Average.ping.to.attacking.IP.milliseconds
data$sqrt_APTAIP <- sqrt(data$Average.ping.to.attacking.IP.milliseconds)
data$Average.ping.to.attacking.IP.milliseconds <- NULL

# Square root transformation for Individual.URLs.requested
data$sqrt_IUR <- sqrt(data$Individual.URLs.requested)
data$Individual.URLs.requested <- NULL

cat("Transformations complete. Features transformed:", 5, "\n")


# ==============================================================================
# SECTION 5: HANDLING MISSING VALUES
# ==============================================================================

cat("\n--- Removing Incomplete Cases ---\n")

# Remove any rows with missing values
initial_rows <- nrow(data)
data_clean <- na.omit(data)
removed_rows <- initial_rows - nrow(data_clean)

cat("Removed rows with missing values:", removed_rows, "\n")
cat("Clean dataset size:", nrow(data_clean), "rows,", ncol(data_clean), "columns\n")

# Ensure the target variable (APT) is properly formatted as a factor
# with explicit levels for classification
data_clean$APT <- factor(data_clean$APT, levels = c("No", "Yes"))

# Display distribution of target variable
cat("\nTarget Variable (APT) Distribution:\n")
print(table(data_clean$APT))
cat("No (Not APT):", sum(data_clean$APT == "No"), "\n")
cat("Yes (Is APT):", sum(data_clean$APT == "Yes"), "\n")


# ==============================================================================
# SECTION 6: DATA PARTITIONING
# ==============================================================================

cat("\n--- Partitioning Data into Train/Test Sets ---\n")

# Split data: 70% for training, 30% for testing
# Training data is used to build the models
# Test data is used to evaluate model performance

set.seed(MY_SEED)
n <- nrow(data_clean)
train_indices <- sample(1:n, size = floor(0.70 * n), replace = FALSE)

train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]

cat("Training set size:", nrow(train_data), "rows\n")
cat("Testing set size:", nrow(test_data), "rows\n")
cat("Training proportion:", round(nrow(train_data)/n * 100, 1), "%\n")
cat("Testing proportion:", round(nrow(test_data)/n * 100, 1), "%\n")


# ==============================================================================
# SECTION 7: MODEL 1 - LOGISTIC REGRESSION WITH LASSO REGULARIZATION
# ==============================================================================

cat("\n========================================\n")
cat("MODEL 1: LOGISTIC REGRESSION (LASSO)\n")
cat("========================================\n")

# Logistic Regression predicts probability of APT attack
# LASSO adds L1 penalty to reduce less important features

# Prepare data matrix (remove target variable)
X_train <- model.matrix(APT ~ ., data = train_data)[, -1]
y_train <- as.numeric(train_data$APT) - 1  # Convert to 0/1

X_test <- model.matrix(APT ~ ., data = test_data)[, -1]
y_test <- as.numeric(test_data$APT) - 1

cat("Training Logistic Regression (LASSO)...\n")

# Train logistic regression with cross-validation to find optimal lambda
set.seed(MY_SEED)
lasso_model <- cv.glmnet(X_train, y_train, 
                         family = "binomial",  # Logistic regression
                         alpha = 1,             # LASSO (alpha=1)
                         nfolds = 5)            # 5-fold cross-validation

# Get predictions on test set
lasso_preds <- predict(lasso_model, X_test, s = "lambda.min", type = "class")
lasso_preds <- as.factor(lasso_preds)

# Calculate accuracy
lasso_accuracy <- sum(lasso_preds == test_data$APT) / nrow(test_data)
cat("Logistic Regression (LASSO) Accuracy:", round(lasso_accuracy, 4), "\n")

# Display confusion matrix manually
conf_lasso <- table(Predicted = lasso_preds, Actual = test_data$APT)
cat("\nConfusion Matrix - Logistic Regression:\n")
print(conf_lasso)


# ==============================================================================
# SECTION 8: MODEL 2 - DECISION TREE
# ==============================================================================

cat("\n========================================\n")
cat("MODEL 2: DECISION TREE (CART)\n")
cat("========================================\n")

# Decision Trees create interpretable rules for classification
# They recursively split the data based on feature values

cat("Training Decision Tree...\n")

set.seed(MY_SEED)
# Train decision tree with complexity parameter constraint
tree_model <- rpart(APT ~ ., 
                    data = train_data, 
                    method = "class",      # Classification tree
                    control = rpart.control(cp = 0.01,  # Complexity parameter
                                           minsplit = 20))  # Min samples in node

# Get predictions on test set
tree_preds <- predict(tree_model, test_data, type = "class")

# Calculate accuracy
tree_accuracy <- sum(tree_preds == test_data$APT) / nrow(test_data)
cat("Decision Tree Accuracy:", round(tree_accuracy, 4), "\n")

# Display confusion matrix
conf_tree <- table(Predicted = tree_preds, Actual = test_data$APT)
cat("\nConfusion Matrix - Decision Tree:\n")
print(conf_tree)

# Display tree structure
cat("\nDecision Tree Structure (first few nodes):\n")
print(tree_model)


# ==============================================================================
# SECTION 9: MODEL 3 - RANDOM FOREST
# ==============================================================================

cat("\n========================================\n")
cat("MODEL 3: RANDOM FOREST\n")
cat("========================================\n")

# Random Forest creates multiple trees and averages predictions
# This reduces overfitting and improves generalization

cat("Training Random Forest...\n")

set.seed(MY_SEED)
# Train random forest classifier
rf_model <- randomForest(APT ~ ., 
                         data = train_data,
                         ntree = 100,        # Number of trees
                         mtry = 4,           # Features sampled per split
                         nodesize = 5,       # Min samples in leaf
                         importance = TRUE)  # Calculate feature importance

# Get predictions on test set
rf_preds <- predict(rf_model, test_data, type = "class")

# Calculate accuracy
rf_accuracy <- sum(rf_preds == test_data$APT) / nrow(test_data)
cat("Random Forest Accuracy:", round(rf_accuracy, 4), "\n")

# Display confusion matrix
conf_rf <- table(Predicted = rf_preds, Actual = test_data$APT)
cat("\nConfusion Matrix - Random Forest:\n")
print(conf_rf)

# Display feature importance
cat("\nFeature Importance (Top 10):\n")
imp <- importance(rf_model)
imp_df <- data.frame(Feature = rownames(imp), Importance = imp[, 1])
imp_df <- imp_df[order(-imp_df$Importance), ]
print(head(imp_df, 10))


# ==============================================================================
# SECTION 10: MODEL COMPARISON & CONCLUSIONS
# ==============================================================================

cat("\n========================================\n")
cat("MODEL COMPARISON SUMMARY\n")
cat("========================================\n")

# Create summary of all three models
model_summary <- data.frame(
    Model = c("Logistic Regression (LASSO)", 
              "Decision Tree", 
              "Random Forest"),
    Accuracy = c(lasso_accuracy, 
                 tree_accuracy, 
                 rf_accuracy),
    Type = c("Regularized Linear", 
             "Tree-based", 
             "Ensemble"),
    Interpretability = c("Good", "Excellent", "Moderate")
)

cat("\n")
print(model_summary)

# Find best model
best_model <- which.max(model_summary$Accuracy)
cat("\nBest Model:", model_summary$Model[best_model], "\n")
cat("Best Accuracy:", round(model_summary$Accuracy[best_model], 4), "\n")

# Analysis and recommendations
cat("\n========================================\n")
cat("ANALYSIS & RECOMMENDATIONS\n")
cat("========================================\n")

cat("\nKey Observations:\n")
cat("1. Logistic Regression provides a probabilistic approach\n")
cat("   - Good for understanding odds of APT attacks\n")
cat("   - Easily interpretable coefficients\n\n")

cat("2. Decision Tree offers explicit decision rules\n")
cat("   - Highly interpretable\n")
cat("   - Easy to explain to stakeholders\n")
cat("   - May overfit if not properly pruned\n\n")

cat("3. Random Forest is an ensemble method\n")
cat("   - Generally most accurate\n")
cat("   - Handles non-linear relationships well\n")
cat("   - Less interpretable but more robust\n\n")

cat("========================================\n")
cat("END OF ANALYSIS\n")
cat("========================================\n")
