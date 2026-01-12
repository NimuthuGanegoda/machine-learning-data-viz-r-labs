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
if(!require(ranger)) install.packages("ranger") # For simultaneous RF tuning
library(tidyverse)
library(caret)
library(glmnet)
library(rpart)
library(ranger)
library(forcats)

# Set seed for reproducibility using Student ID
MY_SEED <- 10695889

# ==============================================================================
# PART 1: DATA PREPARATION & CLEANING
# ==============================================================================

# a) Import Data
dat <- read.csv("WACY-COM.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

# b) Cleaning based on Assignment 1 feedback [cite: 47]
dat <- dat %>% filter(Source.OS.Detected != "???")
dat$Source.Port.Range <- NULL
dat$IP.Range.Trust.Score <- NULL
dat <- dat %>% filter(Average.ping.to.attacking.IP.milliseconds != 99999)
dat <- dat %>% filter(Attack.Source.IP.Address.Count != -1)

# ii. Merging Categories exactly as required 
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected,
    Windows_All = c("Windows 10", "Windows Server 2008")
)

dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS,
    Windows_DeskServ = c("Windows (Desktops)", "Windows (Servers)"),
    MacOS_Linux = c("Linux", "MacOS (All)")
)

# iii. Log and Sqrt Transformations [cite: 53-57]
dat$Average.ping.variability <- log(dat$Average.ping.variability + 1)
dat$Hits <- sqrt(dat$Hits)
dat$Attack.Source.IP.Address.Count <- sqrt(dat$Attack.Source.IP.Address.Count)
dat$Average.ping.to.attacking.IP.milliseconds <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
dat$Individual.URLs.requested <- sqrt(dat$Individual.URLs.requested)

# iv. Final Clean Dataset [cite: 58]
WACY_COM_cleaned <- na.omit(dat)

# c) Partition Data (30% Train / 70% Test) [cite: 60-64]
set.seed(MY_SEED)
trainIndex <- createDataPartition(WACY_COM_cleaned$APT, p = 0.30, list = FALSE)
train_data <- WACY_COM_cleaned[trainIndex, ]
test_data  <- WACY_COM_cleaned[-trainIndex, ]

# ==============================================================================
# PART 2: MODELLING & SIMULTANEOUS TUNING [cite: 77-85]
# ==============================================================================

# Setup Training Control: 5-Fold Repeated CV (Min 2 repeats) 
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

# --- MODEL 1: LOGISTIC RIDGE REGRESSION ---
set.seed(MY_SEED)
grid_ridge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 10))
model_ridge <- train(APT ~ ., data = train_data, method = "glmnet",
                     trControl = fitControl, tuneGrid = grid_ridge)

# --- MODEL 2: CLASSIFICATION TREE ---
set.seed(MY_SEED)
grid_tree <- expand.grid(cp = seq(0.001, 0.05, length = 10))
model_tree <- train(APT ~ ., data = train_data, method = "rpart",
                    trControl = fitControl, tuneGrid = grid_tree)

# --- MODEL 3: RANDOM FOREST (SIMULTANEOUS TUNING)  ---
set.seed(MY_SEED)
rf_grid <- expand.grid(mtry = c(2, 4, 6), 
                       splitrule = "gini", 
                       min.node.size = c(1, 5, 10))

model_rf <- train(APT ~ ., data = train_data, method = "ranger",
                  trControl = fitControl, tuneGrid = rf_grid,
                  num.trees = 500) 

# ==============================================================================
# PART 3: EVALUATION [cite: 86-89]
# ==============================================================================

# Confusion Matrix for the best model (Example: RF)
preds_rf <- predict(model_rf, newdata = test_data)
confusionMatrix(preds_rf, test_data$APT)