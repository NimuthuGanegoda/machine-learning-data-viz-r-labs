## ----------------------------------------------------------------------------------------
# De-comment to install the packages below
# install.packages(c("tidyverse","GGally","caret","corrplot","car"))
library(tidyverse) # For ggplot2
library(GGally) # For scatter plot matrix
library(caret) # Classification and Regression Training package
library(corrplot) # For visualisng correlation matrix
library(car) # For the VIF function


## ----------------------------------------------------------------------------------------
CPU <- read.csv("CPU.csv", header = TRUE, stringsAsFactors = FALSE)
# Read in the data
str(CPU) # Examine its structure


## ----------------------------------------------------------------------------------------
# Create the training and test datasets

set.seed(1) # Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(CPU$PRP, # The outcome variable
  p = 0.75, # proportion of data to form the training set
  list = FALSE # Don't store the result in a list
)
# Step 2: Create the training  dataset
trainData <- CPU[trainRowNumbers, ]

# Step 3: Create the test dataset
testData <- CPU[-trainRowNumbers, ]


## ----------------------------------------------------------------------------------------
# Extract PRP and MMAX from the main training data
trainData.slr <- trainData[, c(5, 9)]

# Plot PRP against MMAX
ggplot(trainData.slr, aes(x = MMAX, y = PRP)) +
  geom_point(size = 3, colour = "red", alpha = 0.5) +
  theme_minimal(base_size = 14)

# Pearson's correlation coefficient
cor(trainData.slr)


## ----------------------------------------------------------------------------------------
mod.slr <- lm(
  formula = PRP ~ MMAX, # PRP as a function of MMAX
  data = trainData.slr
) # The relevant training data

summary(mod.slr) # Summary of the model

# Show the estimated parameter coefficients to 3 dp
mod.slr$coefficients %>% round(digits = 3)


## ----------------------------------------------------------------------------------------
# Predict PRP with the test data
pred <- predict(mod.slr, # model used to make prediction
  newdata = testData # new data, i.e. test data, to predict on
)

# Difference between the actual and estimated PRP in the test set
diff <- testData$PRP - pred

RMSE.slr <- diff^2 %>%
  mean() %>%
  sqrt()
# RMSE
bias.slr <- diff %>% mean() # Bias
cor.slr <- cor(testData$PRP, pred) # Correlation
predR2.slr <- cor.slr^2 # Predicted R2

# Show out the results for SLR
SLR.pf <- c(
  RMSE = RMSE.slr,
  Bias = bias.slr,
  Corr = cor.slr,
  PredR2 = predR2.slr
)
SLR.pf


## ----------------------------------------------------------------------------------------
df <- data.frame(Actual = testData$PRP, Predicted = pred)

ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point(size = 3, colour = "steelblue") +
  xlim(0, 500) +
  # Reference line given by y=x, i.e. slope=1 and intercept=0
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "red", # Colour of the line
    linetype = 2
  ) + # Dotted line
  theme_minimal()


## ----------------------------------------------------------------------------------------
ggpairs(trainData[, 3:9],
  upper = NULL, # Don't display the upper triangle
  ggplot2::aes(fill = "cyan4"),
  lower = list(continuous = wrap("points", colour = "cyan4", alpha = 0.7)),
  diag = list(continuous = wrap("barDiag",
    colour = "white",
    fill = "cyan4", bins = 12
  ))
) +
  theme_bw()


## ----fig.width=6, fig.height=4, fig.align='center'---------------------------------------
cor(trainData[, 3:8]) %>%
  corrplot(
    method = "color", # escribe the magnitude of r by colours
    type = "upper", # Show the upper triangle only
    addCoef.col = "black", # Add the correlation coefficient to the matrix
    tl.col = "black", # Colour of the text label
    tl.srt = 45, # Angle of the text label
    number.digits = 3, # Number of decimal places for r
    diag = FALSE
  ) # Do not show the main diagnal values


## ----------------------------------------------------------------------------------------
# Method 1 - Specify the names of all features
mod.mlr <- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX,
  data = trainData[, 3:9]
)
# Method 2
mod.mlr <- lm(PRP ~ ., # Use all available features in the dataset
  data = trainData[, 3:9]
)
summary(mod.mlr) # Summary of the MLR model

# Show the estimated parameter coefficients to 3 dp
mod.mlr$coefficients %>% round(digits = 3)


## ----------------------------------------------------------------------------------------
vif(mod.mlr)


## ----------------------------------------------------------------------------------------
# Predict PRP with the test data
pred <- predict(mod.mlr, # model used to make prediction
  newdata = testData # new data, i.e. test data, to predict on
)

# Difference between the actual and estimated PRP in the test set
diff <- testData$PRP - pred

RMSE.mlr <- diff^2 %>%
  mean() %>%
  sqrt()
# RMSE
bias.mlr <- diff %>% mean() # Bias
cor.mlr <- cor(testData$PRP, pred) # Correlation
predR2.mlr <- cor.mlr^2 # Predicted R2

# Predictive measures for MLR
MLR.pf <- c(
  RMSE = RMSE.mlr, Bias = bias.mlr,
  Corr = cor.mlr, PredR2 = predR2.mlr
)
MLR.pf


## ----------------------------------------------------------------------------------------
df <- data.frame(Actual = testData$PRP, Predicted = pred)

ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point(size = 3, colour = "steelblue") +
  xlim(0, 500) +
  # Reference line given by y=x, i.e. slope=1 and intercept=0
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "red", # Colour of the line
    linetype = 2
  ) + # Dotted line
  theme_minimal()


## ----------------------------------------------------------------------------------------
# Display the results for SLR and MLR
data.frame(SLR = SLR.pf, MLR = MLR.pf)


## ----------------------------------------------------------------------------------------
set.seed(1)
options(warn = -1) # Turn the warning messages off

subsets <- c(2:6) # Subsets of features to test

# Set the cross validation parameters
ctrl <- rfeControl(
  functions = lmFuncs,
  method = "repeatedcv", # 10-fold CV by default
  repeats = 10, # repeat CV 10 times
  # Prevents copious amounts of output from being produced
  verbose = FALSE
)
# Perform RFE
lmProfile <- rfe(
  x = trainData[, 3:8], # features only
  y = trainData$PRP, # outcome variable
  sizes = subsets,
  rfeControl = ctrl
)
lmProfile


## ----------------------------------------------------------------------------------------
# Perform RFE specifying the formula instead.
set.seed(1)
lmProfile <- rfe(PRP ~ ., # Specifying PRP as a function all other variables in the dataset
  data = trainData[, 3:9], # data sets excluding Vendor and Model
  sizes = subsets,
  rfeControl = ctrl
)
lmProfile

## ---------------------------------------------------------------------------------
# Access the optimal model and show its coefficients
lmProfile$fit


## ---------------------------------------------------------------------------------
# Summarise the optimal model and show the significance of the features
summary(lmProfile$fit)


## ---------------------------------------------------------------------------------
# Predict on the test set using the optimal model
pred.optmod <- predict(lmProfile$fit, newdata = testData)
pred.optmod %>% round(digits = 3) # Show prediction to 3 dp


## --------------------------------------------------------------------
# Convert the categorical features to dummy variables, remove the first
# column and convert the whole data matrix to a data frame.
dummy.testData <- model.matrix(~., data = testData)[, -1] %>% data.frame()


## --------------------------------------------------------------------
pred.optmod <- predict(lmProfile$fit, newdata = dummy.testData)
pred.optmod %>% round(digits = 3) # Show prediction to 3 dp


## --------------------------------------------------------------------
# Difference between the actual and estimated PRP in the test set
diff.rfe <- testData$PRP - pred.optmod

RMSE.rfe <- diff.rfe^2 %>%
  mean() %>%
  sqrt()
# RMSE
bias.rfe <- diff.rfe %>% mean() # Bias
cor.rfe <- cor(testData$PRP, pred.optmod) # Correlation
predR2.rfe <- cor.rfe^2 # Predicted R2

# Show out the results for RFE
RFE.pf <- c(
  RMSE = RMSE.rfe,
  Bias = bias.rfe,
  Corr = cor.rfe,
  PredR2 = predR2.rfe
)
RFE.pf
