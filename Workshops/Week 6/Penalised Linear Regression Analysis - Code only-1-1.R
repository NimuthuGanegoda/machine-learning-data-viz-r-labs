## -----------------------------------------------------------------------
#De-comment to install the packages below
install.packages(c("tidyverse","caret","glmnet"))
library(tidyverse)  #For ggplot2 and dplyr
library(caret)  #Classification and Regression Training package
library(glmnet)  #For penalised regression modelling

## -----------------------------------------------------------------------
CPU <- read.csv("CPU.csv", header=TRUE, stringsAsFactors=FALSE);  #Read in the data
str(CPU)  #Examine its structure


## -----------------------------------------------------------------------
#Create the training and test datasets

set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(CPU$PRP, #The outcome variable
                                       p=0.75, #proportion of data to form the training set
                                       list=FALSE  #Don't store the result in a list
                                       );

# Step 2: Create the training  dataset, and excluding the 1st 2 columns.
trainData <- CPU[trainRowNumbers,3:9]

# Step 3: Create the test dataset, and excluding the 1st 2 columns.
testData <- CPU[-trainRowNumbers,3:9]


## -----------------------------------------------------------------------
lambdas <- 10^seq(-3,3,length=200) #A sequence of lambdas

set.seed(1)
mod.ridge <- train(PRP ~., #Formula
                   data = trainData, #Training data
                   method = "glmnet",  #Penalised regression modelling
                   #Set to c("center", "scale") to standardise data
                   preProcess = NULL,
                   #Perform 10-fold CV, 5 times over.
                   trControl = trainControl("repeatedcv", 
                                            number = 10,
                                            repeats = 5), 
                   tuneGrid = expand.grid(alpha = 0, #Ridge regression
                                          lambda = lambdas)
                  )

#Optimal lambda value
mod.ridge$bestTune


## -----------------------------------------------------------------------
# Model coefficients
coef(mod.ridge$finalModel, mod.ridge$bestTune$lambda)


## -----------------------------------------------------------------------
model.pf <- function(actual,predicted)
{
  diff <- actual - predicted
  RMSE <- diff^2 %>% mean %>% sqrt;  #RMSE
  bias <- diff %>% mean  #Bias
  cor <- cor(actual,predicted)  #Correlation
  predR2 <-cor^2  #Prediction R2

  #return as function output
  return(c(RMSE=RMSE, Bias=bias,
            Corr=cor, PredR2=predR2))
}


## -----------------------------------------------------------------------
pred.ridge <- predict(mod.ridge,testData)  #Predict PRP at the test data
pf.ridge <- model.pf(testData$PRP,pred.ridge); pf.ridge


## -----------------------------------------------------------------------
df <- data.frame(Actual=testData$PRP,Predicted=pred.ridge)

ggplot(df,aes(x=Predicted,y=Actual))+
  geom_point(size=3,colour="steelblue")+
  xlim(0,500)+
  #Reference line given by y=x, i.e. slope=1 and intercept=0
  geom_abline(slope=1,
              intercept=0,
              colour="red",  #Colour of the line
              linetype=2) + #Dotted line  
  theme_minimal()


## -----------------------------------------------------------------------
mod.ols <- lm(formula=PRP~.,  #PRP as a function of all 6 features
              data=trainData)  #The relevant training data

#Predict PRP with the test data
pred.ols <- predict(mod.ols,  #Model used to make prediction
                    newdata=testData  #Test set
                   )
pf.ols <- model.pf(testData$PRP,pred.ols); 

#Compare the results of ridge regression to OLS
rbind(OLS=pf.ols,Ridge=pf.ridge) %>% round(digit=3) 




## -----------------------------------------------------------------------
set.seed(1)
mod.LASSO <- train(PRP ~., #Formula
                   data = trainData, #Training data
                   method = "glmnet",  #Penalised regression modelling
                   #Set preProcess to c("center", "scale") to standardise data
                   preProcess = NULL,
                   #Perform 10-fold CV, 5 times over.
                   trControl = trainControl("repeatedcv", 
                                            number = 10,
                                            repeats = 5), 
                   tuneGrid = expand.grid(alpha = 1, #LASSO regression
                                          lambda = lambdas)
                  )

#Optimal lambda value
mod.LASSO$bestTune


## -----------------------------------------------------------------------
# Model coefficients
coef(mod.LASSO$finalModel, mod.LASSO$bestTune$lambda)


## -----------------------------------------------------------------------
pred.LASSO <- predict(mod.LASSO,testData)  #Predict PRP at the test data
pf.LASSO <- model.pf(testData$PRP,pred.LASSO); #Performance measures of LASSO

#Tabulate and compare the three models.
mod.comp <- rbind(OLS=pf.ols,Ridge=pf.ridge,LASSO=pf.LASSO) %>% data.frame()
mod.comp %>% round(digit=3)


## -----------------------------------------------------------------------
alphas <- seq(0.1,0.9,by=0.1); alphas

set.seed(1)
mod.elnet <- train(PRP ~., #Formula
                   data = trainData, #Training data
                   method = "glmnet",  #Penalised regression modelling
                   #Set preProcess to c("center", "scale") to standardise data
                   preProcess = NULL,
                   #Perform 10-fold CV, 5 times over.
                   trControl = trainControl("repeatedcv",
                                            number = 10,
                                            repeats = 5),
                   tuneGrid = expand.grid(alpha = alphas, 
                                           lambda = lambdas)
                  )

#Optimal lambda value
mod.elnet$bestTune


## -----------------------------------------------------------------------
# Model coefficients
coef(mod.elnet$finalModel, mod.elnet$bestTune$lambda)


## -----------------------------------------------------------------------
pred.elnet <- predict(mod.elnet,testData)  #Predict PRP at the test data
pf.elnet <- model.pf(testData$PRP,pred.elnet); #Performance measures

#Tabulate and compare all 4 models.
mod.comp <- rbind(mod.comp,ElastNet=pf.elnet) %>% data.frame()
mod.comp %>% round(digit=3)


