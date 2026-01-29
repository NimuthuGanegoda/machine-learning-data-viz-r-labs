#install.packages(c("tidyverse","caret","GGally"))
library(caret)  #For knn and auto-tuning
library(tidyverse)
library(GGally)


## -------------------------------------------------------------------------
data(iris) #Load the air quality data
str(iris)  #Examine the structure of the dataset

## -------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(iris$Species, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.80,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset and exclude the first 2 columns
train.iris <- iris[trainRowNum,]

# Step 3: Create the test dataset
test.iris <- iris[-trainRowNum,]


## -------------------------------------------------------------------------
set.seed(1)
model1.iris <- train(Species~.,  #Specify the model
                    data = train.iris,  #Training data
                    method = "knn",  #K-NN
                    #Perform 10-fold CV
                    trControl = trainControl("cv", number = 10),
                    preProcess = c("range"),  #min-max normalisation
                    #Number of possible k values to evaluate starting from 5.
                    tuneLength = 20
                    )

plot(model1.iris)  #Plot the CV results
model1.iris$bestTune  #Display the optimal k.


## -------------------------------------------------------------------------
pred.class <- predict(model1.iris,newdata=test.iris)
confusionMatrix(pred.class,test.iris$Species)


## ----fig.width=6*0.9, fig.height=4*0.9, fig.align='center'----------------
set.seed(1)
model2.iris <- train(Species~.,  #Specify the model
                    data = train.iris,  #Training data
                    method = "knn",  #K-NN
                    #Perform 10-fold CV
                    trControl = trainControl("cv", number = 10),
                    preProcess = c("center","scale"),  #standardisation
                    #Number of possible k values to evaluate starting from 5.
                    tuneLength = 20
                    )

plot(model2.iris)  #Plot the CV results
model2.iris$bestTune


## -------------------------------------------------------------------------
pred.class <- predict(model2.iris,newdata=test.iris)
confusionMatrix(pred.class,test.iris$Species)


## -------------------------------------------------------------------------
data(airquality)  #Load the dataset
#Extract the first 4 columns and remove the incomplete cases
aq <- airquality[,1:4] %>% na.omit
str(aq)

## -------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(aq$Ozone, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.70,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset.
train.aq <- aq[trainRowNum,]

# Step 3: Create the test dataset
test.aq <- aq[-trainRowNum,]


## -------------------------------------------------------------------------
set.seed(1)
model1.aq <- train(Ozone~.,  #Specify the model
                   data = train.aq,  #Training data
                   method = "knn",  #K-NN
                   #Perform 10-fold CV
                   trControl = trainControl("cv", number = 10),
                   preProcess = c("range"),  #min-max normalisation
                   #Number of possible k values to evaluate starting from 5.
                   tuneLength = 20
                   )

plot(model1.aq)  #Plot the CV results
model1.aq$bestTune


## -------------------------------------------------------------------------
pred.Ozone <- predict(model1.aq,newdata=test.aq)  #Predicted Ozone values
(pred.Ozone-test.aq$Ozone)^2 %>% mean %>% sqrt  #RMSE


## ----fig.width=6*0.9, fig.height=4*0.9, fig.align='center'----------------
set.seed(1)
model2.aq <- train(Ozone~.,  #Specify the model
                   data = train.aq,  #Training data
                   method = "knn",  #K-NN
                   #Perform 10-fold CV
                   trControl = trainControl("cv", number = 10),
                   preProcess = c("center","scale"),  #standardisation
                   #Number of possible k values to evaluate starting from 5.
                   tuneLength = 20
                   )

plot(model2.aq)  #Plot the CV results
model2.aq$bestTune


## -------------------------------------------------------------------------
pred.Ozone <- predict(model2.aq,newdata=test.aq)  #Predicted Ozone values
(pred.Ozone-test.aq$Ozone)^2 %>% mean %>% sqrt  #RMSE


## -------------------------------------------------------------------------
ggpairs(train.aq[,2:4],upper=NULL,
        ggplot2::aes(fill="cyan4"),
        lower=list(continuous = wrap("points",colour="cyan4",alpha=0.3)),
        diag=list(continuous=wrap("barDiag",colour="white",
                                  fill="cyan4",bins=10))) +
  theme_bw()
