## --------------------------------------------------------------------------------
#install.packages("tidyverse","caret","rpart","rpart.plot","ipred","ranger","mlbench")
library(tidyverse)
library(caret)
library(rpart)  #For CART modelling
library(rpart.plot)  #For plotting CART
library(ipred)  #Bagging
library(ranger)  #For random forest
library(mlbench)  #For the breast cancer dataset

## --------------------------------------------------------------------------------
data(airquality)  #Load the data

#Select the variables Ozone and air and omit missing observations.
aq <- airquality[,c("Ozone","Wind")] %>% na.omit


## --------------------------------------------------------------------------------
set.seed(123)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum.aq <- createDataPartition(aq$Ozone, #The outcome variable
                                      #proportion of data to form the training set
                                      p=0.80,
                                      #Don't store the result in a list
                                      list=FALSE);

# Step 2: Create the training  dataset and exclude the 1st column
train.aq <- aq[trainRowNum.aq,]

# Step 3: Create the test dataset
test.aq <- aq[-trainRowNum.aq,]

## --------------------------------------------------------------------------------
g1 <- ggplot(data.frame(train.aq),aes(Wind,Ozone))+
      geom_point(colour="steelblue",fill="white",shape=21,size=2,stroke=1.25)+
      theme_light(base_size = 16)+
      labs(x="Wind (mph)", y="Ozone (ppb)")#+
      #geom_smooth(se=FALSE,linetype=2);
g1

## --------------------------------------------------------------------------------
##Standard regression tree model without bagging
rtree <- rpart(Ozone~Wind,train.aq)

#Plot the tree
prp(rtree,type=2,left=TRUE,extra=101,cex=1,branch=1,branch.lty=3,
    fallen.leaves=TRUE,uniform=TRUE,xflip=FALSE,shadow.col="gray",
    box.col=c("darkslategray3","lightcoral"),split.col="red",round=0.5,
    digits=4);


## --------------------------------------------------------------------------------
#Obtain the predicted estimates on the test set
test.pred <- predict(rtree,test.aq);
test.rmse <- (test.aq$Ozone-test.pred)^2 %>% mean %>% sqrt;
test.rmse %>% round(3)


## --------------------------------------------------------------------------------
set.seed(1)
btree <- bagging(Ozone~Wind,
                 data=train.aq,
                 nbagg=100,  #Default value = 25
                 coob=TRUE #combined out-of-bag RMSE
                 ); btree


## --------------------------------------------------------------------------------
test.pred.bt <- predict(btree,newdata=test.aq)  #Predictions from bagging
test.rmse.bt <- (test.aq$Ozone-test.pred.bt)^2 %>% mean %>% sqrt  #Test RMSE for bagging

#Recall the test RMSE from the regresssion model and compare to bagging
c(test.rmse,test.rmse.bt) %>% round(3)


## --------------------------------------------------------------------------------
#Create a sequence of candidate values for nbagg.
nbg <- seq(10,100,5);

#Initialise the vectors to store the OOB and test RMSEs
oob.err <- c();
test.rmse.bt <- c();

for (I in 1:length(nbg))
{
  set.seed(1)
  btree <- bagging(Ozone~Wind,
                   data=train.aq,
                   nbagg=nbg[I],  #Default value = 25
                   coob=TRUE)  #Show the combined OOB RMSE

  oob.err <- c(oob.err,btree$err)  #Combine the OOB error to the previous run

  pred <- predict(btree,newdata=test.aq); #Predict on the test set
  rmse <- (test.aq$Ozone-pred)^2 %>% mean %>% sqrt  #Test RMSE
  test.rmse.bt <- c(test.rmse.bt,rmse)  #Combine the test RMSE to the previous run
}


## --------------------------------------------------------------------------------
#Plot the RMSE results vs minsplit.
ggplot(data.frame(NBagg=nbg,OOB=oob.err),aes(NBagg,OOB))+
  geom_line(colour="steelblue",size=1.25)+
  geom_point(colour="steelblue",size=3,shape=21,fill="white")+
  theme_light(base_size = 16)+
  xlab("Number of Bootstrap Samples")+
  ylab("Out-Of-Bag (OOB) RMSE")+
  scale_x_continuous(breaks=seq(10,100,10))


## --------------------------------------------------------------------------------
#Hyperparamter tuning for nbagg, cp and minsplit
grid <- expand.grid(nbagg=seq(25,150,25),  #A sequence of nbagg values
                    cp=seq(0,0.5,0.1),  #A sequence of cp values
                    minsplit=seq(5,20,5),  #A sequence of minsplits values
                    OOB.rmse=NA,  #Initialise the column to later store the OOB RMSE
                    test.rmse=NA)  #Initialise the column to later store the test RMSE

# Display the search grid
View(grid)

## --------------------------------------------------------------------------------
for (I in 1:nrow(grid))
{
  set.seed(123)
  #Bagging using the hyperparameter valued defined by Row I of the search grid
  bagged.tree <- bagging(Ozone~Wind,data=train.aq,
                         nbagg=grid$nbagg[I],
                         coob=TRUE,
                         control=rpart.control(cp=grid$cp[I],
                                               minsplit=grid$minsplit[I]));
  #Store the OOB RMSE
  grid$OOB.rmse[I] <- bagged.tree$err

  #Store the test RMSE
  pred <- predict(bagged.tree,newdata=test.aq);
  grid$test.rmse[I] <- (test.aq$Ozone-pred)^2 %>% mean %>% sqrt
}

#Sort the results by the OOB rmse and view the first 10 rows.
grid[order(grid$OOB.rmse,decreasing=FALSE)[1:10],] %>% round(3)


## --------------------------------------------------------------------------------
data("BreastCancer")

#Convert the predictors from factors to numeric.
num.col <- ncol (BreastCancer)
for (I in 2:(num.col-1))
{
  BreastCancer[,I] <- as.numeric(as.character((BreastCancer[,I])))
}

#Remove the Id column from dataset and assign to a new variable, "bc".
bc <- na.omit(BreastCancer[,-1])


## --------------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum.bc <- createDataPartition(bc$Class, #The outcome variable
                                      #proportion of data to form the training set
                                      p=0.80,
                                      #Don't store the result in a list
                                      list=FALSE);

# Step 2: Create the training  dataset and exclude the 1st column
train.bc <- bc[trainRowNum.bc,]

# Step 3: Create the test dataset
test.bc <- bc[-trainRowNum.bc,]


## --------------------------------------------------------------------------------
set.seed(1)
btree.bc <- bagging(Class~.,
                    data=train.bc,
                    nbagg=100,
                    coob=TRUE);
btree.bc


## --------------------------------------------------------------------------------
#Summary of predictions on test set
test.pred.bc <- predict(btree.bc,newdata=test.bc,type="class");

test.cf.bc <- confusionMatrix(test.pred.bc %>% relevel(ref="malignant"),
                              test.bc$Class %>% relevel(ref="malignant"))
test.cf.bc


## --------------------------------------------------------------------------------
#Intialise the hyperparamter search grid
grid.bc <- expand.grid(nbagg=seq(25,150,25),  #A sequence of nbagg values
                       cp=seq(0,0.5,0.1),  #A sequence of cp values
                       minsplit=seq(5,20,5),  #A sequence of minsplits values
                       #Initialise columns to store the OOB misclassification rate
                       OOB.misclass=NA,
                       #Initialise columns to store sensitivity, specificity and
                       #accuracy of bagging at each run.
                       test.sens=NA,
                       test.spec=NA,
                       test.acc=NA)

for (I in 1:nrow(grid.bc))
{
  set.seed(123)

  #Perform bagging
  btree.bc <- bagging(Class~.,
                      data=train.bc,
                      nbagg=grid.bc$nbagg[I],
                      coob=TRUE,
                      control=rpart.control(cp=grid.bc$cp[I],
                                            minsplit=grid.bc$minsplit[I]));

  #OOB misclassification rate
  grid.bc$OOB.misclass[I] <- btree.bc$err*100

  #Summary of predictions on test set
  test.pred.bc <- predict(btree.bc,newdata=test.bc,type="class");  #Class prediction

    #Confusion matrix
  test.cf.bc <- confusionMatrix(test.pred.bc %>% relevel(ref="malignant"),
                                test.bc$Class %>% relevel(ref="malignant"))

  prop.cf.bc <- test.cf.bc$table %>% prop.table(2)
  grid.bc$test.sens[I] <- prop.cf.bc[1,1]*100  #Sensitivity
  grid.bc$test.spec[I] <- prop.cf.bc[2,2]*100  #Specificity
  grid.bc$test.acc[I] <- test.cf.bc$overall[1]*100  #Accuracy
}

#Sort the results by the OOB misclassification rate and display them.
grid.bc[order(grid.bc$OOB.misclass,decreasing=FALSE)[1:10],] %>% round(2)


## --------------------------------------------------------------------------------
#Remove last column and samples with missing value(s)
aq <- airquality[,-ncol(airquality)] %>% na.omit;
aq$Month <- as.factor(aq$Month); str(aq) #Convert Month to a factor


## --------------------------------------------------------------------------------
set.seed(123)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum.aq <- createDataPartition(aq$Ozone, #The outcome variable
                                      #proportion of data to form the training set
                                      p=0.80,
                                      #Don't store the result in a list
                                      list=FALSE);

#Create the training and test sets.
train.aq <- aq[trainRowNum.aq,]; #training set
test.aq <- aq[-trainRowNum.aq,]; #test set


## --------------------------------------------------------------------------------
rf.aq <- ranger(Ozone~.,
                 data = train.aq,
                 num.trees = 500,  #Default is 500
                 mtry = 3, #Default is floor(n_features/3),
                 respect.unordered.factors = TRUE,
                 seed = 1,
                 importance="impurity"); rf.aq


## --------------------------------------------------------------------------------
#Predicted ozone values on the test set.R
rf.pred <- predict(rf.aq,data=test.aq);

#RMSE of the prediction for the test set.
rf.rmse <- (test.aq$Ozone-rf.pred$predictions)^2 %>% mean %>% sqrt; rf.rmse


## --------------------------------------------------------------------------------
#Create a search grid for the tuning parameters
grid.rf.oz <- expand.grid(num.trees = c(200,300,400,500),  #Number of trees
                          mtry = c(1,2,3), # split rule
                          min.node.size = seq(2,10,2),  #Tree complexity
                          replace = c(TRUE, FALSE),  #Sampling with or without replacement
                          sample.fraction = c(0.5,0.6,0.7,0.8,1),   #Sampling fraction
                          OOB.rmse = NA, #Column to store the OOB RMSE
                          test.rmse = NA) #results placeholder to store the test RMSE

dim(grid.rf.oz)  #Check the dimension
View(grid.rf.oz)  #View the search grid


## --------------------------------------------------------------------------------
for(I in 1:nrow(grid.rf.oz))
{
  rf <- ranger(Ozone~.,
               data=train.aq,
               num.trees=grid.rf.oz$num.trees[I],
               mtry=grid.rf.oz$mtry[I],
               min.node.size=grid.rf.oz$min.node.size[I],
               replace=grid.rf.oz$replace[I],
               sample.fraction=grid.rf.oz$sample.fraction[I],
               seed=1,
               respect.unordered.factors="order")

  #Store the OOB RMSE
  grid.rf.oz$OOB.rmse[I] <- sqrt(rf$prediction.error) %>% round(3)

  #Predicted ozone values for the test set.
  pred.test <- predict(rf,data=test.aq);

  #RMSE of the prediction for the test set.
  grid.rf.oz$test.rmse[I] <- (test.aq$Ozone-pred.test$predictions)^2 %>%
                              mean %>% sqrt %>% round(3);
}

#Sort the results by the OOB rmse and view the top 10 results
grid.rf.oz[order(grid.rf.oz$OOB.rmse,decreasing=FALSE)[1:10],]


## --------------------------------------------------------------------------------
#Create a search grid for the tuning parameters
grid.rf.bc <- expand.grid(num.trees = c(200,300,400,500),  #Number of trees
                          mtry = c(2,4,6,8),  #Split rule
                          min.node.size = seq(2,10,2),  #Tree complexity
                          replace = c(TRUE, FALSE),  #Sampling with or without replacement
                          sample.fraction = c(0.5,0.7,0.8,1),  #Sampling fraction
                          OOB.misclass = NA,   #Column to store the OOB RMSE
                          test.sens = NA,  #Column to store the test Sensitivity
                          test.spec = NA,  #Column to store the test Specificity
                          test.acc = NA) #Column to store the test Accuracy

dim(grid.rf.bc)  #Check the dimension
View(grid.rf.bc)  #View the search grid


## --------------------------------------------------------------------------------
for (I in 1:nrow(grid.rf.bc))
{
  rf.bc <- ranger(Class~.,
               data=train.bc,
               num.trees=grid.rf.bc$num.trees[I],
               mtry=grid.rf.bc$mtry[I],
               min.node.size=grid.rf.bc$min.node.size[I],
               replace=grid.rf.bc$replace[I],
               sample.fraction=grid.rf.bc$sample.fraction[I],
               seed=1,
               respect.unordered.factors="order")

  grid.rf.bc$OOB.misclass[I] <- rf.bc$prediction.error %>% round(5)*100

  #Test classification
  test.pred.bc <- predict(rf.bc,data=test.bc)$predictions; #Predicted classes

  #Summary of confusion matrix
  test.cf.bc <- confusionMatrix(test.pred.bc %>% relevel(ref="malignant"),
                                test.bc$Class %>% relevel(ref="malignant"));

  prop.cf <- test.cf.bc$table %>% prop.table(2)
  grid.rf.bc$test.sens[I] <- prop.cf[1,1] %>% round(5)*100  #Sensitivity
  grid.rf.bc$test.spec[I] <- prop.cf[2,2] %>% round(5)*100  #Specificity
  grid.rf.bc$test.acc[I] <- test.cf.bc$overall[1] %>% round(5)*100  #Accuracy
}

#Sort the results by the OOB misclassification error and view the top 10 results
grid.rf.bc[order(grid.rf.bc$OOB.misclass,decreasing=FALSE)[1:10],]
