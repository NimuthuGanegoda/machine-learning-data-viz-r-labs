## --------------------------------------------------------------------------------
#De-comment to install the packages below
#install.packages(c("caret","rpart","rpart.plot","mlbench","pROC"))
library(tidyverse)  #For ggplot2 and dplyr
library(caret)  #For training models
library(rpart)  #Recursive Partitioning And Regression Trees
library(rpart.plot)  #For plotting trees
library(mlbench)  #For the breast cancer dataset
library(pROC)  #For AUC


## --------------------------------------------------------------------------------
# #Note that you may need to change directory path
Elderly <- read.csv("ElderlyPopWA.csv", header=TRUE);
View(Elderly)  #View the data


## --------------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(Elderly$Pc_Body_Fat, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.80,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset and exclude the 1st column
trainData <- Elderly[trainRowNum,-1]

# Step 3: Create the test dataset
testData <- Elderly[-trainRowNum,-1]


## --------------------------------------------------------------------------------
rtree <- rpart(Pc_Body_Fat~.,  #Formula
               data=trainData)  #Relevant training data
rtree  #Show the result


## --------------------------------------------------------------------------------
#Generic tree plot
par(xpd = NA) # Ensures the text on the tree is not clipped
plot(rtree)  #Plot the tree
text(rtree,use.n=TRUE,digits = 4)  #Add labels


## --------------------------------------------------------------------------------
rpart.plot(rtree,  #A rpart tree object.
           type=2,  #Type of plot. An integer value between from 0 and 5.
           digits=3)  #Number of significant figures


## --------------------------------------------------------------------------------
prp(rtree,type=2,left=TRUE,extra=100,cex=0.8,branch=1,branch.lty=3,
    fallen.leaves=TRUE,uniform=TRUE,xflip=FALSE,shadow.col="gray",
    box.col=c("darkslategray3","lightcoral"),split.col="red",round=0.5,digits=3)


## --------------------------------------------------------------------------------
#Training set
pred.train <- predict(rtree);   #Estimated % body fat
rmse.train <- (trainData$Pc_Body_Fat-pred.train)^2 %>% mean %>% sqrt  #RMSE

#Test set
pred.test <- predict(rtree,newdata=testData);   #Estimated % body fat
rmse.test <- (testData$Pc_Body_Fat-pred.test)^2 %>% mean %>% sqrt  #RMSE

c(rmse.train,rmse.test)  #Display both results
(rmse.test-rmse.train)/rmse.train*100 #Relative % difference


## --------------------------------------------------------------------------------
set.seed(1)
rtree.pr <- train(Pc_Body_Fat~., data = trainData, method = "rpart",
                  trControl = trainControl("cv", number = 10),  #10-fold CV
                  tuneLength = 15)  #Number of cp values to test
#Plot the CV result
plot(rtree.pr)

#Optimal cp
rtree.pr$bestTune %>% round(digits=3)


## --------------------------------------------------------------------------------
prp(rtree.pr$finalModel,type=2,left=TRUE,extra=100,cex=0.8,branch=1,branch.lty=3,
    fallen.leaves=TRUE,uniform=TRUE,xflip=FALSE,shadow.col="gray",
    box.col=c("darkslategray3","lightcoral"),split.col="red",round=0.5,digits=3)


## --------------------------------------------------------------------------------
#Test set
pred.test <- predict(rtree.pr,newdata=testData);   #Estimated % body fat
rmse.test <- (testData$Pc_Body_Fat-pred.test)^2 %>% mean %>% sqrt  #RMSE

rmse.test %>% round(digits=3)


## --------------------------------------------------------------------------------
#CV results for various cp values
cp.res <- rtree.pr$results  #Access and store the cp results in another variable

#The lower bound (LB) and upper bound (UB) of RMSE +/- SD
cp.res$LB <- cp.res$RMSE - cp.res$RMSESD
cp.res$UB <- cp.res$RMSE + cp.res$RMSESD
cp.res %>% round(digits=3)  #Display results to 3 d.p.

#Plot the CV results with the error bars defined by one SD.
ggplot(cp.res,aes(x=cp,y=RMSE))+
  geom_path(colour="steelblue")+
  geom_errorbar(aes(ymin=LB,ymax=UB),width=0.02,colour="steelblue",linetype=1)+
  geom_point(shape=21,colour="steelblue",fill="white",size=4)+
  geom_hline(aes(yintercept=cp.res$UB[which(cp.res$RMSE==min(cp.res$RMSE))]),
             linetype=2,colour="coral2",size=1)+
  theme_light(base_size=14) +
  labs(x="Complexity Parameter (cp)",y="RMSE (Cross-Validation)")


## --------------------------------------------------------------------------------
rtree.pr2 <- prune(rtree,cp=cp.res$cp[14])  #Prune the tree at the 14th cp value

#Plot the pruned tree
prp(rtree.pr2,type=2,left=TRUE,extra=100,cex=0.8,branch=1,branch.lty=3,
    fallen.leaves=TRUE,uniform=TRUE,xflip=FALSE,shadow.col="gray",
    box.col=c("darkslategray3","lightcoral"),split.col="red",round=0.5,digits=3)


## --------------------------------------------------------------------------------
#Test set
pred.test <- predict(rtree.pr2,newdata=testData);   #Estimated % body fat
rmse.test <- (testData$Pc_Body_Fat-pred.test)^2 %>% mean %>% sqrt  #RMSE

rmse.test %>% round(digits=3)


## --------------------------------------------------------------------------------
data("BreastCancer")  #Load the breast cancer data

#Convert the predictors from factors to numeric.
num.col <- ncol (BreastCancer)
for (I in 2:(num.col-1))
{
  BreastCancer[,I] <- as.numeric(as.character((BreastCancer[,I])))
}

#Remove the Id column from dataset and assign to a new variable, "bc".
bc <- na.omit(BreastCancer[,-1]);

#View the dataset
View(bc)


## --------------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(bc$Class, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.80,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset
trainData <- bc[trainRowNum,]

# Step 3: Create the test dataset
testData <- bc[-trainRowNum,]


## --------------------------------------------------------------------------------
#Classification Tree
ctree <- rpart(Class~.,
               data=trainData); ctree


## --------------------------------------------------------------------------------
#Plot the pruned tree
prp(ctree,type=2,left=TRUE,extra=100,cex=0.8,branch=1,branch.lty=3,
    fallen.leaves=TRUE,uniform=TRUE,xflip=FALSE,shadow.col="gray",
    box.col=c("darkslategray3","lightcoral"),split.col="red",round=0.5,digits=3)


## --------------------------------------------------------------------------------
#Training set
pred.prob <- predict(ctree, type="prob") %>% data.frame
pred.class <- predict(ctree, type="class")

#Confusion matrix
cf.train <- confusionMatrix(pred.class %>% relevel(ref="malignant"),
                            trainData$Class %>% relevel(ref="malignant"))
#Accuracy
acc.train <- cf.train$overall[1] %>% round(digits=3)

#AUC
auc.train <- roc(Class~pred.prob$malignant,data=trainData)$auc %>% round(digits=3)

#------------------------------------------------------------------------------------

#Test set
pred.prob <- predict(ctree, newdata=testData, type="prob") %>% data.frame
pred.class <- predict(ctree, newdata=testData, type="class")

#Confusion matrix
cf.test <- confusionMatrix(pred.class %>% relevel(ref="malignant"),
                           testData$Class %>% relevel(ref="malignant"))

#Accuracy
acc.test <- cf.test$overall[1] %>% round(digits=3)

#AUC
auc.test <- roc(Class~pred.prob$malignant,data=testData)$auc %>% round(digits=3)

#Print combined results
matrix(c(acc.train,auc.train,acc.test,auc.test),nrow=2,
       dimnames=list(c("Accuracy","AUC"),c("Train","Test"))) %>% data.frame


## --------------------------------------------------------------------------------
set.seed(1)
ctree.pr <- train(Class~., data = trainData, method = "rpart",
                  trControl = trainControl("cv", number = 10),  #10-fold CV
                  tuneLength = 15)  #Number of cp values to test
#Plot the CV result
plot(ctree.pr)


## --------------------------------------------------------------------------------
#CV results for various cp values
cp.res <- ctree.pr$results  #Access and store the cp results in another variable

#The lower bound (LB) and upper bound (UB) of Accuracy +/- SD
cp.res$LB <- cp.res$Accuracy - cp.res$AccuracySD
cp.res$UB <- cp.res$Accuracy + cp.res$AccuracySD
cp.res %>% round(digits=3)  #Display results to 3 d.p.

#Plot the CV results with the error bars defined by one SD.
ggplot(cp.res,aes(x=cp,y=Accuracy))+
  geom_path(colour="steelblue")+
  geom_errorbar(aes(ymin=LB,ymax=UB),width=0.02,colour="steelblue",linetype=1)+
  geom_point(shape=21,colour="steelblue",fill="white",size=4)+
  geom_hline(aes(yintercept=cp.res$LB[which(cp.res$Accuracy==max(cp.res$Accuracy))]),
             linetype=2,colour="coral2",size=1)+
  theme_light(base_size=14) +
  labs(x="Complexity Parameter (cp)",y="Accuracy (Cross-Validation)") +
  scale_y_continuous(breaks=seq(0.7,1,0.02)) +
  theme(panel.grid.minor.y=element_blank())
