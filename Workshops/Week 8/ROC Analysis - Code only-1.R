## ---------------------------------------------------------------------------------
#De-comment to install the packages below
#install.packages(c("tidyverse","caret","pROC"))
library(tidyverse)  #For ggplot2 and dplyr
library(caret)  #For penalised regression
library(pROC)  #To perform ROC analysis


## ---------------------------------------------------------------------------------
#Note that you may need to change directory path
Comp <- read.csv("Complication.csv",
                 header=TRUE,
                 stringsAsFactors=TRUE,
                 na.strings="NA") %>%  #Read in the data
          na.omit()  #Complete cases only.

#Change the reference leve to "No complication"
Comp$Complication <- relevel(Comp$Complication,ref="No complication")

View(Comp)  #Examine its structure


## ---------------------------------------------------------------------------------
roc.wcc <- roc(Complication~WCC,  #Formula specifying the outcome and feature of interest
               data=Comp,  #Relevant dataset
               plot=TRUE,  #Show the ROC curve
               percent=TRUE,  #Display metrics in percent
               print.auc=TRUE,  #Display area under the curve in ROC curve
               grid=TRUE,  #Show major grids in ROC curve
               print.thres="best"  #Show the threshold with the best overall accuracy
               )


## ---------------------------------------------------------------------------------
roc.wcc


## ---------------------------------------------------------------------------------
coords(roc.wcc,  #relevant ROC object
       "all",  #Show results of all cut-offs.
       ret=c("threshold","specificity","sensitivity","accuracy") #Measures to diplay
       )%>%
  round(digits=3)  #Round values to 3 decimal places


## ---------------------------------------------------------------------------------
roc.crp <- roc(Complication~CRP,  #Formula specifying the outcome and feature of interest
               data=Comp,  #Relevant dataset
               plot=TRUE,  #Show the ROC curve
               percent=TRUE,  #Display metrics in percent
               print.auc=TRUE,  #Display area under the curve in ROC curve
               grid=TRUE,  #Show major grids in ROC curve
               print.thres="best"  #Show the threshold with the best overall accuracy
               );


## ---------------------------------------------------------------------------------
coords(roc.crp,  #relevant ROC object
       "all",  #Show results of all cut-offs.
       ret=c("threshold","specificity","sensitivity","accuracy") #Measures to diplay
       )%>%
  round(digits=3)  #Round values to 3 decimal places

## ---------------------------------------------------------------------------------
roc.all <- roc(Complication~WCC+CRP,  #Analyse both WCC and CRP
               data=Comp,  #Relevant dataset
               plot=TRUE,  #Show the ROC curve
               percent=TRUE,  #Display metrics in percent
               print.auc=TRUE,  #Display area under the curve in ROC curve
               grid=TRUE,  #Show major grids in ROC curve
               print.thres="best"  #Show the threshold with the best overall accuracy
               )

## ----------------------------------------------------------------------------------
plot(roc.all$WCC,col="coral4")  #Plot ROC curve for WCC first
plot(roc.all$CRP,col="steelblue",lty=2,add=TRUE)  #Add ROC curve for CRP


## ---------------------------------------------------------------------------------
#Alternate approach
mod.wcc <- glm(Complication~WCC, data=Comp, family="binomial"); #Logistic regression
summary(mod.wcc)  #Summary of the model
pred.wcc <- predict(mod.wcc,type="response");  #Predicted probabilities
roc.wcc.alt <- roc(Complication~pred.wcc,
                   data=Comp,
                   plot=TRUE,
                   percent=TRUE,
                   print.auc=TRUE,
                   grid=TRUE,
                   print.thres="best")


## ---------------------------------------------------------------------------------
#Add the predicted probabilities to data frame
df <- cbind(Comp,Probability=pred.wcc)

#Sort the data frame in accordance to the probabilities in ascending order
df[order(df$Probability,decreasing=FALSE),]


## ---------------------------------------------------------------------------------
#Logistic regression model with WCC and CRP
mod.joint <- glm(Complication~WCC+CRP, data=Comp, family="binomial");
summary(mod.joint)  #Summary of the joint model
pred.joint <- predict(mod.joint,type="response");  #Predicted probabilities
roc.joint <- roc(Complication~pred.joint,
                 data=Comp,
                 plot=TRUE,
                 percent=TRUE,
                 print.auc=TRUE,
                 grid=TRUE,
                 print.thres="best")


## ---------------------------------------------------------------------------------
res <- coords(roc.joint,  #relevant ROC object
              "all",  #Show results of all cut-offs.
              ret=c("threshold","specificity","sensitivity","accuracy")
              )%>%
         round(digits=3)  #Round values to 3 decimal places

#Order the results according to the accuracy
res[order(res$accuracy,decreasing=TRUE),]


## ---------------------------------------------------------------------------------
COVID.train <- read.csv("COVID-19_Train.csv", header=TRUE, stringsAsFactors=TRUE);
COVID.test <- read.csv("COVID-19_Test.csv", header=TRUE, stringsAsFactors=TRUE)


## ---------------------------------------------------------------------------------
mod.covid.lg <- glm(death~., family="binomial",data=COVID.train);
summary(mod.covid.lg)  #Summarise the model


## ---------------------------------------------------------------------------------
#Predicted probabilites on the training set
pred.prob.lg.train <- predict(mod.covid.lg,type="response")
pred.class.lg.train <- ifelse(pred.prob.lg.train<0.5,"No","Yes")  #Predicted classes
acc.lg.train <- mean(pred.class.lg.train==COVID.train$death);  #Accuracy

#ROC analysis of the model on the training set.
roc.lg.train <- roc(death~pred.prob.lg.train, data=COVID.train)
auc.lg.train <- roc.lg.train$auc

#Output the accuracy and AUC of the model on the training set
c(Acc_Train=acc.lg.train, AUC_Train=auc.lg.train)


## ---------------------------------------------------------------------------------
#Predicted probabilites on the test set
pred.prob.lg.test <- predict(mod.covid.lg, newdata=COVID.test, type="response")
pred.class.lg.test <- ifelse(pred.prob.lg.test<0.5,"No","Yes")  #Predicted classes
acc.lg.test <- mean(pred.class.lg.test==COVID.test$death);  #Accuracy

#ROC analysis of the model on the test set.
roc.lg.test <- roc(death~pred.prob.lg.test, data=COVID.test)
auc.lg.test <- roc.lg.test$auc

#Output the accuracy and AUC of the model on the test set
c(Acc_Test=acc.lg.test, AUC_Test=auc.lg.test)


## ---------------------------------------------------------------------------------
set.seed(1)
lambdas <- 10^seq(-3,3,length=100) #A sequence 100 lambda values
mod.covid.LASSO <- train(death ~., #Formula
                   data = COVID.train, #Training data
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
# Model coefficients
coef(mod.covid.LASSO$finalModel, mod.covid.LASSO$bestTune$lambda)


## ---------------------------------------------------------------------------------
#Predicted probabilities of death on the training and test sets
#Note that the probabilities for both classes are provided. We use the "Yes" only.
pred.prob.LASSO.train <- predict(mod.covid.LASSO, type="prob")$Yes
pred.prob.LASSO.test <- predict(mod.covid.LASSO, newdata=COVID.test, type="prob")$Yes

#Predicted classes of death on the training and test sets
pred.class.LASSO.train <- predict(mod.covid.LASSO)
pred.class.LASSO.test <- predict(mod.covid.LASSO,newdata=COVID.test)

#Accuracy of the LASSO model on the training and test sets
acc.LASSO.train <- mean(pred.class.LASSO.train==COVID.train$death);
acc.LASSO.test <- mean(pred.class.LASSO.test==COVID.train$death);

#ROC analysis of the LASSO model on the training and test sets
roc.LASSO.train <- roc(death~pred.prob.LASSO.train, data=COVID.train)
auc.LASSO.train <- roc.LASSO.train$auc

roc.LASSO.test <- roc(death~pred.prob.LASSO.test, data=COVID.test)
auc.LASSO.test <- roc.LASSO.test$auc

#Output the accuracy and AUC of the LASSO regression model
c(Acc_Train=acc.LASSO.train,
  AUC_Train=auc.LASSO.train,
  Acc_Test=acc.LASSO.test,
  AUC_Test=auc.LASSO.test)

#Recall the accuracy and AUC of the binary logistic regression model
c(Acc_Train=acc.lg.train,
  AUC_Train=auc.lg.train,
  Acc_Test=acc.lg.test,
  AUC_Test=auc.lg.test)
