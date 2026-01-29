## -------------------------------------------------------------------------
#De-comment to install the packages below
#install.packages(c("tidyverse","caret","glmnet"))
library(tidyverse)  #For ggplot2 and dplyr
library(caret)  #Classification and Regression Training package
library(glmnet)  #For penalised regression modelling


## -------------------------------------------------------------------------
#Note that you may need to change directory path
COVID19 <- read.csv("COVID19.csv", header=TRUE,
                    stringsAsFactors=FALSE,
                    na.strings="NA");  #Read in the data
View(COVID19)  #Examine its structure

## -------------------------------------------------------------------------
#Extract the features of interest
dat <- COVID19[,c("region","gender","age","hosp_visit_date","death")]

#Recode hosp_vist_dat and assign to new variable.
dat$hosp_visit <- ifelse(is.na(dat$hosp_visit_date),"No","Yes");
dat <- na.omit(dat[,-4]) #Remove hosp_visit_date and missing values

#Recode death to Yes and No
dat$death <- ifelse(dat$death=="0","No","Yes");

View(dat) #View the data to check recoding is okay.


## -------------------------------------------------------------------------
#Tabulate the number of cases in each region
df <- table(dat$region) %>% data.frame;
colnames(df) <- c("Region","Frequency");  #renaming the columns

#Only plot those with more than 10 cases
ggplot(df[df$Frequency>10,],
       #Show the frequency bars in descending order of counts
       aes(x=reorder(Region,-Frequency),y=Frequency))+
  geom_bar(stat="identity",aes(fill=Region)) +
  theme_minimal(base_size = 12)+
  xlab("") +
  theme(legend.position="",
        axis.text.x=element_text(angle = 90))


## -------------------------------------------------------------------------
dat.top4 <- subset(dat,region=="China"|
                       region=="Japan"|
                       region=="South Korea"|
                       region=="Hong Kong")
#Check the structure
str(dat.top4)


## -------------------------------------------------------------------------
#Convert the categorical features into factors, except for age (3rd column).
temp <- lapply(dat.top4[,-3],as.factor);
#Combine age to the newly converted factors
dat.top4 <- data.frame(age=dat.top4$age,temp); str(dat.top4)


## -------------------------------------------------------------------------
#Create the training and test datasets

set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(dat.top4$death, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.75,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset
trainData <- dat.top4[trainRowNum,]

# Step 3: Create the test dataset
testData <- dat.top4[-trainRowNum,]


## -------------------------------------------------------------------------
#Logistic regression modelling
mod.covid.lg <- glm(death~., family="binomial", data=trainData);
summary(mod.covid.lg)  #Summarise the model


## -------------------------------------------------------------------------
ggplot(dat.top4,aes(x=death,y=age,colour=death,fill=death)) +
  geom_boxplot(alpha=0.5,show.legend=FALSE) +
  theme_minimal(base_size=14) +
  labs(x="Death","Age in Years")


## -------------------------------------------------------------------------
freq <- table(trainData$region,trainData$death); freq
prop <- freq %>% prop.table(1); prop %>% round(digit=3)

## -------------------------------------------------------------------------
#predicted probability of death on the test data
pred.prob <- predict(mod.covid.lg,new=testData,type="response")
pred.class <- ifelse(pred.prob>0.5,"Yes","No")

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.lg <- table(pred.class %>% as.factor %>% relevel(ref="Yes"),
               testData$death %>% as.factor %>% relevel(ref="Yes"));

prop <- prop.table(cf.lg,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.lg);


## -------------------------------------------------------------------------
lambdas <- 10^seq(-3,3,length=100) #A sequence 100 lambda values

set.seed(1)
mod.covid.ridge <- train(death ~., #Formula
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
mod.covid.ridge$bestTune


## -------------------------------------------------------------------------
# Model coefficients
coef(mod.covid.ridge$finalModel, mod.covid.ridge$bestTune$lambda)


## -------------------------------------------------------------------------
#predicted probability of death on the test data
pred.class.ridge <- predict(mod.covid.ridge,new=testData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.ridge <- table(pred.class.ridge %>% as.factor %>% relevel(ref="Yes"),
                  testData$death %>% as.factor %>% relevel(ref="Yes"));

prop <- prop.table(cf.ridge,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.ridge)


## -------------------------------------------------------------------------
set.seed(1)
mod.covid.LASSO <- train(death ~., #Formula
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
mod.covid.LASSO$bestTune


## -------------------------------------------------------------------------
# Model coefficients
coef(mod.covid.LASSO$finalModel, mod.covid.LASSO$bestTune$lambda)


## -------------------------------------------------------------------------
#predicted probability of death on the test data
pred.class.LASSO <- predict(mod.covid.LASSO,new=testData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.LASSO <- table(pred.class.LASSO %>% as.factor %>% relevel(ref="Yes"),
                  testData$death %>% as.factor %>% relevel(ref="Yes"));

prop <- prop.table(cf.LASSO,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.LASSO)


## -------------------------------------------------------------------------
alphas <- seq(0.1,0.9,by=0.1); alphas  #A sequence of alpha values to test

set.seed(1)
mod.covid.elnet <- train(death ~., #Formula
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
mod.covid.elnet$bestTune


## -------------------------------------------------------------------------
# Model coefficients
coef(mod.covid.elnet$finalModel, mod.covid.elnet$bestTune$lambda)


## -------------------------------------------------------------------------
#predicted probability of death on the test data
pred.class.elnet <- predict(mod.covid.elnet,new=testData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.elnet <- table(pred.class.elnet %>% as.factor %>% relevel(ref="Yes"),
                  testData$death %>% as.factor %>% relevel(ref="Yes"));

prop <- prop.table(cf.elnet,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.elnet)
