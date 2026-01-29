#install.packages("tidyverse","caret","e1071","ggpubr")
library(tidyverse)
library(caret)
library(e1071)  #For naive Bayes modelling
library(ggpubr)  #For multiplots and themes


## -----------------------------------------------------------------------------
#read in the data
lens <- read.csv("lens.csv", header=TRUE, stringsAsFactors=TRUE);

#Re-order the levels of certain factors
lens$Age <- lens$Age %>% factor(levels=c("young","pre-presbyopic","presbyopic"))
lens$SpectaclePrescription <- lens$SpectaclePrescription %>% factor(levels=c("myope","hypermetrope"))
lens$RecommendedLenses <- lens$RecommendedLenses %>% factor(levels=c("none","soft","hard"))


## -----------------------------------------------------------------------------
n.features <- ncol(lens)-1; #number of features in the dataset.
contingency.tab <- list(length=n.features) #Initialise a list to store the contingency tables

#Create the contingency table between the outcome (Recommended Lenses) and each of the features.
for (I in 1:n.features)
{
  contingency.tab[[I]] <- table(lens[,I],lens$RecommendedLenses);
  contingency.tab[[I]] %>% print
}


## -----------------------------------------------------------------------------
#Naive Bayes modelling
NB.lens <- naiveBayes(RecommendedLenses~.,  #Formula
                      data=lens,  #Relevant data
                      laplace=1) #Laplace smoothing. Default is 0.
NB.lens


## -----------------------------------------------------------------------------
#A new patient
new1 <- data.frame(Age="young",
                   SpectaclePrescription="myope",
                   Astigmatism="no",
                   TearProductionRate="normal");

#Estimate the a-posteriori probability for each class based on the first two features
predict(NB.lens,newdata=new1[,1:2],type="raw")

#Predict the class based on the first two features
predict(NB.lens,newdata=new1[,1:2],type="class")


## -----------------------------------------------------------------------------
#Estimate the a-posteriori probability for each class
predict(NB.lens,newdata=new1,type="raw")

#Predict the class
predict(NB.lens,newdata=new1,type="class")


## -----------------------------------------------------------------------------
new2 <- data.frame(Age="young",
                   SpectaclePrescription="myope",
                   Astigmatism="no",
                   TearProductionRate="normal",
                   Gender="male");

#Estimate the a-posteriori probability for each class
predict(NB.lens,newdata=new2,type="raw")

#Predict the class
predict(NB.lens,newdata=new2,type="class")


## -----------------------------------------------------------------------------
Titanic <- read.csv("Titanic.csv", header=TRUE, stringsAsFactors=TRUE);

Titanic$Pclass <- Titanic$Pclass %>% factor(labels=c("Class1","Class2","Class3"))
Titanic$Survived <- Titanic$Survived %>% factor(labels=c("No","Yes"))
Titanic$Age <- Titanic$Age %>% cut(breaks=c(0,16,max(Titanic$Age,na.rm=TRUE)),
                                   labels=c("Child","Adult"))
str(Titanic)


## -----------------------------------------------------------------------------
set.seed(1)  #Set the random seed.

# Step 1: Get row numbers for the training data
trainRowNum <- createDataPartition(Titanic$Survived, #The outcome variable
                                   #proportion of data to form the training set
                                   p=0.80,
                                   #Don't store the result in a list
                                   list=FALSE);

# Step 2: Create the training  dataset and exclude the first 2 columns
train.ttn <- Titanic[trainRowNum,-c(1:2)]

# Step 3: Create the test dataset
test.ttn <- Titanic[-trainRowNum,-c(1:2)]


## -----------------------------------------------------------------------------
#Dimension of the training data
dim(train.ttn)

#Number of missing values
sum(is.na(train.ttn$Age))


## -----------------------------------------------------------------------------
#Naive Bayes modelling
NB.ttn <- naiveBayes(Survived~.,  #Formula
                      data=train.ttn,  #Relevant data
                      laplace=0) #Laplace smoothing. Default is 0.
NB.ttn


## -----------------------------------------------------------------------------
#Predict the class
pred.class <- predict(NB.ttn,newdata=test.ttn,type="class")

#Create the con
cf.ttn <- confusionMatrix(pred.class %>% relevel(ref="Yes"),
                          test.ttn$Survived %>% relevel(ref="Yes")); cf.ttn


## -----------------------------------------------------------------------------
data(iris) #Load the iris dataset
str(iris) #examine the structure of the iris data

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


## -----------------------------------------------------------------------------
#Sepal Length
g1 <- ggplot(train.iris, aes(x=Sepal.Length,fill=Species)) +
        geom_density(colour="white",size=1,alpha=0.5) +
        theme_pubclean()

#Sepal Width
g2 <- ggplot(train.iris, aes(x=Sepal.Width,fill=Species)) +
        geom_density(colour="white",size=1,alpha=0.5) +
        theme_pubclean()

#Petal Length
g3 <- ggplot(train.iris, aes(x=Petal.Length,fill=Species)) +
        geom_density(colour="white",size=1,alpha=0.5) +
        theme_pubclean()

#Petal Width
g4 <- ggplot(train.iris, aes(x=Petal.Width,fill=Species)) +
        geom_density(colour="white",size=1,alpha=0.5) +
        theme_pubclean()

ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,common.legend = TRUE)


## -----------------------------------------------------------------------------
NB.iris <- naiveBayes(Species~., data=train.iris); NB.iris


## -----------------------------------------------------------------------------
pred.class <- predict(NB.iris,
                      newdata=test.iris,
                      type="class")

#Confusion matrix
cf.iris <- confusionMatrix(pred.class,test.iris$Species); cf.iris
