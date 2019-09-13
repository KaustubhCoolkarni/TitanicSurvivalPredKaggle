# Get set working directory
getwd()
setwd("C:/Users/Kaustubh Coolkarni/Desktop/Kaggle Datasets/Titanic")

#import necessary librery

library(readr)
library(randomForest)
library(caret)

#Read training and test data

Train_data = read.csv('train.csv', header = TRUE)
Test_data = read.csv('test.csv', header = TRUE)

dim(Train_data)
dim(Test_data)

colnames(Train_data)



Test_data$Survived = NA

Total_data = rbind(Train_data,Test_data)

dim(Total_data)
View(Total_data)
str(Total_data)


sum(is.na(Total_data))
apply(Total_data,2,function(x) sum(is.na(x)))


install.packages('RANN')
library(RANN)
impute_missing <- preProcess(Total_data, method = "bagImpute")
Total_data <- predict(impute_missing, Total_data)
apply(Total_data,2,function(x) sum(is.na(x)))
str(Total_data)

Total_data$Survived[892:1309] <- NA

table(Total_data$Embarked)
str(Total_data$Embarked)
Total_data$Embarked = as.character(Total_data$Embarked)

Total_data$Embarked[Total_data$Embarked == ""] = 'S'

# Adding Family variable
Total_data$Family <- 1 + Total_data$SibSp + Total_data$Parch

# Adding Stage variable
Total_data$Stage[Total_data$Age < 18] <- "Child"
Total_data$Stage[Total_data$Age >= 18] <- "Adult"
str(Total_data)

boxplot(Total_data$)
# Adding Title variable
# But first we have to transform the Name into character, and separe it. And then, remove the variable Name.

Total_data$Name <- as.character(Total_data$Name)

Total_data$Title <- sapply(Total_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
Total_data$Title <- sub(' ', '', Total_data$Title)

Total_data$Title[Total_data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
Total_data$Title[Total_data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
Total_data$Title[Total_data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

View(Total_data)

#remove Name column now
Total_data$Name = NULL
colnames(Total_data)
str(Total_data)

#Changing the datatype of Survived,Pclass,Embarked,Stage and Title

Total_data$Survived <- as.factor(Total_data$Survived)
Total_data$Pclass <- factor(Total_data$Pclass, ordered = TRUE, levels = c('1','2','3'))
Total_data$Embarked <- as.factor(Total_data$Embarked)
Total_data$Stage <- as.factor(Total_data$Stage)
Total_data$Title <- as.factor(Total_data$Title)

# Standerdize the num columns

#num_cols = c('Age','SibSp','Parch','fare',)
cat_cols = c('Pclass','Sex','Embarked','Stage','Title')

Total_data$Age = decostand(Total_data$Age,"standardize")
Total_data$SibSp = decostand(Total_data$SibSp,"standardize")
Total_data$Parch = decostand(Total_data$Parch,"standardize")
Total_data$Family = decostand(Total_data$Family,"standardize")
Total_data$Fare   = decostand(Total_data$Fare,"standardize")

Total_data$Ticket = NULL
Total_data$Cabin = NULL

#install.packages('dummies')  
library(dummies)

Total_data = dummy.data.frame(Total_data,names = cat_cols)
head(Total_data)

str(Total_data)

#Once I did the feature feature engineering with all data, I'm going to split it again in train and test.
#And use Random Forest to build the model.

#Model Prediction

dim(Test_data)

titanic_train = Total_data[1:891,]
titanic_test = Total_data[892:1309,]

#random forest for fitting
set.seed(123)
RF_Model = randomForest(Survived ~ .,titanic_train, importance = TRUE ,ntree = 1000)

#Predict 

titanic_pred = predict(RF_Model,newdata = titanic_test)

summary(titanic_pred)

#Prepare submission file

titanic_test$Survived = titanic_pred

submit = data.frame(PassengerId=892:1309,Survived=0)

submit$Survived = titanic_pred
#Generate CSV file

write.csv(submit,'gender_submission.csv',row.names = FALSE)
