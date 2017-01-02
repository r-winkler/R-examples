# This example is taken from 
#
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Set working directory and import datafiles
setwd("C:/Users/René Winkler/software/R-examples")
train <- read.csv(file.path("data/titanic/train.csv"), stringsAsFactors = TRUE)
test <- read.csv(file.path("data/titanic/test.csv"), stringsAsFactors = TRUE)

# explore dataset
str(train)
table(train$Survived)
prop.table(table(train$Survived))
summary(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {length(x) - sum(x)})
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

summary(train$Fare)
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

installPackage('rpart')
library('rpart')
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
installPackage('rattle')
installPackage('rpart.plot')
installPackage('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

str(test)
str(train)
test$Survived <- NA
train$Child <- NULL
train$Fare2 <- NULL
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
table(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
                     
Prediction <- predict(fit, test, type = "class")

# create submit dataframe and generate csv
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
