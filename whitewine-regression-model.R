# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
wine <- read.csv(file.path("data/whitewines.csv"), stringsAsFactors = TRUE)

str(wine)
hist(wine$quality)
summary(wine)


wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

installPackage('rpart')
library('rpart')

m.rpart <- rpart(quality ~ ., data=wine_train)
m.rpart
summary(m.rpart)

installPackage('rpart.plot')
library('rpart.plot')

rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# evaluate model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)

# improve model performance
installPackage('RWeka')
library('RWeka')

m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)

p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
