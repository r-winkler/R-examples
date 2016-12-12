# data from http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/

# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
wdbc <- read.csv(file.path("data/wdbc.csv"), stringsAsFactors = FALSE, header = FALSE)
names(wdbc)[1] <- "id"
names(wdbc)[2] <- "diagnosis"

# Check data
head(wdbc)
str(wdbc)

# Explore data
summary(wdbc)
round(prop.table(table(wdbc$diagnosis)) * 100, digits = 1)

# Prepare data
wdbc <- wdbc[-1] # get rid of ID
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"), labels = c("Benigns", "Malignent"))
head(wdbc)

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

wdbc_normalized <- as.data.frame(lapply(wdbc[2:31], normalize))
wdbc_normalized$diagnosis <- wdbc[,1]

# Create training and test data
installPackage('caret')
library('caret')
set.seed(122515)
inTrainRows <- createDataPartition(wdbc_normalized$diagnosis, p=.80, list=FALSE)
trainDataFiltered <- wdbc_normalized[inTrainRows,]
testDataFiltered <- wdbc_normalized[-inTrainRows,]
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
prop.table(table(trainDataFiltered$diagnosis))
wdbc.train.labels <- trainDataFiltered$diagnosis
wdbc.test.labels <- testDataFiltered$diagnosis

# Drop diagnosis column
wdbc_normalized <- wdbc_normalized[ , !(names(wdbc_normalized) %in% c("diagnosis"))] 
trainDataFiltered <- trainDataFiltered[ , !(names(trainDataFiltered) %in% c("diagnosis"))] 
testDataFiltered <- testDataFiltered[ , !(names(testDataFiltered) %in% c("diagnosis"))] 

# Train model
installPackage('class')
library('class')
k = floor(sqrt(nrow(trainDataFiltered)))
wdbc.test.pred <- knn(train = trainDataFiltered, test = testDataFiltered, cl= wdbc.train.labels, k = k)

# Evaluate model performance
installPackage('gmodels')
library('gmodels')
CrossTable(x = wdbc.test.labels, y=wdbc.test.pred, prop.chisq=FALSE)

# Improve model performance

# 1) Try z-score standarization
# All the code stays the same except 
# wdbc_normalized <- as.data.frame(scale(wdbc[-1]))
# But this approach does not lead to a better performance

# 2) Try other k
# play around, not significantly better


# 3) Increase training set
# 20/80 relation seems to be the best
