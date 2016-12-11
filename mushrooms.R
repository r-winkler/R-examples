# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
mushrooms <- read.csv(file.path("data/mushrooms.csv"), stringsAsFactors = TRUE, header = TRUE)

mushrooms.names <- names(mushrooms)[-1]

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(mushrooms$cap.shape)){
  mushrooms[paste("cap.shape", level, sep = ".")] <- ifelse(mushrooms$cap.shape == level, 1.001, 0)
}
for(level in unique(mushrooms$surface)){
  mushrooms[paste("cap.surface", level, sep = ".")] <- ifelse(mushrooms$cap.surface == level, 1.0009, 0)
}
for(level in unique(mushrooms$cap.color)){
  mushrooms[paste("cap.color", level, sep = ".")] <- ifelse(mushrooms$cap.color == level, 1.00091, 0)
}
for(level in unique(mushrooms$bruises)){
  mushrooms[paste("bruises", level, sep = ".")] <- ifelse(mushrooms$bruises == level, 1.00092, 0)
}
for(level in unique(mushrooms$odor)){
  mushrooms[paste("odor", level, sep = ".")] <- ifelse(mushrooms$odor == level, 1.00093, 0)
}
for(level in unique(mushrooms$gill.attachment)){
  mushrooms[paste("gill.attachment", level, sep = ".")] <- ifelse(mushrooms$gill.attachment == level, 1.00094, 0)
}
for(level in unique(mushrooms$gill.spacing)){
  mushrooms[paste("gill.spacing", level, sep = ".")] <- ifelse(mushrooms$gill.spacing == level, 1.00095, 0)
}
for(level in unique(mushrooms$gill.size)){
  mushrooms[paste("gill.size", level, sep = ".")] <- ifelse(mushrooms$gill.size == level, 1.00096, 0)
}
for(level in unique(mushrooms$gill.color)){
  mushrooms[paste("gill.color", level, sep = ".")] <- ifelse(mushrooms$gill.color == level, 1.00097, 0)
}
for(level in unique(mushrooms$stalk.shape)){
  mushrooms[paste("stalk.shape", level, sep = ".")] <- ifelse(mushrooms$stalk.shape == level, 1.00098, 0)
}
for(level in unique(mushrooms$stalk.root)){
  mushrooms[paste("stalk.root", level, sep = ".")] <- ifelse(mushrooms$stalk.root == level, 1.00099, 0)
}
for(level in unique(mushrooms$stalk.surface.above.ring)){
  mushrooms[paste("stalk.surface.above.ring", level, sep = ".")] <- ifelse(mushrooms$stalk.surface.above.ring == level, 1.0011, 0)
}
for(level in unique(mushrooms$stalk.surface.below.ring)){
  mushrooms[paste("stalk.surface.below.ring", level, sep = ".")] <- ifelse(mushrooms$stalk.surface.below.ring == level, 1.0012, 0)
}
for(level in unique(mushrooms$stalk.color.above.ring)){
  mushrooms[paste("stalk.color.above.ring", level, sep = ".")] <- ifelse(mushrooms$stalk.color.above.ring == level, 1.0013, 0)
}
for(level in unique(mushrooms$stalk.color.below.ring)){
  mushrooms[paste("stalk.color.below.ring", level, sep = ".")] <- ifelse(mushrooms$stalk.color.below.ring == level, 1.0014, 0)
}
for(level in unique(mushrooms$veil.type)){
  mushrooms[paste("veil.type", level, sep = ".")] <- ifelse(mushrooms$veil.type == level, 1.0015, 0)
}
for(level in unique(mushrooms$veil.color)){
  mushrooms[paste("veil.color", level, sep = ".")] <- ifelse(mushrooms$veil.color == level, 1.0016, 0)
}
for(level in unique(mushrooms$ring.number)){
  mushrooms[paste("ring.number", level, sep = ".")] <- ifelse(mushrooms$ring.number == level, 1.0017, 0)
}
for(level in unique(mushrooms$ring.type)){
  mushrooms[paste("ring.type", level, sep = ".")] <- ifelse(mushrooms$ring.type == level, 1.0018, 0)
}
for(level in unique(mushrooms$spore.print.color)){
  mushrooms[paste("spore.print.color", level, sep = ".")] <- ifelse(mushrooms$spore.print.color == level, 1.0019, 0)
}
for(level in unique(mushrooms$population)){
  mushrooms[paste("population", level, sep = ".")] <- ifelse(mushrooms$population == level, 1.00195, 0)
}
for(level in unique(mushrooms$habitat)){
  mushrooms[paste("habitat", level, sep = ".")] <- ifelse(mushrooms$habitat == level, 1.002, 0)
}

# drop non-dummy columns
mushrooms <- mushrooms[!(names(mushrooms) %in% mushrooms.names)]

# Check data
head(mushrooms)
str(mushrooms)

# Explore data
summary(mushrooms)
round(prop.table(table(mushrooms$class)) * 100, digits = 1)

# Create training and test data
installPackage('caret')
library('caret')
set.seed(122515)
inTrainRows <- createDataPartition(mushrooms$class, p=.80, list=FALSE)
trainDataFiltered <- mushrooms[inTrainRows,]
testDataFiltered <- mushrooms[-inTrainRows,]
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
prop.table(table(trainDataFiltered$class))
mushrooms.train.labels <- trainDataFiltered$class
mushrooms.test.labels <- testDataFiltered$class

# Drop 'class' column
mushrooms <- mushrooms[ , !(names(mushrooms) %in% c("class"))] 
trainDataFiltered <- trainDataFiltered[ , !(names(trainDataFiltered) %in% c("class"))] 
testDataFiltered <- testDataFiltered[ , !(names(testDataFiltered) %in% c("class"))] 

# Train model
installPackage('class')
library('class')
k = floor(sqrt(nrow(trainDataFiltered)))
mushrooms.test.pred <- knn(train = trainDataFiltered, test = testDataFiltered, cl= mushrooms.train.labels, k = 2)

# Evaluate model performance
installPackage('gmodels')
library('gmodels')
CrossTable(x = mushrooms.test.labels, y=mushrooms.test.pred, prop.chisq=FALSE)
