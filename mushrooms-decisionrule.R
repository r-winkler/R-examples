# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
mushrooms <- read.csv(file.path("data/mushrooms.csv"), stringsAsFactors = TRUE)
mushrooms$veil_type <- NULL

# Explore data
head(mushrooms)
str(mushrooms)
table(mushrooms$type)

# Train model
installPackage('RWeka')
library('RWeka')
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

# Evaluate model performance
summary(mushroom_1R)

# Improve model performance
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)
