# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
insurance <- read.csv(file.path("data/insurance.csv"), stringsAsFactors = TRUE)

str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)

# correlation matrix
cor(insurance[c("age","bmi","children", "expenses")])

pairs(insurance[c("age","bmi","children", "expenses")])

installPackage('psych')
library('psych')
pairs.panels(insurance[c("age","bmi","children", "expenses")])

# calculate model (automatic dummy coding)
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insurance)
# ins_model <- lm(expenses ~ ., data = insurance)

ins_model

# evaluate model performance
summary(ins_model)

# improve model performance
# add non-linear term for age
# create indicator for obsesity
# specify interaction between smoker and bmi30
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + bmi30 + bmi30*smoker + sex + smoker + region, data = insurance)
summary(ins_model2)
