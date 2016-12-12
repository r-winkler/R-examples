# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
credit <- read.csv(file.path("data/credit.csv"), stringsAsFactors = TRUE)

# Explore data
head(credit)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
table(credit$default)

# Get training data
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Train model
installPackage('C50')
library('C50')
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

# Evaluate model performance
credit_pred <- predict(credit_model, credit_test)
installPackage('gmodels')
library('gmodels')
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actaul default', 'predicted default'))

# Improve model
# Add boosting
credit_model10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_pred10 <- predict(credit_model10, credit_test)
summary(credit_model10)
CrossTable(credit_test$default, credit_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actaul default', 'predicted default'))

# Making mistakes more costlier
matrix_dimensions <- list(c('no','yes'), c('no','yes'))
names(matrix_dimensions) <- c('predicted', 'actual')
error_cost <- matrix(c(0,1,4,0), nrow=2, dimnames = matrix_dimensions)
error_cost
credit_model_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_pred_cost <- predict(credit_model_cost, credit_test)
summary(credit_model_cost)
CrossTable(credit_test$default, credit_pred_cost,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actaul default', 'predicted default'))
