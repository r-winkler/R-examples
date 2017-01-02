# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
concrete <- read.csv(file.path("data/concrete.csv"), stringsAsFactors = FALSE)

str(concrete)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

installPackage('neuralnet')
library('neuralnet')

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model)


# evaluate model performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

# improve model performance
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
