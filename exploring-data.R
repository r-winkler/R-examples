# read csv
setwd("C:/Users/René Winkler/software/R-examples")
used.cars <- read.csv(file.path("data/usedcars.csv"), stringsAsFactors = FALSE)

# check data
head(used.cars)
str(used.cars)

# nummerical statistics
summary(used.cars)
mean(used.cars$price)
median(used.cars$price)
max(used.cars$price)
min(used.cars$price)
range(used.cars$price)
diff(range(used.cars$price))
IQR(used.cars$price) # Q3 -Q1
quantile(used.cars$price)
quantile(used.cars$price, probs = c(0.05,0.95))
quantile(used.cars$price, seq(from = 0, to = 1, by = 0.2))
var(used.cars$price)
sd(used.cars$price)

# categorical statistics
table(used.cars$color)
round(prop.table(table(used.cars$color)) * 100, digits = 1)

# visualize statistics
boxplot(used.cars$price, main = "Boxplot of Used Car Prices", ylab = "Price ($)")
hist(used.cars$price, main = "Histogram of Used Cars Prices", xlab = "Price ($)")

# bivariate scatterplot
plot(x = used.cars$mileage, y = used.cars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (miles)",
     ylab = "Used Car Price ($)")

install.packages("gmodels")
library("gmodels")
used.cars$conservative <- used.cars$color %in% c("Black","Gray","Silver","White")
table(used.cars$conservative)
CrossTable(x = used.cars$model, y = used.cars$conservative)
