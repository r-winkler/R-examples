# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
launch <- read.csv(file.path("data/challenger.csv"), stringsAsFactors = TRUE)

### Simple regression

# y = a*x + b
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
a <- mean(launch$distress_ct) -b*mean(launch$temperature)

# correlation
cor(launch$temperature, launch$distress_ct)


### Multiple regression
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

reg(y = launch$distress_ct, x = launch[2])

reg(y = launch$distress_ct, x = launch[2:4])
