# output hello world
print('hello world!')

# create array with values from 1 to 20 and assign it to variable x
x <- 1:20

# other assign using assign function
assign("match.score",300)

# calculate mean of x
mean(x)

# help [exact]
help(mean)
?mean

# help [search]
help.search("average")
??average

# function declaration
Counter <- function() {
  count <- 0
  repeat {
    count <- count + 1
    print(paste("counter =", count))
  }
}

# function call
Counter()
 
# list all installed packages
library()

# display information of installed packages
packages <- installed.packages()
View(packages)

# display loaded packages
search()

# load package
library(parallel)

# unload or detach package
detach(package:parallel, unload = TRUE)

# Read CSV
setwd("C:/Users/René Winkler/software/R-examples")
file <- file.path("data/sample.csv")
my.data <- read.csv(file, stringsAsFactors=FALSE)
str(my.data)
my.data

# read data
library(datasets)
data(package="datasets")
data(iris)
str(iris)
head(iris)

