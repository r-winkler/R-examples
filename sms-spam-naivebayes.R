# Install package only if it is not yet done
installPackage <- function(package.name) {
  isInstalled <- (c(package.name) %in% installed.packages()[,"Package"])
  if(!isInstalled) install.packages(package.name)
}

# Read csv
setwd("C:/Users/René Winkler/software/R-examples")
sms_raw <- read.csv(file.path("data/sms_spam.csv"), stringsAsFactors = FALSE)

# Explore data
head(sms_raw)
str(sms_raw)
table(sms_raw$type)

# Prepare and clean data
sms_raw$type <- factor(sms_raw$type)

installPackage('tm')
library('tm')
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
lapply(sms_corpus[1:4], as.character)
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
installPackage('SnowballC')
library('SnowballC')
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[1:4], as.character)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# all above steps can also be done in one operation
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# Get train and test data
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5559,]$type

# Plot wordcloud
installPackage('wordcloud')
library('wordcloud')
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# Plot spam and ham wordclouds
spam <- subset(sms_raw, type =='spam')
ham <- subset(sms_raw, type =='ham')
wordcloud(spam$text, max.words = 40, scale = c(3,0.5))
wordcloud(ham$text, max.words = 40, scale = c(3,0.5))

# Only words as features with at least 5 counts
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

# Convert counts to categorization
convert_counts <- function(x) {
  x <- ifelse(x>0,'Yes','No')
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# Train model
installPackage('e1071')
library('e1071')
sms_classfier <- naiveBayes(sms_train, sms_train_labels)

# Evaluate model performance
sms_test_pred <- predict(sms_classfier, sms_test)

installPackage('gmodels')
library('gmodels')
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

# Improve model performance
# Use Laplace estimator
sms_classfier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classfier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
