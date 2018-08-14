library(tidyverse)
library(gridExtra)

# Read the raw data
sms_raw <- read_csv("Machine Learning with R/Data/sms_spam.csv")

# Explore the data
glimpse(sms_raw)

# Convert the type into a factor
sms_raw$type <- factor(sms_raw$type)

# Check the ratio of type
table(sms_raw$type)

# dplyr
sms_raw %>% count(type)

# -----------------------------------------------------------------
# Data Preparation: Cleaning and Standardizing Text Data
# -----------------------------------------------------------------

library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

# Summary of specific corpus can be identified using the inspect() function
inspect(sms_corpus[1:2])

# To view the word content of the corpus we use as.character() or the $content
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
sms_corpus[[20]]$content

# We use tm_map to perform pre-processing steps. 
# To see available tm transformer
getTransformations()

# Make everything to lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean[[20]]$content

# Remove numbers because usually they are unique to individual senders (for reference see corpus 20)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean[[1]]$content
as.character(sms_corpus_clean[[20]])

# Remove stop words because they do not provide much useful information for machine learning
# To seee other available languages in the built-in function
?stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
as.character(sms_corpus_clean[[1]])
sms_corpus_clean[[20]]$content

# Remove punctuations
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean[[20]]$content

# Stemming
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean[[20]]$content

# Remove additional whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean[[20]]$content


# Compare before and after
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)


# -----------------------------------------------------------------
# Data Preparation: Splitting Text Documents into Words
# -----------------------------------------------------------------

# Final step is to split the message into individual components through a process called tokenization. 
# The DTM function will take a corpus and create a data structure called sparse matrix.
# The the rows are documents (SMS) and columns are the terms (words).
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# We can also use DTM built-in functions to clean-up our data if we did not do so beforehand.
sms_dtm_post <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# However, the data wil be a bit different due to stopwords
sms_dtm
sms_dtm_post


# -----------------------------------------------------------------
# Data Preparation: Creating Training and Test Datasets
# -----------------------------------------------------------------

# ---Random split 75/25---
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# ---Strata split---
library(caTools)

sms_dtm_tk <- removeSparseTerms(sms_dtm, 0.99) 
sms_tb <- as.tibble(as.matrix(sms_dtm_tk))
sms_tb <- sms_tb %>% mutate(label = sms_raw$type)
sms_tb %>% select(label)

set.seed(24)
spl <- sample.split(sms_tb$label, SplitRatio = 0.75)
table(spl)

sms_tb_train <- sms_tb %>% filter(spl == TRUE) 
sms_tb_test  <- sms_tb %>% filter(spl == FALSE)

# Double check that each have the same proportionality 
sms_tb_train %>% count(label) %>% mutate(prop = n/sum(n))
sms_tb_test %>% count(label) %>% mutate(prop = n/sum(n))


# -----------------------------------------------------------------
# Visualizing Text Data: Word Clouds
# -----------------------------------------------------------------

library(wordcloud)
?wordcloud
# min.freq specify the minimum number of times a word must appear in the corpus.
wordcloud(sms_corpus_clean, min.freq = 75, random.order = FALSE, scale = c(3, 0.2), colors = brewer.pal(9, "RdPu")[c(5,6,7,8,9)], random.color = FALSE)

# Given a vector of raw text strings, it will automatically apply common text preparation process before displaying the cloud
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wc_spam <- wordcloud(spam$text, min.freq = 25, scale = c(3.5, 0.25), random.order = FALSE, colors = brewer.pal(9, "Reds")[c(4,5,6,7,8,9)], random.color = FALSE)
wc_ham <- wordcloud(ham$text, max.words = 65, scale = c(3, 0.1), random.order = FALSE, colors = brewer.pal(9, "Greens")[c(5,6,7,8,9)], random.color = FALSE)
wordcloud(sms_raw$text, min.freq = 50, random.order = FALSE, scale = c(3, 0.25))


# -----------------------------------------------------------------
# Data Preparation: Creating Indicator Features for Frequent Words
# -----------------------------------------------------------------

# The final step in the data preparation is to transform the sparse matrix into a data structure that 
# can be used to train a Naive Bayes classifier. We will eliminate any word that appear in less than 5
# SMS messages
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test  <- sms_dtm_test[, sms_freq_words]

# Naive Bayes classifier is typically trained on data with categorical features. This poses a problem, since 
# the cells in the sparse matrix are numeric and measure the number of times a word appears in a message. 
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, 2, convert_counts)


# -----------------------------------------------------------------
# Training a Model on the Data
# -----------------------------------------------------------------

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# -----------------------------------------------------------------
# Evaluating Model Performance
# -----------------------------------------------------------------

sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c("predicted", "actual"))
