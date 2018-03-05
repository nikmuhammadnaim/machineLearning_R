# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Performing Optical Character Recognition (OCR)     
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

library(tidyverse)
library(caTools)


# -----------------------------------------------------------------
# Exploratory Data Analysis
# -----------------------------------------------------------------

myLetters <- read_csv("Machine Learning with R/Data/letterdata.csv")
myLetters %>% glimpse()
myLetters

# SVM learners require all features to be numeric,and that each feature is scaled to a fairly small interval.
# The R package that we will be using will help us to rescale each feature automatically.
summary(myLetters)


# The data has already been randomized
letters_train <- myLetters[1:16000, ]
letters_test  <- myLetters[16001:20000, ]


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------

# There are multiple different R packages that can be used for SVM. Among the top tier packages are:
# (a) e1071
# (b) klaR
# (c) kernlab
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier


# -----------------------------------------------------------------
# Evaluating Model Performace
# -----------------------------------------------------------------

letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter
table(agreement) %>% prop.table()


# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------

# Using a Radial Basis kernel "Gaussian"
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf) %>% prop.table()


# Using ANOVA RBF kernel
# ---> WARNING: This will take a long time to train
letter_classifier_anova <- ksvm(letter ~ ., data = letters_train, kernel = "anovadot")
letter_predictions_anova <- predict(letter_classifier_anova, letters_test)

agreement_anova <- letter_predictions_anova == letters_test$letter
table(agreement_anova) %>% prop.table()


# Using Bessel kernel
letter_classifier_bessel <- ksvm(letter ~ ., data = letters_train, kernel = "besseldot")
letter_predictions_bessel <- predict(letter_classifier_bessel, letters_test)

agreement_bessel <- letter_predictions_bessel == letters_test$letter
table(agreement_bessel) %>% prop.table()







