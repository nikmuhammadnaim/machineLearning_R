library(tidyverse)
library(caret)
library(nnet)

# Load the dataset. We transform the target variable straight to factor
digits <- read_csv("R Deep Learning Projects/Data/mnist_train.csv",
                   col_types = cols(label = col_factor(levels = c(0:9))))

# Check the data
digits

# Check the structure of the data
glimpse(digits)

# Check the target label
unique(digits$label)

# --------------------------------------
# Exploratory Data Analysis
# --------------------------------------

# Plot out the image using the data. Image size is 28 * 28
# image() takes matrix or list. 
digits %>% 
  filter(row_number() == 8) %>% 
  select(-label) %>% 
  unlist(use.names = FALSE) %>% 
  matrix(nrow = 28, byrow = TRUE) %>% 
  image(col = grey.colors(255))

# Rotate the image 90-degree to the right to make it straight
rotate90 <- function(x) {
  t(apply(x, 2, rev))
}

# Look at another number
digits %>% 
  filter(row_number() == 21) %>% 
  select(-label) %>% 
  unlist(use.names = FALSE) %>% 
  matrix(nrow = 28, byrow = TRUE) %>%
  rotate90() %>% 
  image(col = grey.colors(255))

# Check the distribuition of the target variable
digits %>% count(label) %>% mutate(prop = n * 100 / sum(n))

# Explore the pixels, starting with the central 2*2 block
central_block <- c("pixel376", "pixel377", "pixel404", "pixel405")
par(mfrow = c(2,2))
for (i in 1:9) {
  hist(c(as.matrix(digits[digits$label == i, central_block])), 
       main = sprintf("Histogram for digit %d", i),
       xlab = "Pixel Value")
}

# --------------------------------------
# Logistic Regression
# --------------------------------------

# Split the data into 75% training and 25% testing using the createDataPartition()
set.seed(624)
train_index <- createDataPartition(digits$label, p = 0.75, list = FALSE)

# Training and testing dataset
digits_train <- digits[train_index, ]
digits_test  <- digits[-train_index, ]

# Use nnet library to train a logistic regresion model
digits_lr <- multinom(label ~ ., 
                      data = digits_train, 
                      MaxNWts = 10000, 
                      decay = 5e-3, 
                      maxit = 100)

prediction_lr <- predict(digits_lr, digits_test, type = "class")

prediction_lr %>% head()
digits_test$label %>% head()

# Confusion matrix
table(digits_test$label, prediction_lr)

accuracy_lr <- mean(prediction_lr == digits_test$label)
accuracy_lr
