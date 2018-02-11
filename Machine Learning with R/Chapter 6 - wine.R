# ----Numeric Prediction Trees: Regression Trees and Model Trees---
# Background:
# (a) Despite the name, regression trees do not use linear regression methods. 
# (b) They make predictions based on the average value of examples that reach a leaf.
# (c) Regression decision trees may be better suitted for tasks with many features and/or non-linear relationships.
# (d) Partitioning is done using the divide-and-conquer strategy according to the feature that will result in the
#     greatest increase in homogeneity in the outcome. 
# (e) For numeric decision trees, homogeneity is measured by statistics such as var, sd, or abs dev from the mean.
#     Common splitting criterion is called the Standard Deviation reduction (SDR).

library(tidyverse)
# ......SDR Example.....
# Original 
tee<- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)

# Split A
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)

# Split B
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# Compute the SDR for each split
sdr_a <- sd(tee) - (length(at1)/length(tee) * sd(at1) + length(at2)/length(tee)*sd(at2))
sdr_b <- sd(tee) - (length(bt1)/length(tee) * sd(bt1) + length(bt2)/length(tee) * sd(bt2))

# Compare
sdr_a
sdr_b
#...

# (f) Since the SDR is smaller (more homogeneous) in split B, the decision tree would use B first.
# (g) Suppose that the tree stopped growing here, the regression tree's work is done. 
# (h) One of the advantages of trees is that they can handle many types of data without preprocessing 
#     (no need to normalize or standardize)

# ----Example: Estimating the quality of wines with regression trees and model trees----
# -----------------------------------------------------------------
# Exploratory Data Analysis
# -----------------------------------------------------------------
white_wine <- read_csv("Machine Learning with R/Data/whitewines.csv")
white_wine %>% glimpse()

# Check the distribution of the quality
white_wine %>% 
  ggplot(aes(quality)) +
  geom_bar(fill = "light blue", color = "black") +
  ggtitle("Distribution of White Wine Quality") +
  theme(axis.title = element_text(face = "bold", size = 13),
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        axis.line.x = element_blank()) +
  labs(x = "Quality", y = "Count")



# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------

# Manual partitioning
white_wine_train <- white_wine %>% slice(1:3750)
white_wine_test <- white_wine %>% slice(3751:n())

# Strata sampling
library(caTools)
set.seed(123)
split <- sample.split(white_wine$quality, SplitRatio = 0.7656186)
wine_train <- white_wine %>% filter(split)
wine_test <- white_wine %>% filter(!split)

# Comparing the strata distribution with the complete dataset
white_wine %>% select(quality) %>% table() %>% prop.table() * 100
wine_train %>% select(quality) %>% table() %>% prop.table() * 100
white_wine_train %>% select(quality) %>% table() %>% prop.table() * 100
wine_test %>% select(quality) %>% table() %>% prop.table() * 100
white_wine_test %>% select(quality) %>% table() %>% prop.table() * 100

# Build our cart model
library(rpart)
m.rpart <- rpart(quality ~ ., data = white_wine_train)

my_model <- rpart(quality ~ ., data = wine_train)


# .....Interpreting the model.....
# The number of examples reaching each node is indicated after the splitting condition.
# Node --> split, n, deviance, yval.
# Deviance = maximizing the between group SSE (sum of sequares).
# The '*' indicates a terminal of leaf node, which means they result in a prediction.
m.rpart
my_model

# Visualizing decision trees
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)

# Basic plotting with rpart.plot. The fallen.leaves forces the leaf nodes to be alligned at the bottom of the plot
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
rpart.plot(my_model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Advance plotting with prp function
prp(main = "Estimating Quality of White Wine with Classification and Regression Trees (CART)",
    cex.main = 1.2,
    cex = 0.69,
    x = my_model,
    digits = 4,
    fallen.leaves = TRUE,
    type = 3,
    extra = 101,
    branch.lty = 2,                  # branch line type
    faclen = 0,
    split.suffix = "?",              # put "?" after split text
    split.box.col = "lightpink",     # split boxes color
    split.border.col = "black",      # split boxes border color
    split.round = 0.7,               # smooth the edges of split boxes
    shadow.col = "pink")



# -----------------------------------------------------------------
# Evaluating Model Performace
# -----------------------------------------------------------------
# We use predict() function to test our model which will return estimated numeric value for the outcome variable.
p.rpart <- predict(m.rpart, white_wine_test)
my_prediction <- predict(my_model, wine_test)

# The findings suggests that the model is not correctly identifying the extreme cases 
summary(p.rpart)
summary(white_wine_test$quality)
summary(my_prediction)
summary(wine_test$quality)

# Use correlation to measure the relationship between the predictions and the true value.
cor(p.rpart, white_wine_test$quality)
cor(my_prediction, wine_test$quality)


# .....Measuring performance with the mean absolute error.....
# (a) Another way to think about the model's performance is to consider how far (avg) its prediction from true value.
# (b) This measurement is called the mean absolute error (MAE)

#Create MAE function
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, white_wine_test$quality)
MAE(my_prediction, wine_test$quality)

# (c) The result implies that on average the difference between our model's performance and actual score is 0.587.
# (d) If we predict the mean value of our training sample, we wil be off by 0.67.
# (e) Cortez reported MAE of 0.58 for Neural Network and 0.45 for SVM
MAE(mean(white_wine_train$quality), white_wine_test$quality)



# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------
# A model tree improves on regression trees by replacing the leaf nodes with regression models
# The current state-of-the-art in model trees is the M5' algorithm (M5-prime)
library(RWeka)
m.m5p <- M5P(quality ~ ., data = white_wine_train)
m.m5p

my_m5p <- M5P(quality ~ ., data = wine_train)
my_m5p

# The split can be very similar to the regression tree.
# A key difference, however is that the nodes terminate not in a numeric prediction, but a linear model. 
# The values can be interpreted exactly the same as multiple regression models. 
# Effects estimated by each linear model (LM1, LM2, etc.) apply only to wine samples reaching this node
# Use the summary() function for statistics of the model on the training data.
summary(m.m5p)
summary(my_m5p)

# Check model performance on the test dataset
p.m5p <- predict(m.m5p, white_wine_test)
my_m5p_test <- predict(my_m5p, wine_test)

MAE(white_wine_test$quality, p.m5p)
MAE(wine_test$quality, my_m5p_test)

# This is a slight improvement to the rpart model. 



# ---Conclusion---
# There are 2 forms of decision trees:
# a) Regression trees
# b) Model trees
