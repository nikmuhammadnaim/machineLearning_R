library(tidyverse)

credit <- read.csv("Machine Learning with R/Data/credit.csv")

credit

# -----------------------------------------------------------------
# Exploring and Preparing Data
# -----------------------------------------------------------------

# Check the structure of our data
credit %>% glimpse()

# Check some of the feature that is available for predictions
credit %>% count(checking_balance)

credit %>% count(savings_balance)

summary(credit$months_loan_duration)

# Check percentage of default
credit %>% select(default) %>% table() %>% prop.table()

# Create random training and test datasets via strata sampling
library(caTools)

set.seed(123)

strata <- sample.split(credit$default, SplitRatio = 0.9)

credit_train <- credit %>% filter(strata)

credit_test  <- credit %>% filter(!strata)

credit_train %>% select(default) %>% table() %>% prop.table()

credit_test %>% select(default) %>% table() %>% prop.table()


# -----------------------------------------------------------------
# Training Model on the Data
# -----------------------------------------------------------------
library(C50)

# Check the available setting for tuning the model
?C5.0Control


# Create a C5.0 model using default settings
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

#See the tree's decision
summary(credit_model)

# -----------------------------------------------------------------
# Evaluating Model Performance
# -----------------------------------------------------------------
library(gmodels)

# Evaluate C5.0 Performance
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Default', 'Predicted Value'))

# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------
# Adaptive boosting - rooted in the notion that by combining a number of weak learners,
# you can create a team that is much stronger than any of the learners alone. 
# The C5.0() function makes it easy to add boosting to our C5.0 decision tree. We simply 
# need to add an additional trials parameter indicating the number of seperate decision trees
# to use in the boosted team.
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost15 <- C5.0(credit_train[-17], credit_train$default, trials = 15)
credit_boost15
summary(credit_boost15)

credit_boost20 <- C5.0(credit_train[-17], credit_train$default, trials = 20)
credit_boost20
summary(credit_boost20)

# Test out the new boost model performance
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual", "Predicted"))

credit_boost_pred15 <- predict(credit_boost15, credit_test)
CrossTable(credit_test$default, credit_boost_pred15, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual", "Predicted"))

credit_boost_pred20 <- predict(credit_boost20, credit_test)
CrossTable(credit_test$default, credit_boost_pred20, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual", "Predicted"))

# -----------------------------------------------------------------
# Making Mistakes More Costlier Than Others
# -----------------------------------------------------------------
# The C5.0 algorithm allows us to assign a penalty to different types of errors, 
# in order to discourage trees from makin more costly mistakes. 
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))

names(matrix_dimensions) <- c("predicted", "actual")

matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)

# Create training model
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual", "Predicted"))


# -----------------------------------------------------------------
# Plot of Amount vs Default
# -----------------------------------------------------------------
credit %>% ggplot(aes(x = default, y = amount)) + geom_boxplot()


