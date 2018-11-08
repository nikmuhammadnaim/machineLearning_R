library(tidyverse)
library(naniar)
library(UpSetR)

# Read the data
teens <- read_csv("Machine Learning with R/Data/snsdata.csv",
                  col_types = cols(
                    gradyear = col_factor(NULL),
                    gender = col_factor(NULL)
                  ))
teens


# -----------------------------------------------------------------
# Data Cleaning
# -----------------------------------------------------------------

# Check the structure of the data
teens %>% glimpse()

# Summary of the data
summary(teens)

# Find the number of missing values in the dataset
sum(is.na(teens))

# Visualize the missing values
vis_miss(teens, warn_large_data = FALSE)

gg_miss_upset(teens)

# Find number of rows with missing values
teens %>% filter_all(any_vars(is.na(.))) %>% nrow()

# Check the number of missing values in gender using table
teens %>% select(gender) %>% table(., useNA = "ifany")

# Replace unrealistic data with NA
teens <- teens %>% mutate(age = ifelse(between(age, 13, 20), age, NA))

# Create dummy variable for missing gender
teens <- teens %>% mutate(no_gender = if_else(is.na(gender), 1, 0),
                          female    = if_else(gender == "F" & !is.na(gender), 1, 0))

# ----- Imputing the missing values -----
# Imputing is a technique where we replace the missing values with a guess
# Find the average of graduation based on gradyear
aggregate(age ~ gradyear, teens, mean, na.rm = TRUE)

# Alternative ways to do aggregation
tapply(teens$age, teens$gradyear, function(x) mean(x, na.rm = TRUE))

teens %>% group_by(gradyear) %>% summarise(mean = mean(age, na.rm = TRUE))

# Group average across the data
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))

# Replace missing age according to the gradyear's average
teens <- teens %>% mutate(age = if_else(is.na(age), ave_age, age))


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------

# kmeans() function requires a data frame containing only numeric data and 
# a parameter specifying the desired number of clusters. 

# Alternative to normalizing the data, we can scale the data using z-score standardization. 
# z-score have means of 0 and standard deviation of 1. 

interests <- teens[5:40]

interests_z <- apply(interests, 2, scale) %>% as.tibble()

set.seed(2345)

teen_clusters <- kmeans(interests_z, 5)


# -----------------------------------------------------------------
# Evaluating Model Performance
# -----------------------------------------------------------------

# Check the size of each cluster
teen_clusters$size

# Check the center position
# The highest value in each group (e.g. basketball, footbal, etc.) means that the group have the
# highest interest in that topic. 
teen_clusters$centers

# Put the cluster result into the main data
teens$cluster <- teen_clusters$cluster

# Information for the first five rows
teens[1:5, c("cluster", "gender", "age", "friends")]

# Number of friends of each cluster
aggregate(friends ~ cluster, teens, mean)




