# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#    Identifying Frequently Purchased Groceries with Association Rules        
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
library(tidyverse)
library(arules)

# -----------------------------------------------------------------
# Exploratory Data Analysis
# -----------------------------------------------------------------
# We will need to use sparse matrix for this type of datasets and problem. 
# We can use  the function from arules package to accomplish this. 
groceries <- read.transactions("Machine Learning with R/Data/groceries.csv", sep = ",")
groceries %>% summary()

# In the summary ouput, rows refer to number of transactions. Desity refers to proportion of nonzero matrix. 