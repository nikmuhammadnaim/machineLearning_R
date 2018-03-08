# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#    Identifying Frequently Purchased Groceries with Association Rules        
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
library(tidyverse)
library(arules)

# -----------------------------------------------------------------
# Exploratory Data Analysis
# -----------------------------------------------------------------
# We will need to use sparse matrix for this type of datasets and problem. 
# We can use  the function 'read.transactions' from arules package to accomplish this. 
groceries <- read.transactions("Machine Learning with R/Data/groceries.csv", sep = ",")
groceries %>% summary()

# In the summary ouput, rows refer to number of transactions. Desity refers to proportion of nonzero matrix. 
# We use 'inspect()' function to look at the contents of the sparse matrix
inspect(groceries[45:50])

# Use the 'itemFrequency()' function to see the proportion of transaction that contains the item.
itemFrequency(groceries[,1:3])

# To visualize the data use 'itemFrequencyPlot()'. Use the parameter 'support' or 'topN' for filtering products
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# It's also possible to visualize the whole sparse matrix. 
image(groceries[1:5])
image(groceries[1:100])
image(sample(groceries, 100))


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules








