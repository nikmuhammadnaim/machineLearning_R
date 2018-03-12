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

# In the summary ouput, rows refer to number of transactions. Density refers to proportion of nonzero matrix. 
# We use 'inspect()' function to look at the contents of the sparse matrix
inspect(groceries[45:50])

# Use the 'itemFrequency()' function to see the proportion of transaction that contains the item.
itemFrequency(groceries[,1:3])

# To visualize the data use 'itemFrequencyPlot()'. Use the parameter 'support' or 'topN' for filtering products
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)
itemFrequencyPlot(groceries, support = 0.1, col = "darkorange", main = "Items with at least 10% Support")

# It's also possible to visualize the whole sparse matrix. 
image(groceries[1:5])
image(groceries[1:100])
image(sample(groceries, 200))
image(groceries)


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules


# -----------------------------------------------------------------
# Evaluating Model Performace
# -----------------------------------------------------------------
# To see the high level overview of our association rules, we use the summary() function
# The lift column measures how much more likely one item or itemset is purchased relative to its typical rate of purchase.
# If the lift value is greater than 1, it implies that the itemset are found together more often. 
# A large lift value is a strong indicator that a rule is important. 
summary(groceryrules)

# Use the inpsect() function to check the details of our rule
# Interpretation example: 
# a) {pasta} => {whole milk}
#    >> This rule covers 0.6% of the transactions and is correct in 40% of purchases involving pasta 
# b) {potted plants} => {whole milk}
#    >> This rule covers 0.7% of the transactions and is correct in 40% of purchases involving potted plants
inspect(groceryrules[10:20])

# Some rules aren't very useful and doesn't seem to have a logical explanations.
# A common approach is to take the association rules and divide them into 3 categories:
# a) Actionable
# b) Trivial - Any rules that are obvious (e.g. {diapers} => {formula})
# c) Inexplicable


# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------
# We can sort our rule results with sort() built-in the arules package. 
inspect(sort(groceryrules, by = "lift")[1:5])

# To sort in increasing order
inspect(sort(groceryrules, by = "lift", parameterdecreasing = FALSE)[1:5])


# Taking subsets of association rules
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)


sodarules <- subset(groceryrules, lhs %in% "soda")
inspect(sodarules)

# Saving association rules to a file or data frame
write(groceryrules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# To view the file
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
