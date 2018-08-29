# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#    Modeling the Strength of Concrete    
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

library(tidyverse)
library(psych)
library(caTools)
library(neuralnet)

# -----------------------------------------------------------------
# Exploratory Data Analysis
# -----------------------------------------------------------------

concrete <- read_csv("Machine Learning with R/Data/concrete.csv")
concrete %>% glimpse()

# Check if there's any variables that are highly correlated
pairs.panels(concrete)

# See how concrete strength increases with cement
concrete %>% 
  ggplot(aes(x = cement, y = strength)) +
  geom_point(color = "peru", alpha = 0.5) + 
  geom_smooth(color = "tomato") +
  ggtitle("Relationship between Cement amount (kg/m^3) with Concrete Strength") +
  labs(x = "Cement", y = "Strength") +
  theme(axis.title = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(),
        axis.text = element_text(size = 12, face = "bold"))

# Neural networks work best when the data are scaled to a narrow range aroud zero --> normalize our data.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize all columns
concrete_norm <- concrete %>% mutate_all(funs(normalize))
concrete_norm

# Check that the data have normalized
summary(concrete_norm$strength)
summary(concrete$strength)


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------

# Manual split
concrete_train <- concrete_norm[1:773, ]
concrete_test  <- concrete_norm[774:1030, ]

# Strata split
strata <- sample.split(concrete_norm$strength, SplitRatio = 0.75)
train  <- concrete_norm %>% filter(strata)
test   <- concrete_norm %>% filter(!strata)

# As we have learned in ISL, highly flexible model may result in different f function. Hence, the objective of strata
# split is to see if that is truly the case.

# We are going to use the neuralnet package. It is the most widely used and easy to understand neural network package.
# However, it's limited to a single layer of hidden nodes.
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                            data = concrete_train)
ann_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                      data = train)

# The topology of the neural network can be visualize using the plot() function
plot(concrete_model, rep = "best")
plot(concrete_model)
plot(ann_model)
plot(ann_model, rep = "best")


# There is one input node for each of the eight features, followed by a single hidden node and a single output node
# that predict the concrete strength. At the bottom of the plot, R reports the number of training steps and an error
# measure called the Sum of Squared Errors (SSE). A lower SSE indicates better predictive performance. 
# The bias terms (indicated by the nodes labeled with the number 1) is a numeric constants that allow the value at
# the indicated nodes to be shifted upward or downward, much like the intercept in a linear equation. 
# As expected, the two models have slightly different weight and bias. 

# -----------------------------------------------------------------
# Evaluating Model Performace
# -----------------------------------------------------------------

concrete_results <- compute(concrete_model, concrete_test[, 1:8])
ann_results <- compute(ann_model, test[, 1:8])

# The compute() function returns 2 components:
#   - $neurons    --> stores the neurons for each layer in the network.
#   - $net.result --> stores the predicted values.
predicted_strength <- concrete_results$net.result
ann_strength <- ann_results$net.result

# We cannot use confusion matrix to examine model accuracy. Instead, we must measure the correlation between our 
# predicted concrete strength and the true value. 
cor(predicted_strength, concrete_test$strength)
cor(ann_strength, test$strength)


# -----------------------------------------------------------------
# Improving Model Performance
# -----------------------------------------------------------------

# More complex topologies can learn more difficult concepts. We will now increase the hidden nodes to 5.
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
ann_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                        data = train, hidden = 5)

# Plot our improved neural network
plot(concrete_model2)
plot(ann_model2)
plot(concrete_model2, rep = "best")
plot(ann_model2, rep = "best")

# Evaluate the new model
concrete_results2 <- compute(concrete_model2, concrete_test[, 1:8])
predicted_strength2 <- concrete_results2$net.result
cor(predicted_strength2, concrete_test$strength)

ann_results2 <- compute(ann_model2, test[, 1:8])
ann_strength2 <- ann_results2$net.result
cor(ann_strength2, test$strength)

