library(tidyverse)

sms_results <- read_csv("Machine Learning with R/Data/sms_results.csv", 
                        col_types = cols(
                          actual_type  = col_factor(levels = c("ham", "spam")),
                          predict_type = col_factor(levels = c("ham", "spam"))
                        ))

sms_results


# -------------------------------------------
# Kappa Statistics with Caret
# -------------------------------------------

# Use the built-in confusionMatrix() from caret for better performance evaluation
library(caret)

# Specify the class of interest with the positive parameter. 
# First parameter is the predict vector (data =  ), second is the actual (reference = )
myOutput <- confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")
myOutput

# Check for available command retrieval from the caret confusionMatrix()
names(myOutput)

# The kappa statistics is similar to the accuracy but it takes into account the possibility 
# of a correct prediction by chance/luck. Range from 0 to 1. 
myOutput$overall['Kappa']

# Create a CrossTable to help calculate the kappa statistics
CrossTable(sms_results$actual_type, sms_results$predict_type, prop.chisq = FALSE)

# pr_a is the accuracy
pr_a <- 0.9748

# pr_e is the probability that the chance alone would lead the predicted and actual values to match
# For binary classifier, row_1 * col_1 + row_2 * col_2.
# For our example: p(actual_ham) * p(pred_ham) + p(actual_spam) * p(pred_spam)
pr_e <- 0.868 * 0.888 + 0.132 * 0.112

# Calculate the kappa manually
kappa_manual <- (pr_a - pr_e) / (1 - pr_e)
kappa_manual


# -------------------------------------------
# Kappa Statistics with VCD
# -------------------------------------------

# vcd = Visualizing Categorical Data
library(vcd)

Kappa(table(sms_results$actual_type, sms_results$predict_type))


# -------------------------------------------
# Sensitivity, Specificity & Precision
# -------------------------------------------

# Sensitivity (also known as recall) is the proportion of actual positives that were correctly identified.
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = 'spam')
myOutput$byClass['Sensitivity']

# Specificity measures the rate of negative examples that were correctly classified. 
myOutput$byClass['Specificity']

# Precision is the proportion of positive identifications by the model that are correct
myOutput$byClass['Precision']


# -------------------------------------------
# F-measure/F-Score/F1
# -------------------------------------------

# F-measure combines precision and recall into a single number using 'harmonic mean'
# Harmonic mean is used for rates of change
# f_measure <- (2 * true_positive) / (2 * true_positive + false_positive + false_negative)
myOutput$byClass['F1']


# -------------------------------------------
# Visualizing Performance Trade-Offs
# -------------------------------------------

# The Receiver Operating Characteristic (ROC) curve is commonly used to examine the trade-off 
# between the dection of true positives, while avoiding the false positives.

library(ROCR)

# The ROC curve requires the probability of  getting 'TRUE' for each observation
pred <- prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)

# The AUC (area under the ROC curve) can be used to compare model performance.
# It ranges from 0.5 to 1.0
# Use the performance() to create the ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 3)

# Add line to show the baseline
abline(a = 0, b = 1, lwd = 2, lty = 2)

# Calculate the aur
perf_auc <- performance(pred, measure = "auc")
str(perf_auc)
