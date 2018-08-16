library(tidyverse)
library(MASS)
library(ISLR)
library(car)
library(psych)

# Find out more about the dataset that will be used
?Boston

# Open the editor window to directly edit the data if needed
fix(Boston)

# Check the column names 
names(Boston)

# Change from dataframe to tibble
Boston <- as.tibble(Boston)
Boston

# -----------------------------------------
# Simple Linear Regression
# -----------------------------------------

# Create the linear model based on the median value 
lm.boston <- lm(medv ~ lstat, data = Boston)

# Check the detailed summary of the linear model
summary(lm.boston)

# Find out the available information inside the linear model
names(lm.boston)

# Obtain the confidence interval for the coefficient estimates
confint(lm.boston)

# Use predict() to produce confidence and prediction interval of the y-value (medv)
predict(lm.boston, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.boston, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

# Plot the simple linear regression
plot(Boston$lstat, Boston$medv, xlab = "Lower Status of the Population (%)", 
     ylab = "Median Value of Homes ($1000)", pch = 20)
abline(lm.boston, col = "#ff007f", lwd = 2)

# Plot using ggplot2
Boston %>% 
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(color = "#ff007f", method = "lm") + 
  labs(x = "Lower Status of the Population (%)", y = "Median Value of Homes ($1000)") +
  theme_classic()

# Create diagnostic plots
par(mfrow = c(2, 2))
plot(lm.boston)

# Create residual plot 
par(mfrow = c(1, 1))
plot(predict(lm.boston), residuals(lm.boston), xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residual Plot")

# Create studentized residual plot
plot(predict(lm.boston), rstudent(lm.boston), xlab = "Fitted Values", 
     ylab = "Studentized Residuals", main = "Studentized Residual Plot")

# Use Leverage statistics to find influential point that will impact the linear regression
plot(hatvalues(lm.boston))
which.max(hatvalues(lm.boston))


# -----------------------------------------
# Multiple Linear Regression
# -----------------------------------------

# Create a multiple linear regression model
lm.boston2 <- lm(medv ~ lstat + age, data = Boston)

# Check the detailed summary
summary(lm.boston2)

# Create a multiple linear regression model using all 
lm.boston3 <- lm(medv ~ ., data = Boston)
summary(lm.boston3)

# Get the adjusted R-square from the summary table
summary(lm.boston3)$r.squared

# Find out other available commands
names(summary(lm.boston3))

# Compute the variance inflation factors using vif() from the car library
# VIF is used to assess multi-colinearity
vif(lm.boston3)

# Check the correlation of each variables with one another
pairs.panels(Boston)

# Remove the age variable from the linear model
lm.boston4 <- lm(medv ~ . - age, data = Boston)
summary(lm.boston4)

# -----------------------------------------
# Interaction Term
# -----------------------------------------

# If only interaction term, use lstat:age else use lstat*age
lm.boston5 <- lm(medv ~ lstat * age, data = Boston)
summary(lm.boston5)

# -----------------------------------------
# Non-linear transformation of predictors
# -----------------------------------------
lm.boston6 <- lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm.boston6)

# Use anova() to further quantify the extent to which the extent to which the quadratic fit is
# superior ot the ln
anova(lm.boston6, lm.boston)
