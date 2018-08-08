library(tidyverse)
library(forcats)

# Read the data
wbcd <- read_csv("Machine Learning with R/Data/wisc_bc_data.csv")

# Look into the data
wbcd

# Remove the id column
wbcd <- wbcd[-1]

#Check how many malignant and benign cases
wbcd %>% count(diagnosis)

#Change the 'diagnosis' variable from char to factor
wbcd <- wbcd %>% 
  mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")))

#Check proportionality of diagnosis
round(prop.table(table(wbcd$diagnosis)) * 100, 1)

#Check 3 features/variables only: radius_mean, area_mean, smoothness_mean
wbcd %>% select(radius_mean, area_mean, smoothness_mean) %>% summary(.)

#Create a min-max function
normalize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

#Check if function working as intended
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# Apply the normalize function on all the columns
# Method 1
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
# Method 2
wbcd_n1 <- wbcd %>% mutate_at(vars(2:31), funs(normalize))
# Method 3
wbcd_n2 <- wbcd %>% transmute_at(vars(-starts_with("diagnosis")), funs(normalize))

#Training set and test set
wbcd_train <- wbcd_n[1:469,]
wbcd_test  <- wbcd_n[470:569,]

#For training a k-NN model, we will need to store the label/classifier on a different variable
wbcd_train_labels <- wbcd %>% filter(row_number() <= 469) %>% select(diagnosis) %>% unlist()
wbcd_test_labels  <- wbcd %>% filter(row_number() > 469)  %>% select(diagnosis) %>% unlist()

#Load k-NN Models and use the knn function to train and test the kNN algorithm
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)


#Evaluating Model Performance
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#----Improving Model Performance----
#z-Score Standardization
wbcd_z <- wbcd %>% transmute_at(vars(-starts_with("diagnosis")), funs(scale))

#Check we got a correct score
summary(wbcd_z$area_mean)

#Perfom KNN 
wbcd_z_train <- wbcd_z[1:469,]
wbcd_z_test  <- wbcd_z[470:569,]

wbcd_z_train_labels <- wbcd[1:469, 1] %>% unlist(use.names = FALSE)
wbcd_z_test_labels  <- wbcd[470:569, 1] %>% unlist(use.names = FALSE)

# Use the same k as the original model (k = 21)
wbcd_z_pred <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_z_train_labels, k = 21)

# Use different values of k (k = 5)
wbcd_z_pred2 <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_z_train_labels, k = 5)

# Display the results
CrossTable(x = wbcd_z_test_labels, y = wbcd_z_pred, prop.chisq = FALSE)
CrossTable(x = wbcd_z_test_labels, y = wbcd_z_pred2, prop.chisq = FALSE)
