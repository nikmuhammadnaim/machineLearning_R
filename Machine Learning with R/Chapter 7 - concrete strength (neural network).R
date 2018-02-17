# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#    Modeling the Strength of Concrete    
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
library(tidyverse)
library(psych)
library(caTools)

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
concrete_norm <- concrete %>% mutate_all(funs(normalize(.)))

# Check that the data have normalized
summary(concrete_norm$strength)
summary(concrete$strength)


# -----------------------------------------------------------------
# Training a model on the data
# -----------------------------------------------------------------








