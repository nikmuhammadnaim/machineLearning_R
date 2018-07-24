library(tidyverse)
library(h2o)

# Connect to H2O cluster 
h2o.init(nthreads = -1)

# Define the path
base_path <- normalizePath("H2O Tutorial/Data/")
flights_path <- str_c(base_path, "flights.csv")
weather_path <- str_c(base_path, "weather.csv")

# Read the data into H2O
flights <- h2o.importFile(path = flights_path, destination_frame = "flights")
weather <- h2o.importFile(path = weather_path, destination_frame = "weather")