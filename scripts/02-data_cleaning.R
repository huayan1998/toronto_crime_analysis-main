#### Preamble ####
# Purpose: Clean the raw data to make it ready for analysis
# Author: Huayan Yu
# Date: 24 September 2024
# Contact: huayan.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Library `tidyverse` installed

#### Workspace setup ####
library(tidyverse)

# Load the data
data <- read.csv("data/raw_data/toronto_crime_victims_raw.csv")

# Remove unnecessary columns and clean missing records
data <- data %>% select(-ASSAULT_SUBTYPE) %>% na.omit()

# Convert necessary columns to factors and ensure numeric for COUNT_
data$CATEGORY <- as.factor(data$CATEGORY)
data$SEX <- as.factor(data$SEX)
data$AGE_COHORT <- as.factor(data$AGE_COHORT)
data$AGE_GROUP <- as.factor(data$AGE_GROUP)
data$SUBTYPE <- as.factor(data$SUBTYPE)
data$COUNT_ <- as.numeric(data$COUNT_)

write.csv(data, "data/analysis_data/toronto_crime_victims_cleaned.csv")