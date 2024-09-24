#### Preamble ####
# Purpose: Simulate the data for analysis that adheres to the format of the Open Data Toronto victims of crimes data set
# Author: Huayan Yu
# Date: 24 September 2024
# Contact: huayan.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Libraries `tidyverse` installed

#### Workspace setup ####
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Simulate categorical data based on original levels (1000 rows)
REPORT_YEAR <- sample(c(2019, 2020, 2021, 2022, 2023), 1000, replace = TRUE) # from 2019 - 2023
CATEGORY <- sample(c("Crimes Against the Person", "Crimes Against Property", "Other Crimes"), 1000, replace = TRUE)
SUBTYPE <- sample(c("Assault", "Theft", "Robbery", "Fraud", "Other"), 1000, replace = TRUE)
ASSAULT_SUBTYPE <- sample(c("Assault Peace Officer", "Assault Resist Arrest", "Other", NA), 1000, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
SEX <- sample(c("M", "F", "U"), 1000, replace = TRUE)
AGE_GROUP <- sample(c("Youth", "Adult", "Senior"), 1000, replace = TRUE)
AGE_COHORT <- sample(c("15 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"), 1000, replace = TRUE)

# Assign weights for different factors affecting COUNT_
category_weights <- c("Crimes Against the Person" = 1.5,
                      "Crimes Against Property" = 1.2, 
                      "Other Crimes" = 0.9)
sex_weights <- c("M" = 1.6, "F" = 0.9, "U" = 1.1)
age_cohort_weights <- c("15 to 24" = 1.0, "25 to 34" = 1.2, "35 to 44" = 1.6, 
                        "45 to 54" = 1.5, "55 to 64" = 1.1, "65+" = 0.8)
subtype_weights <- c("Assault" = 1.5, "Theft" = 1.2, "Robbery" = 1.4, "Fraud" = 1.1, "Other" = 0.7)
year_weights <- c("2019" = 1.4, "2020" = 1.2, "2021" = 1.05, "2022" = 1.15, "2023" = 1.25) # U-curve for crimes

# Simulate the COUNT variable using a weights and introduce some random normal noises
base_count <- 20 * category_weights[CATEGORY] * sex_weights[SEX] * 
  age_cohort_weights[AGE_COHORT] * subtype_weights[SUBTYPE] * 
  year_weights[as.character(REPORT_YEAR)]
COUNT_ <- round(base_count + rnorm(1000, mean = 0, sd = 20))

# Ensure no negative counts
COUNT_[COUNT_ < 0] <- 0

# Combine into a data frame
simulated_data <- data.frame(
  REPORT_YEAR,
  CATEGORY,
  SUBTYPE,
  ASSAULT_SUBTYPE,
  SEX,
  AGE_GROUP,
  AGE_COHORT,
  COUNT_
)

# Test basic summary statistics and relationships
summary(simulated_data)
table(simulated_data$SEX)
table(simulated_data$AGE_COHORT, simulated_data$CATEGORY)

# Some example two-sample t-tests
# 1. Male vs female for COUNT_
t_test_sex <- t.test(COUNT_ ~ SEX, data = simulated_data %>% filter(SEX %in% c("M", "F")))
print(t_test_sex)

# 2. Crimes Against the person vs property
t_test_category <- t.test(COUNT_ ~ CATEGORY, data = simulated_data %>% filter(CATEGORY %in% c("Crimes Against the Person", "Crimes Against Property")))
print(t_test_category)

# 3. Younger people (15 to 24, 25 to 34) vs Older people (55 to 64, 65+)
younger_cohorts <- simulated_data %>% filter(AGE_COHORT %in% c("15 to 24", "25 to 34"))
older_cohorts <- simulated_data %>% filter(AGE_COHORT %in% c("55 to 64", "65+"))
t_test_age <- t.test(younger_cohorts$COUNT_, older_cohorts$COUNT_)
print(t_test_age)

# Write the csv data to project repository
write.csv(simulated_data, "data/analysis_data/toronto_crime_victims.csv", row.names = FALSE)