#### Preamble ####
# Purpose: Generate the data visualizations and tables as in the paper.
# Author: Huayan Yu
# Date: 24 September 2024
# Contact: huayan.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Libraries `tidyverse` and `knitr` installed

#### Workspace setup ####
library(ggplot2)
library(dplyr)
library(knitr)

# Load the data
data <- read.csv("data/analysis_data/toronto_crime_victims.csv")

# Convert necessary columns to factors
data$CATEGORY <- as.factor(data$CATEGORY)
data$SEX <- as.factor(data$SEX)
data$AGE_COHORT <- as.factor(data$AGE_COHORT)

#### Visualizations with ggplot2 ####
# Visualization 1: a line chart of crime categories by year (2019 - 2023) 
# Aggregate data by CATEGORY and REPORT_YEAR
category_trend <- data %>%
  group_by(REPORT_YEAR, CATEGORY) %>%
  summarize(Total_Count = sum(COUNT_))

# Create line plot
vis1 <- ggplot(category_trend, aes(x=REPORT_YEAR, y=Total_Count, color=CATEGORY)) +
  geom_line(linewidth=1) +
  labs(title="Crime Trends by Category Over the Years",
       x="Year", y="Total Crimes", color="Crime Category") +
  theme_minimal()
vis1

#### Table creations with knitr ####