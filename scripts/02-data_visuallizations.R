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
# Aggregate data by year and category
category_vs_year <- data %>%
  group_by(REPORT_YEAR, CATEGORY) %>%
  summarize(Total_Count = sum(COUNT_))

# Create line plot
vis1 <- ggplot(category_vs_year, aes(x=REPORT_YEAR, y=Total_Count, color=CATEGORY)) +
  geom_line(linewidth=1) +
  labs(title="Crime Trends by Category Over the Years",
       x="Year", y="Total Crimes", color="Crime Category") +
  theme_minimal()
vis1

# Visualization 2: a bar chart of crime distribution by sex
# Aggregate data by sex and category
crime_by_sex <- data %>%
  group_by(SEX, CATEGORY) %>%
  summarize(Total_Count = sum(COUNT_))

# Create bar plot
vis2 <- ggplot(crime_by_sex, aes(x = SEX, y = Total_Count, fill = CATEGORY)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crime Distribution by Sex",
       x = "Sex", y = "Total Crimes",
       fill = "Crime Category") +
  theme_minimal()
vis2

# Visualization 3: side-by-side boxplot for crimes by age cohort
vis3 <- ggplot(data, aes(x = AGE_COHORT, y = COUNT_, fill = CATEGORY)) +
  geom_boxplot() +
  labs(title = "Crime Count Distribution by Age Cohort",
       x = "Age Cohort", y = "Crime Count", fill = "Category") +
  theme_minimal()
vis3

#### Table creations with knitr ####
# Table 1: proportion of crimes by category, year, and sex
crime_proportion_sex_year <- data %>%
  group_by(REPORT_YEAR, SEX) %>%
  summarize(Total_Count = sum(COUNT_)) %>%
  group_by(REPORT_YEAR) %>%
  mutate(Proportion = round(Total_Count / sum(Total_Count), 3))
kable(crime_proportion_sex_year, caption = "Proportion of Crimes by Category, Year, and Sex")


# Table 2: the top 5 most common subtypes of crimes for each year
top_subtypes_by_year <- data %>%
  group_by(SUBTYPE, REPORT_YEAR) %>%
  summarize(Total_Count = sum(COUNT_)) %>%
  arrange(desc(Total_Count)) %>%
  group_by(REPORT_YEAR) %>%
  top_n(5, Total_Count)

# Reshape the data to have a column for each year
top_subtypes_wide <- top_subtypes_by_year %>%
  pivot_wider(names_from = REPORT_YEAR, values_from = Total_Count, values_fill = 0) %>%
  select(SUBTYPE, `2019`, `2020`, `2021`, `2022`, `2023`)

# Add a row for total and concatenate
total_row <- top_subtypes_wide %>%
  summarize(across(`2019`:`2023`, sum)) %>%
  mutate(SUBTYPE = "Total")
top_subtypes_with_total <- bind_rows(top_subtypes_wide, total_row)
kable(top_subtypes_with_total, caption = "Top 5 Most Common Crime Subtypes by Year")