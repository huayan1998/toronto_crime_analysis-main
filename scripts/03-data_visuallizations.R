#### Preamble ####
# Purpose: Generate the data visualizations and tables as in the paper.
# Author: Huayan Yu
# Date: 24 September 2024
# Contact: huayan.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Libraries `tidyverse` and `knitr` installed

#### Workspace setup ####
library(tidyverse)
library(knitr)

# Load the data
data <- read.csv("data/analysis_data/toronto_crime_victims_cleaned.csv")

#### Visualizations with ggplot2 ####
# Visualization 1: a line chart of victim subtypes by year (2014 - 2023) 
# Aggregate data by year and subtypes
category_vs_year <- data %>%
  group_by(REPORT_YEAR, SUBTYPE) %>%
  summarize(Total_Count = sum(COUNT_))

# Create line plot
vis1 <- ggplot(category_vs_year, aes(x=REPORT_YEAR, y=Total_Count, color=SUBTYPE)) +
  geom_line(linewidth=1) +
  labs(title="Victim Trends by Subtype Over the Years",
       x="Year", y="Total Victims", color="Crime Subtype") +
  theme_minimal()
vis1

# Visualization 2: a bar chart of assault victim distribution by sex and age group
# Aggregate data by sex and age group and filtered on assault subtype
assault_victim_by_sex_age <- data %>%
  filter(SUBTYPE == "Assault") %>%
  group_by(SEX, AGE_GROUP) %>%
  summarize(Total_Count = sum(COUNT_))

# Create bar plot
vis2 <- ggplot(assault_victim_by_sex_age, aes(x = SEX, y = Total_Count, fill = AGE_GROUP)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Assault Victim Distribution by Sex and Age Group",
       x = "Sex", y = "Total Victims",
       fill = "Age Group") +
  theme_minimal()
vis2

# Visualization 3: side-by-side boxplot for victims by age groups over year (last 4 years)
vis3 <- ggplot(data %>% filter(REPORT_YEAR >= 2020), 
               aes(x = AGE_GROUP, y = COUNT_, color = AGE_GROUP)) +
  geom_boxplot() +
  labs(title = "Victim Count Distribution by Age Cohort", 
       x = "Age Group", y = "Victim Count", color = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~ REPORT_YEAR)
vis3


#### Table creations with knitr ####
# Table 1: data summarization table
# Replace NA values with empty strings for the selected columns
summary_data <- summary(data[,c(3, 4, 6, 8)])
summary_df <- as.data.frame.matrix(summary_data)
summary_df[is.na(summary_df)] <- ""
kable(summary_df, caption = "Number of Records in Each Categorical Variable", row.names = FALSE)

# Table 2: proportion and its growth of victims by year and sex (last 4 years)
proportion_sex_year <- data %>%
  filter(REPORT_YEAR >= 2020) %>%
  group_by(REPORT_YEAR, SEX) %>%
  summarize(Total_Count = sum(COUNT_)) %>%
  group_by(REPORT_YEAR) %>%
  mutate(Proportion = round(Total_Count / sum(Total_Count), 3)) %>%
  arrange(SEX) %>%
  group_by(SEX) %>%
  mutate(Percentage_Change_Total = round((Total_Count - lag(Total_Count)) / lag(Total_Count) * 100, 2))

colnames(proportion_sex_year) <- c("Year", "Sex", "Total", "Proportion",
                                   "Pct Change Total")
proportion_sex_year[is.na(proportion_sex_year)] = 0
kable(proportion_sex_year, row.names=FALSE)

# Table 3: percentage change for the three age groups from 2020 to 2023.

victim_change <- data %>%
  filter(REPORT_YEAR %in% c(2020, 2023)) %>%
  group_by(REPORT_YEAR, AGE_GROUP) %>%
  summarize(Total_Victims = sum(COUNT_)) %>%
  spread(REPORT_YEAR, Total_Victims) %>%
  mutate(Pct_Change = round(((`2023` - `2020`) / `2020`) * 100, 2)) %>%
  select(AGE_GROUP, `2020`, `2023`, Pct_Change)
victim_change <- victim_change[c(1, 2, 4),] # remove the unknown category
colnames(victim_change) <- c("Age Group", "2020", "2023", "Percentage Change")
kable(victim_change, row.names=FALSE)