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
data <- read.csv("data/raw_data/toronto_crime_victims_raw.csv")

# Convert necessary columns to factors
data$CATEGORY <- as.factor(data$CATEGORY)
data$SEX <- as.factor(data$SEX)
data$AGE_COHORT <- as.factor(data$AGE_COHORT)
data$AGE_GROUP <- as.factor(data$AGE_GROUP)
data$SUBTYPE <- as.factor(data$SUBTYPE)
data$ASSAULT_SUBTYPE <- as.factor(data$ASSAULT_SUBTYPE)


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

# Visualization 2: a bar chart of assault victim distribution by sex
# Aggregate data by sex and filtered on assault subtype
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

# Visualization 3: side-by-side boxplot for victims by age groups over year (last 5 years)
vis3 <- ggplot(data %>% filter(REPORT_YEAR >= 2019), 
               aes(x = AGE_GROUP, y = COUNT_, color = AGE_GROUP)) +
  geom_boxplot() +
  labs(title = "Victim Count Distribution by Age Cohort", 
       x = "Age Group", y = "Victim Count", color = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~ REPORT_YEAR)
vis3


#### Table creations with knitr ####
# Table 0: data summarization table
# Replace NA values with empty strings for the selected columns
summary_data <- summary(data[,c(3, 4, 6, 8)])
summary_df <- as.data.frame.matrix(summary_data)
summary_df[is.na(summary_df)] <- ""
kable(summary_df, caption = "Number of Records in Each Categorical Variable", row.names = FALSE)

# Table 1: proportion of victims by category, year, and sex
proportion_sex_year <- data %>%
  filter(REPORT_YEAR >= 2019) %>%
  group_by(REPORT_YEAR, SEX) %>%
  summarize(Total_Count = sum(COUNT_)) %>%
  group_by(REPORT_YEAR) %>%
  mutate(Proportion = round(Total_Count / sum(Total_Count), 3))
kable(proportion_sex_year, caption = "Proportion of Victims by Category, Year, and Sex")


# Table 2: the top 5 most common subtypes of victims for each year
top_subtypes_by_year <- data %>%
  group_by(SUBTYPE, REPORT_YEAR) %>%
  summarize(Total_Count = sum(COUNT_)) %>%
  arrange(desc(Total_Count)) %>%
  group_by(REPORT_YEAR) %>%
  top_n(5, Total_Count)

# Reshape the data to have a column for each year
top_subtypes_wide <- top_subtypes_by_year %>%
  pivot_wider(names_from = REPORT_YEAR, values_from = Total_Count, values_fill = 0) %>%
  select(SUBTYPE, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

# Add a row for total and concatenate
total_row <- top_subtypes_wide %>%
  summarize(across(`2014`:`2023`, sum)) %>%
  mutate(SUBTYPE = "Total")
top_subtypes_with_total <- bind_rows(top_subtypes_wide, total_row)
kable(top_subtypes_with_total, caption = "Top 5 Most Common Victim Subtypes by Year")