# Install and load packages

required_packages <- c("tidyverse", "fivethirtyeight", "janitor")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages)) install.packages(missing_packages)

library(tidyverse)
library(fivethirtyeight)
library(janitor)

# Load the comma survey dataset
data("comma_survey")

# Preview the data to get a sense of structure
head(comma_survey)

# Inspect variable types and layout
glimpse(comma_survey)

# Standardize column names 
comma_survey <- comma_survey |> clean_names()

# Generate a basic frequency table for Oxford comma awareness
comma_survey |> 
  count(heard_oxford_comma) |>
  mutate(percent = round(100 * n / sum(n), 1))

# The following visualizations were provided in the starter code.


# Visualization: Awareness of the Oxford comma
comma_survey |>
  ggplot(aes(x = heard_oxford_comma, fill = heard_oxford_comma)) +
  geom_bar() +
  labs(
    title = "Who’s Heard of the Oxford Comma?",
    x = "",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Visualization: Grammar preferences by education level
comma_survey |>
  ggplot(aes(x = education, fill = more_grammar_correct)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Grammar Preferences by Education Level",
    x = "Education",
    y = "Proportion of Respondents",
    fill = "More Grammatically Correct"
  ) +
  theme_minimal()

# Visualization: Perceived importance of proper grammar by gender
comma_survey |>
  ggplot(aes(x = care_proper_grammar, fill = gender)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(
    title = "Importance of Proper Grammar by Gender",
    x = "Level of Importance",
    y = "Number of Respondents",
    fill = "Gender"
  ) +
  theme_minimal()

# Explore how respondents wrote the example sentence
comma_survey |>
  count(write_following, sort = TRUE)

# Cross-tabulation: caring about the Oxford comma by prior awareness
comma_survey |>
  count(care_oxford_comma, heard_oxford_comma) |>
  group_by(heard_oxford_comma) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(x = heard_oxford_comma, y = percent, fill = care_oxford_comma)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Caring About the Oxford Comma by Awareness",
    x = "Had Heard of It Before",
    y = "Proportion of Respondents",
    fill = "Care Level"
  ) +
  theme_minimal()

# A simple descriptive check: average level of grammar care by education
comma_survey |>
  group_by(education) |>
  summarise(
    mean_care = mean(as.numeric(factor(care_proper_grammar)), na.rm = TRUE)
  ) |>
  ggplot(aes(x = reorder(education, mean_care), y = mean_care)) +
  geom_col(fill = "darkorchid") +
  coord_flip() +
  labs(
    title = "Do Education Levels Correlate with Caring About Grammar?",
    x = "Education Level",
    y = "Average Care Score (Higher = More Care)"
  ) +
  theme_minimal()

# Additional plot: caring about the Oxford comma by age
comma_survey |>
  count(care_oxford_comma, age) |>
  group_by(age) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(x = age, y = percent, fill = care_oxford_comma)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Caring About the Oxford Comma by Age",
    x = "Age",
    y = "Proportion of Respondents",
    fill = "Care Level"
  ) +
  theme_minimal()

# Load package for inspecting missing data
install.packages("naniar")
library(naniar)

# Summarize missingness across all variables
miss_var_summary(comma_survey)

# Research questions guiding the inferential analyses:
# 1. Is awareness of the Oxford comma associated with education level?
# 2. Do grammar preferences differ by education level?
# 3. Does perceived importance of proper grammar differ by gender?

# Preliminary data checks before statistical testing
library(dplyr)

# Verify independence by checking for repeated respondent IDs
comma_survey %>%
  count(respondent_id) %>%
  filter(n > 1)

# Inspect category distributions to identify sparse levels or ordering issues
comma_survey %>% count(gender)
comma_survey %>% count(age)
comma_survey %>% count(household_income)
comma_survey %>% count(education)
comma_survey %>% count(care_oxford_comma)
comma_survey %>% count(care_data)
comma_survey %>% count(care_proper_grammar)

# Check for inconsistencies or unexpected labels in categorical variables
unique(comma_survey$gender)
unique(comma_survey$age)
unique(comma_survey$household_income)
unique(comma_survey$education)
unique(comma_survey$care_oxford_comma)
unique(comma_survey$care_data)
unique(comma_survey$care_proper_grammar)


# RQ1: Oxford comma awareness by education

rq1_data <- comma_survey |>
  filter(!is.na(heard_oxford_comma),
         !is.na(education))

# Create contingency table
tab_rq1 <- table(rq1_data$heard_oxford_comma,
                 rq1_data$education)

# Chi-square test
rq1_chi <- chisq.test(tab_rq1)

# Check expected counts
rq1_chi$expected

# Effect size
library(effectsize)
rq1_v <- cramers_v(tab_rq1)

rq1_chi
rq1_v

# RQ2 Strategy A: Treat "Don't know" as NA

rq2_data_A <- comma_survey |>
  mutate(
    more_grammar_correct = ifelse(
      more_grammar_correct == "Don't know",
      NA,
      more_grammar_correct
    )
  ) |>
  filter(!is.na(more_grammar_correct),
         !is.na(education))

tab_rq2_A <- table(rq2_data_A$more_grammar_correct,
                   rq2_data_A$education)

rq2_chi_A <- chisq.test(tab_rq2_A)
rq2_v_A <- cramers_v(tab_rq2_A)

rq2_chi_A
rq2_v_A

# RQ3: Importance of proper grammar by gender

rq3_data <- comma_survey |>
  filter(!is.na(care_proper_grammar),
         !is.na(gender)) |>
  mutate(
    care_numeric = as.numeric(factor(care_proper_grammar,
                                     levels = c("Not at all", "Not much", "Somewhat", "Very much"),
                                     ordered = TRUE
    ))
  )

# Mann–Whitney U test
rq3_mw <- wilcox.test(care_numeric ~ gender,
                      data = rq3_data,
                      exact = FALSE)

# Effect size
rq3_r <- rank_biserial(care_numeric ~ gender,
                       data = rq3_data)

rq3_mw
rq3_r


sessionInfo()
