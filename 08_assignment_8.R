
# Load the packages
library(tidyverse)   # data wrangling and plotting
library(lme4)        # mixed-effects models
library(partykit)    # conditional inference trees (ctree)
library(readr)       # reading .tsv files
library(dplyr)       # data manipulation

# Set working directory
setwd(file.path(path.expand("~"), "Documents"))

# Confirm the current working directory
getwd()

# Download the HWAT dataset

# URL for the zipped TSV file
url_hwat <- "https://drupal-s3fs-prod.s3.eu-west-1.amazonaws.com/resources/hwat_2-13-24%20tsv.zip"

# Local file name for the downloaded zip
zip_hwat <- "hwat_2-13-24_tsv.zip"

# Download the data
download.file(url_hwat, destfile = zip_hwat, mode = "wb")

# Unzip the contents into a folder
unzip(zip_hwat, exdir = "hwat_data")

# Read the TSV file into R as a tibble
hwat_data <- read_tsv("hwat_2-13-24.tsv")

# Inspect the structure of the dataset
glimpse(hwat_data)

# Data preparation and analysis

# Convert the dependent variable 
hwat_data$dep_var <- factor(hwat_data$dep_var, levels = c("w", "hw"))

# Fit a conditional inference tree to explore how year of birth predicts HWAT use
data_tree <- ctree(dep_var ~ Year_of_Birth, data = hwat_data)
plot(data_tree)

# Create an age group variable based on the split identified in the tree
hwat_data$age_group <- cut(
  hwat_data$Year_of_Birth,
  breaks = c(-Inf, 1932, Inf),
  labels = c("Older", "Younger"),
  ordered_result = TRUE
)

# Create a simplified occupation variable and remove missing values
hwat_data <- hwat_data %>%
  drop_na(occ1) %>%
  mutate(
    occ2 = factor(occ1, levels = c("blue_coll", "white_coll"))
  )

# Define a helper function to summarize HWAT rates by predictor
hw_predictor <- function(var_name) {
  hwat_data %>%
    group_by(.data[[var_name]]) %>%
    summarise(
      n = n(),
      hw_rate = mean(dep_var == "hw"),
      .groups = "drop"
    )
}

# Examine HWAT rates across potential predictors
hw_predictor("Type")
hw_predictor("pre_vc")
hw_predictor("edu1")
hw_predictor("gender")
hw_predictor("age_group")
hw_predictor("occ2")

# Based on these summaries:
# Type: HWAT is more common with content words than function words
# pre_vc: HWAT occurs more often after vowels than non-vowels
# edu1: little to no difference by education
# gender: little to no difference by gender
# age_group: older speakers use HWAT more frequently
# occ2: blue-collar speakers use HWAT more than white-collar speakers

# Fit a mixed-effects logistic regression model using the strongest predictors
hw_model <- glmer(
  dep_var ~ Type + pre_vc + age_group + occ2 +
    (1 | indiv) + (1 | word1),
  data = hwat_data,
  family = binomial
)

# Gender and education were excluded due to minimal differences in the descriptive analysis.
# The model produces convergence warnings, which may reflect data imbalance or model complexity.
# These warnings were investigated, but the model is retained because the predictors are theoretically and empirically motivated.

summary(hw_model)

# Overall, the model suggests that HWAT is more likely after content words and vowels,
# and that older speakers and those from blue-collar backgrounds are more likely to produce HWAT.
# The convergence warnings indicate that results should be interpreted cautiously,
# as they may reflect limitations or unevenness in the data.
