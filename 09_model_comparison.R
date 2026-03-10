library(tidyverse)   
library(lme4)       
library(partykit)    
library(readr)       
library(dplyr)       

# Set the working directory to Documents
setwd(file.path(path.expand("~"), "Documents"))

# Check the current working directory
getwd()

# URL for the HWAT dataset
url_hwat <- "https://drupal-s3fs-prod.s3.eu-west-1.amazonaws.com/resources/hwat_2-13-24%20tsv.zip"

# Name of the downloaded zip file
zip_hwat <- "hwat_2-13-24_tsv.zip"

# Download the dataset
download.file(url_hwat, destfile = zip_hwat, mode = "wb")

# Unzip the contents into a local directory
unzip(zip_hwat, exdir = "hwat_data")

# Read the TSV file
hwat_data <- read_tsv("hwat_2-13-24.tsv")

# Create an age group variable based on YOB
hwat_data$age_group <- cut(
  hwat_data$Year_of_Birth,
  breaks = c(-Inf, 1932, Inf),
  labels = c("Older", "Younger"),
  ordered_result = TRUE
)

# Remove observations with missing occupation values
hwat_data <- hwat_data %>%
  drop_na(occ1)

# Create a occupation variable
hwat_data <- hwat_data %>%
  mutate(
    occ2 = factor(occ1, levels = c("blue_coll", "white_coll"))
  )

# Fit a mixed-effects logistic regression model predicting /hw/ vs /w/
hw_model <- glmer(
  dep_var ~ Type + pre_vc + age_group + occ2 +
    (1 | indiv) + (1 | word1),
  data = hwat_data,
  family = binomial
)

summary(hw_model)

# Fit a reduced model excluding age group to evaluate its contribution
hw_model_no_age <- glmer(
  dep_var ~ Type + pre_vc + occ2 +
    (1 | indiv) + (1 | word1),
  data = hwat_data,
  family = binomial
)

summary(hw_model_no_age)

# Fit a reduced model excluding preceding phonological context
hw_model_no_vowel <- glmer(
  dep_var ~ Type + age_group + occ2 +
    (1 | indiv) + (1 | word1),
  data = hwat_data,
  family = binomial
)

summary(hw_model_no_vowel)

# Assignment 9 write-up

# Two mixed-effects logistic regression models were compared to
# assess whether preceding phonological context (pre_vc)
# independently contributes to predicting the realization
# of /hw/ versus /w/. Both models included lexical type,
# age group, and occupation as fixed effects, along with
# random intercepts for speaker and word. The full model
# included pre_vc, while the reduced model excluded it.

# A likelihood-ratio test showed that including pre_vc did not
# significantly improve model fit (χ²(1) = 1.2, p = .27).
# Consistent with this result, the reduced model had a slightly
# lower AIC (1767.2) than the model including pre_vc (1768.0),
# indicating no clear advantage to retaining phonological context.

# These findings suggest that once social and lexical factors are
# accounted for, preceding phonological context does not make an
# independent contribution to predicting /hw/ realization.
# While vowel contexts appear to favor /hw/ in the descriptive
# data, this effect does not remain robust in the multivariate model.

# As with earlier analyses, the results should be interpreted
# with caution due to convergence warnings and uneven token
# distributions. Still, the model comparison helps clarify
# the relative influence of phonological versus social and
# lexical factors in conditioning /hw/ variation.

