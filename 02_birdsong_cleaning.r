#############################
### 1. LOAD PACKAGES AND DATA
#############################

# Load necessary packages
library(readr)    # for reading delimited files
library(dplyr)    # for data manipulation
library(stringr)  # for string operations
library(tidyr)    # for splitting and reshaping

# Load dataset using the correct delimiter (pipe: "|")
df <- read_delim("british_birdsongs_metadata.txt", delim = "|", trim_ws = TRUE)

# Take a quick look at the dataset structure
glimpse(df)

#############################
### 2. CLEAN IRREGULARITIES
#############################

clean_df <- df %>%
  # Standardize column names to lowercase
  rename_with(str_to_lower) %>%
  
  # Trim whitespace and replace multiple spaces with single space
  mutate(across(where(is.character), ~ str_trim(.x))) %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "\\s+", " "))) %>%
  
  # Standardize text capitalization for key columns
  mutate(
    latin_name = str_to_lower(latin_name),
    english_cname = str_to_lower(english_cname),
    country = str_to_lower(country)
  ) %>%
  
  # Convert empty strings to NA
  mutate(across(where(is.character), ~ na_if(.x, "")))

#############################
### 3. ADD GENUS COLUMN
#############################

# Genus is the first word of latin_name
clean_df <- clean_df %>%
  mutate(genus = word(latin_name, 1))

#############################
### 4. SAVE CLEANED DATA
#############################

# Save as tab-delimited text file
write_tsv(clean_df, "cleaned_dataset.txt")

#############################
### 5. REQUIRED SUMMARY VALUES
#############################

# A. Number of tokens per genus
tokens_per_genus <- clean_df %>%
  group_by(genus) %>%
  summarise(n_tokens = n())

print(tokens_per_genus)

# B. Number of audio files that contain "call" in the description
# B. Number of audio files containing "call" in any text column
num_calls <- clean_df %>%
  select(where(is.character)) %>%              # check all text columns
  mutate(call_present = rowSums(across(everything(), ~ str_detect(.x, "call")), na.rm = TRUE)) %>%
  filter(call_present > 0) %>%
  nrow()

print(num_calls)


# C. Countries with greatest and fewest contributors
contributors_by_country <- clean_df %>%
  group_by(country) %>%
  summarise(n_contributors = n_distinct(who_provided_recording)) %>%
  arrange(desc(n_contributors))

print(contributors_by_country)

# Country with the greatest contributors
head(contributors_by_country, 1)

# Country with the fewest contributors
tail(contributors_by_country, 1)

# D. Total number of unique species
num_species <- clean_df %>%
  summarise(n_species = n_distinct(latin_name))

print(num_species)

# E. Total number of unique contributors
num_contributors <- clean_df %>%
  summarise(n_unique_contributors = n_distinct(who_provided_recording))

print(num_contributors)

#############################
### 6. REFLECTION
#############################

# In this assignment, I cleaned the dataset by standardizing column names,
# trimming whitespace, fixing inconsistent capitalization, and converting
# empty strings to NA values. I used readr for reading the pipe-delimited
# file, dplyr for data wrangling, stringr for string cleaning, and tidyr
# where needed for splitting text. Key steps included extracting the genus
# from latin_name and ensuring all text columns were clean for analysis.
# Remaining challenges include handling any unexpected characters in text
# fields and verifying completeness of contributor data. This cleaning
# process ensures accurate counts and summaries for downstream analysis.
