
# 1. Load packages


library(jsonlite)  
library(dplyr)    
library(tidyr)     
library(stringr)   
library(readr)     

# Load the vot.json file
vot_data <- fromJSON("vot.json", flatten = TRUE)

# Inspect the data
glimpse(vot_data)


# 2. Tidy the data


vot_tidy <- vot_data %>%
  pivot_longer(
    cols = starts_with("vot_"),
    names_to = "repetition",
    values_to = "vot"
  ) %>%
  mutate(
    # Create phoneme column
    phoneme = str_sub(item, 1, 1),
    
    # Create participant_type column
    participant_type = ifelse(
      str_detect(participant, "^mono"),
      "monolingual",
      "bilingual"
    )
  )



# 3. Save the data


write_csv(vot_tidy, "vot_tidy.csv")


# 4. Summary


vot_summary <- vot_tidy %>%
  group_by(phoneme, participant_type, language) %>%
  summarise(
    mean_vot = mean(vot, na.rm = TRUE),
    sd_vot = sd(vot, na.rm = TRUE),
    min_vot = min(vot, na.rm = TRUE),
    max_vot = max(vot, na.rm = TRUE),
    .groups = "drop"
  )

print(vot_summary)


# 5. Reflection


# In this assignment, I tidied the VOT dataset by reshaping repeated measurements
# into a long format with pivot_longer(), so that each repetition appears as its
# own row. I created a phoneme column by extracting the first letter of the item
# and a participant_type column based on the participant name to identify
# monolingual vs bilingual speakers. I used dplyr functions such as mutate(),
# group_by(), and summarise() for computing mean, standard deviation, minimum, and maximum VOT values by phoneme,
# participant type, and language. Stringr was useful for text extraction and pattern detection. One
# challenge was ensuring the correct columns for repetitions were included and
# handling any missing VOT values with na.rm = TRUE.
