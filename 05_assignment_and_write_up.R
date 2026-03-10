# Assignment 5
# Install packages and load libraries

# install.packages(c("gutenbergr", "tidyverse", "tidytext", "tidymodels"))

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(tidymodels)

# This comparison is designed to test whether the model captures
# Doyle’s individual stylistic fingerprint, rather than
# broader conventions of late-Victorian detective fiction.

options(gutenberg_mirror = "ftp://ftp.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/")

# 1. Build author corpora

# Doyle corpus (Sherlock Holmes texts)
doyle <- gutenberg_works(author == "Doyle, Arthur Conan") |>
  filter(str_detect(title, "Sherlock Holmes|Adventures|Memoirs|Return|Hound|Valley of Fear")) |>
  distinct(gutenberg_id, title, .keep_all = TRUE)

# Hornung corpus (Raffles stories)
hornung <- gutenberg_works(author == "Hornung, E. W. (Ernest William)") |>
  filter(str_detect(title, "Raffles|Cracksman|Amateur Cracksman")) |>
  distinct(gutenberg_id, title, .keep_all = TRUE)

# Green corpus
green <- gutenberg_works(author == "Green, Anna Katharine") |>
  filter(str_detect(title, "Leavenworth|Affair|Detective|Mystery")) |>
  distinct(gutenberg_id, title, .keep_all = TRUE)

# Wodehouse corpus
wodehouse <- gutenberg_works(author == "Wodehouse, P. G. (Pelham Grenville)") |>
  filter(str_detect(title, "Jeeves|Wooster|My Man Jeeves|Inimitable Jeeves|Right Ho")) |>
  distinct(gutenberg_id, title, .keep_all = TRUE)

# 2. Download texts from Project Gutenberg

doyle_texts <- gutenberg_download(doyle$gutenberg_id, meta_fields = "title") |>
  mutate(author = "Doyle")

hornung_texts <- gutenberg_download(hornung$gutenberg_id, meta_fields = "title") |>
  mutate(author = "Hornung")

green_texts <- gutenberg_download(green$gutenberg_id, meta_fields = "title") |>
  mutate(author = "Green")

wodehouse_texts <- gutenberg_download(wodehouse$gutenberg_id, meta_fields = "title") |>
  mutate(author = "Wodehouse")

# 2a. Add Doyle’s non-Holmes texts

# Doyle non-Holmes corpus
doyle_non_holmes <- gutenberg_works(author == "Doyle, Arthur Conan") |>
  filter(str_detect(title, "The Lost World|The Poison Belt|The Land Of Mist")) |>
  distinct(gutenberg_id, title, .keep_all = TRUE)

doyle_nh_texts <- gutenberg_download(doyle_non_holmes$gutenberg_id, meta_fields = "title") |>
  mutate(author = "Doyle")

# Combine all comparison authors (excluding non-Holmes texts)
texts_all <- bind_rows(doyle_texts, hornung_texts, green_texts, wodehouse_texts)

# 3. Tokenization and Cleaning

# Tokenization splits texts into individual word units.
# Punctuation and numeric tokens are removed to focus on lexical style.

tokens <- texts_all |>
  unnest_tokens(word, text) |>
  mutate(word = tolower(word)) |>
  filter(str_detect(word, "^[a-z']+$"))

# 4. Feature Engineering

# These features quantify stylistic tendencies:
#   • lexical diversity (vocabulary richness)
#   • average word length (lexical complexity)
#   • pronoun usage (narrative perspective and involvement)
#   • function word frequency (grammatical style)

features <- tokens |>
  group_by(title, author) |>
  summarise(
    total_words = n(),
    unique_words = n_distinct(word),
    lexical_diversity = unique_words / total_words,
    avg_word_length = mean(str_length(word)),
    prop_pronouns = mean(word %in% c("he","she","i","you","they","we")),
    prop_function_words = mean(word %in% stop_words$word)
  ) |>
  ungroup()

#5. Training vs Testing Data

# Training data consists of known author texts.
# Testing data includes Doyle’s non-Holmes works,
# which the model has not seen during training.

train_data <- features  

test_data <- doyle_nh_texts |>
  group_by(title) |>
  unnest_tokens(word, text) |>
  mutate(word = tolower(word)) |>
  filter(str_detect(word, "^[a-z']+$")) |>
  summarise(
    total_words = n(),
    unique_words = n_distinct(word),
    lexical_diversity = unique_words / total_words,
    avg_word_length = mean(str_length(word)),
    prop_pronouns = mean(word %in% c("he","she","i","you","they","we")),
    prop_function_words = mean(word %in% stop_words$word)
  ) |>
  mutate(author = "Doyle")

# 6. Train the Classification Model

# A multinomial logistic regression model is used
# to learn stylistic differences between authors.

author_model <- multinom_reg(mode = "classification") |>
  set_engine("nnet")

author_recipe <- recipe(author ~ lexical_diversity + avg_word_length +
                          prop_pronouns + prop_function_words,
                        data = train_data)

workflow_fit <- workflow() |>
  add_model(author_model) |>
  add_recipe(author_recipe) |>
  fit(data = train_data)

# 7. Predict Authorship Probabilities

# The model returns probabilities for each author.
# Higher probabilities indicate closer stylistic alignment.

pred <- predict(workflow_fit, test_data, type = "prob")
pred


#The results are likely driven by differences in lexical diversity, 
#word length, pronoun use, and function word frequency. 
#Doyle’s non-Holmes fiction appears to use a broader vocabulary and 
#different narrative structures than the tightly constrained Holmes 
#stories, leading to stylistic divergence from his own detective canon. 
#Similarities in pronoun patterns and function word usage suggest closer 
#grammatical alignment with Anna Katharine Green’s writing, indicating 
#that Doyle’s non-Holmes works adopt stylistic conventions common to 
#contemporary detective fiction rather than those distinctive of the Sherlock 
#Holmes series.

