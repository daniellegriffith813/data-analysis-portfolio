
#Load libraries

library(tidyverse)
library(tidytext)
library(ggwordcloud)


# Setup

fig_dir <- file.path("migrant_stories", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

migrants <- read_tsv(
  file.path("migrant_stories", "migrant_stories.tsv"),
  show_col_types = FALSE
)

migrants <- migrants |>
  mutate(
    country_or = str_squish(country_or),
    country_de = str_squish(country_de),
    gender     = tolower(gender),
    story      = str_squish(story)
  )


# Overview of dataset

nrow(migrants)

migrants |> distinct(country_or) |> nrow()
migrants |> distinct(country_de) |> nrow()

migrants |> 
  filter(is.na(country_or) | is.na(country_de))

migrants |> 
  distinct(conti_or, conti_de)


# Number of stories by continent
p_origin <- migrants |>
  count(conti_or, sort = TRUE) |>
  ggplot(aes(x = reorder(conti_or, n), y = n)) +
  geom_col() +
  coord_flip()

ggsave(
  file.path(fig_dir, "origin_continent_bar.png"),
  p_origin, width = 8, height = 6
)

# Number of stories per continent orgainized by destination
p_dest <- migrants |>
  count(conti_de, sort = TRUE) |>
  ggplot(aes(x = reorder(conti_de, n), y = n)) +
  geom_col() +
  coord_flip()

ggsave(
  file.path(fig_dir, "destination_continent_bar.png"),
  p_dest, width = 8, height = 6
)

# Creating heatmap
p_flow <- migrants |>
  count(conti_or, conti_de) |>
  ggplot(aes(conti_or, conti_de, fill = n)) +
  geom_tile()

ggsave(
  file.path(fig_dir, "continent_flow_heatmap.png"),
  p_flow, width = 8, height = 6
)

# Close and far migration
p_distance <- migrants |>
  mutate(distance = if_else(conti_or == conti_de, "close", "far")) |>
  count(distance) |>
  ggplot(aes(distance, n)) +
  geom_col()

ggsave(
  file.path(fig_dir, "close_far_migration.png"),
  p_distance, width = 6, height = 5
)


# Story lengths
story_lengths <- migrants |>
  mutate(word_count = str_count(story, "\\w+"))

story_lengths |> arrange(word_count) |> slice(1)
story_lengths |> arrange(desc(word_count)) |> slice(1)

# Tokenizing words
tokens <- migrants |>
  unnest_tokens(word, story) |>
  anti_join(stop_words, by = "word")

# Creating word cloud
p_wordcloud <- tokens |>
  count(word, sort = TRUE) |>
  slice_max(n, n = 200) |>
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15)

ggsave(
  file.path(fig_dir, "wordcloud.png"),
  p_wordcloud, width = 8, height = 6
)

# Creating bigrams
bigrams <- migrants |>
  unnest_tokens(bigram, story, token = "ngrams", n = 2) |>
  separate(bigram, c("w1", "w2"), sep = " ") |>
  filter(!w1 %in% stop_words$word,
         !w2 %in% stop_words$word) |>
  unite(bigram, w1, w2, sep = " ")

p_bigrams <- bigrams |>
  count(bigram, sort = TRUE) |>
  slice_max(n, n = 15) |>
  ggplot(aes(reorder(bigram, n), n)) +
  geom_col() +
  coord_flip()

ggsave(
  file.path(fig_dir, "top_bigrams.png"),
  p_bigrams, width = 8, height = 6
)


# Craeting destination sentiment analysis
dest_sentiment <- migrants |>
  unnest_tokens(word, story) |>
  inner_join(get_sentiments("bing"), by = "word") |>
  mutate(score = if_else(sentiment == "positive", 1, -1)) |>
  group_by(country_de) |>
  summarise(avg_sentiment = mean(score), n = n()) |>
  filter(n >= 5)

p_dest_sent <- dest_sentiment |>
  ggplot(aes(reorder(country_de, avg_sentiment), avg_sentiment)) +
  geom_col() +
  coord_flip()

ggsave(
  file.path(fig_dir, "destination_sentiment.png"),
  p_dest_sent, width = 8, height = 6
)

# Craeting origin sentiment chart
origin_sentiment <- migrants |>
  unnest_tokens(word, story) |>
  inner_join(get_sentiments("bing"), by = "word") |>
  mutate(score = if_else(sentiment == "positive", 1, -1)) |>
  group_by(country_or) |>
  summarise(avg_sentiment = mean(score), n = n()) |>
  filter(n >= 5)

p_origin_sent <- origin_sentiment |>
  ggplot(aes(reorder(country_or, avg_sentiment), avg_sentiment)) +
  geom_col() +
  coord_flip()

ggsave(
  file.path(fig_dir, "origin_sentiment.png"),
  p_origin_sent, width = 8, height = 6
)
