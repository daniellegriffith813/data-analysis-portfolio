###### 0. Set the working directory and load packages and libraries   ######

setwd(file.path(path.expand("~"), "Documents"))

required_packages <- c("tidyverse", "jsonlite", "tidytext", "widyr")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) install.packages(missing_packages)

library(tidyverse)
library(jsonlite)
library(tidytext)
library(widyr)
library(ggplot2)


###### 1.  Read dataset from HuggingFace link into R, view dataset, filter cocktails to titles with "Zombie", filter cocktails to titles with "Zombie" and pull the recipe instructions, filter cocktails to titles with "Fizz a la Violette", pull the recipe for the cocktail titled "Fizz a la Violette"  ######

cocktails <- read_csv("https://huggingface.co/datasets/erwanlc/cocktails_recipe_no_brand/resolve/main/train.csv")


View(cocktails)


cocktails |> 
  filter(title == "Zombie")

cocktails |> 
  filter(title == "Zombie") |>
  pull(recipe) |>
  cat()


cocktails |> 
  filter(title == "Fizz á la Violette")

cocktails |> 
  filter(title == "Fizz á la Violette") |>
  pull(recipe) |>
  cat()


###### 2. Make cocktails equal to itself, but have any duplicate rows removed, locate and read "Zombie" row, In cocktails locate any missing recipe values with an empty string & split recipe text wherever it reads "Alternatively:", expand lists into multiple rows    ######

cocktails <- cocktails |>
  distinct()


cocktails |> 
  filter(title == "Zombie")



cocktails <- cocktails |>
  mutate(
    recipe = if_else(is.na(recipe), "", recipe),
    recipe = str_split(recipe, "Alternatively:")
    ) |>
  unnest(recipe)


cocktails |> 
  filter(title == "Fizz á la Violette")


cocktails |>
  count(title) |>
  filter(n > 1) |>
  summarise(number_with_alternatives = n())



###### 3. Clean the ingredient strings so they can be read as JSON and convert them into expanded individual rows and named to amount/ingredient columns and analyze ingredients.  ######

cocktails |> 
  filter(title == "Zombie") |>
  pull(ingredients) |>
  cat()

cocktails <- cocktails |>
  mutate(
    ingredients = str_replace_all(ingredients, "(?<=\\[)'", "\""),
    ingredients = str_replace_all(ingredients, "'(?=\\])", "\""),
    ingredients = str_replace_all(ingredients, ",\\s*'", ", \""),
    ingredients = str_replace_all(ingredients, "'(?=,)", "\"")
    )

cocktails |> 
  filter(title == "Zombie") |>
  pull(ingredients) |>
  cat()

cocktails <- cocktails |>
  mutate(ingredients = map(ingredients, fromJSON))

cocktails |>
  filter(title == "Zombie") |>
  pluck("ingredients", 1)

cocktails <- cocktails |>
  mutate(ingredients = map(ingredients, as_tibble))


cocktails |>
  filter(title == "Zombie") |>
  pluck("ingredients", 1)

cocktails |>
  filter(title == "Zombie")

cocktails <- cocktails |>
  unnest(ingredients)


cocktails <- cocktails |>
  rename(
    amount = V1,
    ingredient = V2
  )

cocktails |>
  filter(title == "Zombie") 


###### 4. Standardize ingredient names by converting the text into lowercase and remove punctuation and any special characters.  ######

cocktails |> 
  filter(title == "Zombie") |> 
  pull(ingredient)

cocktails <- cocktails |>
  mutate(ingredient = tolower(ingredient),
         ingredient = str_replace_all(ingredient, "[^a-z\\s]", ""))


cocktails |> 
  filter(title == "Zombie") |> 
  pull(ingredient)


###### 5. Create indicators for whether a cocktail contains a specific type of alcohol, based on ingredient text. Extract preparation method from recipe instructions.  ######

brown_terms <- c(
  "whisky", 
  "whiskey",     
  "scotch",                
  "bourbon",               
  "rye",                   
  "irish whiskey", 
  "irish whisky",
  "tennessee whiskey",
  "blended whiskey", 
  "blended whisky",
  "cognac", "brandy",
  "aged rum", 
  "gold rum", 
  "dark rum", 
  "amber rum"
)

cocktails <- cocktails |>
  group_by(title) |>
  mutate(
    contains_rum = any(str_detect(ingredient, "rum")),
    contains_vodka = any(str_detect(ingredient, "vodka")),
    contains_lemon_juice = any(str_detect(ingredient, "lemon juice")),
    contains_brown_liquor = any(str_detect(ingredient, str_c(brown_terms, collapse = "|"))),
    method = case_when(
      str_detect(recipe, "\\bSHAKE\\b") ~ "SHAKEN",
      str_detect(recipe, "\\bSTIR\\b") ~ "STIRRED",
      TRUE ~ "OTHER")
  ) |>
  ungroup()

head(cocktails)




###### 6.Measure the frequency of each ingredient in the cocktails and identify the top 15 most common ingredients. Summarize dominant ingredients in dataset.    ######

top_ingredients <- cocktails |>
  count(ingredient, sort = TRUE) |>
  slice_head(n = 15)

top_ingredients

###### 7. Measure how often ingredient pairs appear across cocktails and identify combinations. Extract and count capitalized action verbs from recipes to analyze preparation patterns. ######

cooccur <- cocktails |>
  distinct(title, ingredient) |>
  pairwise_count(ingredient, title, sort = TRUE, upper = FALSE)

cooccur


actions <- cocktails |>
  mutate(actions = str_extract_all(recipe, "\\b[A-Z]{2,}\\b")) |>
  unnest(actions)


action_counts <- actions |>
  count(actions, sort = TRUE)

action_counts 

###### 9. Summarize the ingredients and cocktail data into summaries, and then calculate the ratio of cocktails with specific alcohol tyes. Also summarize glassware usage in dataset.  ######


summary_stats <- cocktails |>
  group_by(title) |>
  summarise(
    rum = any(contains_rum),
    vodka = any(contains_vodka),
    lemon_juice = any(contains_lemon_juice),
    brown_liquor = any(contains_brown_liquor),
    glass = first(str_to_lower(glass))
  )

summary_stats |>
  summarise(
    pct_rum = (sum(rum, na.rm = TRUE) / n()) * 100,
    pct_vodka = (sum(vodka, na.rm = TRUE) /n()) * 100,
    pct_lemon_juice = (sum(lemon_juice, na.rm = TRUE) / n()) * 100,
    pct_brown_liquor = (sum(brown_liquor, na.rm = TRUE) / n()) * 100
  )


summary_stats |>
  count(glass, sort = TRUE)


###### 11. In this step, gglpot function visualizes the data into a bar chart of the top ingredients and also a pie chart of the prep methods. The charts are then saved as images in to the working directory. ######

p1 <- ggplot(top_ingredients, aes(x = reorder(ingredient, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Ingredients in Cocktails",
       x = "Ingredient", y = "Count") +
  theme_minimal()

ggsave("Top_Ingredients.png", p1)


method_counts <- cocktails |>
  count(method, sort = TRUE)


p2<-ggplot(method_counts, aes(x = "", y = n, fill = method)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Cocktail Preparation Methods",
    fill = "Method"
  ) +
  theme_void(base_size = 14) +
  theme(legend.position = "right")

ggsave("Cocktail_Methods_Pie.png", p2)


###### 12. Reflection ######
#The most challenging step to interpret was Step 9. Trying to understand what the script was doing for the ratios was difficult for me to understand.
#The patterns in the data that stood out to me were how often lemon juice is used as an ingredient compared to actual alcohol.
# If I explored this dataset further, I would like to know the association with preparation methods for different alcohols. 
