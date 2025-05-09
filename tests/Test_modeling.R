#––– 1. Setup ----------------------------------------------------------------
# (Run this after sourced Modeling.R so final_model exists)
library(readr)      # read_csv
library(dplyr)      # data manipulation
library(lubridate)  # dates if needed
library(caret)      # dummyVars

#––– 2. Read & clean test data ----------------------------------------------
test_raw <- read_csv("test_data_IMDB_Fall_2024.csv")

# Drop identifiers & unused text columns
test_clean <- test_raw %>%
  select(-movie_title, -imdb_link, -plot_keywords)

#––– 3. Handle missing / recode ------------------------------------------------
# Language: fill “None” and binarize English vs non-English
test_clean <- test_clean %>%
  mutate(
    language = ifelse(language == "None", "English", language),
    language = ifelse(language == "English", "English", "non-English")
  )

#––– 4. Feature engineering ---------------------------------------------------
# Log‐transform numeric predictors exactly as in training
test_features <- test_clean %>%
  mutate(
    movie_budget_log        = log(movie_budget),
    duration_log            = log(duration),
    nb_news_articles_log    = log(nb_news_articles),
    actor1_star_meter_log   = log(actor1_star_meter),
    movie_meter_IMDBpro_log = log(movie_meter_IMDBpro)
  )

#––– 5. Create dummy variables ------------------------------------------------
# NOTE: We need the same factor columns and reference levels as in training.
# We’ll rebuild the caret dummyVars object here using our training factor levels.

# Define the factor columns we used:
factor_cols <- c("maturity_rating", "country", "continent",
                 "release_day_group", "day_of_week", "weekday_or_weekend")

# re-import our training cleaned_df here
imdb_processed <- read_excel("IMDb_processed.xlsx")

# dummyVars() with fullRank = TRUE to drop base levels :contentReference[oaicite:0]{index=0}:contentReference[oaicite:1]{index=1}
dv <- dummyVars(~ maturity_rating + country + continent +
                  release_day_group + day_of_week + weekday_or_weekend,
                data = imdb_processed, fullRank = TRUE)

test_dummies <- predict(dv, newdata = test_features) %>% as.data.frame()

#––– 6. Top‐10 flags for personnel (director, cinematographer, distributor) -----
# Load our lists of top_10 names from training:
top_dirs <- c(
  "Woody Allen", "Steven Spielberg", "Clint Eastwood",
  "Spike Lee", "Steven Soderbergh", "Martin Scorsese",
  "Barry Levinson", "Bobby Farrelly", "Francis Ford Coppola",
  "Joel Schumacher"
)

# Top 10 cinematographers
top_cinem <- c(
  "multiple", "Roger Deakins", "Mark Irwin", "John Bailey",
  "Andrew Dunn", "Jack N. Green", "Matthew F. Leonetti",
  "Robert Elswit", "Dean Cundey", "Don Burgess"
)

# Top 10 distributors
top_dist <- c(
  "Warner Bros.", "Universal Pictures", "Paramount Pictures",
  "Twentieth Century Fox", "Columbia Pictures Corporation",
  "New Line Cinema", "Buena Vista Pictures", "Miramax",
  "United Artists", "Metro-Goldwyn-Mayer (MGM)"
)

test_flags <- test_features %>%
  transmute(
    top_director        = as.integer(director       %in% top_dirs),
    top_cinematographer = as.integer(cinematographer %in% top_cinem),
    top_distributor     = as.integer(distributor     %in% top_dist)
  )

#––– 7. Assemble model frame --------------------------------------------------
# Select only the columns our final_model expects:
#   movie_budget_log, poly(duration_log,2), nb_news_articles_log, 
#   movie_meter_IMDBpro_log, poly(actor1_star_meter_log,2),
#   maturity_rating.R, action, …, country.USA, top_* flags

# Numeric core:
core_nums <- test_features %>%
  select(movie_budget_log, duration_log, nb_news_articles_log,
         movie_meter_IMDBpro_log, actor1_star_meter_log)

# Genre & country: ensure these exist as 0/1 in test_features already
core_flags <- test_features %>%
  select(action, adventure, scifi, thriller, western, sport, horror,
         drama, war, crime, biography, comedy, fantasy, history, family,
         country.USA, maturity_rating.R)

# Combine everything
test_model_df <- bind_cols(
  core_nums,
  # apply polynomial terms manually:
  duration_log_2     = core_nums$duration_log^2,
  actor1_star_meter_2= core_nums$actor1_star_meter_log^2,
  core_nums["nb_news_articles_log"],
  core_nums["movie_meter_IMDBpro_log"],
  core_nums["movie_budget_log"],
  core_nums["actor1_star_meter_log"],
  core_flags,
  as.data.frame(test_dummies),
  test_flags
)

#––– 8. Predict -------------------------------------------------------------
predictions <- predict(final_model, newdata = test_model_df)

# Combine with movie identifiers
output <- test_raw %>%
  select(movie_title) %>%
  mutate(predicted_imdb = predictions)

print(output)
