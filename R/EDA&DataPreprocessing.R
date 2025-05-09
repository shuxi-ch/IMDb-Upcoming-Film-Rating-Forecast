## ----message=FALSE, warning=FALSE, paged.print=FALSE--------------------------
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(stringr) 
library(GGally)
library(psych)
library(lubridate)
library(igraph)
library(ggraph)
library(reshape2)


## -----------------------------------------------------------------------------
df <- read.csv("IMDB_data_Fall_2024.csv")


## -----------------------------------------------------------------------------
dim(df)


## -----------------------------------------------------------------------------
str(df)


## -----------------------------------------------------------------------------
summary(df)


## -----------------------------------------------------------------------------
df$movie_title <- NULL
df$imdb_link <- NULL
df$plot_keywords <- NULL

dim(df)


## -----------------------------------------------------------------------------
hist(df$imdb_score, main = "Distribution of IMDb Scores", xlab = "IMDb Score", col = "lightblue", breaks = 30)


## -----------------------------------------------------------------------------
boxplot(df$imdb_score, main = "Boxplot of IMDb Scores", ylab = "IMDb Score", col = "lightblue")


## -----------------------------------------------------------------------------
numerical_cols <- df[, c("imdb_score", "movie_budget", "release_day", "release_year", "duration", 
                         "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", 
                         "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")]

cor_matrix <- cor(numerical_cols, use = "pairwise.complete.obs")
# sort on decreasing correlations with imdb_score
cor_sorted <- cor_matrix[order(-cor_matrix[, "imdb_score"]), order(-cor_matrix["imdb_score", ])]
# plot it
corrplot.mixed(cor_sorted, tl.col = "black", tl.pos = "lt")


## -----------------------------------------------------------------------------
options(scipen=999)

ggpairs(df[c("imdb_score", colnames(numerical_cols))],
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = "barDiag"),
        progress = FALSE, message = FALSE, warnings = FALSE)


## -----------------------------------------------------------------------------
par(mfrow = c(5, 3), mar = c(2, 2, 2, 2))

genre_columns <- colnames(df)[grepl("action|adventure|scifi|thriller|musical|romance|western|sport|horror|drama|war|animation|crime", colnames(df))]

for (genre in genre_columns) {

  genre_counts <- table(df[[genre]])
  
  bar_heights <- barplot(genre_counts, main = paste("Distribution of", genre), col = "lightblue", ylim = c(0, max(genre_counts) * 1.1))
  
  text(x = bar_heights, y = genre_counts / 2, labels = genre_counts, cex = 0.8, col = "black", pos = 3)

}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


## -----------------------------------------------------------------------------
par(mfrow = c(3, 5), mar = c(3, 3, 3, 3)) 

for (genre in genre_columns) {
  boxplot(df$imdb_score ~ df[[genre]], 
          main = paste("IMDb Score by", genre), 
          ylab = "IMDb Score", 
          xlab = genre, 
          col = "lightblue", 
          las = 2)  # las = 2 makes the axis labels perpendicular to the axis
}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


## -----------------------------------------------------------------------------
for (genre in genre_columns) {
  correlation <- biserial(df$imdb_score, df[[genre]])
  print(paste(genre, ":", correlation))
}


## -----------------------------------------------------------------------------
colnames(df)[colSums(is.na(df)) > 0]


## -----------------------------------------------------------------------------
for (col in colnames(df)){
  na_checking = df[df[col] == "" | df[col] == "NaN" | df[col] == "None", ]
  if (nrow(na_checking) > 0) {
    print(col)
  }
}


## -----------------------------------------------------------------------------
df[df$language == "" | df$language == "NaN" | df$language == "None", ]


## -----------------------------------------------------------------------------
df$language[df$language == "None"] <- "English"
nrow(df[df$language == "" | df$language == "NaN" | df$language == "None", ]) # 0


## -----------------------------------------------------------------------------
ggplot(df, aes(x = language)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Languages", x = "Language", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
df$language[df$language != "English"] <- "non-English"


## -----------------------------------------------------------------------------
boxplot(imdb_score ~ language, data = df, main = "Boxplot of IMDb Scores by Language", 
        xlab = "Language Group", ylab = "IMDb Score", col = "lightblue")


## -----------------------------------------------------------------------------
table(df$language)


## -----------------------------------------------------------------------------
pairwise_results <- pairwise.t.test(df$imdb_score, df$language, p.adjust.method = "bonferroni")
print(pairwise_results)


## -----------------------------------------------------------------------------
df$genre_list <- strsplit(as.character(df$genres), "\\|")
head(df$genre_list)


## -----------------------------------------------------------------------------
unique_genres <- tolower(unique(unlist(df$genre_list)))
unique_genres


## -----------------------------------------------------------------------------
existing_genre_columns <- colnames(df)[colnames(df) %in% unique_genres]
existing_genre_columns


## -----------------------------------------------------------------------------
for (genre in unique_genres) {
  
  if (!(genre %in% existing_genre_columns)) {
    
    df[[genre]] <- sapply(df$genre_list, function(x) ifelse(str_to_title(genre) %in% x, 1, 0))
    
  }
}

df$genre_list <- NULL

head(df)


## -----------------------------------------------------------------------------
dim(df)


## -----------------------------------------------------------------------------
colnames(df)
# extracted genres: "biography", "comedy", "music", "fantasy", "history", "mystery", "family", "documentary"


## -----------------------------------------------------------------------------
genres_all <- c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                   "western", "sport", "horror", "drama", "war", "animation", 
                   "crime", "biography", "comedy", "music", "fantasy", 
                   "history", "mystery", "family", "documentary")

df_long <- pivot_longer(df, cols = all_of(genres_all), names_to = "genre", values_to = "genre_indicator")
df_long <- df_long[df_long$genre_indicator == 1, ]

head(df_long)


## -----------------------------------------------------------------------------
ggplot(df_long, aes(x = genre, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by Genre",
       x = "Genre",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
table(df_long$genre)


## -----------------------------------------------------------------------------
for (genre in genres_all) {
  # Welch's t-test
  t_test_results <- t.test(df$imdb_score[df[[genre]] == 1], df$imdb_score[df[[genre]] == 0])
  
  # Fetch and print insignificant genres (threshold: p-value = 0.05)
  if (t_test_results$p.value >= 0.05) {
    print(paste(genre, "- p-value:", t_test_results$p.value))
  }
}


## -----------------------------------------------------------------------------
grouped_genres <- c("animation", "documentary", "musical", "romance", "music", "mystery")

df$other_genre <- 0

for (col in grouped_genres) {
  df$other_genre[df[[col]] == 1] <- 1
}

drop_genres_cols <- c("genres", "animation", "documentary", "musical", "romance", "music", "mystery")

df[drop_genres_cols] <- NULL

colnames(df)


## -----------------------------------------------------------------------------
ggplot(df, aes(x = colour_film, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by colour_film",
       x = "colour_film",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
ggplot(df, aes(x = maturity_rating, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by maturity_rating",
       x = "maturity_rating",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
table(df$maturity_rating)


## -----------------------------------------------------------------------------
group_G <- c("Approved", "TV-G", "GP")
group_PG <- c("PG-13", "TV-14")
group_R <- c("NC-17", "M", "Passed", "X")

df$maturity_rating[df$maturity_rating %in% group_G] <- "G"
df$maturity_rating[df$maturity_rating %in% group_PG] <- "PG"
df$maturity_rating[df$maturity_rating %in% group_R] <- "R"

table(df$maturity_rating)


## -----------------------------------------------------------------------------
ggplot(df, aes(x = maturity_rating, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by maturity_rating",
       x = "maturity_rating",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
pairwise_results <- pairwise.t.test(df$imdb_score, df$maturity_rating, p.adjust.method = "bonferroni")
print(pairwise_results)


## -----------------------------------------------------------------------------
table(df$country) 


## -----------------------------------------------------------------------------
Africa <- c("South Africa", "Kenya", "Nigeria", "Egypt", "Ghana", "Morocco", "Uganda")
Asia <- c("China", "India", "Japan", "South Korea", "Indonesia", "Kyrgyzstan", "Russia", "Taiwan", "Hong Kong")
Europe <- c("Belgium", "Czech Republic", "Denmark", "France", "Georgia", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Spain", "UK", "West Germany")
North_America <- c("USA", "Canada", "Mexico")
Central_South_America <- c("Aruba", "Brazil", "Colombia", "Peru")
Australia <- c("Australia", "New Zealand")
Other <- c("Official site")

df$continent <- 0

df$continent[df$country %in% Africa] <- "Africa"
df$continent[df$country %in% Asia] <- "Asia"
df$continent[df$country %in% Europe] <- "Europe"
df$continent[df$country %in% North_America] <- "North America"
df$continent[df$country %in% Central_South_America] <- "Central South America"
df$continent[df$country %in% Australia] <- "Australia"
df$continent[df$country %in% Other] <- "Other"

table(df$continent)


## -----------------------------------------------------------------------------
other_continent <- c("Africa", "Asia", "Australia", "Central South America", "Other")
df$continent[df$continent %in% other_continent] <- "other_continent"

table(df$continent)


## -----------------------------------------------------------------------------
ggplot(df, aes(x = continent, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by continent",
       x = "continent",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
pairwise_results <- pairwise.t.test(df$imdb_score, df$continent, p.adjust.method = "bonferroni")
print(pairwise_results)


## -----------------------------------------------------------------------------
over_30_countries <- c("Germany", "West Germany", "Canada", "France", "UK", "USA")
over_30_germany <- c("West Germany")

df$country[!(df$country %in% over_30_countries)] <- "other_countries"
df$country[df$country %in% over_30_germany] <- "Germany"

table(df$country)


## -----------------------------------------------------------------------------
ggplot(df, aes(x = country, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of IMDb Scores by country",
       x = "country",
       y = "IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -----------------------------------------------------------------------------
pairwise_results <- pairwise.t.test(df$imdb_score, df$country, p.adjust.method = "bonferroni")
print(pairwise_results)


## -----------------------------------------------------------------------------
personnel_cols <- c("director", "actor1", "actor2", "actor3", "cinematographer", "production_company", "distributor")

top_10_list <- list()

for (col in personnel_cols) {
  top_10 <- head(sort(table(df[[col]]), decreasing = TRUE), 10)
  
  # Store the top 10 in the list with the column name as the key
  top_10_list[[col]] <- top_10
  
  cat("Top 10 for", col, ":\n")
  print(top_10)
  cat("-------------------------\n") 
  
  sum_top_10 <- sum(top_10)
  cat("Sum of Top 10:", sum_top_10, "\n") 
  cat("-------------------------\n")
}


## -----------------------------------------------------------------------------
for (col in personnel_cols) {

  top_10_names <- names(top_10_list[[col]])

  new_col_name <- paste0("top_", col)
  
  df[[new_col_name]] <- ifelse(df[[col]] %in% top_10_names, 1, 0)
}

df[personnel_cols] <- NULL

colnames(df)
head(df)


## -----------------------------------------------------------------------------
# convert the chr to factor, so that the plot will present in order
df$release_month <- factor(df$release_month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

df$release_day_group <- cut(as.numeric(df$release_day), 
                            breaks = c(0, 10, 20, 31), 
                            labels = c("Early Month", "Mid Month", "Late Month"),
                            right = TRUE)

release_cols <- colnames(df)[grepl("release_day|release_day_group|release_month|release_year", colnames(df))]
for (col in release_cols) {
  print(col)
  print(table(df[[col]]))
  barplot(table(df[[col]]), main=paste("Distribution of", col), col="lightblue")
}


## -----------------------------------------------------------------------------
for (col in release_cols) {
  boxplot(df$imdb_score ~ df[[col]], main=paste("IMDb Score by", col), ylab="IMDb Score", xlab=col, col = "lightblue")
}


## -----------------------------------------------------------------------------
for (col in release_cols) {

  p <- ggplot(df, aes_string(x = col, y = "imdb_score")) +
    geom_point(color = "black", alpha = 0.6) + 
    geom_smooth(method = "loess", color = "red", se = TRUE) +  
    labs(x = col, y = "IMDb Rating", title = paste("IMDb Rating Trends by", col)) +
    theme_minimal()  # Cleaner theme
    
  print(p)
}


## -----------------------------------------------------------------------------
df$date <- as.Date(with(df, paste(release_year, release_month, release_day, sep = "-")), "%Y-%b-%d")

# Get the day of the week (1 = Sunday, 7 = Saturday)
df$day_of_week <- wday(df$date, label = TRUE)
df$weekday_or_weekend <- ifelse(df$day_of_week %in% c("Sat", "Sun"), "Weekend", "Weekday")

head(df[c("release_year", "release_month", "release_day", "day_of_week", "weekday_or_weekend")])


## -----------------------------------------------------------------------------
for (col in c("day_of_week", "weekday_or_weekend", "release_month", "release_day_group")) {
  print(table(df[col]))
  boxplot(df$imdb_score ~ df[[col]], main=paste("IMDb Score by", col), ylab="IMDb Score", xlab=col, col = "lightblue")
}


## -----------------------------------------------------------------------------
time_cols <- c("day_of_week", "weekday_or_weekend", "release_day_group")

for (col in time_cols) {
  
    pairwise_results <- pairwise.t.test(df$imdb_score, df[[col]], p.adjust.method = "bonferroni")
    print(pairwise_results)

}


## -----------------------------------------------------------------------------
drop_release_cols <- c("date", "release_year", "release_month", "release_day")
df[drop_release_cols] <- NULL

colnames(df)


## -----------------------------------------------------------------------------
num_cols <- c("movie_budget", "nb_faces", "nb_news_articles", "movie_meter_IMDBpro", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter") 

options(scipen = 999)

par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

for (col in num_cols) {
  hist(df[[col]], 
       main = paste("Distribution of ", col), 
       xlab = col, 
       col = "lightblue", 
       breaks = 30,
       ylim = c(0, max(table(cut(df[[col]], breaks = 30)), na.rm = TRUE) * 1.1) # Adjust ylim to add some padding
  )
}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


## -----------------------------------------------------------------------------
par(mfrow = c(2, 4), mar = c(4, 5, 2, 1), oma = c(0, 0, 0, 0)) 

for (col in num_cols) {
  
  boxplot(df[[col]], 
          main = paste("Boxplot of", col), 
          col = "lightblue", 
          las = 2, 
          ylim = range(df[[col]], na.rm = TRUE) + c(-1, 1) * diff(range(df[[col]], na.rm = TRUE)) * 0.1
          ) 
}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


## -----------------------------------------------------------------------------
cleaned_df <- df

par(mfrow = c(2, 4), mar = c(4, 5, 2, 1), oma = c(0, 0, 0, 0))

for (col in num_cols) {
  
  Q1 <- quantile(cleaned_df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(cleaned_df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  lower_bound <- Q1 - 3 * IQR
  upper_bound <- Q3 + 3 * IQR

  cleaned_df <- cleaned_df[cleaned_df[[col]] >= lower_bound & cleaned_df[[col]] <= upper_bound, ]
  
  hist(cleaned_df[[col]], 
         main = paste("Distribution of", col), 
         xlab = col, 
         col = "lightblue", 
         breaks = 30,
         ylim = c(0, max(table(cut(cleaned_df[[col]], breaks = 30)), na.rm = TRUE) * 1.1) 
      )
}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

print(dim(cleaned_df)) #  dropped rows = 479, drop rate = 25%


## -----------------------------------------------------------------------------
for (col in num_cols) {
  # use log (x + 1) since there're 0 in some predictors
  cleaned_df[[paste0(col, "_log")]] <- log(cleaned_df[[col]] + 1)  
  }


## -----------------------------------------------------------------------------
log_num_cols <- paste0(num_cols, "_log")

par(mfrow = c(2, 4), mar = c(4, 5, 2, 1), oma = c(0, 0, 0, 0))

for (col in log_num_cols) {
 hist(cleaned_df[[col]], 
         main = paste("Distribution of", col), 
         xlab = col, 
         col = "lightblue", 
         breaks = 30,
         ylim = c(0, max(table(cut(cleaned_df[[col]], breaks = 30)), na.rm = TRUE) * 1.1) # Adjust ylim to add padding
      )
  }

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


## -----------------------------------------------------------------------------
options(scipen=999)

ggpairs(cleaned_df[c("imdb_score", num_cols)],
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = "barDiag"),
        progress = FALSE, message = FALSE, warnings = FALSE)


## -----------------------------------------------------------------------------
plot_list <- list()
  
for (time in time_cols) {
  
  df_summary <- cleaned_df %>%
    group_by(.data[[time]]) %>%
    summarise(mean_value = mean(.data[["imdb_score"]], na.rm = TRUE)) 
  
  p <- ggplot(df_summary, aes_string(x = time, y = "mean_value", group = 1)) +
    geom_line(color = "lightblue", size = 1) +
    geom_point(color = "black", size = 2) +
    labs(title = "imdb_score",
         x = time,
         y = "Average imdb_score") +
    theme_minimal() +  
    theme(
      plot.title = element_text(size = 8),      
      axis.title.y = element_text(size = 7),    
      axis.title.x = element_text(size = 7),    
      axis.text.x = element_text(size = 7) 
    )
  
  plot_list <- c(plot_list, list(p))
}

grid.arrange(grobs = plot_list, ncol = 3, nrow = 1)


## -----------------------------------------------------------------------------
plot_list <- list()

for (col in num_cols) {
  
  for (time in time_cols) {
    
    df_summary <- cleaned_df %>%
      group_by(.data[[time]]) %>%
      summarise(mean_value = mean(.data[[col]], na.rm = TRUE)) 
    
    p <- ggplot(df_summary, aes_string(x = time, y = "mean_value", group = 1)) +
      geom_line(color = "lightblue", size = 1) +
      geom_point(color = "black", size = 2) +
      labs(title = col,
           x = time,
           y = paste("Average", col)) +
      theme_minimal() +  
      theme(
        plot.title = element_text(size = 8),      
        axis.title.y = element_text(size = 6),    
        axis.title.x = element_text(size = 5),    
        axis.text.x = element_text(size = 5) 
      )
    
    plot_list <- c(plot_list, list(p))
  }
}

grid.arrange(grobs = plot_list, ncol = 6, nrow = 4)


## -----------------------------------------------------------------------------
options(contrasts = c("contr.treatment", "contr.treatment"))  # Use simple dummy coding for unordered factors

factor_columns <- c("maturity_rating", "country", "continent", "release_day_group", "day_of_week", "weekday_or_weekend")

# dummy base setting
cleaned_df$maturity_rating <- relevel(as.factor(cleaned_df$maturity_rating), ref = "G")  
cleaned_df$country <- relevel(as.factor(cleaned_df$country), ref = "other_countries")
cleaned_df$continent <- relevel(as.factor(cleaned_df$continent), ref = "other_continent")
cleaned_df$release_day_group <- relevel(as.factor(cleaned_df$release_day_group), ref = "Mid Month")
cleaned_df$day_of_week <- factor(cleaned_df$day_of_week, ordered = FALSE)
cleaned_df$day_of_week <- relevel(cleaned_df$day_of_week, ref = "Mon")
cleaned_df$weekday_or_weekend <- relevel(as.factor(cleaned_df$weekday_or_weekend), ref = "Weekday")

dummy <- dummyVars(~ ., data = cleaned_df[, factor_columns], fullRank = TRUE)  # fullRank = TRUE excludes the base level

dummy_data <- predict(dummy, newdata = cleaned_df)
dummy_data <- as.data.frame(dummy_data)

numeric_cols <- cleaned_df[, !names(cleaned_df) %in% c("movie_id", "colour_film", "language", "maturity_rating", "country", "continent", "release_day_group", "day_of_week", "weekday_or_weekend", "other_genre")]
df_dummies <- cbind(numeric_cols, dummy_data)


## -----------------------------------------------------------------------------
correlation_matrix <- cor(df_dummies)

melted_correlation_matrix <- melt(correlation_matrix)

ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Color of the grid
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, hjust = 1)) +
  theme(axis.text.y = element_text(size = 8)) +
  coord_fixed() +  # Fix the aspect ratio
  labs(title = "Heatmap of correlations", x = "Variables", y = "Variables")


## -----------------------------------------------------------------------------
cor_with_target <- correlation_matrix[, "imdb_score"]

cor_with_target_sorted <- sort(cor_with_target, decreasing = TRUE)

cor_with_target_sorted


## -----------------------------------------------------------------------------
genres_retained <- c("action", "adventure", "scifi", "thriller", "western",
                     "sport", "horror", "drama", "war", "crime", "biography", 
                     "comedy", "fantasy", "history", "family", "other_genre")

high_rated_movies <- cleaned_df %>% filter(imdb_score >= 7.3) # 3rd Qu = 7.3

genre_matrix <- high_rated_movies %>% select(all_of(genres_retained))

# co-occurrence matrix calculation
co_occurrence_matrix <- as.matrix(t(genre_matrix)) %*% as.matrix(genre_matrix)

co_occurrence_df <- melt(co_occurrence_matrix) # need to convert to a dataframe
colnames(co_occurrence_df) <- c("Genre1", "Genre2", "Count")

# remove 0 and self-pairings
co_occurrence_df <- co_occurrence_df %>% filter(Count > 0 & Genre1 != Genre2)

graph <- graph_from_data_frame(co_occurrence_df, directed = FALSE)

ggraph(graph, layout = "fr") + 
  geom_edge_link(aes(edge_width = Count), edge_colour = "lightblue") + 
  geom_node_point(size = 5, color = "darkblue") + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 4) +
  theme_void() +
  labs(title = "Co-occurrence network of genres in high rated movies")


## -----------------------------------------------------------------------------
genre_corr_matrix <- cor(genre_matrix)

ggplot(melt(genre_corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Correlation between genres",
       x = "Genre",
       y = "Genre")


## -----------------------------------------------------------------------------
df_genres <- pivot_longer(cleaned_df, cols = all_of(genres_retained), names_to = "genre", values_to = "genre_indicator")
df_genres <- df_genres[df_genres$genre_indicator == 1, ]

genre_country_summary <- df_genres %>%
  group_by(country, genre) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(genre_country_summary, aes(x = genre, y = country, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Genre Popularity Across Countries", x = "Genre", y = "Country")


## -----------------------------------------------------------------------------
genre_continent_summary <- df_genres %>%
  group_by(continent, genre) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(genre_continent_summary, aes(x = genre, y = continent, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of genre popularity ccross continent", x = "Genre", y = "Continent")


## -----------------------------------------------------------------------------
imdb_scores_by_personnel <- df_dummies %>%
  pivot_longer(cols = starts_with("top_"), names_to = "Personnel", values_to = "Presence")

ggplot(imdb_scores_by_personnel, aes(x = Personnel, y = imdb_score, fill = factor(Presence))) +
  geom_boxplot() +
  labs(title = "IMDb Scores by top 10 personnel involvement", 
       x = "Personnel", 
       y = "IMDb score", 
       fill = "Presence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


## -----------------------------------------------------------------------------
country_by_personnel <- df_dummies %>%
  pivot_longer(cols = starts_with("top_"), names_to = "Personnel", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  group_by(Personnel) %>%
  summarise(across(starts_with("country"), sum))

country_by_personnel_melted <- melt(country_by_personnel)
ggplot(country_by_personnel_melted, aes(x = Personnel, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Top Personnel Distribution Across Countries", x = "Personnel", y = "Count of Movies", fill = "Country")


## -----------------------------------------------------------------------------
top_personnels <- c("top_director", "top_actor1", "top_actor2", "top_actor3", "top_cinematographer", "top_production_company", "top_distributor")

plot_list <- list()

for (personnel in top_personnels) {
  df_personnel <- cleaned_df %>% filter(!!sym(personnel) == 1)
  
  df_long <- df_personnel %>%
    pivot_longer(cols = all_of(genres_retained), names_to = "genre", values_to = "genre_indicator") %>%
    filter(genre_indicator == 1)

  genre_avg_scores <- df_long %>%
    group_by(genre) %>%
    summarise(avg_score = mean(imdb_score, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(avg_score))  # Sort by average score in descending order

  p <- ggplot(genre_avg_scores, aes(x = reorder(genre, avg_score), y = avg_score)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = paste(personnel, " - Average IMDb Scores by Genre"), x = "Genre", y = "Average IMDb Score") +
    theme(axis.text.x = element_text(hjust = 1)) +
    coord_flip()

  plot_list[[personnel]] <- p
}

grid.arrange(grobs = plot_list, ncol = 4, nrow = ceiling(length(plot_list) / 4))


## -----------------------------------------------------------------------------
X <- colnames(df_dummies)[colnames(df_dummies) != "imdb_score"]

# Set options to suppress scientific notation
options(scipen=999)

# Create the ggpairs plot without the ignored arguments
ggpairs(df_dummies[c("imdb_score", X)],
        upper = list(continuous = wrap("cor", size = 4)),  
        lower = list(continuous = wrap("points", alpha = 0.6)),  
        diag = list(continuous = "barDiag"),  
        progress = FALSE)


## -----------------------------------------------------------------------------
library(car)

no_log_cols <- c("movie_budget", "nb_faces", "movie_meter_IMDBpro", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter")

# No log transformation, excluding continent and day_of_week
X_noLog_country_weekday_or_weekend <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^continent", colnames(df_dummies)) & 
  !grepl("^day_of_week", colnames(df_dummies))
]

# No log transformation, including country and excluding day_of_week
X_noLog_country_day_of_week <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^continent", colnames(df_dummies)) &
  !grepl("^day_of_week", colnames(df_dummies))
]

# No log transformation, including continent and excluding weekday_or_weekend
X_noLog_continent_weekday_or_weekend <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^country", colnames(df_dummies)) &
  !grepl("^weekday_or_weekend$", colnames(df_dummies))
]

# No log transformation, including continent and excluding day_of_week
X_noLog_continent_day_of_week <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^country", colnames(df_dummies)) &
  !grepl("^day_of_week", colnames(df_dummies))
]

# Log transformation, excluding continent and including weekday_or_weekend
X_Log_country_weekday_or_weekend <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^continent", colnames(df_dummies)) & 
  !grepl("^day_of_week", colnames(df_dummies))
]

# Log transformation, including country and excluding day_of_week
X_Log_country_day_of_week <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^continent", colnames(df_dummies)) &
  !grepl("^day_of_week", colnames(df_dummies))
]

# Log transformation, including continent and excluding weekday_or_weekend
X_Log_continent_weekday_or_weekend <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^country", colnames(df_dummies)) &
  !grepl("^weekday_or_weekend$", colnames(df_dummies))
]

# Log transformation, including continent and excluding day_of_week
X_Log_continent_day_of_week <- df_dummies[
  !grepl("_log$", colnames(df_dummies)) & 
  !grepl("^country", colnames(df_dummies)) &
  !grepl("^day_of_week", colnames(df_dummies))
]

# Create a list of data frames for the models
df_lists <- list(
  X_noLog_country_weekday_or_weekend,
  X_noLog_country_day_of_week,
  X_noLog_continent_weekday_or_weekend,
  X_noLog_continent_day_of_week,
  X_Log_country_weekday_or_weekend,
  X_Log_country_day_of_week,
  X_Log_continent_weekday_or_weekend,
  X_Log_continent_day_of_week
)

heteroskedastic_predictors_list <- list()

for (df_index in seq_along(df_lists)) {
  df <- df_lists[[df_index]]  
  heteroskedastic_predictors <- c()  

  for (pred in colnames(df)[-1]) {  

    lm_model <- lm(imdb_score ~ get(pred), data = df)
    
    test_result <- ncvTest(lm_model)
    
    # threshold = 0.05
    if (test_result$p < 0.05) {
      heteroskedastic_predictors <- c(heteroskedastic_predictors, pred)
    }
  }

  heteroskedastic_predictors_list[[df_index]] <- heteroskedastic_predictors
}

for (i in seq_along(heteroskedastic_predictors_list)) {
  cat(sprintf("Heteroskedastic predictors for data frame %d:\n", i))
  print(heteroskedastic_predictors_list[[i]])
  cat("=======================================================\n")
}


## -----------------------------------------------------------------------------
library(broom)  # For tidy output of regression results

regression_results_list <- list()

for (df_index in seq_along(df_lists)) {
  df <- df_lists[[df_index]]  # get the current data frame
  regression_results <- data.frame(Predictor = character(), 
                                    P_Value = numeric(), 
                                    R_Squared = numeric(), 
                                    stringsAsFactors = FALSE)  

  # loop through each predictor in the current data frame
  for (pred in colnames(df)[-1]) {  # exclude imdb_score
    
    lm_model <- lm(imdb_score ~ get(pred), data = df)
  
    model_summary <- summary(lm_model)
    
    p_value <- round(coef(model_summary)[2, 4], 4)  
    r_squared <- model_summary$r.squared    

    regression_results <- rbind(regression_results, 
                                 data.frame(Predictor = pred, 
                                            P_Value = p_value, 
                                            R_Squared = r_squared, 
                                            stringsAsFactors = FALSE))
  }
  
  regression_results_list[[df_index]] <- regression_results
}

for (i in seq_along(regression_results_list)) {
  cat(sprintf("Regression Results for Data Frame %d:\n", i))
  print(regression_results_list[[i]])
  cat("=======================================================\n")
}



## -----------------------------------------------------------------------------
library(writexl)
write_xlsx(df_dummies, "IMDb_processed.xlsx")


## -----------------------------------------------------------------------------
colnames(df_dummies)

