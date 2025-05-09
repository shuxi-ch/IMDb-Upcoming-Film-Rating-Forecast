
######################## MODEL SELECTION #####################

# Prepare ----
library(readxl)
library(stargazer)
library(ggplot2)
library(car)
library(lmtest)
library(plm)
library(psych)
library(methods)
library(splines)
library(boot)
library(egg)
df <- read_xlsx("IMDb_processed.xlsx")
attach(df)
df$duration_log <- log(duration)
attach(df)

# Simple Linear Regressions ----
reg1 <- lm(imdb_score ~ movie_budget_log)
reg2 <- lm(imdb_score ~ duration)
reg3 <- lm(imdb_score ~ nb_news_articles_log)
reg4 <- lm(imdb_score ~ nb_faces_log)
reg5 <- lm(imdb_score ~ movie_meter_IMDBpro_log)
reg6 <- lm(imdb_score ~ actor1_star_meter_log)
reg7 <- lm(imdb_score ~ actor2_star_meter_log)
reg8 <- lm(imdb_score ~ actor3_star_meter_log)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8,
          type="html",
          title="Simple Linear Regression Results",
          align=TRUE,
          dep.var.labels=c("IMDb Score"),
          covariate.labels=c("Log of Movie Budget",
                             "Movie Duration",
                             "Log of Number of News Articles",
                             "Log of Number of Faces",
                             "Log of IMDbPro Movie Meter",
                             "Log of Star Meter: Actor 1",
                             "Log of Star Meter: Actor 2",
                             "Log of Star Meter: Actor 3"),
          no.space=TRUE)

# Multiple Linear Regression & Collinearity ----
mreg1 <- lm(imdb_score ~ movie_budget_log + duration + nb_news_articles_log + movie_meter_IMDBpro_log + actor1_star_meter_log)

vif_values <- vif(mreg1)
print(vif_values)

# Linearity Test ----
# Residual plots

plot_data <- data.frame(
  Fitted = fitted(mreg1),
  Residuals = residuals(mreg1),
  x1 = movie_budget_log,
  x2 = duration,
  x3 = nb_news_articles_log,
  x4 = movie_meter_IMDBpro_log,
  x5 = actor1_star_meter_log
)

plot1 <- ggplot(plot_data, aes(x = x1, y = Residuals)) + 
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Movie Budget", y = "PearsonResiduals")

plot2 <- ggplot(plot_data, aes(x = x2, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Movie Duration", y = "PearsonResiduals")

plot3 <- ggplot(plot_data, aes(x = x3, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Number of News Articles", y = "PearsonResiduals")

plot4 <- ggplot(plot_data, aes(x = x4, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of IMDbPro Movie Meter", y = "PearsonResiduals")

plot5 <- ggplot(plot_data, aes(x = x5, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Star Meter: Actor 1", y = "PearsonResiduals")

plot6 <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Fitted Values", y = "PearsonResiduals")
residualPlots(mreg1)
ggarrange(plot1, plot2, plot3, plot4, plot5, plot6 , ncol = 2, nrow = 3)

# log duration
mreg2 <- lm(imdb_score ~ movie_budget_log + duration_log + nb_news_articles_log + movie_meter_IMDBpro_log + actor1_star_meter_log)
# Residual plots
plot_data_2 <- data.frame(
  Fitted = fitted(mreg2),
  Residuals = residuals(mreg2),
  x1 = movie_budget_log,
  x2 = duration_log,
  x3 = nb_news_articles_log,
  x4 = movie_meter_IMDBpro_log,
  x5 = actor1_star_meter_log
)

plot1 <- ggplot(plot_data_2, aes(x = x1, y = Residuals)) + 
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Movie Budget", y = "PearsonResiduals")

plot2 <- ggplot(plot_data_2, aes(x = x2, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Movie Duration", y = "PearsonResiduals")

plot3 <- ggplot(plot_data_2, aes(x = x3, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Number of News Articles", y = "PearsonResiduals")

plot4 <- ggplot(plot_data_2, aes(x = x4, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of IMDbPro Movie Meter", y = "PearsonResiduals")

plot5 <- ggplot(plot_data_2, aes(x = x5, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Log of Star Meter: Actor 1", y = "PearsonResiduals")

plot6 <- ggplot(plot_data_2, aes(x = Fitted, y = Residuals)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Fitted Values", y = "PearsonResiduals")
residualPlots(mreg2)
ggarrange(plot1, plot2, plot3, plot4, plot5, plot6 , ncol = 2, nrow = 3)
k1 <- quantile(duration, .25)
k2 <- quantile(duration, .50)
k3 <- quantile(duration, .75)

# Adding Categorical Vars & Polynomial Linear Regression ----

polyreg1 <- lm(imdb_score ~ movie_budget_log + duration_log + nb_news_articles_log + movie_meter_IMDBpro_log + actor1_star_meter_log +
                 maturity_rating.R + # Maturity Rating
                 action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                 country.USA + # Country
                 top_cinematographer + top_director + top_distributor
)

stargazer(mreg2, polyreg1, type = "html")

polyreg2 <- lm(imdb_score ~ movie_budget_log + poly(duration_log, 2) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 2) +
                 maturity_rating.R + # Maturity Rating
                 action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                 country.USA + # Country
                 top_cinematographer + top_director + top_distributor
)

polyreg3 <- lm(imdb_score ~ movie_budget_log + poly(duration_log, 3) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 2) +
                 maturity_rating.R + # Maturity Rating
                 action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                 country.USA + # Country
                 top_cinematographer + top_director + top_distributor
)
polyreg4 <- lm(imdb_score ~ movie_budget_log + poly(duration_log, 2) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 3) +
                 maturity_rating.R + # Maturity Rating
                 action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                 country.USA + # Country
                 top_cinematographer + top_director + top_distributor
)

polyreg5 <- lm(imdb_score ~ movie_budget_log + poly(duration_log, 3) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 3) +
                 maturity_rating.R + # Maturity Rating
                 action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                 country.USA + # Country
                 top_cinematographer + top_director + top_distributor
)

anova(polyreg1, polyreg2, polyreg3, polyreg5)
anova(polyreg1, polyreg2, polyreg4, polyreg5)
# Final Model ----
final_model <- lm(imdb_score ~ movie_budget_log + poly(duration_log, 2) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 2) +
                    maturity_rating.R + # Maturity Rating
                    action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
                    country.USA + # Country
                    top_cinematographer + top_director + top_distributor
)

# Heteroskedasticity Test for Final Model ----
summary(final_model)
ncvTest(final_model)
coeftest(final_model, vcoc=cvocHC(final_model, type='HC1'))
# Stargazer
stargazer(final_model,
          type="html",
          title="IMDB Score Prediction Model",
          align=TRUE,
          dep.var.labels=c("IMDb Score"),
          covariate.labels=c("Log of Movie Budget",
                             "Log of Movie Duration",
                             "Log of Movie Duration<sup>2</sup>",
                             "Log of Number of News Articles",
                             "Log of IMDbPro Movie Meter",
                             "Log of Star Meter: Actor 1",
                             "Log of Star Meter: Actor 1<sup>2</sup>",
                             "Maturity Rating: R",
                             "Genre: Action",
                             "Genre: Adventure",
                             "Genre: Sci-Fi",
                             "Genre: Thriller",
                             "Genre: Western",
                             "Genre: Sport",
                             "Genre: Horror",
                             "Genre: Drama",
                             "Genre: War",
                             "Genre: Crime",
                             "Genre: Biography",
                             "Genre: Comedy",
                             "Genre: Fantasy",
                             "Genre: History",
                             "Genre: Family",
                             "Country: USA",
                             "Top Cinematographer",
                             "Top Director",
                             "Top Distributor"
          ),
          no.space=TRUE)
# Cross Validation ----
fit <- glm(imdb_score ~ movie_budget_log + poly(duration_log, 2) + nb_news_articles_log + movie_meter_IMDBpro_log + poly(actor1_star_meter_log, 2) +
             maturity_rating.R + # Maturity Rating
             action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + # Genres
             country.USA + # Country
             top_cinematographer + top_director + top_distributor
)
loocv_mse <- cv.glm(df, fit)$delta[1]
loocv_mse

kfold_mse <- cv.glm(df, fit, K=10)$delta[1]
kfold_mse




########################### PREDICTION #################################

df <- read_excel("IMDb_processed.xlsx")

reg_final <- lm(imdb_score ~  movie_budget_log + poly(log(duration), 2) + nb_news_articles_log + poly(actor1_star_meter_log, 2) + maturity_rating.R + action + adventure + scifi + thriller + western + sport + horror + drama + war + crime + biography + comedy + fantasy + history + family + country.USA + top_cinematographer + top_director + top_distributor + movie_meter_IMDBpro_log, data = imdb_data)

predictions_final_model <- predict(reg_final, df)

residuals_final_model <- df$imdb_score - predictions_final_model

venom <- data.frame(
  movie_budget_log = log(110000000),
  duration = 109,
  nb_news_articles_log = log(618),
  actor1_star_meter_log = log(105),
  maturity_rating.R = 0,
  action = 1,
  adventure = 1,
  scifi = 1,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(132)
)

venom_prediction <- predict(reg_final, newdata = venom)

kanguva <- data.frame(
  movie_budget_log = log(40000000),
  duration = 146,
  nb_news_articles_log = log(83),
  actor1_star_meter_log = log(3922),
  maturity_rating.R = 1,
  action = 1,
  adventure = 0,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 1,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 1,
  history = 0,
  family = 0,
  country.USA = 0,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(14143)
)
kanguva_pre <- predict(reg_final, newdata = kanguva)


y_monster <- data.frame(
  movie_budget_log = log(20000000),
  duration = 98,
  nb_news_articles_log = log(81),
  actor1_star_meter_log = log(162),
  maturity_rating.R = 1,
  action = 0,
  adventure = 0,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 1,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 0,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(1768)
)
y_monster_pre <- predict(reg_final, newdata = y_monster)


Hitpig <- data.frame(
  movie_budget_log = log(300000000),
  duration = 86,
  nb_news_articles_log = log(7),
  actor1_star_meter_log = log(1151),
  maturity_rating.R = 0,
  action = 1,
  adventure = 1,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 0,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(5888)
)
Hitpig_pre <- predict(reg_final, newdata = Hitpig)

a_real_pain <- data.frame(
  movie_budget_log = log(10000000),
  duration = 90,
  nb_news_articles_log = log(283),
  actor1_star_meter_log = log(884),
  maturity_rating.R = 1,
  action = 0,
  adventure = 0,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 1,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(1168)
)
a_real_pain_pre <- predict(reg_final, newdata = a_real_pain)


elevation <- data.frame(
  movie_budget_log = log(35000000),
  duration = 90,
  nb_news_articles_log = log(44),
  actor1_star_meter_log = log(53),
  maturity_rating.R = 0,
  action = 1,
  adventure = 0,
  scifi = 1,
  thriller = 1,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(3468)
)
elevation_pre <- predict(reg_final, newdata = elevation)


best_christmas <- data.frame(
  movie_budget_log = log(20000000),
  duration = 99,
  nb_news_articles_log = log(24),
  actor1_star_meter_log = log(675),
  maturity_rating.R = 0,
  action = 0,
  adventure = 1,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 1,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(3531)
)
best_christmas_pre <- predict(reg_final, newdata = best_christmas)


red_one <- data.frame(
  movie_budget_log = log(250000000),
  duration = 123,
  nb_news_articles_log = log(186),
  actor1_star_meter_log = log(207),
  maturity_rating.R = 0,
  action = 1,
  adventure = 1,
  scifi = 0,
  thriller = 0,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(403)
)
red_one_pre <- predict(reg_final, newdata = red_one)


Heretic <- data.frame(
  movie_budget_log = log(50000000),
  duration = 110,
  nb_news_articles_log = log(126),
  actor1_star_meter_log = log(120),
  maturity_rating.R = 1,
  action = 0,
  adventure = 0,
  scifi = 0,
  thriller = 1,
  western = 0,
  sport = 0,
  horror = 1,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(123)
)
Heretic_pre <- predict(reg_final, newdata = Heretic)


Bonhoeffer <- data.frame(
  movie_budget_log = log(15000000),
  duration = 132,
  nb_news_articles_log = log(11),
  actor1_star_meter_log = log(4013),
  maturity_rating.R = 0,
  action = 0,
  adventure = 0,
  scifi = 0,
  thriller = 1,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 1,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 0,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 0,
  movie_meter_IMDBpro_log = log(5760)
)
Bonhoeffer_pre <- predict(reg_final, newdata = Bonhoeffer)


Gladiator_II <- data.frame(
  movie_budget_log = log(310000000),
  duration = 150,
  nb_news_articles_log = log(882),
  actor1_star_meter_log = log(181),
  maturity_rating.R = 1,
  action = 1,
  adventure = 1,
  scifi = 0,
  thriller = 1,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 1,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 1,
  movie_meter_IMDBpro_log = log(101)
)
Gladiator_II_pre <- predict(reg_final, newdata = Gladiator_II)


Wicked <- data.frame(
  movie_budget_log = log(145000000),
  duration = 160,
  nb_news_articles_log = log(1979),
  actor1_star_meter_log = log(512),
  maturity_rating.R = 0,
  action = 0,
  adventure = 0,
  scifi = 0,
  thriller = 1,
  western = 0,
  sport = 0,
  horror = 0,
  drama = 0,
  war = 0,
  crime = 0,
  biography = 0,
  comedy = 0,
  fantasy = 0,
  history = 0,
  family = 0,
  country.USA = 1,
  top_cinematographer = 0,
  top_director = 0,
  top_distributor = 1,
  movie_meter_IMDBpro_log = log(160)
)
Wicked_pre <- predict(reg_final, newdata = Wicked)


cat("IMDB score for Venom：", venom_prediction, "\n")
cat("IMDB score for You Monster：", y_monster_pre, "\n")
cat("IMDB score for Hitpig!：", Hitpig_pre, "\n")
cat("IMDB score for A Real Pain：", a_real_pain_pre, "\n")
cat("IMDB score for Elevation：", elevation_pre, "\n")
cat("IMDB score for The Best Christmas Pageant Ever：", best_christmas_pre, "\n")
cat("IMDB score for kanguva：", kanguva_pre, "\n")
cat("IMDB score for Red One：", red_one_pre, "\n")
cat("IMDB score for Heretic：", Heretic_pre, "\n")
cat("IMDB score for Bonhoeffer: Pastor. Spy. Assassin.：", Bonhoeffer_pre, "\n")
cat("IMDB score for Gladiator II：", Gladiator_II_pre, "\n")
cat("IMDB score for Wicked：", Wicked_pre, "\n")

max(venom_prediction, y_monster_pre, Hitpig_pre, a_real_pain_pre, elevation_pre, best_christmas_pre, kanguva_pre, red_one_pre, Heretic_pre, Bonhoeffer_pre, Gladiator_II_pre, Wicked_pre)


movie_names <- c("Venom", "You Monster", "Hitpig", "A Real Pain", "Elevation", 
                 "The Best Christmas Pageant Ever", "Kanguva", "Red One", 
                 "Heretic", "Bonhoeffer: Pastor. Spy. Assassin.", "Gladiator II", "Wicked")

movie_scores <- c(venom_prediction, y_monster_pre, Hitpig_pre, a_real_pain_pre, 
                  elevation_pre, best_christmas_pre, kanguva_pre, red_one_pre, 
                  Heretic_pre, Bonhoeffer_pre, Gladiator_II_pre, Wicked_pre)

movies_df <- data.frame(Movie = movie_names, IMDB_Score = movie_scores)

sorted_movies_df <- movies_df[order(-movies_df$IMDB_Score), ]

print(sorted_movies_df)