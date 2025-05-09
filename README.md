# IMDb-Upcoming-Film-Rating-Forecast
### Overview

This project forecasts IMDb ratings for 12 upcoming films by training a multiple linear regression model on a dataset of \~1,930 historical movies. It delivers a reproducible R pipeline—complete with data cleaning, feature engineering, model training, and evaluation—culminating in an adjusted R² of 0.476 and the lowest MSE in a class-wide prediction challenge.

### Directory layout

   ```bash
   .
   ├── README.md
   ├── data
   │   ├── raw
   │   │   ├── IMDB_data_Fall_2024.csv
   │   │   └── test_data_IMDB_Fall_2024.csv
   │   └── processed
   │       └── IMDb_processed.xlsx
   ├── R                          
   │   ├── EDA&DataPreprocessing.R                 
   │   └── Modeling.R                
   ├── reports
   │   ├── EDA_Report.pdf  
   │   └── IMDB_Score_Prediction_Report.pdf
   ├── docs
   │   └── data_dictionary&EDA_process note.csv
   └── tests
       └── Test_modeling.R 
   ```

### Problem Statement

Studios and distributors need reliable forecasts of audience reception to guide budgeting, marketing, and release strategies. However, IMDb ratings depend on multiple film attributes—budgets, duration, star power, media coverage, genres, and production details—making accurate prediction nontrivial. This project addresses that challenge by building and validating a transparent regression model to predict ratings for films yet to be released.

### Data Sources

* IMDB\_data\_Fall\_2024.csv: Historical dataset of \~1,930 films (budget, duration, news articles count, star-meter rankings, maturity rating, genres, country, top personnel flags, and IMDb score).
* test\_data\_IMDB\_Fall\_2024.csv: 12 upcoming film records without scores for out-of-sample evaluation.
* data\_dictionary\_IMDB\_Fall\_2024.csv: Descriptions and value ranges for each variable.

### Methodology / Approach

1. **Data Cleaning & EDA**

   * Dropped non-predictive fields (`movie_title`, `imdb_link`, `plot_keywords`).
   * Log-transformed skewed variables: `movie_budget`, `nb_news_articles`, `actor*_star_meter`, and `movie_meter_IMDBpro`.
   * Visualized distributions and correlations to spot anomalies and multicollinearity.
2. **Feature Engineering**

   * Added polynomial terms (`poly(duration,2)`, `poly(actor1_star_meter,2)`) to capture nonlinear effects.
   * One-hot encoded categorical predictors: maturity ratings, country, top director/actor/distributor flags, and genre indicators.
   * Evaluated Variance Inflation Factors (VIF) and dropped collinear features to stabilize estimates.
3. **Model Training & Evaluation**

   * Performed an 80/20 stratified train/test split.
   * Fitted a multiple linear regression (`lm()`) and assessed performance via adjusted R², RMSE, and residual diagnostics.
   * Achieved **Adjusted R² = 0.476** on training data and won the class challenge with the lowest MSE on the 12-film test set.

### Key Results & Visualizations
*   **Model Performance:**
    *   R-squared: 0.4857
    *   Adjusted R-squared: 0.476
    *   Mean Squared Error (LOOCV): 0.5967
*   **Test MSE**: Predicted IMDb Scores for 12 Upcoming Movies (Lowest among peer submissions on 12 upcoming films)
*   **Key Predictors:** Movie budget (log), duration (log, quadratic), number of news articles (log), IMDbPro movie meter (log), specific genres (Drama, Horror, Comedy, etc.), maturity rating (R), and presence of top-tier production personnel.
*   Visualizations included histograms of variable distributions, boxplots for categorical comparisons, correlation matrices/heatmaps, and residual plots. (Refer to `MGSC661 Midterm Report.pdf` and `EDA_DataPreprocessing.html` for figures).

### Key Learnings & Challenges

* **Feature Engineering:** Extensive feature engineering for categorical variables (genres, personnel, country) was crucial. Log-transformations and outlier handling significantly improved model assumptions for numerical data.
* **Addressing Model Assumptions:** Systematically checked for and addressed issues like multicollinearity (VIF) and heteroskedasticity (NCV test, residual analysis). Non-linearity was handled with polynomial terms.
* **Model Interpretability vs. Complexity:** Balanced model complexity with interpretability, opting for a robust linear model with polynomial terms rather than a black-box approach, to provide clear insights into prediction drivers.
* **Out-of-Sample Validation:** Emphasized LOOCV for a more reliable estimate of predictive performance on unseen data.
* **Nonlinearity**: Polynomial terms captured diminishing returns of duration and star power.

### Next Steps / Future Enhancements

*   Explore more advanced regression techniques (e.g., Ridge, Lasso, Elastic Net) or tree-based models (Random Forest, Gradient Boosting) to potentially improve predictive accuracy.
*   Incorporate interaction terms between predictors (e.g., genre and budget, actor and director).
*   Refine feature engineering, potentially using more sophisticated text analysis for plot keywords or sentiment analysis for news articles if available.
*   Expand the dataset with more historical data or additional relevant features.
*   Test on additional upcoming film batches and track real vs. predicted ratings over time.

