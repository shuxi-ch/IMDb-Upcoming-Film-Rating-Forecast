# IMDb-Upcoming-Film-Rating-Forecast

Forecast IMDb ratings of upcoming films using a reproducible R pipeline and multiple linear regression.

## Repo Structure

```

.
├── data
│   ├── raw
│   └── processed
├── R
├── notebooks
├── reports
├── docs
├── tests
├── renv.lock
├── .gitignore
└── README.md

````

## 🚀 Getting Started

1. **Clone the repo**  
   ```bash
   git clone https://github.com/shuxi-ch/IMDb-Upcoming-Film-Rating-Forecast.git
   cd IMDb-Upcoming-Film-Rating-Forecast
````

2. **Restore R dependencies**

   ```r
   install.packages("renv")
   renv::restore()      # installs exactly the versions in renv.lock
   ```

   > If you don’t have renv.lock yet, see “Managing Packages” below.

3. **Run the Notebooks**

   * `notebooks/01_data_preprocessing.Rmd`
   * `notebooks/02_model_training.Rmd`

4. **Explore Outputs**

   * Cleaned data: `data/processed/imdb_cleaned.csv`
   * Final report: `reports/IMDB_Score_Prediction_Report.pdf`

5. **Use the R scripts**

   ```r
   source("R/data_preparation.R")
   df <- load_and_clean("data/raw/IMDB_data_Fall_2024.csv")
   source("R/modeling.R")
   fit  <- train_lm(df)
   ```

## Key Features

* **Data Cleaning & EDA**: Log-transforms, outlier handling, multicollinearity pruning via VIF
* **Feature Engineering**: Polynomial terms, genre & production flags, scaling
* **Modeling**: Multiple linear regression (`lm()`), adjusted R² = 0.476, lowest MSE on 12 upcoming films
* **Reproducibility**: RMarkdown notebooks, renv lockfile for dependency management
