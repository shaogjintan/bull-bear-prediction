# Predicting Bull and Bear Markets with Machine Learning

EC4308 group project. We forecast the bull/bear state of the S&P 500 (Lunde-Timmermann filter, 20% threshold) 1, 3 and 6 months ahead. We compare LASSO logit, random forest, bagging and boosting, all estimated on a large pool of FRED-MD, Goyal and S&P 500 predictors, against a persistence benchmark and a Nyberg (2013) logit. All models use a 300-month rolling window, and the test period runs from June 2001 to December 2025.

## Run order

Run the scripts from the repository root in this order. Each step reads files written by the steps before it.

Notes:
- `01_pull_sp500.R` and `pull_portfolio_data.R` download live data. The files they produce are already in `data/`, so you can skip both steps to reproduce the report exactly.
- `rf.R`, `bagging.R` and `boosting.R` each take about an hour. They use `set.seed(4308)`.

| Step | Script | What it does | Main output |
|---|---|---|---|
| 1 | `01_pull_sp500.R` | Downloads daily S&P 500 prices from Yahoo Finance | `data/sp500_daily.csv` |
| 2 | `03_label_bull_bear.R` | Dates bull/bear states with the Lunde-Timmermann filter and builds the h = 1, 3, 6 targets | `data/target.csv` |
| 3 | `04_combined_data.R` | Runs `02_data_processing.R` (cleans and transforms S&P 500, FRED-MD and Goyal data), then merges the predictors with the target and adds lags 1–6 | `data/panel.csv` |
| 4 | `eda.R` | Descriptive tables and the S&P 500 bear-shading figure | `tables/eda_*.csv`, `figures/sp500_bear_shading.png` |
| 5 | `persistence.R` | Persistence benchmark: forecasts the current state | `results/persistence_h*.csv` |
| 6 | `logit.R` | Logit benchmark (state, return, term spread, Δd/p) | `results/logit_h*_w300.csv` |
| 7 | `lasso.R` | LASSO logit, with the penalty chosen by BIC in each window | `results/lasso_*`, `tables/lasso_selection_frequency_w300.csv` |
| 8 | `rf.R` | Random forest (500 trees, mtry = √p), plus feature importance | `results/rf_h*_w300.csv`, `tables/importance_rf_top10_w300.csv` |
| 9 | `bagging.R` | Bagging (500 trees, mtry = p), plus feature importance | `results/bagging_h*_w300.csv`, `tables/importance_bagging_top10_w300.csv` |
| 10 | `boosting.R` | XGBoost at depths 2 and 5, with the number of trees tuned on a validation window, plus feature importance | `results/boost_d*_h*_w300.csv`, `results/boost_tuning_w300.csv`, `tables/importance_boosting_top10_w300.csv` |
| 11 | `07_ensemble.R` | Model average and majority vote of LASSO, RF, bagging and boosting (d = 2) | `results/ensemble_*_w300.csv` |
| 12 | `05_evaluate.R` | AUC, QPS and classification metrics at the 0.5 and rolling cutoffs, plus result figures | `results/summary_w300.csv`, `tables/results_*.csv`, `figures/` |
| 13 | `06_dm_tests.R` | Diebold-Mariano tests against the logit and persistence | `results/dm_tests_w300.csv`, `figures/dm_*.png` |
| 14 | `pull_portfolio_data.R` | Downloads SPY returns and the T-bill rate for the portfolio exercise | `data/portfolio_data.csv` |
| 15 | `portfolio.R` | Market-timing strategies vs buy-and-hold | `tables/portfolio_results.csv` |

## Folders

| Folder | Contents |
|---|---|
| `data/` | Raw inputs (FRED-MD 2026-09 vintage, Goyal data, S&P 500, SPY) and the built panel |
| `results/` | Out-of-sample forecasts (one file per model and horizon), tuning results, summary metrics and DM tests |
| `tables/` | Tables used in the report |
| `figures/` | Figures used in the report |

## R packages

`dplyr`, `tidyr`, `lubridate`, `zoo`, `readxl`, `quantmod`, `glmnet`, `randomForest`, `xgboost`, `pROC`, `sandwich`
