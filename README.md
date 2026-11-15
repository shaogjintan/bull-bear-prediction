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

## Where to find the report's tables and figures

| Report | Content | File | Made by |
|---|---|---|---|
| Table 1 | Predictor sources and selection | `tables/eda_predictors_by_source.csv` (counts; exclusion details written by hand) | `eda.R` |
| Table 2 | Definitions of Goyal and constructed predictors | Written by hand (variables built in `02_data_processing.R`) | – |
| Table 3 | Bull and bear months by decade | `tables/eda_by_decade.csv` | `eda.R` |
| Table 4 | Pre-validation, validation and test sets | `tables/eda_periods.csv` | `eda.R` |
| Table 5 | Bear markets identified by the LT filter | `tables/eda_bear_chronology.csv` | `eda.R` |
| Table 6 | Number of boosting trees chosen, validation AUC | `results/boost_tuning_w300.csv` (best row per horizon and depth) | `boosting.R` |
| Table 7 | AUC and QPS | `tables/results_auc_qps.csv` | `05_evaluate.R` |
| Table 8 | Classification metrics, Panel A (0.5) and Panel B (rolling cutoff) | `tables/results_classification_050.csv`, `tables/results_classification_ybar.csv` | `05_evaluate.R` |
| Table 9 | Diebold-Mariano tests | `results/dm_tests_w300.csv` | `06_dm_tests.R` |
| Table 10 | LASSO selection frequency | `tables/lasso_selection_frequency_w300.csv` | `lasso.R` |
| Table 11 | Tree-based feature importance (top 5) | `tables/importance_{rf,bagging,boosting}_top10_w300.csv` (RF and bagging in decimals, boosting in %) | `rf.R`, `bagging.R`, `boosting.R` |
| Table 12 | Portfolio performance | `tables/portfolio_results.csv` | `portfolio.R` |
| Figure 1 | S&P 500 with bear months shaded | `figures/sp500_bear_shading.png` | `eda.R` |
| Figure 2 | AUC and QPS by horizon | `figures/auc_qps_by_horizon.png` | `05_evaluate.R` |
| Figure 3 | Bear recall at both cutoffs | `figures/bear_recall.png` | `05_evaluate.R` |
| Figure 4 | Predicted bear probability, h = 3 | `figures/bear_prob_h3.png` | `05_evaluate.R` |

Other outputs (`figures/roc_curves.png`, `figures/bear_prob_h1.png`, `figures/bear_prob_h6.png`, `figures/dm_*.png`, `tables/eda_bull_vs_bear_means.csv`) are supplementary and not in the report.

## Folders

| Folder | Contents |
|---|---|
| `data/` | Raw inputs (FRED-MD 2026-09 vintage, Goyal data, S&P 500, SPY) and the built panel |
| `results/` | Out-of-sample forecasts (one file per model and horizon), tuning results, summary metrics and DM tests |
| `tables/` | Tables for the report |
| `figures/` | Figures for the report, plus supplementary figures |

## R packages

`dplyr`, `tidyr`, `lubridate`, `zoo`, `readxl`, `quantmod`, `glmnet`, `randomForest`, `xgboost`, `pROC`, `sandwich`
