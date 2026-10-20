library(dplyr)
library(lubridate)
#####################
## MERGE AND LAGS  ##
#####################

# needs 02_data_processing.R (builds `dat`, the predictors) and
# 03_label_bull_bear.R (writes data/target.csv)
source("02_data_processing.R")

## merge predictors with the bull/bear target ##
target <- read.csv("data/target.csv") |>
  mutate(date = as.Date(date)) |>
  select(date, state, y_h1, y_h3, y_h6)

panel <- dat |>
  arrange(date) |>
  inner_join(target, by = "date")

# months should be consecutive, otherwise lag() would skip periods
stopifnot(all(diff(interval(min(panel$date), panel$date) %/% months(1)) == 1))

## lags ##
n_lags <- 6

# predictors: everything except the date, the price level (not stationary)
# and the targets
x_vars <- setdiff(names(dat), c("date", "sp500"))

make_lags <- function(df, vars, k) {
  df |>
    select(all_of(vars)) |>
    mutate(across(everything(), ~ lag(.x, k))) |>
    rename_with(~ paste0(.x, "_l", k))
}

lag_list <- lapply(1:n_lags, function(k) make_lags(panel, x_vars, k))

# lags of the bull/bear state (for the dynamic models)
state_lags <- lapply(1:n_lags, function(k) make_lags(panel, "state", k))

panel_lags <- bind_cols(panel, lag_list, state_lags)

dim(panel_lags)

## missing data ##
# raw predictors (before lagging)
na_x <- colSums(is.na(panel[, x_vars]))
sort(na_x[na_x > 0], decreasing = TRUE)

# first month where all predictors are available
complete <- complete.cases(panel[, x_vars])
min(panel$date[complete])
sum(complete)

# after adding lags, first row with no missing values at all
complete_lags <- complete.cases(panel_lags)
min(panel_lags$date[complete_lags])
sum(complete_lags)

# targets should have no missing values in this window
colSums(is.na(panel[, c("state", "y_h1", "y_h3", "y_h6")]))
