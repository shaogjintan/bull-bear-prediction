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

## predictors ##
# everything except the date and the price level (not stationary)
x_vars <- setdiff(names(dat), c("date", "sp500"))

## availability: drop predictors that do not cover the whole sample ##
start_date <- as.Date("1960-01-01") # first months lost to differencing
end_date <- max(panel$date)

first_obs <- sapply(x_vars, function(v) {
  as.character(min(panel$date[!is.na(panel[[v]])]))
})
last_obs <- sapply(x_vars, function(v) {
  as.character(max(panel$date[!is.na(panel[[v]])]))
})

avail <- data.frame(var = x_vars,
                    first_obs = as.Date(first_obs),
                    last_obs = as.Date(last_obs),
                    n_na = colSums(is.na(panel[, x_vars])))

problem <- avail |>
  filter(first_obs > start_date | last_obs < end_date) |>
  arrange(desc(first_obs))
print(problem, row.names = FALSE)

late <- problem$var
panel <- panel |> select(-all_of(late))
x_vars <- setdiff(x_vars, late)
length(x_vars) # predictors left

## lags ##
n_lags <- 6

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
# anything left in the raw predictors?
na_x <- colSums(is.na(panel[, x_vars]))
sort(na_x[na_x > 0], decreasing = TRUE)

# first month with no missing values at all, after adding lags
complete <- complete.cases(panel_lags)
min(panel_lags$date[complete])
sum(complete)

# targets should have no missing values in this window
colSums(is.na(panel[, c("state", "y_h1", "y_h3", "y_h6")]))

## save ##
panel_final <- panel_lags[complete, ]
dim(panel_final)
write.csv(panel_final, "data/panel.csv", row.names = FALSE)
