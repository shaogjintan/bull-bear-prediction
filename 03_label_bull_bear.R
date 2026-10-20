library(dplyr)
library(lubridate)
#########################
## BULL / BEAR LABELS  ##
#########################

# Lunde-Timmermann (2004) filter on monthly S&P 500 closes. It is one-sided:
# the state at month t only uses prices up to t.
# - bull: track the highest close. A fall of lambda_bear from it starts a bear.
# - bear: track the lowest close. A rise of lambda_bull from it starts a bull.

lambda_bear <- 0.20 # fall from peak that signals a bear
lambda_bull <- 0.20 # rise from trough that signals a bull

# monthly closes, same cleaning as 02_data_processing.R
sp <- read.csv("data/sp500_daily.csv")

sp_df <- sp |>
  mutate(date = as.Date(date)) |>
  group_by(month = floor_date(date, "month")) |>
  slice_max(date, n = 1) |> # last trading day of the month
  ungroup() |>
  filter(month < as.Date("2026-10-01")) |> # drop incomplete oct 26
  select(date = month, sp500 = GSPC.Close)

price <- sp_df$sp500
n <- length(price)

## real-time state: 1 = bear, 0 = bull ##
state <- integer(n)
in_bear <- FALSE
peak <- 1 # position of the highest close in the current bull
trough <- NA # position of the lowest close in the current bear
turns <- data.frame(type = character(), turn = integer(), signal = integer())

for (t in 2:n) {
  if (!in_bear) {
    if (price[t] > price[peak]) peak <- t
    if (price[t] <= price[peak] * (1 - lambda_bear)) {
      turns <- rbind(turns, data.frame(type = "peak", turn = peak, signal = t))
      in_bear <- TRUE
      trough <- t
    }
  } else {
    if (price[t] < price[trough]) trough <- t
    if (price[t] >= price[trough] * (1 + lambda_bull)) {
      turns <- rbind(turns,
                     data.frame(type = "trough", turn = trough, signal = t))
      in_bear <- FALSE
      peak <- t
    }
  }
  state[t] <- as.integer(in_bear)
}

## delay d: months from the turning point to the month it is signalled ##
turns <- turns |>
  mutate(turn_date   = sp_df$date[turn],
         signal_date = sp_df$date[signal],
         d           = signal - turn)
print(select(turns, type, turn_date, signal_date, d))
cat("d (months): mean", mean(turns$d), ", max", max(turns$d), "\n")

## targets: state h months ahead, NA for the last h months ##
target <- sp_df |>
  mutate(state = state,
         y_h1  = lead(state, 1),
         y_h3  = lead(state, 3),
         y_h6  = lead(state, 6))

# checking
print(mean(target$state)) # share of bear months
print(sum(turns$type == "peak")) # number of bear markets

write.csv(target, "data/target.csv", row.names = FALSE)
