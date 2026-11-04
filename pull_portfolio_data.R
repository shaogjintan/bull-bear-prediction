library(quantmod)
library(dplyr)
library(lubridate)
##########################################
## DATA FOR THE PORTFOLIO EXERCISE      ##
##########################################
# Output: data/portfolio_data.csv with one row per month:
#   date    = month (first of month, same convention as panel.csv)
#   spy_adj = SPY adjusted close on the last trading day of the month (includes dividends)
#   spy_ret = SPY total return over the month (end of previous month -> end of this month)
#   rf      = 3-month T-bill return for holding during the month (monthly rate, known at the start of the month)

## 1. pull SPY daily from Yahoo and save the raw file (so the code can run from raw data later) ##
spy <- getSymbols("SPY", src = "yahoo", from = "1993-01-01", auto.assign = FALSE)
write.csv(data.frame(date = index(spy), coredata(spy)),
          "data/spy_daily.csv", row.names = FALSE)

## 2. monthly SPY total returns ##
spy_raw <- read.csv("data/spy_daily.csv")

spy_m <- spy_raw |>
  mutate(day = as.Date(date)) |>
  group_by(date = floor_date(day, "month")) |>
  slice_max(day, n = 1) |>                      # last trading day of the month
  ungroup() |>
  filter(date < as.Date("2026-10-01")) |>      # drop incomplete Oct 2026
  arrange(date) |>
  transmute(date,
            spy_adj = SPY.Adjusted,             # adjusted close: includes dividends
            spy_ret = spy_adj / lag(spy_adj) - 1)

## 3. risk-free rate: 3-month T-bill (TB3MS) from the raw FRED-MD file ##
# TB3MS is an annual % rate (monthly average). The rate for holding during month t
# is the one known at the start of month t, i.e. last month's TB3MS, divided by 12
fred_raw <- read.csv("data/fred_md_2026-09.csv")

rf_m <- fred_raw |>
  slice(-1) |>                                  # remove the transformation-code row
  transmute(date  = as.Date(sasdate, format = "%m/%d/%Y"),
            tb3ms = as.numeric(TB3MS)) |>       # untransformed level, not the differenced panel column
  mutate(date = date %m+% months(1),            # shift: last month's rate applies to this month
         rf   = tb3ms / 100 / 12) |>
  select(date, rf)

## 4. merge and save ##
port <- spy_m |> left_join(rf_m, by = "date")
write.csv(port, "data/portfolio_data.csv", row.names = FALSE)

## checks ##
head(port); tail(port)
test <- filter(port, date >= as.Date("2001-06-01"))
cat("Test-period months:", nrow(test), " missing spy_ret:", sum(is.na(test$spy_ret)),
    " missing rf:", sum(is.na(test$rf)), "\n")
cat("Annualised buy-and-hold SPY return, Jul 2001 - Dec 2025:",
    round(100 * (prod(1 + filter(port, date >= as.Date("2001-07-01"), date <= as.Date("2025-12-01"))$spy_ret)^(12 /
                                                                                                                 nrow(filter(port, date >= as.Date("2001-07-01"), date <= as.Date("2025-12-01")))) - 1), 2), "%\n")
