library(dplyr)
###################################
## EDA: BULL/BEAR DISTRIBUTION   ##
###################################

panel <- read.csv("data/panel.csv") |> mutate(date = as.Date(date))

## 1. bull and bear months per year ##
by_year <- panel |>
  group_by(year = as.integer(format(date, "%Y"))) |>
  summarise(bull = sum(state == 0), bear = sum(state == 1))
print(by_year, n = Inf)

## 2. each bear market: start, end, length ##
r <- rle(panel$state)
ends <- cumsum(r$lengths)
starts <- ends - r$lengths + 1
episodes <- data.frame(start  = panel$date[starts],
                       end    = panel$date[ends],
                       months = r$lengths)[r$values == 1, ]
print(episodes, row.names = FALSE)

## 3. bull/bear counts in each candidate period ##
count_period <- function(name, from, to) {
  s <- panel$state[panel$date >= from & panel$date <= to]
  data.frame(period = name, months = length(s), bull = sum(s == 0), bear = sum(s == 1),
             bear_markets = sum(episodes$end >= from & episodes$start <= to))
}
periods <- rbind(
  count_period("Before 2001",  min(panel$date),       as.Date("2000-12-01")),
  count_period("2001-2009",    as.Date("2001-01-01"), as.Date("2009-12-01")),
  count_period("2010 onwards", as.Date("2010-01-01"), max(panel$date)))
print(periods, row.names = FALSE)

