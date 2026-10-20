library(quantmod)
library(readxl)
library(dplyr)
library(lubridate)
###################
## DATA CLEANING ##
###################

## s&P data from Yahoo Finance ##

# read in s&p data
sp <- read.csv("data/sp500_daily.csv")

# cleaning, convert to monthly
sp_df <- sp |>
  mutate(date = as.Date(date)) |> # convert to date format
  group_by(month = floor_date(date, "month")) |> # group days by same month
  slice_max(date, n = 1) |> # keep last day of the month
  ungroup() |>
  filter(month < as.Date("2026-10-01")) |> # drop incomplete oct 26
  select(date = month, last_trading_day = date, sp500 = GSPC.Close) #taking only monthly close prices

#glimpse(sp_df)
#head(sp_df)
#tail(sp_df)
#summary(sp_df)

## FRED MD ##
fred_raw <- read.csv("data/fred_md_2026-09.csv")

# need to transform variables
tcode <- fred_raw |> slice(1) |> select(-sasdate) |> unlist() # retrieve transformation codes

fred <- fred_raw |>
  slice(-1) |> # remove code row
  mutate(date = as.Date(sasdate, format = "%m/%d/%Y")) |> # convert to date format
  select(-sasdate) |>
  relocate(date) # move date to first col
fred <- fred |> tidyr::fill(-date, .direction = "down")

# define transformation function (standard according to fred appendix)
tf <- function(x, code) { # function takes in variable, transformation code
  d <- function(v) v - lag(v) # helper function that gives 1st difference
  switch(as.character(code),
         "1" = x, # if code is 1, return x unchanged
         "2" = d(x), # if code is 2, return first diff
         "3" = d(d(x)), # return 2nd diff
         "4" = log(x), # etc
         "5" = d(log(x)),
         "6" = d(d(log(x))),
         "7" = d(x / lag(x) - 1))
}
# apply to all cols. for each col, calls (column, code)
fred_t <- fred |> mutate(across(-date, ~ tf(.x, tcode[[cur_column()]])))

sort(colSums(is.na(fred_t)), decreasing = TRUE) |> head(15) #missing values check
# not dropping missing values yet
#glimpse(fred_t)


## GOYAL'S DATA ##
g_raw <- read_excel("data/goyal_data_2025.xlsx", sheet = "Monthly")

# keep all vars for now
g_all <- g_raw |>
  rename_with(make.names) |> # clean names and dates
  mutate(date = ymd(paste0(yyyymm, "01"))) |>
  select(-yyyymm, -price) |>
  relocate(date)
# restrict to fred window
win <- g_all |> filter(date >= as.Date("1960-03-01"), date <= as.Date("2025-12-01"))

# drop variables that are na for the whole time
all_na <- names(win)[sapply(win, function(x) all(is.na(x)))]

# Drop full-sample-only Goyal variables (per ReadMe): built on the whole sample, so they leak future info
leaky_gw <- c("ogap", "tchi", "shtint", "fbm", "sntm")

# duplicated by FRED-MD / Yahoo features (or exact combinations of other columns)
dup_gw <- c("d12", "e12", "ret", "retx",
            "Rfree", "tbl", "lty", "AAA", "BAA",
            "tms", "dfy", "dfr",
            "infl", "wtexas",
            "svar", "dtoy", "dtoat",
            "d.y", "e.p", "d.e", "ygap")

g_all <- g_all |> select(-any_of(c(all_na, leaky_gw, dup_gw)))

# label the source
g <- g_all |> rename_with(~ paste0("gw_", .x), -date)

names(g)

###################
### FEATURE ENG ###
###################

# Build up the features from GOYAL using fred/yfin variables

# Extra features from FRED-MD
# Spreads and valuation ratios are built from the untransformed `fred`, not `fred_t`
fred_feat <- fred |>
  transmute(date,
            tms = GS10 - TB3MS, # term spread: long minus short rate
            dfy = BAA - AAA, # default spread: credit risk premium
            earn_yld = 1 / S.P.PE.ratio, # earnings yield (e/p)
            payout = S.P.div.yield * S.P.PE.ratio) # proportional to dividend/earnings

# summary(fred_feat)

# Realised variance from Yahoo daily prices
# Sum of squared daily log returns within each month
sp_vol <- sp |>
  mutate(date  = as.Date(date),
         ret_d = log(GSPC.Close) - lag(log(GSPC.Close))) |>
  group_by(month = floor_date(date, "month")) |>
  summarise(svar = sum(ret_d^2, na.rm = TRUE), .groups = "drop") |>
  filter(month < as.Date("2026-10-01")) |># drop incomplete Oct 2026
  rename(date = month)

# Return and price-position features from monthly closes
sp_feat <- sp_df |>
  arrange(date) |>
  mutate(ret_m = log(sp500) - lag(log(sp500)), # monthly return
         dtoy  = log(sp500) - log(zoo::rollmax(sp500, 12, fill = NA, align = "right")), # distance from 12-month high
         dtoat = log(sp500) - log(cummax(sp500))) |> # distance from all-time high
  select(date, ret_m, dtoy, dtoat)

#summary(sp_feat)


###########
## MERGE ##
###########

dat <- sp_df |>
  select(date, sp500) |>
  inner_join(fred_t,    by = "date") |>
  inner_join(fred_feat, by = "date") |>
  inner_join(sp_vol,    by = "date") |>
  inner_join(sp_feat,   by = "date") |>
  inner_join(g,         by = "date")

