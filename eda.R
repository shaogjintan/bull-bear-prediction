library(dplyr)
#############################################
## EDA: DATA OVERVIEW BEFORE ANY MODELLING ##
#############################################
# Output: tables printed and saved to tables/, one figure saved to figures/

panel  <- read.csv("data/panel.csv")  |> mutate(date = as.Date(date)) # modelling sample
target <- read.csv("data/target.csv") |> mutate(date = as.Date(date)) # full S&P series with labels
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

## Table 1: bear market chronology (Lunde-Timmermann, 20% rule) ##
# a bear run = consecutive months with state == 1 (starts in the month the 20% fall is signalled)
# peak = highest close in the bull run before it; trough = lowest close inside it
r      <- rle(target$state)
ends   <- cumsum(r$lengths)
starts <- ends - r$lengths + 1
n      <- nrow(target)

chron <- do.call(rbind, lapply(which(r$values == 1), function(k) {
  s <- starts[k]; e <- ends[k]
  bull_start <- starts[k - 1] # runs alternate, so the run before a bear run is a bull run
  peak   <- bull_start - 1 + which.max(target$sp500[bull_start:(s - 1)])
  trough <- s - 1 + which.min(target$sp500[s:e])
  data.frame(peak           = target$date[peak],
             bear_signalled = target$date[s],
             trough         = target$date[trough],
             bull_signalled = if (e < n) target$date[e + 1] else as.Date(NA),
             months_in_bear = e - s + 1,
             fall_pct       = round(100 * (target$sp500[trough] / target$sp500[peak] - 1), 1),
             delay_peak     = s - peak,                            # months from peak to bear signal
             delay_trough   = if (e < n) e + 1 - trough else NA)   # months from trough to bull signal
}))
print(chron, row.names = FALSE)
write.csv(chron, "tables/eda_bear_chronology.csv", row.names = FALSE)

## Table 2: bull/bear counts in each period of the design ##
W          <- 300                                  # rolling window used for boosting validation
val_start  <- panel$date[W + 6]                    # first validation origin (full window for h up to 6)
test_start <- panel$date[nrow(panel) - 295 + 1]    # Jun 2001
before     <- function(d) seq(d, by = "-1 month", length.out = 2)[2] # the month before d

count_period <- function(name, from, to) {
  s <- panel$state[panel$date >= from & panel$date <= to]
  data.frame(period = name, from = from, to = to, months = length(s),
             bull = sum(s == 0), bear = sum(s == 1), bear_share = round(mean(s), 3),
             bear_markets = sum(diff(c(0, s)) == 1)) # bear runs starting (or already running) in the period
}
periods <- rbind(
  count_period("Before validation",   min(panel$date), before(val_start)),
  count_period("Boosting validation", val_start,       before(test_start)),
  count_period("Test",                test_start,      max(panel$date)),
  count_period("Full sample",         min(panel$date), max(panel$date)))
print(periods, row.names = FALSE)
write.csv(periods, "tables/eda_periods.csv", row.names = FALSE)

## Table 3: predictors by source ##
base <- names(panel)[!grepl("_l[0-9]+$", names(panel))] # lag-0 names = one per variable
base <- setdiff(base, c("date", "sp500", "state", "y_h1", "y_h3", "y_h6"))
source_of <- ifelse(grepl("^gw_", base), "Goyal et al. (2024)",
                    ifelse(base %in% c("tms", "dfy", "earn_yld", "payout"), "Constructed from FRED-MD",
                           ifelse(base %in% c("ret_m", "dtoy", "dtoat", "svar"), "S&P 500 (Yahoo Finance)",
                                  "FRED-MD")))
feat <- as.data.frame(table(source = source_of), responseName = "variables")
feat <- rbind(feat, data.frame(source = "Total", variables = sum(feat$variables)))
print(feat, row.names = FALSE)
cat("Sample:", format(min(panel$date)), "to", format(max(panel$date)), "(", nrow(panel), "months )\n")
cat("Columns in panel.csv (variables, lags 1-6, state and its lags, targets):", ncol(panel), "\n")
write.csv(feat, "tables/eda_predictors_by_source.csv", row.names = FALSE)

## Table 4: key predictors in bull vs bear months ##
# FRED-MD series are in their transformed form (e.g. INDPRO = monthly log growth, UNRATE = change)
vars <- intersect(c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy",
                    "earn_yld", "S.P.div.yield", "INDPRO", "UNRATE"), names(panel))
regime <- data.frame(
  variable  = vars,
  mean_bull = sapply(vars, function(v) mean(panel[[v]][panel$state == 0])),
  mean_bear = sapply(vars, function(v) mean(panel[[v]][panel$state == 1])))
regime$difference <- regime$mean_bear - regime$mean_bull
regime[, -1] <- round(regime[, -1], 4)
print(regime, row.names = FALSE)
write.csv(regime, "tables/eda_bull_vs_bear_means.csv", row.names = FALSE)

## Table 5: bull and bear months per decade ##
by_decade <- panel |>
  group_by(decade = paste0(floor(as.integer(format(date, "%Y")) / 10) * 10, "s")) |>
  summarise(months = n(), bull = sum(state == 0), bear = sum(state == 1),
            bear_share = round(mean(state), 3))
print(by_decade)
write.csv(by_decade, "tables/eda_by_decade.csv", row.names = FALSE)

## Figure 1: S&P 500 (log scale) with bear markets shaded ##
png("figures/sp500_bear_shading.png", width = 1600, height = 700, res = 150)
plot(target$date, log(target$sp500), type = "n", xlab = "", ylab = "log S&P 500",
     main = "S&P 500 with bear markets shaded (Lunde-Timmermann, 20% rule)")
bear_runs <- which(r$values == 1)
rect(as.numeric(target$date[starts[bear_runs]]), par("usr")[3],
     as.numeric(target$date[ends[bear_runs]]),   par("usr")[4],
     col = "grey80", border = NA)
lines(target$date, log(target$sp500))
dev.off()

