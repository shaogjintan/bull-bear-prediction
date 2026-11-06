portfolio <- read.csv("data/portfolio_data.csv")
pred <- read.csv("results/bagging_h1_w300.csv")

portfolio$date <- as.Date(portfolio$date)
pred$date <- as.Date(pred$date)

#shifting the dates by one month to decide current strategy based on one-step ahead forecast
pred$date <- seq(as.Date("2001-07-01"),
                 by = "month",
                 length.out = nrow(pred))
results <- merge(pred, portfolio, by = "date")

results <- results[
  results$date >= as.Date("2001-07-01") &
    results$date <= as.Date("2025-12-01"), ]

#The 3 different strategies
#Strategy 1: If probability of bear market is above 0.5, then switch to risk-free
results$s1 <- ifelse(results$prob < 0.5,
                        results$spy_ret,
                        results$rf)

#Strategy 2: To use a different threshold than 0.5 since bear markets are rare
#Proportion of bear markets in training data of 480 months 
panel <- read.csv("data/panel.csv")
mean(panel$state[1:480] == 1)

#If probability of bear market is above 0.0979, then switch to risk-free
results$s2 <- ifelse(results$prob < 0.0979,
                        results$spy_ret,
                        results$rf)

#Strategy 3: Buy and Hold
results$s3 <- results$spy_ret

evaluate <- function(s, rf) {
  n <- length(s)
  
  #Annualized average return
  annual_return <- mean(s) * 12
  
  #Sharpe ratio
  excess <- s - rf
  sharpe <- sqrt(12) * mean(excess) / sd(excess)
  
  #Portfolio wealth
  wealth <- 10000 * cumprod(1 + s)
  
  #Final wealth
  final_wealth <- tail(wealth, 1)
  
  # CAGR
  years <- n / 12
  cagr <- (final_wealth / 10000)^(1 / years) - 1
  
  #Maximum drawdown (initial wealth as starting point)
  #calculates maximum drawdown from highest peak so far 
  wealth_path <- c(10000, wealth)
  peak <- cummax(wealth_path)
  drawdown <- wealth_path / peak - 1
  max_drawdown <- min(drawdown)
  
  return(c(
    Annual_Return = annual_return,
    Sharpe = sharpe,
    Final_Wealth = final_wealth,
    CAGR = cagr,
    Max_Drawdown = max_drawdown
  ))
}

comparison <- rbind(
  Buy_Hold = evaluate(results$s3, results$rf),
  Threshold_0.5 = evaluate(results$s1, results$rf),
  Threshold_0.0979 = evaluate(results$s2, results$rf)
)

#Convert returns to percentages
comparison[, "Annual_Return"] <- comparison[, "Annual_Return"] * 100
comparison[, "CAGR"] <- comparison[, "CAGR"] * 100
comparison[, "Max_Drawdown"] <- comparison[, "Max_Drawdown"] * 100

print(round(comparison, 2))
