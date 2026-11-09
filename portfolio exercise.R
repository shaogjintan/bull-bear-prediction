portfolio <- read.csv("data/portfolio_data.csv")
portfolio$date <- as.Date(portfolio$date)

panel <- read.csv("data/panel.csv")

files <- c("results/bagging_h1_w300.csv", "results/boost_d2_h1_w300.csv", "results/boost_d5_h1_w300.csv",
           "results/ensemble_avg_h1_w300.csv", "results/ensemble_vote_h1_w300.csv", "results/logit_h1_w300.csv",
           "results/persistence_h1.csv", "results/rf_h1_w300.csv", "results/rlasso_h1_w300.csv")

comparison <- data.frame()

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

#Buy-and-Hold benchmark
bh <- portfolio[portfolio$date >= as.Date("2001-07-01") & portfolio$date <= as.Date("2025-12-01"), ]
comparison <- rbind(Buy_Hold = evaluate(bh$spy_ret, bh$rf))

for (f in files){
  pred <- read.csv(f)
  pred$date <- as.Date(pred$date)
  
  #shifting the dates by one month to decide current strategy based on one-step ahead forecast
  pred$date <- seq(pred$date[1], by = "month", length.out = nrow(pred) + 1)[-1]
  results <- merge(pred, portfolio, by = "date")
  
  results <- results[results$date >= as.Date("2001-06-01") & results$date <= as.Date("2025-12-01"), ]
  
  #Strategy 1: If probability of bear market is above 0.5, then switch to risk-free
  results$s1 <- ifelse(results$prob < 0.5, results$spy_ret, results$rf)
  
  #Strategy 2: To use a different threshold than 0.5 since bear markets are rare
  #Proportion of bear markets in training data of 300 months 
  results$threshold <- NA
  
  for (j in 1:nrow(results)) {
    i <- match(results$date[j], panel$date)
    results$threshold[j] <- mean(panel$y_h1[(i - 301):(i - 2)]
    )
  }
  
  #If probability of bear market is above the threshold, then switch to risk-free
  results$s2 <- ifelse(results$prob < results$threshold, results$spy_ret, results$rf)
  
  temp <- rbind(
    Threshold_0.5 = evaluate(results$s1, results$rf),
    Threshold_rolling = evaluate(results$s2, results$rf)
  )
  rownames(temp) <- paste(basename(f), rownames(temp), sep = "_")
  comparison <- rbind(comparison, temp)
}

#Convert returns to percentages
comparison[, "Annual_Return"] <- comparison[, "Annual_Return"] * 100
comparison[, "CAGR"] <- comparison[, "CAGR"] * 100
comparison[, "Max_Drawdown"] <- comparison[, "Max_Drawdown"] * 100

print(round(comparison, 2))


