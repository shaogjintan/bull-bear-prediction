library(dplyr)
library(hdm) #rlassologit
# library(caret)
# library(HDeconometrics)
# library(glmnet)
# library(pROC)

data = read.csv("data/panel.csv")
dir.create("results", showWarnings = FALSE)

# settings
horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts
nprev = 295 # number of test months
windows  <- c(300)

## predictors (X) ##
# lag-0 columns kept: market variables, known at month end
keep_lag0  <- c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy")
lag0_cols  <- names(data)[!grepl("_l[0-9]+$", names(data))] # columns without an _l1.._l6 suffix
state_cols <- c("state", paste0("state_l", 1:6))           # rlasso leaves out the market state

dropped_columns <- unique(c("date", "sp500", "y_h1", "y_h3", "y_h6", state_cols,
                            setdiff(lag0_cols, keep_lag0))) # lag-0 macro/Goyal: not released yet at t
X_all <- as.matrix(select(data, -all_of(dropped_columns)))

# the window has W + h rows: train on the first W (their targets are known at the origin),
# then predict the last row (the origin)
runrlassologit=function(y, X, lag){
  X_train= X[1:(nrow(X)-lag),] #drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = y[1:(nrow(X)-lag)] 
  keep = apply(X_train, 2, sd) > 0 # columns that are not constant in this window
  X_train <- X_train[, keep, drop = FALSE]
  X_test = X[nrow(X), keep, drop = FALSE] # forecast row, same columns as training    
  model = rlassologit(x=X_train,y=y_train, post=FALSE)
  # compute the forecast by hand: predict() from hdm mishandles a single row
  b = model$coefficients # intercept first, then one coefficient per column
  score = b[1] + sum(X_test[1, ] * b[-1]) # intercept + sum of (value x coefficient)
  pred = plogis(score) # turn the score into a bear probability between 0 and 1
  return(list("model"=model,"pred"=pred)) # Return the estimated model and h-step forecast
}


rlassologit.rolling.window=function(data,nprev,lag,rollingwindow_size){
  save.pred = numeric(nprev)
  save.selected = vector("list", nprev)
  X = X_all # regressors built at the top (lag-0 macro/Goyal and state already dropped)
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){ 
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] # Define the estimation window 
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),] 
    lassologit=runrlassologit(y.window,X.window,lag) #  Fit rlassologit and predict
    save.pred[(1+nprev-i)]=lassologit$pred # Save the forecast
    b = lassologit$model$coefficients[-1]
    save.selected[[1+nprev-i]] = names(b)[b != 0]
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(list("pred"=save.pred,"selected"=save.selected)) # CHANGED: was return(save.pred)
}

## run: 2 windows x 3 horizons, save each to results/ ##
for (W in windows) {
  for (h in horizons) {
    out    = rlassologit.rolling.window(data, nprev, h, W)
    prob   = out$pred
    actual = tail(data[[paste0("y_h", h)]], nprev)
    dates  = tail(data$date, nprev)
    res    = data.frame(date = dates, actual = actual, prob = prob)
    write.csv(res, sprintf("results/rlasso_h%d_w%d.csv", h, W), row.names = FALSE)
    # variables selected in each window, one row per selected variable
    selected = data.frame(date = rep(dates, lengths(out$selected)), variable = unlist(out$selected))
    write.csv(selected, sprintf("results/rlasso_selected_w%d_h%d.csv", W, h), row.names = FALSE)
  }
}
## selection frequency: % of windows where any lag of a variable is selected ##
W = 300
sel_all = do.call(rbind, lapply(horizons, function(h) {
  s = read.csv(sprintf("results/rlasso_selected_w%d_h%d.csv", W, h)); s$h = h; s }))
sel_all$base = sub("_l[0-9]+$", "", sel_all$variable) # group the lags of each variable
freq = sel_all |> distinct(h, date, base) |> count(h, base) |> mutate(pct = round(100 * n / nprev, 1))
tab = tidyr::pivot_wider(freq, id_cols = base, names_from = h, values_from = pct, values_fill = 0, names_prefix = "h")
tab = tab[order(-rowSums(tab[, c("h1", "h3", "h6")])), c("base", "h1", "h3", "h6")]
write.csv(tab, sprintf("tables/rlasso_selection_frequency_w%d.csv", W), row.names = FALSE)
print(as.data.frame(head(tab, 15)))
print(sel_all |> count(h) |> mutate(avg_selected = n / nprev)) # average number of variables per window


#checks
# AUC without extra packages
auc_fn <- function(prob, actual) {
  r <- rank(prob); n1 <- sum(actual == 1); n0 <- sum(actual == 0)
  (sum(r[actual == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

for (f in list.files("results", pattern = "^rlasso_h.*\\.csv$", full.names = TRUE)) {
  d <- read.csv(f)
  pred <- as.integer(d$prob > 0.5)
  cat(sprintf("%-25s rows=%d  %s to %s  NA=%d  prob %.3f-%.3f  AUC=%.3f  bears caught=%d/%d  false alarms=%d\n",
              basename(f), nrow(d), d$date[1], tail(d$date, 1), sum(is.na(d$prob)),
              min(d$prob), max(d$prob), auc_fn(d$prob, d$actual),
              sum(pred == 1 & d$actual == 1), sum(d$actual == 1),
              sum(pred == 1 & d$actual == 0)))
}

# rlassologit1 = rlassologit.rolling.window(data, 295, 1)
# rll1_pred = factor(as.numeric(rlassologit1 > 0.5), levels=c(0,1))
# actual1 = factor(tail(data$y_h1, 295), levels=c(0,1))
# 
# rlassologit3 = rlassologit.rolling.window(data, 295, 3)
# rll3_pred = factor(as.numeric(rlassologit3 > 0.5), levels=c(0,1))
# actual3 = factor(tail(data$y_h3, 295), levels=c(0,1))
# 
# rlassologit6 = rlassologit.rolling.window(data, 295, 6)
# rll6_pred = factor(as.numeric(rlassologit6 > 0.5), levels=c(0,1))
# actual6 = factor(tail(data$y_h6, 295), levels=c(0,1))
# 
# cm_rll1 = confusionMatrix(rll1_pred,actual1)
# cm_rll3 = confusionMatrix(rll3_pred,actual3)
# cm_rll6 = confusionMatrix(rll6_pred,actual6)
# 
# roc1 = roc(response = actual1, predictor = rlassologit1, levels = c(0, 1), direction = "<")
# roc3 = roc(response = actual3, predictor = rlassologit3, levels = c(0, 1), direction = "<")
# roc6 = roc(response = actual6, predictor = rlassologit6, levels = c(0, 1), direction = "<")
# 
# auc(roc1)
# auc(roc3)
# auc(roc6)