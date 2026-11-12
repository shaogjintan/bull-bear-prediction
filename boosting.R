library(dplyr)
library(xgboost)
#library(caret)
library(pROC)

data = read.csv("data/panel.csv")
data$date = as.Date(data$date)
dir.create("results", showWarnings = FALSE)
set.seed(4308)

params <- list(
  nthread = parallel::detectCores() - 1, 
  objective = "binary:logistic"
)

rollingwindow_size = 300 # 25-year rolling window (40 years leaves no room for validation before 2001)
horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts

# Test window: Jun 2001 - Dec 2025, the same 295 months as every other model
nprev_test = 295
test_start_row = nrow(data) - nprev_test + 1

# Validation window: from the first origin with a full window (for h up to 6) to h months before the test,
# so every validation target (the state h months later) is already known when the test starts
val_start_row = rollingwindow_size + 6
cat("Validation start:", format(data$date[val_start_row]), "\n")
cat("Test:", format(data$date[test_start_row]), "to", format(data$date[nrow(data)]), "\n")

## predictors (X) ##
# lag-0 columns kept: market variables and the current state, known at month end
keep_lag0 = c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy", "state")
lag0_cols = names(data)[!grepl("_l[0-9]+$", names(data))] # columns without an _l1.._l6 suffix
dropped_columns = unique(c("date", "sp500", "y_h1", "y_h3", "y_h6",
                           setdiff(lag0_cols, keep_lag0))) # lag-0 macro/Goyal: not released yet at t
X_all = as.matrix(select(data, -all_of(dropped_columns)))

# Boosting parameters
depths = c(2, 5) # Tree depth of 2 or 5
eta = 0.05 # Fixed learning rate 
nrounds_grid = seq(25, 1000, by = 25) # Number of trees from 25 to 1000

parameters = function(d){
  c(params, list(max_depth = d, eta = eta)) # Eta is fixed at 0.05, while d is either 2 or 5
}

# the window has W + h rows: train on the first W (their targets are known at the origin),
# then predict the last row (the origin)
runboosting=function(y, X,lag,params,nrounds){
  X_train= X[1:(nrow(X)-lag),] # Drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = y[1:(nrow(X)-lag)]
  X_test = X[nrow(X),,drop = FALSE] # Regressors that are used for the forecast
  dtrain = xgb.DMatrix(data=X_train, label=y_train)
  model=xgb.train(params=params, data=dtrain, nrounds=max(nrounds)) # fit once with the largest number of trees
  # iterationrange counts the end differently across xgboost versions:
  # check whether c(1, max) already uses all trees; if not, the end is excluded and needs +1
  full = predict(model, X_test)
  shift = ifelse(abs(predict(model, X_test, iterationrange = c(1, max(nrounds))) - full) < 1e-10, 0, 1)
  predicted = numeric(length(nrounds))
  for (i in 1:length(nrounds)) {
    predicted[i] = predict(model, X_test, iterationrange = c(1, nrounds[i] + shift)) # forecast using the first nrounds[i] trees
  }
  return(list("model"= model,"pred"= predicted)) # Return the estimated model and h-step forecast
}

boosting.rolling.window=function(data,nprev,lag,end_row,params,nrounds){
  save.pred = matrix(NA,nprev,length(nrounds))
  X = X_all # regressors built at the top (includes state and its lags)
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){
    y.window=y[(end_row-i+1-(rollingwindow_size+lag-1)):(end_row-i+1)] # Define the estimation window
    X.window=X[(end_row-i+1-(rollingwindow_size+lag-1)):(end_row-i+1),]
    boost=runboosting(y.window,X.window,lag,params,nrounds) #  Fit boosting and predict
    save.pred[(1+nprev-i),]=boost$pred # Save the forecast
    cat("iteration",(1+nprev-i),"\n") # display iteration number
  }
  return(save.pred)
}

## 1. tune the number of trees on the validation window, using AUC, for each horizon and depth ##
tune.boosting=function(lag,d){
  y = data[[paste0("y_h", lag)]]
  val_end = test_start_row - lag # last validation origin: its target (state at test start) is known at the first test origin
  nprev_val = val_end - val_start_row + 1
  actual_val = y[val_start_row:val_end]
  p = boosting.rolling.window(data, nprev_val, lag, val_end, parameters(d), nrounds_grid)
  auc = apply(p, 2, function(s) as.numeric(auc(roc(response = actual_val, predictor = s,
                                                   levels = c(0, 1), direction = "<", quiet = TRUE))))
  return(data.frame(lag = lag, depth = d, nrounds = nrounds_grid, val_auc = auc))
}

tuning = data.frame()
for(lag in horizons){
  cat("validation bear months, h =", lag, ":", sum(data[[paste0("y_h", lag)]][val_start_row:(test_start_row - lag)]), "\n")
  for(d in depths){
    tuning = rbind(tuning, tune.boosting(lag,d))
  }
}
write.csv(tuning, sprintf("results/boost_tuning_w%d.csv", rollingwindow_size), row.names = FALSE)

# best number of trees for each horizon and depth
best = tuning |> group_by(lag, depth) |> slice_max(val_auc, n = 1, with_ties = FALSE) |> ungroup()
print(best)

# 2. test set (Jun 2001 - Dec 2025) with the chosen number of trees, both depths ##
for(k in 1:nrow(best)){
  h = best$lag[k]; d = best$depth[k]; M = best$nrounds[k]
  prob = boosting.rolling.window(data, nprev_test, h, nrow(data), parameters(d), M)[, 1]
  actual = tail(data[[paste0("y_h", h)]], nprev_test)
  res = data.frame(date = tail(data$date, nprev_test), actual = actual, prob = prob)
  write.csv(res, sprintf("results/boost_d%d_h%d_w%d.csv", d, h, rollingwindow_size), row.names = FALSE)
}

# checks ##
for (f in list.files("results", pattern = "^boost_d.*\\.csv$", full.names = TRUE)) {
  dd = read.csv(f)
  pred = as.integer(dd$prob > 0.5)
  cat(sprintf("%-25s rows=%d  %s to %s  NA=%d  prob %.3f-%.3f  AUC=%.3f  bears caught=%d/%d  false alarms=%d\n",
              basename(f), nrow(dd), dd$date[1], tail(dd$date, 1), sum(is.na(dd$prob)),
              min(dd$prob), max(dd$prob),
              as.numeric(auc(roc(dd$actual, dd$prob, levels = c(0, 1), direction = "<", quiet = TRUE))),
              sum(pred == 1 & dd$actual == 1), sum(dd$actual == 1),
              sum(pred == 1 & dd$actual == 0)))
}