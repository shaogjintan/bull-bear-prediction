library(dplyr)
library(randomForest)

data = read.csv("data/panel.csv")
dir.create("results", showWarnings = FALSE)
set.seed(4308)

windows = c(300) # 25-year rolling windows
horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts
nprev = 295 # number of test months (Jun 2001 - Dec 2025)

# Unlike rlasso, RF keeps state and its lags, because trees can use them
keep_lag0 = c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy", "state") # lag-0 columns known at month end
lag0_cols = names(data)[!grepl("_l[0-9]+$", names(data))] # columns without an _l1.._l6 suffix
dropped_columns = unique(c("date", "sp500", "y_h1", "y_h3", "y_h6",
                           setdiff(lag0_cols, keep_lag0))) # lag-0 macro/Goyal: not released yet at t
X_all = as.matrix(select(data, -all_of(dropped_columns)))

# the window has W + h rows: train on the first W (their targets are known at the origin),
# then predict the last row (the origin)
runrf=function(y, X,lag){
  X_train= X[1:(nrow(X)-lag),] #drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = factor(y[1:(nrow(X)-lag)], levels=c(0,1))
  X_test = X[nrow(X),,drop = FALSE] # Regressors that are used for the forecast
  model=randomForest(x=X_train,y=y_train,importance = FALSE,ntree=500, mtry = floor(sqrt(ncol(X_train))))  
  pred=predict(model,newdata=X_test,type="prob")[, "1"] # Generate the forecast
  return(list("model"=model,"pred"=pred)) # Return the estimated model and h-step forecast
}


rf.rolling.window=function(data,nprev,lag,rollingwindow_size){
  save.pred = numeric(nprev)
  X = X_all # regressors built at the top
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){ 
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] # Define the estimation window 
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),] 
    rf=runrf(y.window,X.window,lag) #  Fit random forest and predict
    save.pred[(1+nprev-i)]=rf$pred # Save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(save.pred)
}

## run: 3 horizons, save each to results/ ##
for (W in windows) {
  for (h in horizons) {
    set.seed(4308)
    prob   = rf.rolling.window(data, nprev, h, W)
    actual = tail(data[[paste0("y_h", h)]], nprev)
    res    = data.frame(date = tail(data$date, nprev), actual = actual, prob = prob)
    write.csv(res, sprintf("results/rf_h%d_w%d.csv", h, W), row.names = FALSE)
  }
}

## checks ##
auc_fn <- function(prob, actual) {
  r <- rank(prob); n1 <- sum(actual == 1); n0 <- sum(actual == 0)
  (sum(r[actual == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

for (f in list.files("results", pattern = "^rf_h.*\\.csv$", full.names = TRUE)) {
  d <- read.csv(f)
  pred <- as.integer(d$prob > 0.5)
  cat(sprintf("%-25s rows=%d  %s to %s  NA=%d  prob %.3f-%.3f  AUC=%.3f  bears caught=%d/%d  false alarms=%d\n",
              basename(f), nrow(d), d$date[1], tail(d$date, 1), sum(is.na(d$prob)),
              min(d$prob), max(d$prob), auc_fn(d$prob, d$actual),
              sum(pred == 1 & d$actual == 1), sum(d$actual == 1),
              sum(pred == 1 & d$actual == 0)))
}
