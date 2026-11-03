library(dplyr)
library(xgboost)
library(caret)
library(pROC)
data = read.csv("data/panel.csv")
data$date = as.Date(data$date)

params <- list(
  nthread = parallel::detectCores() - 1, 
  objective = "binary:logistic"
)

rollingwindow_size = 480 # Number of observations used in rolling window

# Validation window: 1998-01 to 2009-12 
# Test window: 2010-01 onwards
val_start_row = rollingwindow_size + 6 
val_end_row = max(which(data$date <= as.Date("2009-12-31")))
nprev_val = val_end_row - val_start_row + 1 # Number of observations in the validation window
nprev_test = nrow(data) - val_end_row # Number of observations in the test window

dropped_columns = c("date","sp500", "y_h1", "y_h3", "y_h6")

# Boosting parameters
depths = c(2, 5) # Tree depth of 2 or 5
eta = 0.05 # Fixed learning rate 
nrounds_grid = seq(25, 1000, by = 25) # Number of trees from 25 to 1000

runboosting=function(y, X,lag,params,nrounds){
  X_train= X[1:(nrow(X)-lag),] # Drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = y[1:(nrow(X)-lag)]
  X_test = X[nrow(X),,drop = FALSE] # Regressors that are used for the forecast
  dtrain = xgb.DMatrix(data=X_train, label=y_train) 
  model=xgb.train(params=c(params,list(objective="binary:logistic", seed=4308)), data=dtrain, nrounds=max(nrounds)) # fit once with the largest number of trees
  predicted = numeric(length(nrounds))
  for (i in 1:length(nrounds)) {
    predicted[i] = predict(model, X_test, iterationrange = c(1, nrounds[i])) # Generate forecast
  }
  return(list("model"= model,"pred"= predicted)) # Return the estimated model and h-step forecast
}


boosting.rolling.window=function(data,nprev,lag,end_row,params,nrounds){
  save.pred = matrix(NA,nprev,length(nrounds))
  X = select(data, -all_of(dropped_columns)) # Matrix of regressors (includes state and its lags)
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){
    y.window=y[(end_row-i+1-(rollingwindow_size+lag-1)):(end_row-i+1)] # Define the estimation window
    X.window=X[(end_row-i+1-(rollingwindow_size+lag-1)):(end_row-i+1),]
    X.window = as.matrix(X.window)
    boost=runboosting(y.window,X.window,lag,params,nrounds) #  Fit boosting and predict
    save.pred[(1+nprev-i),]=boost$pred # Save the forecast
    cat("iteration",(1+nprev-i),"\n") # display iteration number
  }
  return(save.pred)
}

parameters = function(d){
  list(max_depth = d, eta = eta) # Eta is fixed at 0.05, while d is either
}


# Tune the number of trees on the validation window using AUC, for each tree depth
tune.boosting=function(lag,d){
  y = data[[paste0("y_h", lag)]]
  actual_val = factor(y[val_start_row:val_end_row], levels=c(0,1))
  set.seed(4308)
  p = boosting.rolling.window(data, nprev_val, lag, val_end_row, parameters(d), nrounds_grid)
  auc = apply(p, 2, function(s) as.numeric(auc(roc(response = actual_val, predictor = s, levels = c(0, 1), direction = "<", quiet = TRUE))))
  return(data.frame(lag = lag, depth = d, nrounds = nrounds_grid, val_auc = auc))
}

tuning = list()
for(lag in c(1,3,6)){
  for(d in depths){
    tuning[[paste0("h",lag,"_d",d)]] = tune.boosting(lag,d)
  }
}

best_nrounds = sapply(tuning, function(r) r$nrounds[which.max(r$val_auc)])
best_val_auc = sapply(tuning, function(r) max(r$val_auc))
data.frame(best_nrounds, best_val_auc) # selected number of trees and validation AUC for each horizon/depth

# Evaluation on the test set (2010-01 onwards) based on the chosen hyperparameters
# h=1: nrounds=50, tree depth = 2
set.seed(4308)
boosting1_d2 = boosting.rolling.window(data, nprev_test, 1, nrow(data), parameters(2), 50)
boosting1_d2_pred = factor(as.numeric(boosting1_d2 > 0.5), levels=c(0,1))
# h=3: nrounds=250, tree depth = 5
set.seed(4308)
boosting3_d5 = boosting.rolling.window(data, nprev_test, 3, nrow(data), parameters(5), 250)
boosting3_d5_pred = factor(as.numeric(boosting3_d5 > 0.5), levels=c(0,1))

# h=6: nrounds=25, tree depth = 2
set.seed(4308)
boosting6_d2 = boosting.rolling.window(data, nprev_test, 6, nrow(data), parameters(2), 25)
boosting6_d2_pred = factor(as.numeric(boosting6_d2 > 0.5), levels=c(0,1))


actual1 = factor(tail(data$y_h1, nprev_test), levels=c(0,1))
actual3 = factor(tail(data$y_h3, nprev_test), levels=c(0,1))
actual6 = factor(tail(data$y_h6, nprev_test), levels=c(0,1))

# Evaluation metrics
cm_boosting1_d2 = confusionMatrix(boosting1_d2_pred, actual1, positive = "1", mode = "everything")
cm_boosting6_d2 = confusionMatrix(boosting6_d2_pred, actual6, positive = "1", mode = "everything")
cm_boosting3_d5 = confusionMatrix(boosting3_d5_pred, actual3, positive = "1", mode = "everything")

roc1_d2 = roc(response = actual1, predictor = as.numeric(boosting1_d2), levels = c(0, 1), direction = "<")
roc6_d2 = roc(response = actual6, predictor = as.numeric(boosting6_d2), levels = c(0, 1), direction = "<")
roc3_d5 = roc(response = actual3, predictor = as.numeric(boosting3_d5), levels = c(0, 1), direction = "<")

auc(roc1_d2); auc(roc6_d2); auc(roc3_d5)
