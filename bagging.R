library(dplyr)
library(randomForest)

data = read.csv("data/panel.csv")
dir.create("results", showWarnings = FALSE)
set.seed(4308)

windows = c(300) # 25 year rolling windows
horizons = c(1, 3, 6) #1,3,6 step-ahead forecasts
nprev = 295 #number of out-of-sample observations (Jun 2001 - Dec 2025)

#lag 0 columns that are known at the end of the month, the other lag 0 columns (FRED, Goyal) are published later
keep_lag0 = c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy", "state")
lag0_cols = names(data)[!grepl("_l[0-9]+$", names(data))] #columns without an _l1.._l6 suffix
dropped_columns = unique(c("date", "sp500", "y_h1", "y_h3", "y_h6",
                           setdiff(lag0_cols, keep_lag0)))
X_all = as.matrix(select(data, -all_of(dropped_columns))) #Matrix of regressors, same as rf.R

runbagging=function(y, X,lag){
  X_train= X[1:(nrow(X)-lag),] #drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = factor(y[1:(nrow(X)-lag)], levels=c(0,1))
  X_test = X[nrow(X),,drop = FALSE] #Regressors that are used for the forecast
  model=randomForest(x=X_train,y=y_train,importance = FALSE,ntree=500, mtry = ncol(X_train))
  pred=predict(model,newdata=X_test,type="prob")[, "1"] #Generate the forecast
  return(list("model"=model,"pred"=pred)) #Return the estimated model and h-step forecast
}


bagging.rolling.window=function(data,nprev,lag,rollingwindow_size){
  save.pred = numeric(nprev)
  X = X_all
  y = data[[paste0("y_h", lag)]] 
  for(i in nprev:1){
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] #Define the estimation window
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),]
    bag=runbagging(y.window,X.window,lag) #Fit bagging and predict
    save.pred[(1+nprev-i)]=bag$pred #Save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(save.pred)
}

#run for both windows and all horizons, one file per window and horizon
for (W in windows) {
  for (h in horizons) {
    set.seed(4308)
    prob = bagging.rolling.window(data, nprev, h, W)
    actual = tail(data[[paste0("y_h", h)]], nprev)
    res = data.frame(date = tail(data$date, nprev), actual = actual, prob = prob)
    write.csv(res, sprintf("results/bagging_h%d_w%d.csv", h, W), row.names = FALSE)
  }
}
