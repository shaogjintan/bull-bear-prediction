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

  imp= importance(model)[, "MeanDecreaseGini"]
  imp= tapply(imp, sub("_l[0-9]+$", "", names(imp)), sum)   # Compute feature importance by summing importance of all lags
  imp = imp / sum(imp)                                        # shares that add to 1

  return(list("model"=model,"pred"=pred, "imp"=imp)) #Return the estimated model and h-step forecast
}


bagging.rolling.window=function(data,nprev,lag,rollingwindow_size){
  save.pred = numeric(nprev)
  save.imp = vector("list", nprev)              
  X = X_all
  y = data[[paste0("y_h", lag)]] 
  for(i in nprev:1){
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] #Define the estimation window
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),]
    bag=runbagging(y.window,X.window,lag) #Fit bagging and predict
    save.pred[(1+nprev-i)]=bag$pred #Save the forecast
    save.imp[[1+nprev-i]] = bag$imp # Save feature importance
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(list(pred = save.pred, imp = do.call(rbind, save.imp)))
}

#run for both windows and all horizons, one file per window and horizon
imp_all = data.frame() 

for (W in windows) {
  for (h in horizons) {
    set.seed(4308)
    out = bagging.rolling.window(data, nprev, h, W)
    prob = out$pred
    actual = tail(data[[paste0("y_h", h)]], nprev)
    res = data.frame(date = tail(data$date, nprev), actual = actual, prob = prob)
    write.csv(res, sprintf("results/bagging_h%d_w%d.csv", h, W), row.names = FALSE)
    m = colMeans(out$imp) # Calculate average importance share of each variable across all rolling windows
    imp_all = rbind(imp_all, data.frame(h = h, model = "bagging", variable = names(m), mean_share = as.numeric(m), windows = nrow(out$imp))) 

  }
}

top_features = imp_all |>
  group_by(h) |>
  slice_max(mean_share, n = 10, with_ties = FALSE) |>
  ungroup()

write.csv(top_features, sprintf("tables/importance_bagging_top10_w%d.csv", windows[1]), row.names = FALSE)