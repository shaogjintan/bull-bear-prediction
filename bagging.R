library(dplyr)
library(hdm)
data = read.csv("data/panel.csv")

library(randomForest)

horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts
nprev = 295

runrf=function(y, X,lag,pl){
  X_train= X[1:(nrow(X)-lag),] #drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = factor(y[1:(nrow(X)-lag)], levels=c(0,1))
  X_test = X[nrow(X),,drop = FALSE] # Regressors that are used for the forecast
  model=randomForest(x=X_train,y=y_train,importance = FALSE,ntree=500, mtry = ncol(X_train))   
  pred=predict(model,newdata=X_test,type="prob")[, "1"] # Generate the forecast
  return(list("model"=model,"pred"=pred)) # Return the estimated model and h-step forecast
}


rf.rolling.window=function(data,nprev,lag,pl=FALSE){
  rollingwindow_size = 480 # Number of observations used in the model
  save.pred = numeric(nprev)  
  dropped_columns = c("date","sp500", "y_h1", "y_h3", "y_h6")
  X = select(data, -all_of(dropped_columns)) # Matrix of regressors
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){ 
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] # Define the estimation window 
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),] 
    X.window = as.matrix(X.window)
    rf=runrf(y.window,X.window,lag,pl) #  Fit rlassologit and predict
    save.pred[(1+nprev-i)]=rf$pred # Save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(save.pred)
}

bagging1 = rf.rolling.window(data, 295, 1)
bagging1_pred = factor(as.numeric(bagging1 > 0.5), levels=c(0,1))
actual1 = factor(tail(data$y_h1, 295), levels=c(0,1))

bagging3 = rf.rolling.window(data, 295, 3)
bagging3_pred = factor(as.numeric(bagging3 > 0.5), levels=c(0,1))
actual3 = factor(tail(data$y_h3, 295), levels=c(0,1))

bagging6 = rf.rolling.window(data, 295, 6)
bagging6_pred = factor(as.numeric(bagging6 > 0.5), levels=c(0,1))
actual6 = factor(tail(data$y_h6, 295), levels=c(0,1))

cm_bagging1 = confusionMatrix(bagging1_pred,actual1)
cm_bagging3 = confusionMatrix(bagging3_pred,actual3)
cm_bagging6 = confusionMatrix(bagging6_pred,actual6)

