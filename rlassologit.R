library(dplyr)
library(hdm)
library(caret)
library(HDeconometrics)
library(glmnet)
library(pROC)
data = read.csv("data/panel.csv")



horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts
nprev = 295

runrlassologit=function(y, X,lag){
    X_train= X[1:(nrow(X)-lag),] #drop the last h rows such that they are not used to train the model (to prevent leakage)
    y_train = y[1:(nrow(X)-lag)] 
    X_train <- X_train[, apply(X_train, 2, sd) > 0, drop = FALSE]
    X_test = X[nrow(X),,drop = FALSE] # Regressors that are used for the forecast
    model = rlassologit(x=X_train,y=y_train, post=FALSE)
    pred=predict(model,newdata=X_test,type="response")[1] # Generate the forecast
    return(list("model"=model,"pred"=pred)) # Return the estimated model and h-step forecast
}


rlassologit.rolling.window=function(data,nprev,lag){
  rollingwindow_size = 360 # Number of observations used in the model
  save.pred = numeric(nprev)  
  dropped_columns = c("date","sp500", "y_h1", "y_h3", "y_h6", "state_l1", "state_l2", "state_l3", "state_l4", "state_l5", "state_l6", "state")
  X = select(data, -all_of(dropped_columns)) # Matrix of regressors
  y = data[[paste0("y_h", lag)]] # Vector of target variable (y_h1, y_h3, y_h6) to be forecasted
  for(i in nprev:1){ 
    y.window=y[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1)] # Define the estimation window 
    X.window=X[(nrow(X)-i+1-(rollingwindow_size+lag-1)):(nrow(X)-i+1),] 
    X.window = as.matrix(X.window)
    lassologit=runrlassologit(y.window,X.window,lag) #  Fit rlassologit and predict
    save.pred[(1+nprev-i)]=lassologit$pred # Save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  return(save.pred)
}

rlassologit1 = rlassologit.rolling.window(data, 295, 1)
rll1_pred = factor(as.numeric(rlassologit1 > 0.5), levels=c(0,1))
actual1 = factor(tail(data$y_h1, 295), levels=c(0,1))

rlassologit3 = rlassologit.rolling.window(data, 295, 3)
rll3_pred = factor(as.numeric(rlassologit3 > 0.5), levels=c(0,1))
actual3 = factor(tail(data$y_h3, 295), levels=c(0,1))

rlassologit6 = rlassologit.rolling.window(data, 295, 6)
rll6_pred = factor(as.numeric(rlassologit6 > 0.5), levels=c(0,1))
actual6 = factor(tail(data$y_h6, 295), levels=c(0,1))

cm_rll1 = confusionMatrix(rll1_pred,actual1)
cm_rll3 = confusionMatrix(rll3_pred,actual3)
cm_rll6 = confusionMatrix(rll6_pred,actual6)

roc1 = roc(response = actual1, predictor = rlassologit1, levels = c(0, 1), direction = "<")
roc3 = roc(response = actual3, predictor = rlassologit3, levels = c(0, 1), direction = "<")
roc6 = roc(response = actual6, predictor = rlassologit6, levels = c(0, 1), direction = "<")

auc(roc1)
auc(roc3)
auc(roc6)