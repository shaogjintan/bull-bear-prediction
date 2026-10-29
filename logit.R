data <- read.csv("panel.csv")
library(ROCR)

#Nyberg uses the 8th lag of the first difference of the dividend-price ratio
data$ddp <- c(NA, diff(data$gw_d.p))
data$ddp_l7 <- c(rep(NA, 7), head(data$ddp, -7))
data$ddp_l5 <- c(rep(NA, 5), head(data$ddp, -5))
data$ddp_l2 <- c(rep(NA, 2), head(data$ddp, -2))

#We wish to predict the 1, 3, 6-step ahead forecast 
forecast_horizons <- c(1, 3, 6)

for (h in forecast_horizons){
  testrows <- which(data$date >= as.Date("2001-06-01"))
  prob = rep(NA, length(testrows))
  y <- paste0("y_h", h)
  ddp_var <- paste0("ddp_l", 8-h)
  
  #Rolling window of 480 months forms the training set
  for(k in 1:length(testrows)){
    i <- testrows[k]
    last_train <- i-h
    traindata <- data[(last_train-479):last_train,]
    model <- glm(as.formula(paste(y, "~ state + ret_m + tms + ", ddp_var)), data = traindata, family = binomial(link="logit"))
    prob[k] <- predict(model, newdata = data[i,], type = "response")
    
  }
  
  #Evaluation metrics
  res <- data.frame(date = data$date[testrows], actual = data[[y]][testrows], prob = prob)
  res$forecast <- ifelse(res$prob >= 0.5, 1, 0)
  confusion.matrix <- table(factor(res$actual, levels = c(0, 1)),factor(res$forecast, levels = c(0, 1)))
  
  FP <- confusion.matrix["0", "1"]
  TP <- confusion.matrix["1", "1"]
  FN <- confusion.matrix["1", "0"]
  TN <- confusion.matrix["0", "0"]
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  F1score <- 2*(precision*recall)/(precision+recall)
  
  
  pred <- prediction(res$prob, res$actual)
  roc <- performance(pred, "tpr", "fpr")
  auc <- performance(pred, "auc")@y.values[[1]]
  
  cat("\nForecast horizon:", h, "months\n")
  print(confusion.matrix)
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 score:", F1score, "\n")
  cat("AUC:", auc, "\n")
}


