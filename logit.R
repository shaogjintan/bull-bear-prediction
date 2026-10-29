setwd("~/EC4308 ML Forecasting/bull-bear-prediction/data")
data <- read.csv("panel.csv")
library(ROCR)
#term spread is tms
#data$ret_m
#data$tms
#okay i cant get the dividend price ratio yet but i know how to - need the 8th lag
#lets just try building the base structure first


testrows <- which(data$date >= as.Date("2001-06-01"))
prob = rep(NA, length(testrows))

for(k in 1:length(testrows)){
  i <- testrows[k]
  traindata <- data[(i-480):(i-1),]
  
  model <- glm(state ~ state_l1 + ret_m_l1 + tms_l1, data = traindata, family = binomial(link="logit"))
  prob[k] <- predict(model, newdata = data[i,], type = "response")
  
}

res <- data.frame(date = data$date[testrows], actual = data$state[testrows], prob = prob)
res$forecast <- ifelse(res$prob >= 0.5, 1, 0)
confusion.matrix <- table(res$actual, res$forecast)

FP <- confusion.matrix["0", "1"]
TP <- confusion.matrix["1", "1"]
FN <- confusion.matrix["1", "0"]
TN <- confusion.matrix["0", "0"]
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
precision <- TP/(TP + FP)
recall <- TP/(TP + FN)
F1score <- 2*(precision*recall)/(precision+recall)


pred = prediction(res$prob, res$actual)
roc = performance(pred, "tpr", "fpr")
auc = performance(pred, "auc") 
auc@y.values[[1]]
plot(roc, col = "red", main = paste("Area under the curve:", round(auc@y.values[[1]], 4)))

