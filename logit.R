data <- read.csv("data/panel.csv")

#Nyberg uses the 8th lag of the first difference of the dividend-price ratio
data$ddp <- c(NA, diff(data$gw_d.p))
data$ddp_l7 <- c(rep(NA, 7), head(data$ddp, -7))
data$ddp_l5 <- c(rep(NA, 5), head(data$ddp, -5))
data$ddp_l2 <- c(rep(NA, 2), head(data$ddp, -2))

#We wish to predict the 1, 3, 6-step ahead forecast
forecast_horizons <- c(1, 3, 6)

windows <- c(480, 300) #40-year and 25-year rolling windows, same as the ML models
testrows <- which(data$date >= as.Date("2001-06-01"))

for (W in windows){
  for (h in forecast_horizons){
    prob = rep(NA, length(testrows))
    y <- paste0("y_h", h)
    ddp_var <- paste0("ddp_l", 8-h)
    
    #Rolling window of W months forms the training set
    for(k in 1:length(testrows)){
      i <- testrows[k]
      last_train <- i-h
      traindata <- data[(last_train-W+1):last_train,]
      model <- glm(as.formula(paste(y, "~ state + ret_m + tms + ", ddp_var)), data = traindata, family = binomial(link="logit"))
      prob[k] <- predict(model, newdata = data[i,], type = "response")
    }
    
    #save the predictions into results folder with one file per window and horizon
    res <- data.frame(date = data$date[testrows], actual = data[[y]][testrows], prob = prob)
    write.csv(res, sprintf("results/logit_h%d_w%d.csv", h, W), row.names = FALSE)
  }
}