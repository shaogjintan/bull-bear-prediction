data <- read.csv("data/panel.csv")

forecast_horizons <- c(1, 3, 6)

testrows <- which(data$date >= as.Date("2001-06-01"))

for (h in forecast_horizons){
  y <- paste0("y_h", h)

  #Forecast of the state at i+h is the state at i
  res <- data.frame(date = data$date[testrows], actual = data[[y]][testrows], prob = data$state[testrows])

  #save results into results folder with one file per horizon
  write.csv(res, paste0("results/persistence_h", h, ".csv"), row.names = FALSE)
}
