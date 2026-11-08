#forecast combination of the machine learning models (25-year window)
#average: mean of the bear probabilities of the models
#vote: bear if at least half of the models give a probability of 0.5 or more (a tie counts as a bear)
#boosting enters with its depth 2 forecast, so it has one vote like the other methods

window <- 300
members <- c("rlasso", "rf", "bagging", "boost_d2")

for (h in c(1, 3, 6)){
  preds <- lapply(members, function(m) read.csv(sprintf("results/%s_h%d_w%d.csv", m, h, window)))

  for (p in preds) stopifnot(all(p$date == preds[[1]]$date), all(p$actual == preds[[1]]$actual))

  prob <- sapply(preds, function(p) p$prob)
  avg <- rowMeans(prob)
  vote <- as.numeric(rowSums(prob >= 0.5) >= length(members)/2)

  res <- data.frame(date = preds[[1]]$date, actual = preds[[1]]$actual)
  write.csv(cbind(res, prob = avg), sprintf("results/ensemble_avg_h%d_w%d.csv", h, window), row.names = FALSE)
  write.csv(cbind(res, prob = vote), sprintf("results/ensemble_vote_h%d_w%d.csv", h, window), row.names = FALSE)
}
