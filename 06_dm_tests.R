library(sandwich)

#DM tests like in lecture 4, loss is the squared error of the bear probability
#d = loss of baseline - loss of model, positive t means the model is better

common_start <- as.Date("2010-01-01")
baselines <- c("logit", "persistence")

files <- list.files("results", pattern = "_h[136]\\.csv$", full.names = TRUE)

preds <- data.frame()
for (f in files){
  name <- sub("\\.csv$", "", basename(f))
  res <- read.csv(f)
  res$date <- as.Date(res$date)
  preds <- rbind(preds, data.frame(model = sub("_h[136]$", "", name),
                                   h = as.numeric(sub(".*_h([136])$", "\\1", name)),
                                   date = res$date, actual = res$actual, prob = res$prob))
}
preds <- preds[preds$date >= common_start, ]
preds$loss <- (preds$prob - preds$actual)^2

out <- data.frame()
for (h in c(1, 3, 6)){
  for (b in baselines){
    for (m in setdiff(unique(preds$model), b)){
      base <- preds[preds$model == b & preds$h == h, c("date", "actual", "loss")]
      mod <- preds[preds$model == m & preds$h == h, c("date", "actual", "loss")]
      both <- merge(base, mod, by = "date", suffixes = c("_base", "_model"))
      stopifnot(all(both$actual_base == both$actual_model))

      d <- both$loss_base - both$loss_model
      lag <- round(nrow(both)^(1/3)) #n^(1/3) rule from the lecture
      if (var(d) == 0){
        tstat <- NA
      } else {
        dm <- lm(d ~ 1)
        tstat <- as.numeric(dm$coefficients/sqrt(NeweyWest(dm, lag = lag)))
      }
      out <- rbind(out, data.frame(model = m, baseline = b, h = h, n = nrow(both), lag = lag,
                                   mean_d = mean(d), t = tstat, p = 2*pnorm(-abs(tstat))))
    }
  }
}

print(out |> transform(mean_d = round(mean_d, 4), t = round(t, 2), p = round(p, 3)), row.names = FALSE)

write.csv(out, "results/dm_tests.csv", row.names = FALSE)
