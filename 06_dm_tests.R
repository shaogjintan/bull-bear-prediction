library(sandwich)

#DM tests like in lecture 4, loss is the squared error of the bear probability
#d = loss of baseline - loss of model, positive t means the model is better
#all models are tested on the same 295 months (Jun 2001 - Dec 2025), 25-year rolling window

window <- 300 #same window as 05_evaluate.R
baselines <- c("logit", "persistence")

#model files for this window (e.g. rf_h1_w300.csv, boost_d2_h1_w300.csv) plus persistence (no window)
files <- c(list.files("results", pattern = paste0("_h[136]_w", window, "\\.csv$"), full.names = TRUE),
           list.files("results", pattern = "^persistence_h[136]\\.csv$", full.names = TRUE))

preds <- data.frame()
for (f in files){
  name <- sub("\\.csv$", "", basename(f))
  res <- read.csv(f)
  res$date <- as.Date(res$date)
  preds <- rbind(preds, data.frame(model = sub("_h[136](_w[0-9]+)?$", "", name),
                                   h = as.numeric(sub(".*_h([136])(_w[0-9]+)?$", "\\1", name)),
                                   date = res$date, actual = res$actual, prob = res$prob))
}
preds$loss <- (preds$prob - preds$actual)^2

model_order <- c("persistence", "logit", "rlasso", "rf", "bagging", "boost_d2", "boost_d5",
                 "ensemble_avg", "ensemble_vote")
models <- intersect(model_order, unique(preds$model))

out <- data.frame()
for (h in c(1, 3, 6)){
  for (b in baselines){
    #vs logit: every other model except persistence (logit vs persistence is already in the persistence rows)
    exclude <- if (b == "logit") c("logit", "persistence") else b
    for (m in setdiff(models, exclude)){
      base <- preds[preds$model == b & preds$h == h, c("date", "actual", "loss")]
      mod <- preds[preds$model == m & preds$h == h, c("date", "actual", "loss")]
      both <- merge(base, mod, by = "date", suffixes = c("_base", "_model"))
      stopifnot(nrow(both) == nrow(base), all(both$actual_base == both$actual_model)) #same months, same outcomes
      
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

write.csv(out, sprintf("results/dm_tests_w%d.csv", window), row.names = FALSE)

######################
## DM PLOTS (QPS)   ##
######################
dir.create("figures", showWarnings = FALSE)
dq <- if ("loss" %in% names(out)) out[out$loss == "qps", ] else out
lossvar <- if ("qps" %in% names(preds)) "qps" else "loss"
model_label <- c(persistence = "Persistence", logit = "Logit", rlasso = "rlassologit",
                 rf = "Random forest", bagging = "Bagging", boost_d2 = "Boosting d=2",
                 boost_d5 = "Boosting d=5", ensemble_avg = "Average of models",
                 ensemble_vote = "Majority vote")
cols <- c(rlasso = "#1b9e77", rf = "#d95f02", bagging = "#7570b3",
          boost_d2 = "#e7298a", boost_d5 = "#66a61e")

## Plot 1: DM t-statistics, one panel per horizon ##
png("figures/dm_tstats.png", width = 2400, height = 900, res = 170)
par(mfrow = c(1, 3), mar = c(4.5, 11, 3, 1))
for (hh in c(1, 3, 6)) {
  dh <- dq[dq$h == hh, ]
  barplot(dh$t, names.arg = paste(model_label[dh$model], "vs", model_label[dh$baseline]),
          horiz = TRUE, las = 1, cex.names = 0.65, xlab = "DM t-statistic (positive = model better)",
          col = ifelse(dh$t > 0, "#1b9e77", "#d95f02"), main = paste0("h = ", hh),
          xlim = range(c(dh$t, -2.5, 2.5), na.rm = TRUE))
  abline(v = c(-1.96, 1.96), lty = 2)
}
dev.off()

## Plot 2: cumulative loss difference vs the logit, one panel per horizon ##
ml_models <- intersect(names(cols), unique(preds$model))
png("figures/dm_cumulative_vs_logit.png", width = 2400, height = 900, res = 170)
par(mfrow = c(1, 3), mar = c(3, 4.5, 3, 1))
for (hh in c(1, 3, 6)) {
  base <- preds[preds$model == "logit" & preds$h == hh, ]; base <- base[order(base$date), ]
  cum <- sapply(ml_models, function(m) {
    mod <- preds[preds$model == m & preds$h == hh, ]; mod <- mod[order(mod$date), ]
    cumsum(base[[lossvar]] - mod[[lossvar]])
  })
  plot(base$date, cum[, 1], type = "n", ylim = range(cum, 0), xlab = "",
       ylab = "Cumulative loss difference (logit - model)", main = paste0("vs logit, h = ", hh))
  r <- rle(base$actual); e <- cumsum(r$lengths); s <- e - r$lengths + 1 # shade bear months
  for (k in which(r$values == 1))
    rect(as.numeric(base$date[s[k]]), par("usr")[3], as.numeric(base$date[e[k]]) + 30, par("usr")[4],
         col = "grey88", border = NA)
  abline(h = 0, lty = 3)
  for (j in seq_along(ml_models)) lines(base$date, cum[, j], col = cols[ml_models[j]], lwd = 2)
}
legend("topleft", legend = model_label[ml_models], col = cols[ml_models], lwd = 2, bty = "n", cex = 0.7)
dev.off()

