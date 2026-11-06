library(dplyr)
library(pROC)

#metrics for the predictions saved in results/
#each file has date, actual and prob (bear = 1)
#all models are tested on the same 295 months (Jun 2001 - Dec 2025)

window <- 300 #main results use the 25-year rolling window (set to 480 for the robustness table, no boosting there)

panel <- read.csv("data/panel.csv")
panel$date <- as.Date(panel$date)

#model files for this window (e.g. rf_h1_w300.csv, boost_d2_h1_w300.csv) plus persistence (no window)
files <- c(list.files("results", pattern = paste0("_h[136]_w", window, "\\.csv$"), full.names = TRUE),
           list.files("results", pattern = "^persistence_h[136]\\.csv$", full.names = TRUE))

metrics <- function(actual, prob, threshold){
  forecast <- as.numeric(prob >= threshold)
  confusion.matrix <- table(factor(actual, levels = c(0, 1)), factor(forecast, levels = c(0, 1)))
  FP <- confusion.matrix["0", "1"]
  TP <- confusion.matrix["1", "1"]
  FN <- confusion.matrix["1", "0"]
  TN <- confusion.matrix["0", "0"]
  precision <- if (TP + FP > 0) TP/(TP + FP) else NA #NA if the model never predicts a bear
  recall <- TP/(TP + FN)
  data.frame(accuracy = (TP + TN)/length(actual),
             precision = precision,
             recall = recall,
             F1 = if (!is.na(precision) && precision + recall > 0) 2*precision*recall/(precision + recall) else NA,
             TP = TP, FP = FP, FN = FN, TN = TN)
}

out <- data.frame()
for (f in files){
  name <- sub("\\.csv$", "", basename(f))
  model <- sub("_h[136](_w[0-9]+)?$", "", name)              #e.g. "boost_d2", "rf", "persistence"
  h <- as.numeric(sub(".*_h([136])(_w[0-9]+)?$", "\\1", name))
  res <- read.csv(f)
  res$date <- as.Date(res$date)
  
  #share of bears in the training window, used as a second cutoff (Nyberg's ybar)
  y <- paste0("y_h", h)
  i <- match(res$date, panel$date)
  ybar <- sapply(i, function(k) mean(panel[[y]][max(1, k-h-window+1):(k-h)]))
  
  roc_h <- roc(response = res$actual, predictor = res$prob, levels = c(0, 1), direction = "<", quiet = TRUE)
  auc_h <- as.numeric(auc(roc_h))
  qps <- 2*mean((res$prob - res$actual)^2) #Nyberg's quadratic probability score (lower is better)
  
  for (threshold in c("0.5", "ybar")){
    cutoff <- if (threshold == "0.5") 0.5 else ybar
    out <- rbind(out, data.frame(model = model, h = h, threshold = threshold,
                                 start = min(res$date), end = max(res$date),
                                 n = nrow(res), bears = sum(res$actual), AUC = auc_h, QPS = qps,
                                 metrics(res$actual, res$prob, cutoff)))
  }
}

#order: benchmarks first, then the ML models
model_order <- c("persistence", "logit", "rlasso", "rf", "bagging", "boost_d2", "boost_d5")
out <- out |>
  mutate(model = factor(model, levels = union(model_order, unique(model)))) |>
  arrange(h, threshold, model)

#all models should cover the same test months
check <- out |> group_by(h) |> summarise(n_months = n_distinct(n), n_start = n_distinct(start), .groups = "drop")
if (any(check$n_months > 1 | check$n_start > 1)) warning("models have different test months")
print(table(out$model[out$threshold == "0.5"], out$h[out$threshold == "0.5"])) #each model should appear once per horizon

print(out |> select(-start, -end) |> mutate(across(c(AUC, QPS, accuracy, precision, recall, F1), ~ round(.x, 3))),
      row.names = FALSE)

write.csv(out, sprintf("results/summary_w%d.csv", window), row.names = FALSE)

#############################################
## TABLES AND FIGURES (from the summary above) ##
##############################################
# Output: tables/ (csv) and figures/ (png)
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

summ <- out |> mutate(model = as.character(model)) # the summary table built above

model_label <- c(persistence = "Persistence", logit = "Logit (Nyberg)", rlasso = "rlassologit",
                 rf = "Random forest", bagging = "Bagging", boost_d2 = "Boosting (d=2)",
                 boost_d5 = "Boosting (d=5)")
cols <- c(persistence = "grey50", logit = "black", rlasso = "#1b9e77", rf = "#d95f02",
          bagging = "#7570b3", boost_d2 = "#e7298a", boost_d5 = "#66a61e")
models   <- intersect(model_order, unique(summ$model)) # models that actually have results
horizons <- c(1, 3, 6)

# read one prediction file
read_pred <- function(m, h) {
  f <- if (m == "persistence") sprintf("results/persistence_h%d.csv", h)
  else sprintf("results/%s_h%d_w%d.csv", m, h, window)
  d <- read.csv(f); d$date <- as.Date(d$date); d
}

# wide table: one row per model, columns metric_h1, metric_h3, metric_h6
wide <- function(df, vars) {
  out <- data.frame(model = model_label[models], row.names = NULL)
  for (hh in horizons) for (v in vars) {
    d <- df[df$h == hh, ]
    out[[paste0(v, "_h", hh)]] <- round(d[[v]][match(models, d$model)], 3)
  }
  out
}

####################
## TABLES         ##
####################

## Table 1: probability accuracy (no threshold needed): AUC (higher = better), QPS (lower = better) ##
tab_prob <- wide(summ[summ$threshold == "0.5", ], c("AUC", "QPS"))
print(tab_prob, row.names = FALSE)
write.csv(tab_prob, "tables/results_auc_qps.csv", row.names = FALSE)

## Tables 2-3: classification metrics at each threshold ##
for (thr in c("0.5", "ybar")) {
  tab <- wide(summ[summ$threshold == thr, ], c("accuracy", "precision", "recall", "F1"))
  cat("\nThreshold:", thr, "\n"); print(tab, row.names = FALSE)
  write.csv(tab, sprintf("tables/results_classification_%s.csv", ifelse(thr == "0.5", "050", "ybar")),
            row.names = FALSE)
}

####################
## FIGURES        ##
####################

## Figure 1: AUC and QPS by horizon ##
png("figures/auc_qps_by_horizon.png", width = 2000, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
d <- summ[summ$threshold == "0.5", ]
for (v in c("AUC", "QPS")) {
  plot(NA, xlim = c(1, 6), ylim = range(d[[v]], if (v == "AUC") 0.5),
       xaxt = "n", xlab = "Forecast horizon (months)", ylab = v,
       main = if (v == "AUC") "AUC (higher is better)" else "QPS (lower is better)")
  axis(1, at = horizons)
  if (v == "AUC") abline(h = 0.5, lty = 3) # 0.5 = no better than random ranking
  for (m in models) {
    dm <- d[d$model == m, ]; dm <- dm[order(dm$h), ]
    lines(dm$h, dm[[v]], type = "b", pch = 19, lwd = 2, col = cols[m])
  }
}
legend("topleft", legend = model_label[models], col = cols[models], lwd = 2, pch = 19,
       bty = "n", cex = 0.75)
dev.off()

## Figure 2: bear recall by model and horizon, at both thresholds ##
png("figures/bear_recall.png", width = 2000, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
for (thr in c("0.5", "ybar")) {
  dt <- summ[summ$threshold == thr, ]
  mat <- sapply(horizons, function(hh) { dh <- dt[dt$h == hh, ]; dh$recall[match(models, dh$model)] })
  colnames(mat) <- paste0("h = ", horizons)
  barplot(mat, beside = TRUE, col = cols[models], ylim = c(0, 1), ylab = "Bear recall",
          main = paste("Threshold:", ifelse(thr == "0.5", "0.5", "share of bears in training window")))
}
legend("topright", legend = model_label[models], fill = cols[models], bty = "n", cex = 0.7)
dev.off()

## Figure 3: ROC curves, one panel per horizon ##
png("figures/roc_curves.png", width = 2400, height = 850, res = 170)
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
for (hh in horizons) {
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "False positive rate",
       ylab = "True positive rate (bear recall)", main = paste0("ROC, h = ", hh))
  abline(0, 1, lty = 3)
  for (m in models) {
    p <- read_pred(m, hh)
    r <- roc(response = p$actual, predictor = p$prob, levels = c(0, 1), direction = "<", quiet = TRUE)
    lines(1 - r$specificities, r$sensitivities, col = cols[m], lwd = 2)
  }
}
legend("bottomright", legend = model_label[models], col = cols[models], lwd = 2, bty = "n", cex = 0.7)
dev.off()

## Figures 4a-c: predicted bear probability over the test period, one figure per horizon ##
# x-axis = forecast origin t; shaded = months where the target (state at t+h) is bear
shade <- function(dates, a) {
  r <- rle(a); e <- cumsum(r$lengths); s <- e - r$lengths + 1
  for (k in which(r$values == 1))
    rect(as.numeric(dates[s[k]]), 0, as.numeric(dates[e[k]]) + 30, 1, col = "grey85", border = NA)
}
plot_models <- setdiff(models, "persistence") # persistence is only 0/1, not informative here
for (hh in horizons) {
  png(sprintf("figures/bear_prob_h%d.png", hh), width = 2000,
      height = 450 * ceiling(length(plot_models) / 2), res = 170)
  par(mfrow = c(ceiling(length(plot_models) / 2), 2), mar = c(3, 4, 2.5, 1))
  for (m in plot_models) {
    p <- read_pred(m, hh)
    plot(p$date, p$prob, type = "n", ylim = c(0, 1), xlab = "", ylab = "P(bear)",
         main = paste0(model_label[m], ", h = ", hh))
    shade(p$date, p$actual)
    abline(h = 0.5, lty = 2, col = "grey40")
    lines(p$date, p$prob, col = cols[m], lwd = 1.5)
  }
  dev.off()
}

cat("Tables saved to tables/, figures saved to figures/\n")

