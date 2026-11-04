library(dplyr)
library(pROC)

#metrics for the predictions saved in results/
#each file has date, actual and prob (bear = 1)

common_start <- as.Date("2010-01-01") #boosting is tested from here
window <- 480

panel <- read.csv("data/panel.csv")
panel$date <- as.Date(panel$date)

files <- list.files("results", pattern = "_h[136]\\.csv$", full.names = TRUE)

metrics <- function(actual, prob, threshold){
  forecast <- as.numeric(prob >= threshold)
  confusion.matrix <- table(factor(actual, levels = c(0, 1)), factor(forecast, levels = c(0, 1)))
  FP <- confusion.matrix["0", "1"]
  TP <- confusion.matrix["1", "1"]
  FN <- confusion.matrix["1", "0"]
  TN <- confusion.matrix["0", "0"]
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  data.frame(accuracy = (TP + TN)/length(actual),
             precision = precision,
             recall = recall,
             F1 = 2*precision*recall/(precision + recall),
             TP = TP, FP = FP, FN = FN, TN = TN)
}

out <- data.frame()
for (f in files){
  name <- sub("\\.csv$", "", basename(f))
  model <- sub("_h[136]$", "", name)
  h <- as.numeric(sub(".*_h([136])$", "\\1", name))
  res <- read.csv(f)
  res$date <- as.Date(res$date)

  #share of bears in the training window, used as a second cutoff
  y <- paste0("y_h", h)
  i <- match(res$date, panel$date)
  ybar <- sapply(i, function(k) mean(panel[[y]][max(1, k-h-window+1):(k-h)]))

  for (sample in c("common", "full")){
    keep <- if (sample == "common") res$date >= common_start else rep(TRUE, nrow(res))
    r <- res[keep, ]
    roc_h <- roc(response = r$actual, predictor = r$prob, levels = c(0, 1), direction = "<", quiet = TRUE)
    auc_h <- as.numeric(auc(roc_h))
    qps <- 2*mean((r$prob - r$actual)^2)

    for (threshold in c("0.5", "ybar")){
      cutoff <- if (threshold == "0.5") 0.5 else ybar[keep]
      out <- rbind(out, data.frame(model = model, h = h, sample = sample, threshold = threshold,
                                   n = nrow(r), bears = sum(r$actual), AUC = auc_h, QPS = qps,
                                   metrics(r$actual, r$prob, cutoff)))
    }
  }
}

out <- arrange(out, sample, h, model, threshold)

#models should have the same number of test months
check <- out |> filter(sample == "common", threshold == "0.5") |> group_by(h) |> summarise(n_months = n_distinct(n), .groups = "drop")
if (any(check$n_months > 1)) warning("models have a different number of test months")

print(out |> filter(sample == "common") |> mutate(across(c(AUC, QPS, accuracy, precision, recall, F1), ~ round(.x, 3))), row.names = FALSE)

write.csv(out, "results/summary.csv", row.names = FALSE)
