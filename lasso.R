library(dplyr)
library(glmnet)
library(pROC)

# Lasso logit with the same regressors as the tree models (state and its lags included).
# The penalty is chosen by BIC inside every rolling window, using the training rows only
# (as in the lecture code func-lasso.R), so no validation window is needed.

data = read.csv("data/panel.csv")
data$date = as.Date(data$date)
dir.create("results", showWarnings = FALSE)

rollingwindow_size = 300 # 25-year rolling window
horizons = c(1, 3, 6) # 1,3,6 step-ahead forecasts

# Test window: Jun 2001 - Dec 2025, the same 295 months as every other model
nprev_test = 295

## predictors (X) ##
# lag-0 columns kept: market variables and the current state, known at month end
keep_lag0 = c("ret_m", "dtoy", "dtoat", "svar", "tms", "dfy", "state")
lag0_cols = names(data)[!grepl("_l[0-9]+$", names(data))] # columns without an _l1.._l6 suffix
dropped_columns = unique(c("date", "sp500", "y_h1", "y_h3", "y_h6",
                           setdiff(lag0_cols, keep_lag0))) # lag-0 macro/Goyal: not released yet at t
X_all = as.matrix(select(data, -all_of(dropped_columns)))

# count glmnet convergence warnings
n_warn = 0
countwarn = function(w){
  n_warn <<- n_warn + 1
  invokeRestart("muffleWarning")
}

# the window has W + h rows: train on the first W (their targets are known at the origin),
# then predict the last row (the origin)
runlasso = function(y, X, lag){
  X_train = X[1:(nrow(X)-lag),] # drop the last h rows such that they are not used to train the model (to prevent leakage)
  y_train = y[1:(nrow(X)-lag)]
  keep = apply(X_train, 2, sd) > 0 # columns that are not constant in this window
  X_train = X_train[, keep, drop = FALSE]
  X_test = X[nrow(X), keep, drop = FALSE] # forecast row, same columns as training
  model = withCallingHandlers(
    glmnet(X_train, factor(y_train, levels = c(0, 1)), family = "binomial", alpha = 1),
    warning = countwarn)
  # BIC along the lambda path: deviance (= -2 x log-likelihood) + log(n) x number of nonzero coefficients
  bic = deviance(model) + log(nrow(X_train)) * model$df
  best = which.min(bic)
  pred = as.numeric(predict(model, newx = X_test, type = "response", s = model$lambda[best]))
  return(list("model" = model, "pred" = pred, "best" = best))
}

lasso.rolling.window = function(data, nprev, lag){
  save.pred = numeric(nprev)
  save.lambda = numeric(nprev)
  save.selected = vector("list", nprev)
  X = X_all # regressors built at the top
  y = data[[paste0("y_h", lag)]] # target (y_h1, y_h3, y_h6)
  end_row = nrow(data)
  for(i in nprev:1){
    rows = (end_row-i+1-(rollingwindow_size+lag-1)):(end_row-i+1) # estimation window
    fit = runlasso(y[rows], X[rows,], lag)
    save.pred[(1+nprev-i)] = fit$pred
    save.lambda[(1+nprev-i)] = fit$model$lambda[fit$best]
    b = as.matrix(coef(fit$model, s = fit$model$lambda[fit$best]))
    b = b[rownames(b) != "(Intercept)", 1]
    save.selected[[1+nprev-i]] = names(b)[b != 0]
    cat("iteration", (1+nprev-i), "\n")
  }
  return(list("pred" = save.pred, "lambda" = save.lambda, "selected" = save.selected))
}

## test set (Jun 2001 - Dec 2025), one run per horizon ##
for(h in horizons){
  n_warn = 0
  out = lasso.rolling.window(data, nprev_test, h)
  actual = tail(data[[paste0("y_h", h)]], nprev_test)
  dates = tail(data$date, nprev_test)
  res = data.frame(date = dates, actual = actual, prob = out$pred)
  write.csv(res, sprintf("results/lasso_h%d_w%d.csv", h, rollingwindow_size), row.names = FALSE)
  # variables selected in each window, one row per selected variable
  sel = data.frame(date = rep(dates, lengths(out$selected)), variable = unlist(out$selected))
  write.csv(sel, sprintf("results/lasso_selected_w%d_h%d.csv", rollingwindow_size, h), row.names = FALSE)
  # penalty chosen in each window and number of variables selected
  write.csv(data.frame(date = dates, lambda = out$lambda, n_selected = lengths(out$selected)),
            sprintf("results/lasso_lambda_w%d_h%d.csv", rollingwindow_size, h), row.names = FALSE)
  cat("h =", h, ": glmnet warnings =", n_warn, "  mean selected =", round(mean(lengths(out$selected)), 1), "\n")
}

## selection frequency: % of windows where any lag of a variable is selected ##
W = rollingwindow_size
sel_all = do.call(rbind, lapply(horizons, function(h) {
  s = read.csv(sprintf("results/lasso_selected_w%d_h%d.csv", W, h)); s$h = h; s }))
sel_all$base = sub("_l[0-9]+$", "", sel_all$variable) # group the lags of each variable
freq = sel_all |> distinct(h, date, base) |> count(h, base) |> mutate(pct = round(100 * n / nprev_test, 1))
tab = tidyr::pivot_wider(freq, id_cols = base, names_from = h, values_from = pct, values_fill = 0, names_prefix = "h")
tab = tab[order(-rowSums(tab[, c("h1", "h3", "h6")])), c("base", "h1", "h3", "h6")]
dir.create("tables", showWarnings = FALSE)
write.csv(tab, sprintf("tables/lasso_selection_frequency_w%d.csv", W), row.names = FALSE)
print(as.data.frame(head(tab, 15)))
print(sel_all |> count(h) |> mutate(avg_selected = n / nprev_test)) # average number of variables per window

# checks ##
for (f in list.files("results", pattern = "^lasso_h.*\\.csv$", full.names = TRUE)) {
  dd = read.csv(f)
  pred = as.integer(dd$prob > 0.5)
  cat(sprintf("%-25s rows=%d  %s to %s  NA=%d  prob %.3f-%.3f  AUC=%.3f  bears caught=%d/%d  false alarms=%d\n",
              basename(f), nrow(dd), dd$date[1], tail(dd$date, 1), sum(is.na(dd$prob)),
              min(dd$prob), max(dd$prob),
              as.numeric(auc(roc(dd$actual, dd$prob, levels = c(0, 1), direction = "<", quiet = TRUE))),
              sum(pred == 1 & dd$actual == 1), sum(dd$actual == 1),
              sum(pred == 1 & dd$actual == 0)))
}
