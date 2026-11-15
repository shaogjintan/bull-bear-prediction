# 01_pull_sp500.R: downloads daily S&P 500 prices (^GSPC, 1950 onwards) from Yahoo Finance via quantmod
# Output: data/sp500_daily.csv

# install packages
#options(install.packages.compile.from.source = "never")
#install.packages("quantmod"))
library(quantmod)

# get s&p daily. use to.monthly() to convert to monthly after.
sp500_daily <- getSymbols("^GSPC", src = "yahoo",
                          from = "1950-01-01", auto.assign = FALSE)
# checking
head(sp500_daily)
tail(sp500_daily)

# save to csv
write.csv(data.frame(date = index(sp500_daily), coredata(sp500_daily)),
          "data/sp500_daily.csv", row.names = FALSE)