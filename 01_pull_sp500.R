# data from yahoo finance are loaded from quantmod as we do not have the subscription
# to download from yahoo finance directly

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