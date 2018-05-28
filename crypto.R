################################################

# CoinMarketCap data using the CoinMarketCap API

###############################################

#CoinmarketCap package. can be used to look at total marketcap, ticker and plot of to 5 coins.
install.packages("coinmarketcapr")
library(coinmarketcapr)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  
  
library(ggplot2)
  packages <- ( c("magrittr", "jsonlite", "dplyr", "lubridate", "curl", "utils", "parallel",
"stats", "doSNOW", "tidyr"))

ipak(packages)

  library( c(magrittr, jsonlite, dplyr, lubridate, curl, utils,parallel,
           stats, doSNOW, tidyr))

ticker <- get_marketcap_ticker_all()
View(ticker)

plot_top_5_currencies()

install.packages( c("formatR", "yaml", "googleVis", "knitr"))

library(formatR)
library(yaml)
library(googleVis)
library(knitr)
op <- options(gvis.plot.tag="chart")



top_cc <- get_marketcap_ticker_all()
kable(head(top_cc))

for(i in c(4:ncol(top_cc))) {
  top_cc[,i] <- as.double(top_cc[,i])
}

View(top_cc)

mark <- gvisColumnChart(top_cc[1:15, ], "name", "market_cap_usd", options = list(title = "Market Cap Of Leading Cryptocurrencies", 
                                                                                 legend = "left"))
plot(mark)


var <- gvisColumnChart(top_cc[1:15, ], "name", c("percent_change_1h", "percent_change_24h", 
                                                 "percent_change_7d"), options = list(title = "% change of prices", legend = "top"))
plot(var)


##########################################
# crypto 

##################################

install.packages("crypto")
library(crypto)

XRP <- getCoins(coin = "XRP", start_date = 20180431, end_date = 20180531)

write.csv2(XRP, file = "XRP data5.csv")
