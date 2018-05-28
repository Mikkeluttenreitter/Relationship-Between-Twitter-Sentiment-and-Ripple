install.packages("TTR")
install.packages("dplyr")
install.packages("corrplot")
install.packages("corrr")
install.packages("cowplot")
install.packages("roll")
install.packages("tidyquant")
install.packages("psych")
install.packages("fpp")
install.packages("forecast")
install.packages("gglot")


library(TTR)
library(ggplot2)
library(dplyr)
library(corrplot)
library(corrr)
library(cowplot)
library(roll)
library(tidyquant)
library(psych)
library(fpp)
library(forecast)
library(ggplot)


sentiment <- read.csv("sentiments-4.csv", header = TRUE) 
XRP_BTC <- read.table("XRP_BTC.txt", header = T)
XRP_USD <- read.table("XRP_USD.txt", header = T)
View(sentiment)
View(XRP_BTC)
View(XRP_USD)


#XRP_BTC$price_satoshi <- as.integer(XRP_BTC$price_satoshi) 

Data_tweet <- cbind(sentiment, XRP_BTC, XRP_USD)
View(Data_tweet)

# pick a subset of variables that should be included in the analysis 
Correlation_data <- subset(Data_tweet, select = c(3:8,10,12:17, 19:24, 27, 33, 36, 37, 39, 40))
pairs (Correlation_data)
str(Correlation_data)

View(Correlation_data)
str(Correlation_data)
Price_data_plot <- mutate_all(Correlation_data, function(x) as.numeric(as.character(x)))
str(Price_data_plot)
View(Price_data_plot)


##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

# Here we'll only make a brief visual inspection of vars
pairs.panels(Correlation_data, col="red")

# for sta data 
pairs.panels(final_com2, col="red")


################## Correlation on the specific day ################

# Correlation 
cor_pearson <- cor(Correlation_data)
cor_kendall <- cor(Correlation_data, method = "kendall")
cor_spearman <- cor(Correlation_data, method = "spearman")

cor.test(Correlation_data$price_USD , Correlation_data$Sentiment.Afinn)


#plot correlation
par(mfrow = c(1,1))
corrplot(cor_pearson, order = "AOE", method = "color")
corrplot(cor_pearson, order = "AOE", method = "color", addCoef.col ="gray")
commmm <- corrplot(cor_pearson, method = "number")

View(commmm)
corrplot(cor_kendall, method = "number")
corrplot(cor_spearman, method = "ellipse", type = "lower" )


# Correlation table
Correlation_table <- 
  Correlation_data %>%
  correlate() 

# Pretty printing
View(Correlation_table %>%
       shave(upper = T))

#correlation test for specific variables
cor.test(Correlation_data$price_satoshi, Correlation_data$Sentiment.NRC)
cor.test(Correlation_data$price_USD , Correlation_data$Sentiment.NRC)
cor.test(Correlation_data$volume , Correlation_data$Sentiment.NRC) # <-- not signiticant
cor.test(Correlation_data$volume , Correlation_data$Total.tweets) # not significant



######## NOT WORKING AT THE MOMENET  ##############
#network plot (visual plot of the Correlation table)

# Network plot  
network_plot(Correlation_table)
# the hard way
Cor_network_plot <- Correlation_table %>%
  network_plot(colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "Correlations",
    subtitle = "test test test"
  ) +
  expand_limits(x = c(-0.8, 0.8), y = c(-0.8, 0.9)) +
  theme_tq() +
  theme(legend.position = "bottom")

Cor_network_plot


# hypothesis-test
#cor.test (Correlation_data$Negative.NRC, Correlation_data$Negative.Afinn)
#

#ADF-test
#adf.te



############# Cross-correltion analysis ################ 


###test#######
#Correlation_data_dif <- apply(Correlation_data, 2, diff)
#View(Correlation_data_dif)
#####Test slut ######

# we test for non-stationarity for one series
par(mfrow = c(1,1))
acf(Correlation_data$price_satoshi, main = "XRP PRICE")
diff_2 <- diff(Correlation_data$price_satoshi)
acf(diff_2, main = "Second diff. XRP PRICE")
plot(diff_2)
pacf(diff_2)
View(diff_2)

Correlation_data$Time <- seq.int(nrow(Correlation_data))

ggplot(Correlation_data, aes(Time, price_satoshi)) +
  geom_line() +
  ylab("XRP Price")
ggplot

ggplot

?geom_line
#### ADF ####

# testing for stationarity of the different time series (all at the same time)
Correlation_data_ADF <- lapply(Correlation_data, adf.test)
Correlation_data_ADF

### extracting the non-stationary time series
NOTstationary_data <- Correlation_data %>%
  select(-Positive.Bing, -Sentiment.Bing, -close_ratio)

stationary_data <- Correlation_data %>%
  select(Positive.Bing, Sentiment.Bing, close_ratio)

### 1st diff ####
NOTstationary_data.diff <- apply(NOTstationary_data, 2, diff)

NOTstationary_data.diff <- data.frame(NOTstationary_data.diff)

Correlation_data_ADF2 <- lapply(NOTstationary_data.diff, adf.test)
Correlation_data_ADF2
# 3 varibles are still not stationary -> price_satoshi, price_USD, close

#extracting the non-stationary time series second time to take second difference
NOTstationary_data2 <- Correlation_data %>%
 select(price_satoshi, price_USD, close)

##2st diff ####
diff_23 <- diff(NOTstationary_data2$price_satoshi, differences = 2)
diff_24 <- diff(NOTstationary_data2$price_USD, differences = 2)
diff_25 <- diff(NOTstationary_data2$close, differences = 2)

NOTstationary_data.diff2 <- cbind(diff_23, diff_24, diff_25)
NOTstationary_data.diff2 <- setNames(NOTstationary_data.diff2, c("price_satoshi","price_USD", "close"))
NOTstationary_data.diff2 <- data.frame(NOTstationary_data.diff2)

View(NOTstationary_data.diff2)
Correlation_data_ADF3 <- lapply(NOTstationary_data.diff2, adf.test)
Correlation_data_ADF3
## now all data is stationary and they are in 3 different documents
Stationary_final1 <- stationary_data
Stationary_final2 <- NOTstationary_data.diff
Stationary_final3 <- NOTstationary_data.diff2

write.csv(Stationary_final1, file = "final1.csv")
write.csv (Stationary_final2, file = "final2.csv")
write.csv(Stationary_final3, file = "final3.csv")

# load above data in one data frame made in Excel.
final_com <- read.csv2("final_com2.csv", header = T)
View(final_com)

#mutate data from factor to numeric
final_com <- mutate_all(final_com, function(x) as.numeric(as.character(x)))
str(final_com)

#change data from a data frame to matrix

final_com  <- as.matrix(final_com)
View(final_com)
cor(final_com)
# correlation plot 
corrplot(cor(final_com))
#correlation plot for the report
corrplot.mixed(cor(final_com), upper = "number", lower = "ellipse",
               tl.pos = "lt" ,number.cex = .7,  tl.col = "black")


### THe analysis of cross-correlation ###
# transform back to data frame, to use it for cross-correlation
final_com2 <- as.data.frame(final_com)
# ACF
#cross-correlation for total Sentiments = Positive + Negative

{par(mfrow = c(2,3))
ccf(final_com2$Sentiment.total.Afinn, final_com2$price_satoshi, main = "Afinn XRP/BTC") 
ccf(final_com2$Sentiment.total.Bing, final_com2$price_satoshi, main = "Bing XRP/BTC")
ccf(final_com2$Sentiment_total.NRC, final_com2$price_satoshi, main= "NRC XRP/BTC")

ccf(final_com2$Sentiment.total.Afinn, final_com2$price_USD , main = "Afinn XRP/USD")
ccf(final_com2$Sentiment.total.Bing, final_com2$price_USD, main = "Bing XRP/USD")
ccf(final_com2$Sentiment_total.NRC, final_com2$price_USD, main= "NRC XRP/USD")
#title("ACF for the total sentiment", side = 3, line = -, outer = TRUE)
} 

###############slet slet slet slet "#######################
{par(mfrow = c(2,3))
  ccf(final_com2$price_satoshi, final_com2$Sentiment.total.Afinn, main = "Afinn XRP/BTC") 
  ccf(final_com2$price_satoshi, final_com2$Sentiment.total.Bing, main = "Bing XRP/BTC")
  ccf(final_com2$price_satoshi, final_com2$Sentiment_total.NRC, main= "NRC XRP/BTC")
  
  ccf(final_com2$price_USD, final_com2$Sentiment.total.Afinn, main = "Afinn XRP/USD")
  ccf(final_com2$price_USD, final_com2$Sentiment.total.Bing, main = "Bing XRP/USD")
  ccf(final_com2$price_USD, final_com2$Sentiment_total.NRC, main= "NRC XRP/USD")
  #title("ACF for the total sentiment", side = 3, line = -, outer = TRUE)
} 

###############slet slet slet slet "#######################


#cross-correlation for balanced Sentiment = Positive - Negative
{par(mfrow = c(2,3))
ccf(final_com2$Sentiment.Afinn, final_com2$price_satoshi, main = "Afinn XRP/BTC")
ccf(final_com2$Sentiment.Bing, final_com2$price_satoshi, main = "Bing XRP/BTC")
ccf(final_com2$Sentiment.NRC, final_com2$price_satoshi, main= "NRC XRP/BTC")

ccf(final_com2$Sentiment.Afinn, final_com2$price_USD , main = "Afinn XRP/USD")
ccf(final_com2$Sentiment.Bing, final_com2$price_USD, main = "Bing XRP/USD")
ccf(final_com2$Sentiment.NRC, final_com2$price_USD, main= "NRC XRP/USD")
#title("ACF for the overall sentiment", side = 3, line = -28, outer = TRUE)
} 
#cross-correlation for Negative:
{par(mfrow = c(2,3))
ccf (final_com2$Negative.Afinn, final_com2$price_satoshi, main = "Afinn XRP/BTC")
ccf (final_com2$Negative.Bing, final_com2$price_satoshi, main = "Bing XRP/BTC")
ccf (final_com2$Negative.NRC, final_com2$price_satoshi, main= "NRC XRP/BTC")

ccf (final_com2$Negative.Afinn, final_com2$price_USD , main = "Afinn XRP/USD")
ccf (final_com2$Negative.Bing, final_com2$price_USD, main = "Bing XRP/USD")
ccf (final_com2$Negative.NRC, final_com2$price_USD, main= "NRC XRP/USD")
#title("ACF plot for Negative", side = 3, line = -28, outer = TRUE)
} 
#cross-correlation for Positive:
{par(mfrow = c(2,3))
ccf (final_com2$Positive.Afinn, final_com2$price_satoshi, main = "Afinn XRP/BTC")
ccf (final_com2$Positive.Bing, final_com2$price_satoshi, main = "Bing XRP/BTC")
ccf (final_com2$Positive.NRC, final_com2$price_satoshi, main= "NRC XRP/BTC")

ccf (final_com2$Positive.Afinn, final_com2$price_USD , main = "Afinn XRP/USD")
ccf (final_com2$Positive.Bing, final_com2$price_USD, main = "Bing XRP/USD")
ccf (final_com2$Positive.NRC, final_com2$price_USD, main= "NRC XRP/USD")
#title("ACF plot for Positive", side = 3, line = -28, outer = TRUE)
#par(mfrow = c(1,1))
} 
#cross-correlation for total number of tweets:
{par(mfrow = c(1,2))
ccf (final_com2$Total.tweets, final_com2$price_satoshi, main = "Afinn XRP/BTC")
ccf (final_com2$Total.tweets, final_com2$price_USD , main = "Afinn XRP/USD")
#title("ACF plot for Positive", side = 3, line = -2, outer = TRUE)
par(mfrow = c(1,1))
} 


#install.packages("forecast")
#install.packages("lmtest")


# test for unit root and number of differences required, you can also test for seasonality with nsdiffs
library(forecast)
ndiffs(final_com2$price_satoshi, alpha=0.05, test=c("kpss"))
# [1] 1
ndiffs(final_com2$Sentiment.NRC, alpha=0.05, test=c("kpss"))
# [1] 1

# differenced time series
Price_diff <- diff(Correlation_data$price_satoshi)
Sentiment_diff <- diff(Correlation_data$Sentiment.NRC)
plot.ts(dchick)
plot.ts(degg)


# do eggs granger cause chickens?
library(lmtest)
x <- 1:10

# (afinn)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
test <- grangertest(final_com2$price_satoshi ~ final_com2$Sentiment.total.Afinn, order=laag)
p.vaerdi <- test[2,4]
return(p.vaerdi)
}
p..value.afinn <- data.frame()

for(i in x){
  p..value.afinn <- rbind(p..value.afinn,loop.test(i))
}
p..value.afinn

# (bing)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
  test <- grangertest(final_com2$price_satoshi ~ final_com2$Sentiment.total.Bing, order=laag)
  p.vaerdi <- test[2,4]
  return(p.vaerdi)
}
p..value.bing <- data.frame()

for(i in x){
  p..value.bing <- rbind(p..value.bing,loop.test(i))
}

p..value.bing
# (NRC)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
  test <- grangertest(final_com2$price_satoshi ~ final_com2$Sentiment_total.NRC, order=laag)
  p.vaerdi <- test[2,4]
  return(p.vaerdi)
}
p..value.nrc <- data.frame()

for(i in x){
  p..value.nrc <- rbind(p..value.nrc,loop.test(i))
}

p..value.nrc

#combine all the p-values for the different lexicons
p..values.price.to.sentiment <- cbind(p..value.afinn, p..value.bing, p..value.nrc )

p..values.price.to.sentiment<- setNames(p..values.price.to.sentiment, c("Afinn", "Bing", "NRC"))
View(p..values.price.to.sentiment)















# (afinn)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
  test <- grangertest(final_com2$Sentiment.total.Afinn ~ final_com2$price_satoshi, order=laag)
  p.vaerdi <- test[2,4]
  return(p.vaerdi)
}
p..value.afinn.1 <- data.frame()

for(i in x){
  p..value.afinn.1 <- rbind(p..value.afinn.1,loop.test(i))
}
p..value.afinn.1

# (bing)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
  test <- grangertest(final_com2$Sentiment.total.Bing ~ final_com2$price_satoshi, order=laag)
  p.vaerdi <- test[2,4]
  return(p.vaerdi)
}
p..value.bing.1 <- data.frame()

for(i in x){
  p..value.bing.1 <- rbind(p..value.bing.1,loop.test(i))
}
p..value.bing.1

# (nrc)Granger test and p-values from lag 1 to 10
loop.test <- function (laag) {
  test <- grangertest(final_com2$Sentiment_total.NRC ~ final_com2$price_satoshi, order=laag)
  p.vaerdi <- test[2,4]
  return(p.vaerdi)
}
p..value.nrc.1 <- data.frame()

for(i in x){
  p..value.nrc.1 <- rbind(p..value.nrc.1,loop.test(i))
}
p..value.nrc.1

#combine all the p-values for the different lexicons
p..values.sentiment.to.price <- cbind(p..value.afinn.1, p..value.bing.1, p..value.nrc.1 )

p..values.sentiment.to.price <- setNames(p..values.sentiment.to.price, c("Afinn", "Bing", "NRC"))


View(p..values.sentiment.to.price)
View(p..values.price.to.sentiment)














functi
grangertest(final_com2$price_USD ~ final_com2$Sentiment.total.Bing, order=3)
grangertest(final_com2$price_USD ~ final_com2$Sentiment_total.NRC, order=3)
# we are using 4 lags, (there is a method to find this)

# significant with 5 % alpha level

#significant p-value, but what about the other direction?

# do chickens granger cause eggs, at lag 7?
grangertest(final_com2$Sentiment.NRC ~ final_com2$price_satoshi, order=6)

# It is even more significant, so we can say the chickens Granger-Cause eggs!



#### Reference
https://www.r-bloggers.com/chicken-or-the-egg-granger-causality-for-the-masses/
  
  https://www.r-bloggers.com/granger-causality-test/
  
  https://en.wikipedia.org/wiki/Granger_causality


# plotting time series
ts.plot(Data_tweet$Total.tweets)
ts.plot(Data_tweet$Sentiment.total.Afinn)
ts.plot(Data_tweet$price_satoshi)




    