
library(rtweet)
library(plyr)
library(stringr)
library(tidyverse)

## Aooname 
appname <- "BI"

## api key from twitter
key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXx"

## api secret from twtitter
secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXX"

## create token to reuse
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# hashtags and strings to search for
hashtags <- c("#Ripple", "#ripple", "#xrp", "XRP", "xrp", "#XRP")
hashtags1 <- c("#Ripple", "#xrp")

# combine into one
hashtags <- paste(hashtags, collapse = " OR ")
hashtags1 <- paste(hashtags1, collapse = " OR ")


#NEW DATE
DAYAPRIL01_full <- search_tweets(hashtags, n = 18000, lang ="en", since='2018-04-01', until = '2018-04-02', retryonratelimit = TRUE)
View(DAYAPRIL01_full)
save_as_csv(DAYAPRIL01_full, "DAYAPRIL01_full.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


#NEW DATE
DAYMAY09_full <- search_tweets(hashtags, n = 18000, lang ="en", since='2018-05-09', until = '2018-05-10', retryonratelimit = TRUE)
save_as_csv(DAYMAY09_full, "DAYMAY09_full.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

