# Import packages
library(dplyr)
library(tm) 
library(SnowballC) 
library(wordcloud)
library(tidytext)
library(tidyverse)
library(glue)
library(stringr)
library(reshape2)



##### Import DATA #####

# get a list of the files in the input directory
files <- list.files("C:/Users/mikke/Desktop/CDATA", pattern="*.csv")

# Create a function for the sentiment
FunctionSentiment <- function(file){
  
# stick together the path to the file & 1st file name
Twitter_data <- glue("C:/Users/mikke/Desktop/CDATA/", file, sep = "")

# get rid of any sneaky trailing spaces
Twitter_data <- trimws(Twitter_data)

Data_tweet <- read.csv(Twitter_data) # load data

# Seperate tweets and retweets
Data_tweet$text2 <- ifelse(Data_tweet$is_retweet==FALSE,
                           paste(Data_tweet$text),
                           paste(Data_tweet$retweet_text))
  
###### text preparation ########

# extracting tweets
TextCorpus <- Corpus(VectorSource(Data_tweet$text2))

#transform to lower case
TextCorpus <- tm_map(TextCorpus, content_transformer(tolower)) 

# Removing stopwords
mystopwords <- c(stopwords('english'),"the","giveaway","follow")# remove custom stopwords
TextCorpus <- tm_map(TextCorpus, removeWords, stopwords()) #remove stopwords

#remove_url
remove_url <- function(x) gsub("http[^[:space:]]*","",x) 
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_url))

#count number of mentions = @
nr_mentions <- str_count(TextCorpus, "@[^[:space:]]*")

#removeing @
remove_at <- function(x) gsub("@[^[:space:]]*","",x) 
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_at))

#remove dollar signs 
remove_dollar <- function(x) gsub("$*","",x) # repetition_hashtag
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_dollar))

#remove ampersand 
remove_and <- function(x) gsub("&*","",x) # repetition_hashtag
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_and))

#count nr. of hashtags 
nr_hashtags <- str_count(TextCorpus, "#[^[:space:]]*")

#remove hashtags 
remove_hashtag <- function(x) gsub("#*","",x) # repetition_hashtag
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_hashtag))

#remove hashtags 
remove_ht <- function(x) gsub("#[^[:space:]]*","",x) #remove_hashtags 
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_ht))

# Removing quotation marks or replace with apexes  to no confuse the code with "
remove_quotation <- function(x) gsub('"*',"",x) # repetition_hashtag
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_quotation)) 

# Removing special characters which stand alone ()[]{} // etc.
remove_brackets <- function(x) gsub("\\[|\\]", "", x) #removebrackets_ []
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets))

remove_brackets2 <- function(x) gsub("\\{|\\}", "", x) #removebrackets_ {}
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets2))

remove_brackets3 <- function(x) gsub("\\(|\\)", "", x) #removebrackets_ ()
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets3))

# from COOOOOOOOOOL to coool (Repeated characters)
replace_3vowels <-  function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1\\1", x) #replace 3+ chr. with 3chr only 
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_3vowels)) # we keep 3 for keeping the emphasis

# count the emphasized words 
NR_emphasized <- str_count(TextCorpus, "([[:alpha:]])\\1{2,}")

# Replace slag or add to lexicon
replace_great <- function(x) gsub ("gr8", "great", x) #great
replace_great2 <- function(x) gsub ("gr8t", "great", x)
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_great))
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_great2))

replace_lol <- function(x) gsub("lol", "laughing out loud", x) #lol
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_lol))

replace_bff <- function(x) gsub("bff", "best friend forever", x) #bff
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_bff))

replace_idk <- function(x) gsub("idk ", "I don't know", x) #IDK
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_idk))

replace_ftw <- function(x) gsub("ftw ", "for the win", x) #ftw
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_ftw))

replace_afaic <- function(x) gsub("afaic ", "as far as I am concerned", x) #AFAIC
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_afaic))

replace_afaik <- function(x) gsub("afaik ", "as far as I know", x) #AFAIK 
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_afaik))

#Negation
replace_negtions <-function(x) gsub("(ca|do|wo|is)n't" , "NOT ", x) #replace can't, don't and won't
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_negtions))

replace_never <-function(x) gsub("never" , "NOT ", x) #replace "never" with NOT 
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_never))

replace_cannot <-function(x) gsub("cannot" , "NOT ", x) #replace cannot
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_cannot))

# Remove "giveaway"
replace_give <-function(x) gsub("giveaway" , "", x) #replace empty
TextCorpus <- tm_map(TextCorpus, content_transformer(replace_give))

#Count Negations 
nr_neg_tag <-  str_count(TextCorpus, "NOT")

# remove numbers
remove_numbers <- function(x) gsub("[0-9]*","",x) # repetition_numbers
TextCorpus <- tm_map(TextCorpus, content_transformer(remove_numbers))

# Remove puncutaions
TextCorpus <- tm_map(TextCorpus, removePunctuation) # punctuation

# Remove whitespaces
TextCorpus <- tm_map(TextCorpus, stripWhitespace) # whitespace

# Steam all the words
TextCorpus <- tm_map(TextCorpus, stemDocument) #stemming



##### Tokenization  ####

# tranform to dataframe
tweet3 <- data.frame(text = sapply(TextCorpus, as.character), stringsAsFactors = FALSE)

#include ID for each tweets
tweet3 <- tibble::rowid_to_column(tweet3, "ID") # add ID number

#create tokens.
tokens <- tweet3 %>% unnest_tokens(word, text)


###################### adjust lexicons #############


### add more words to bing lexicon
additional_sentiment <- tibble(word=c("moon","mooning","bull","bullish","lambo","hodl", "hodling","bear","bearish", "shitcoin","dump"),
                               sentiment=c("positive","positive","positive","positive","positive","positive","positive","negative","negative","negative","negative"))

bing <- get_sentiments("bing")%>%
  rbind(additional_sentiment)

################# Calculate sentiments  ################

# table of positive and negative words including sum of sentiment bing
sentiment <- bind_rows(tokens %>% 
            inner_join(bing) %>%
            mutate(method = "Bing et al."),
          tokens %>% 
            inner_join(get_sentiments("nrc") %>% 
                         filter(sentiment %in% c("positive", 
                                                 "negative"))) %>%
            mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(sentiment_total = positive + negative) %>%
  mutate(Pos_Procent = sum(positive) / sum(sentiment_total)) %>%
  mutate(Pos_negative = sum(negative) / sum(sentiment_total)) %>%
  mutate(Name = file)

# table of positive and negative words including sum of sentiment afin
afinn_sen <- tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(method = "afinn") %>% # pull out only sentiment words
  mutate(positive = sum(score>0),
         negative = sum(score<0)) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(sentiment_total = positive + negative) %>%
  mutate(Pos_Procent = sum(positive) / sum(sentiment_total)) %>%
  mutate(Pos_negative = sum(negative) / sum(sentiment_total)) %>%
  select(method, negative, positive, sentiment, sentiment_total, Pos_Procent, Pos_negative) %>%
  head(1) %>%
  mutate(Name = file)

# bind the sentiment of different lexicons together
sentiment <- rbind(sentiment, afinn_sen)

    # return our sentiment dataframe
    return(sentiment)
}
    
FunctionSentiment(files[3])

### apply over all the files ##

# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments <- rbind(sentiments, FunctionSentiment(i))
}

# save as csv.file
write.csv(sentiments, file = "sentiments_output.csv")




### get the number of tweets each day
Get.nr.twitter <- function(file){

# stick together the filenames again
fileName <- glue("C:/Users/mikke/Desktop/CDATA/", file, sep = "")
  
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)
  
Data_tweet <- read.csv(fileName) # load data
  
# Seperate tweets and retweets
Data_tweet$text2 <- ifelse(Data_tweet$is_retweet==FALSE,
                             paste(Data_tweet$text),
                             paste(Data_tweet$retweet_text))

nr_twitter <- Data_tweet %>%
  count("text2") %>%
  mutate(Total_tweets = n) %>%
  select(Total_tweets )%>%
  mutate(Name = file)
  
  return(nr_twitter)
}

Get.nr.twitter(files[3])


# counting the number of tweets pr. day for all CSV files (days)
nr_twitters <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  nr_twitters <- rbind(nr_twitters, Get.nr.twitter(i))
}


# combining sentiments with number of tweets pr. day
sentiments <- sentiments %>% arrange(method )%>%
  left_join(nr_twitters, by = "Name")

View(sentiments)

sentiments1 <- sentiments[1:39,]
sentiments1 <- setNames(sentiments1, c("Method", "Negative-Afinn", "Positive-Afinn", "Sentiment-Afinn", "Sentiment.total-Afinn","Pos.percentage-afinn", "Neg.percentage-afinn",  "Day", "Total.tweets"))
sentiments1 <- data.frame(sentiments1)


sentiments2 <- sentiments[40:78,]
sentiments2 <- setNames(sentiments2, c("Method", "Negative-Bing", "Positive-Bing", "Sentiment-Bing", "Sentiment.total-Bing", "Pos.percentage-Bing", "Neg.percentage-Bing", "Day", "Total.tweets"))
sentiments2 <- data.frame(sentiments2)

sentiments3 <- sentiments[79:117,]
colnames(sentiments3) <- c("Method", "Negative-NRC", "Positive-NRC", "Sentiment-NRC", "Sentiment_total-NRC", "Pos.percentage-NRC", "Neg.percentage-NRC", "Day", "Total.tweets")
sentiments3 <- data.frame(sentiments3)

sentiments_final <- sentiments1 %>%
  left_join(sentiments2, by =c("Day","Total.tweets")) %>%
  left_join(sentiments3, by =c("Day","Total.tweets"))

View(sentiments_final)
#save as CSV

write.csv(sentiments_final, file = "sentiments.csv")


#### Combined tokens


##### Import DATA #####

# get a list of the files in the input directory
files <- list.files("C:/Users/mikke/Desktop/CDATA", pattern="*.csv")

# Create a function for the sentiment.
Gettoken <- function(file){
  # stick together the path to the file & 1st file name
  fileName <- glue("C:/Users/mikke/Desktop/CDATA/", file, sep = "")
  
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  Data_tweet <- read.csv(fileName) # load data
  
  # Seperate tweets and retweets
  Data_tweet$text2 <- ifelse(Data_tweet$is_retweet==FALSE,
                             paste(Data_tweet$text),
                             paste(Data_tweet$retweet_text))
  
  ###### text preparation ########
  
  # extracting tweets
  TextCorpus <- Corpus(VectorSource(Data_tweet$text2))
  
  #transform to lower case
  TextCorpus <- tm_map(TextCorpus, content_transformer(tolower)) 
  
  # Removing stopwords
  mystopwords <- c(stopwords('english'),"the","giveaway","follow")# remove custom stopwords
  TextCorpus <- tm_map(TextCorpus, removeWords, stopwords()) #remove stopwords
  
  #remove_url
  remove_url <- function(x) gsub("http[^[:space:]]*","",x) 
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_url))
  
  #count number of mentions = @
  nr_mentions <- str_count(TextCorpus, "@[^[:space:]]*")
  
  #removeing @
  remove_at <- function(x) gsub("@[^[:space:]]*","",x) 
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_at))
  
  #remove dollar signs 
  remove_dollar <- function(x) gsub("$*","",x) # repetition_hashtag
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_dollar))
  
  #remove ampersand 
  remove_and <- function(x) gsub("&*","",x) # repetition_hashtag
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_and))
  
  #count nr. of hashtags 
  nr_hashtags <- str_count(TextCorpus, "#[^[:space:]]*")
  
  #remove hashtags 
  remove_hashtag <- function(x) gsub("#*","",x) # repetition_hashtag
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_hashtag))
  
  #remove hashtags 
  remove_ht <- function(x) gsub("#[^[:space:]]*","",x) #remove_hashtags 
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_ht))
  
  # Removing quotation marks or replace with apexes  to no confuse the code with "
  remove_quotation <- function(x) gsub('"*',"",x) # repetition_hashtag
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_quotation)) 
  
  # Removing special characters which stand alone ()[]{} // etc.
  remove_brackets <- function(x) gsub("\\[|\\]", "", x) #removebrackets_ []
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets))
  
  remove_brackets2 <- function(x) gsub("\\{|\\}", "", x) #removebrackets_ {}
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets2))
  
  remove_brackets3 <- function(x) gsub("\\(|\\)", "", x) #removebrackets_ ()
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_brackets3))
  
  # from COOOOOOOOOOL to coool (Repeated characters)
  replace_3vowels <-  function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1\\1", x) #replace 3+ chr. with 3chr only 
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_3vowels)) # we keep 3 for keeping the emphasis
  
  # count the emphasized words 
  NR_emphasized <- str_count(TextCorpus, "([[:alpha:]])\\1{2,}")
  
  # Replace slag or add to lexicon
  replace_great <- function(x) gsub ("gr8", "great", x) #great
  replace_great2 <- function(x) gsub ("gr8t", "great", x)
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_great))
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_great2))
  
  replace_lol <- function(x) gsub("lol", "laughing out loud", x) #lol
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_lol))
  
  replace_bff <- function(x) gsub("bff", "best friend forever", x) #bff
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_bff))
  
  replace_idk <- function(x) gsub("idk ", "I don't know", x) #IDK
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_idk))
  
  replace_ftw <- function(x) gsub("ftw ", "for the win", x) #ftw
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_ftw))
  
  replace_afaic <- function(x) gsub("afaic ", "as far as I am concerned", x) #AFAIC
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_afaic))
  
  replace_afaik <- function(x) gsub("afaik ", "as far as I know", x) #AFAIK 
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_afaik))
  
  #Negation
  replace_negtions <-function(x) gsub("(ca|do|wo|is)n't" , "NOT ", x) #replace can't, don't and won't
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_negtions))
  
  replace_never <-function(x) gsub("never" , "NOT ", x) #replace "never" with NOT 
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_never))
  
  replace_cannot <-function(x) gsub("cannot" , "NOT ", x) #replace cannot
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_cannot))
  
  # Remove "giveaway"
  replace_give <-function(x) gsub("giveaway" , "", x) #replace empty
  TextCorpus <- tm_map(TextCorpus, content_transformer(replace_give))
  
  #Count Negations 
  nr_neg_tag <-  str_count(TextCorpus, "NOT")
  
  # remove numbers
  remove_numbers <- function(x) gsub("[0-9]*","",x) # repetition_numbers
  TextCorpus <- tm_map(TextCorpus, content_transformer(remove_numbers))
  
  # Remove puncutaions
  TextCorpus <- tm_map(TextCorpus, removePunctuation) # punctuation
  
  # Remove whitespaces
  TextCorpus <- tm_map(TextCorpus, stripWhitespace) # whitespace
  
  # Steam all the words
  TextCorpus <- tm_map(TextCorpus, stemDocument) #stemming
  
  ##### Tokenization  ####
  
  tweet3 <- data.frame(text = sapply(TextCorpus, as.character), stringsAsFactors = FALSE)
  
  tweet3 <- tibble::rowid_to_column(tweet3, "ID") # add ID number
  
  tokens <- tweet3 %>% unnest_tokens(word, text)
  
  return(tokens)
}

dianna <- Gettoken(files[2])
View(dianna)

tokens <- data_frame()

# get the tokens for each file in our datset
for(i in files){
  tokens <- rbind(tokens, Gettoken(i))
}




######## number of postive and negative words in each lexicon #####

## NRC number of positve and negative words
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

## bing number of positve and negative words
get_sentiments("bing") %>% 
  count(sentiment)

### add more words to bing
additional_sentiment <- tibble(word=c("moon","mooning","bull","bullish","lambo","hodl", "hodling","bear","bearish", "shitcoin","dump"),
                               sentiment=c("positive","positive","positive","positive","positive","positive","positive","negative","negative","negative","negative"))

bing <- get_sentiments("bing")%>%
  rbind(additional_sentiment)

bing %>% 
  count(sentiment)


## afinn number of positve and negative words
get_sentiments("afinn") %>%
  mutate(positive = sum(score>0),
         negative = sum(score<0)) %>%
  select(negative, positive) %>%
  head(1)





### explorative data analysis ###

#### Most common positive and negative words in bing
bing_word_counts <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#### Most common positive and negative words in nrc
bing_word_counts <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

write.csv(bing_word_counts, file = "bing_word_counts.csv")

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

## two graphs

nrc_p <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

nrc_n <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative") 

tweet5 <- tokens %>%
  inner_join(nrc_p) %>%
  count(word, sort = TRUE) %>%
  mutate(sentiment = "positive") %>%
  select(word, sentiment, n)

tweet6 <- tokens %>%
  inner_join(nrc_n) %>%
  count(word, sort = TRUE) %>%
  mutate(sentiment = "negative") %>%
  select(word, sentiment, n)

nrc_word_counts <- rbind(tweet5, tweet6)

write.csv(nrc_word_counts, file = "nrc_word_counts.csv")

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment: NRC",
       x = NULL) +
  coord_flip()

#### Most common positive and negative words in Afinn
affin_p <- get_sentiments("afinn") %>%
  filter(score > 0)  

affin_n <- get_sentiments("afinn") %>%
  filter(score < 0)  

tweet9 <- tokens %>%
  inner_join(affin_p) %>%
  count(word, sort = TRUE) %>%
  mutate(sentiment = "positive") %>%
  select(word, sentiment, n)

tweet10 <- tokens %>%
  inner_join(affin_n) %>%
  count(word, sort = TRUE) %>%
  mutate(sentiment = "negative") %>%
  select(word, sentiment, n)

afinn_word_counts <- rbind(tweet9, tweet10)

write.csv(afinn_word_counts, file = "afinn_word_counts.csv")

afinn_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment: Afinn",
       x = NULL) +
  coord_flip()



#### Wordclouds

Tooooookenz<- tokens %>% 
  filter(!word %in% c("xrp", "rippl"))

tokens %>% head(10)

Tooooookenz %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

