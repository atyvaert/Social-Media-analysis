########################################
## DESCPRITIVE ## 
rm(list=ls()) #Clean the environment
setwd("/Users/konstantinlazarov/Desktop/SMWA/Group_Assignement")
# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv, stringr, qdap, httr, wordcloud2, tm, tidytext, wordcloud)

#install.packages("twitteR")
library(twitteR)


source('tokensandkeys.R')
token <- get_token()

search.string <- "#Starbucks"

tweets <- search_tweets(search.string, n = 10000, 
                        include_rts = FALSE,
                        retryonratelimit = FALSE,
                        lang = 'en',
                        token = get_token())


#https://developer.twitter.com/en/portal/projects/1495455141645926414/apps
tweets2 <- searchTwitter(search.string, n = 18000, 
                        since = "2022-03-01") 
list_starbucks = twListToDF(tweets2)
plot(list_starbucks$created)
min(list_starbucks$created)
max(list_starbucks$created)

setwd("/Users/konstantinlazarov/Documents/GitHub/SMWA_Performance/data")
save(tweets, file = "Scrape1_21_03.RData")


#https://www.rdocumentation.org/packages/rtweet/versions/0.7.0/topics/search_tweets 

setwd("C:\\Users\\bertj\\OneDrive\\Documenten\\GitHub\\SMWA_Performance\\data")
save(tweets, file = "Scrape_18_03_RData")

typeof(tweets)
tweets_table <- as.data.frame(tweets)

p_load(ggplot2)
plot(tweets$created_at) 
hist(tweets$created_at)
tweets$retweet_location
plot(tweets_table$created_at)





