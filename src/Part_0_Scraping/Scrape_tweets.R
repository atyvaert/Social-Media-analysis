########################################
## DESCPRITIVE ## 
rm(list=ls()) #Clean the environment
setwd("/Users/konstantinlazarov/Desktop/SMWA/Group_Assignement")
# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv, stringr, qdap, httr, wordcloud2, tm, tidytext, wordcloud)


source('tokensandkeys.R')

token <- get_token()

search.string <- "#Netflix"

tweets <- search_tweets(search.string, n = 10000, 
                        include_rts = FALSE,
                        retryonratelimit = FALSE,
                        lang = 'en',
                        token = get_token())
tweets_max <- search_tweets(search.string, n = 20000, 
                        include_rts = FALSE,
                        retryonratelimit = FALSE,
                        lang = 'en',
                        token = get_token())
setwd("/Users/konstantinlazarov/Documents/GitHub/SMWA_Performance/src")
save(tweets, file = "First_Scrape9.03.RData")

typeof(tweets)
tweets_table <- as.data.frame(tweets)

p_load(ggplot2)
ggplot(tweets$created_at) 
hist(tweets$created_at)
tweets$retweet_location
plot(tweets_table$created_at)


text <- tweets_data(tweets) %>% pull(text)
myCorpus <- Corpus(VectorSource(text))

myCorpus <- Corpus(VectorSource(text)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


tdm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- tibble(word = names(v),freq=v)

d <- d %>% filter(word != tolower(search.string)) %>% arrange(desc(freq))
#d <- d %>% filter(word != "tesla") %>% arrange(desc(freq))
#d <- d %>% filter(word != "elonmusk") %>% arrange(desc(freq))
#d <- d %>% filter(word != "musk") %>% arrange(desc(freq))
#d <- d %>% filter(word != "elon") %>% arrange(desc(freq))
#d <- d %>% filter(word != "you") %>% arrange(desc(freq))

options(warn=-1) #turn warnings off
wordcloud(d$word,d$freq)
options(warn=0) #turn warnings back on

wordcloud_netflix <- wordcloud2(d)




