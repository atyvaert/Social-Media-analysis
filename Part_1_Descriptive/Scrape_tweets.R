########################################
## DESCPRITIVE ## 
rm(list=ls()) #Clean the environment
setwd("/Users/konstantinlazarov/Desktop/SMWA/Group_Assignement")
# Install and load packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, rtweet, httpuv, stringr, qdap, httr, wordcloud2, tm, tidytext, wordcloud)


source('tokensandkeys.R')

token <- get_token()

search.string <- "#facebook"

tweets <- search_tweets(search.string, n = 1000, 
                        include_rts = FALSE,
                        retryonratelimit = FALSE,
                        lang = 'en')

tweets_data(tweets)
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

wordcloud_tesla <- wordcloud2(d)




