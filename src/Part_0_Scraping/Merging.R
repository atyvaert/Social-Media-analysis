
setwd("C:\\Users\\bertj\\OneDrive\\Documenten\\GitHub\\SMWA_Performance\\data")

load("First_Scrape9_03.Rdata")
tweets1 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape_09_03.Rdata")
tweets2 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_10_03.Rdata")
tweets3 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_13_03.Rdata")
tweets4 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_14_03.Rdata")
tweets5 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_15_03.Rdata")
tweets6 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])

all_tweets <- rbind(tweets1, tweets2, tweets3, tweets4, tweets5, tweets6)
colnames(all_tweets) <- c("user_id", "date", "text")

class(all_tweets)

save(all_tweets, file = "all_tweets.Rdata")
