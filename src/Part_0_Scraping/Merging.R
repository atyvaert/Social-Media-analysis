rm(list=ls())
setwd("C:\\Users\\bertj\\OneDrive\\Documenten\\GitHub\\SMWA_Performance\\data")

# load in all files containing tweets
load("First_Scrape9_03.Rdata")
tweets1 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_10_03.Rdata")
tweets3 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_13_03.Rdata")
tweets4 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_14_03.Rdata")
tweets5 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_15_03.Rdata")
tweets6 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_16_03.Rdata")
tweets7 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape2_16_03.Rdata")
tweets8 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape3_16_03.Rdata")
tweets9 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape4_16_03.Rdata")
tweets10 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape5_16_03.Rdata")
tweets11 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape5_03.Rdata")
tweets12 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape3_22_03.Rdata")
tweets13 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape3_19_03.Rdata")
tweets14 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape3_18_03.Rdata")
tweets15 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape2_22_03.Rdata")
tweets16 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape2_19_03.Rdata")
tweets17 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape2_18_03.Rdata")
tweets18 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_22_03.Rdata")
tweets19 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape1_19_03.Rdata")
tweets20 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape_17_03_tweets2.Rdata")
tweets21 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape_17_03.Rdata")
tweets22 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape_18_03.Rdata")
tweets23 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape2_22_03.Rdata")
tweets24 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])
load("Scrape_24_03.Rdata")
tweets25 <- as.data.frame(tweets[, c("user_id", "created_at", "text")])


# combine all tweets
all_tweets <- rbind(tweets1, tweets3, tweets4, tweets5, tweets6, tweets7, tweets8, tweets9, tweets10, tweets11, tweets12, tweets13, tweets14,
                    tweets15, tweets16, tweets17, tweets18, tweets19, tweets20, tweets21, tweets22, tweets23, tweets24, tweets25)

# set column names
colnames(all_tweets) <- c("user_id", "date", "text")

# remove duplicate tweets
sum(duplicated(all_tweets))
all_tweets <- all_tweets[!duplicated(all_tweets), ]

# change date for all tweet in order to merge with target variable and financial data into one basetable
all_tweets$date <- ifelse(as.integer(format(all_tweets$date, format = "%H%M")) <= 1330,
                          ifelse(as.integer(format(all_tweets$date, format = "%M")) < 30,
                                 all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (30 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + (14 - as.integer(format(all_tweets$date, format = "%H"))) * 60 * 60,
                                 all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (90 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + (14 - (as.integer(format(all_tweets$date, format = "%H")) + 1)) * 60 * 60),
                          ifelse(as.integer(format(all_tweets$date, format = "%H%M")) <= 1930,
                                 ifelse(as.integer(format(all_tweets$date, format = "%M")) < 30,
                                        all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (30 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + 60 * 60,
                                        all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (90 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + 60 * 60),
                                 ifelse(as.integer(format(all_tweets$date, format = "%M")) < 30,
                                        all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (30 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + (38 - as.integer(format(all_tweets$date, format = "%H"))) * 60 * 60,
                                        all_tweets$date + 60 - as.integer(format(all_tweets$date, format = "%S")) + (90 - (as.integer(format(all_tweets$date, format = "%M")) + 1)) * 60 + (38 - (as.integer(format(all_tweets$date, format = "%H")) + 1)) * 60 * 60)))

all_tweets$date <- all_tweets$date - 60 * 60

all_tweets$date <- as.POSIXct(all_tweets$date, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00")

all_tweets$weekday <- weekdays(all_tweets$date)

all_tweets$date <- ifelse(all_tweets$weekday == "zaterdag", all_tweets$date + 48 * 60 *60,
                          ifelse(all_tweets$weekday == "zondag", all_tweets$date + 24 * 60 * 60, all_tweets$date))

all_tweets$date <- as.POSIXct(all_tweets$date, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00")

all_tweets <- subset(all_tweets, select = -c(weekday))

# store tweets
save(all_tweets, file = "all_tweets.Rdata")
