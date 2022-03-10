#Install and load packages to retrieve network data from Twitter
if (!require("pacman")) install.packages("pacman", quiet=TRUE);require("pacman")

p_load(rtweet, httpuv)

#Create a token to set up your authentication                                                                       
app <- 'Bert Jonckheere'
consumer_key <- 'VXrmNWpS8BsgY1tQw6sNtTjem'
consumer_secret <- 'TqjcNWfmWl7ZWsAnBKYFoEyMMhAod0wiVw0JCicuetNBVNteEk'
access_token <- '1495465003171917827-eAE3jD8gXTvxLZPIR5FtlLQPXx36EN'
access_secret <- 'lkVkBm2mDSjn4tuRB5BmsddNa0nhCh7sNs7O0EA9KAhZz'

## authenticate via web browser
token <- create_token(
  app = app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret, set_renv = FALSE)
#You can put set_renv = TRUE then the created token will be saved in the default environment
#However, this can create an overload of .rds files