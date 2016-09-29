# =================================================
# MODULE 1 - DOWNLOAD 10,000 TWEETS DAILY
# =================================================

# -------------------------------------------------
# Install library
# -------------------------------------------------

# Clean-up everything
# rm(list = setdiff(ls(), lsf.str()))

library(twitteR)

# -------------------------------------------------
# Twitter Authentication
# -------------------------------------------------

options(httr_oauth_cache = TRUE)
api_key <- "gvAwYUghROx9Ji4fNbEUoSbd1"
api_secret <- "Pgwy7ZykLu87qxH5qntXSiNWSYubN41qwJFlHmMNphN15ikPEM"
access_token <- "560011434-uHOBikJKCmjp3eQtqJdhDE447nKbNENykAJfuzoO"
access_token_secret <- "rkmwVtSWl9tb9Pz4SFoDlRUgcfQz8jxaHRKMdY3sUjrqo"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# -------------------------------------------------
# Extracting tweets from Twitter and save to CSVs
# -------------------------------------------------

today <- c("20160217")
path <- c("D:/twitter_us-election-2016/tweets/")
tweet.num <- 10000

DownloadTweet <- function(text, name, num) {
  tweet <- searchTwitter(text, n = num, lang = "en", resultType = "recent")
  tweet.df <- twListToDF(tweet)
  #corpus <- VCorpus(VectorSource(tweet.df$text))
  
  tweet.path <- paste0(path, name, "_", today, ".csv")
  write.csv(tweet.df, tweet.path)
  return(tweet.df)
}

tweet.df.trump <- DownloadTweet("Donald Trump", "trump", tweet.num)
tweet.df.clinton <- DownloadTweet("Hillary Clinton", "clinton", tweet.num)
tweet.df.sanders <- DownloadTweet("Bernie Sanders", "sanders", tweet.num)
tweet.df.cruz <- DownloadTweet("Ted Cruz", "cruz", tweet.num)
tweet.df.rubio <- DownloadTweet("Marco Rubio", "rubio", tweet.num)