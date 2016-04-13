## load library
library(twitteR)

## setting up OAuth ----
# api_key <-  ""
# api_secret <- ""
# access_token <- ""
# access_secret <- ""
# setup_twitter_oauth(api_key, api_secret, access_token = access_token, access_secret = access_secret)

## helper functions ----
## get tweets from Twitter
getTweets <- function(term, start_date, end_date, num_tweets) {
  data <- searchTwitter(term, since = start_date, until = end_date, lang = "en", n = num_tweets)
  return(data)
}

## separate tweets by date
dailyTweets <- function(data) {
  if (length(data) <= 0) {
    stop("No tweets found within specified date range.")
  } else {
    orig_tweets <- data[!(sapply(data, function(x) {x$getRetweeted()}))]
  }
  tweet_text <- sapply(orig_tweets, function(x) {iconv(x$getText(), to = "UTF-8")})
  tweet_date <- sapply(orig_tweets, function(x) {as.Date(x$getCreated())})
  return(data.table("date" = as.Date(tweet_date, origin="1970-01-01"), "tweet_text" = tweet_text))
}

## parsing tweets into words
parseTweets <- function(term, data) {
  if (length(data) > 0) {
    orig_tweets <- data[!(sapply(data, function(x) {x$getRetweeted()}))]
  } else {
    stop("No tweets found within specified date range.")
  }
  data_text <- sapply(orig_tweets, function(x) {iconv(x$getText(), to = "UTF-8")})
  data_corpus <- Corpus(VectorSource(data_text))
  tdm <- TermDocumentMatrix(data_corpus,
                            control=list(
                              removePunctuation = TRUE,
                              removeNumbers = TRUE,
                              tolower = TRUE,
                              stemming = TRUE,
                              stopwords = c(unlist(strsplit(term, " ")), stopwords("english"))
                            ))
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE) 
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  return(dm)
}
