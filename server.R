library(shiny)
library(data.table)
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(scales)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
# reqURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# api_key <-  "o735p4lWKoZlvtaTY6y7ZrTJm"
# api_secret <- "w7G7v1hVSMd8T6qCLCErTBfStTr8aTToykpGUZWoxXaLc6F5MR"
# access_token <- "237160629-8R5K1VZP5Mkejt9BuAkNXarrDQHLTsYHZnjBoc3m"
# access_secret <- "tGtdUsmPEDAdVUNkFhT7S0QQjvSWtRkClB3rOsc3c3QZF"
# twitCred <- OAuthFactory$new(
#   consumerKey = api_key, 
#   consumerSecret = api_secret,
#   requestURL = reqURL,
#   accessURL = accessURL, 
#   authURL = authURL
# )
# twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# save(twitCred, file="R/Shiny/twitter-sentiment-analysis/credential.RData")
load("credential.RData")
registerTwitterOAuth(twitCred)

shinyServer(function(input, output) {
  getTweets <- function(term, start_date, end_date) {
    data <- searchTwitter(term, since=as.character(start_date), until=as.character(end_date), lang="en", n=800)
    return(data)
  }
  
  dailyTweets <- function(data) {
    if (length(data) > 0) {
      orig_tweets <- data[!(unlist(lapply(data, function(x){x$getRetweeted()})))]
    } else {
      stop("No tweets found within specified date range.")
    }
    tweet_text <- sapply(orig_tweets, function(x) {x$getText()})
    tweet_date <- sapply(orig_tweets, function(x) {as.Date(x$getCreated())})
    return(data.table("date"=as.Date(tweet_date, origin="1970-01-01"), "tweet_text"=tweet_text))
  }
  
  parseTweets <- function(term, data) {
    if (length(data) > 0) {
      orig_tweets <- data[!(unlist(lapply(data, function(x){x$getRetweeted()})))]
    } else {
      stop("No tweets found within specified date range.")
    }
    data_text <- sapply(orig_tweets, function(x) x$getText())
    data_corpus <- Corpus(VectorSource(data_text))
    tdm <- TermDocumentMatrix(
      data_corpus,
      control=list(
        removePunctuation=FALSE,
        removeNumbers=TRUE,
        tolower=TRUE,
        stopwords=c(tolower(term), tolower(gsub("#", "", term)), stopwords("english"))
      )
    )
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing=TRUE) 
    pre_dm <- data.frame(word=names(word_freqs), freq=word_freqs)
    punc_index <- grep("[[:punct:]]", row.names(pre_dm))
    dm <- pre_dm[-punc_index,]
    return(dm)
  }
  
  rawTweets <- reactive({getTweets(input$term, input$period[1], input$period[2])})  
  tweetDate <- reactive({dailyTweets(rawTweets())})
  tweetWords <- reactive({parseTweets(input$term, rawTweets())})
  
  output$word_cloud <- renderPlot({
    input$search_tweets
    if (input$search_tweets == 0) {
      return()
    }
    withProgress(message="Retrieving tweets from Twitter API", {
      data <- tweetWords()
      withProgress(message="Generating word cloud", {
        isolate(wordcloud(data$word, data$freq, scale=c(8, 0.3), min.freq=3, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2")))
      })
    })
  }, width=1024)
  
  output$tweets <- renderDataTable({
    input$search_tweets
    if (input$search_tweets == 0) {
      return()
    }
    tweetDate()
  })
  
  output$twitter_ts <- renderPlot({
    raw_data <- tweetDate()[, list(tweets=length(tweet_text)), keyby=date]
    date_data <- data.table("date"=as.Date(min(raw_data$date):max(raw_data$date), origin="1970-01-01"), key="date")
    data <- merge(date_data, raw_data, all.x=TRUE)
    data[is.na(tweets), tweets:=0]
    ggplot(data, aes_string(x="date", y="tweets")) +
      geom_point() +
      geom_line() +
      scale_x_date(labels=date_format("%m-%d")) +
      scale_y_continuous(labels=comma) +
      theme(
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15, vjust=1)
      )
  }, width=1024)
  
})


