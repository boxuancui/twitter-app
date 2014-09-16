library(shiny)
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RCurl)

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
    data <- searchTwitter(term, since=as.character(start_date), until=as.character(end_date), lang="en", n=300)
    data_text <- sapply(data, function(x) x$getText())
    return(data_text)
  }
  
  parseTweets <- function(term, data_text) {
    data_corpus <- Corpus(VectorSource(data_text))
    tdm <- TermDocumentMatrix(
      data_corpus,
      control=list(
        removePunctuation=TRUE,
        stopwords=c(term, stopwords("english")),
        removeNumbers=TRUE, tolower=TRUE)
    )
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing=TRUE) 
    dm <- data.frame(word=names(word_freqs), freq=word_freqs)
    return(dm)
  }
  
  rawTweets <- reactive({getTweets(input$term, input$period[1], input$period[2])})
  tweetWords <- reactive({parseTweets(input$term, rawTweets())})
  
  output$word_cloud <- renderPlot({
    wordcloud(tweetWords()$word, tweetWords()$freq, scale=c(8,0.3), min.freq=3, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"))
  })
  
  output$tweets <- renderTable({
    data.frame("Tweets"=rawTweets())
  })
})


