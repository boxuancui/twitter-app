library(shiny)
library(data.table)
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(scales)

shinyServer(function(input, output, session) {
  observe({
    search_term <- input$search_term
    updateTextInput(session, "stop_words", value = gsub(" ", ",", search_term))
  })
  
  raw_tweets <- eventReactive(input$search_tweets, {
    search_term <- input$search_term
    start_date <- input$search_period[1]
    end_date <- input$search_period[2]
    num_tweets <- input$num_tweets
    
    searchTwitter(search_term, since = as.character(start_date), until = as.character(end_date), lang = "en", n = num_tweets)
  })
  
  tweet_words <- reactive({
    input$update_cloud
    
    stop_words <- input$stop_words
    raw_tweets <- raw_tweets()
    
    incProgress(6 / 10, detail = "Analyzing tweets ...")
    if (length(raw_tweets) <= 0) {
      stop("No tweets found within specified date range.")
    } else {
      orig_tweets <- raw_tweets[!(sapply(raw_tweets, function(x) {x$getRetweeted()}))]
    }
    
    data_text <- sapply(orig_tweets, function(x) {iconv(x$getText(), to = "UTF-8")})
    data_corpus <- Corpus(VectorSource(data_text))
    tdm <- TermDocumentMatrix(data_corpus,
                              control=list(
                                removePunctuation = TRUE,
                                removeNumbers = TRUE,
                                tolower = TRUE,
                                stemming = TRUE,
                                stopwords = c(unlist(strsplit(stop_words, "[,\ ]")), stopwords("english"))
                              ))
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    data.frame(word = names(word_freqs), freq = word_freqs)
  })
  
  daily_tweet <- reactive({
    data <- raw_tweets()
    
    if (length(data) <= 0) {
      stop("No tweets found within specified date range.")
    } else {
      orig_tweets <- data[!(sapply(data, function(x) {x$getRetweeted()}))]
    }
    tweet_text <- sapply(orig_tweets, function(x) {iconv(x$getText(), to = "UTF-8")})
    tweet_date <- sapply(orig_tweets, function(x) {as.Date(x$getCreated())})
    data.table("date" = as.Date(tweet_date, origin="1970-01-01"), "tweet_text" = tweet_text)
  })
  
  output$word_cloud <- renderPlot({
    input$search_tweets
    input$update_cloud
    if (input$search_tweets == 0) return()
    
    isolate({
      withProgress(message = "Please wait!", value = 0, {
        incProgress(1 / 10, detail = "Retrieving tweets ...")
        data <- tweet_words()
        incProgress(7 / 10, detail = "Creating word cloud ...")
        wordcloud(data$word, data$freq, min.freq = 3, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(8, 0.3))
        incProgress(10 / 10, detail = "Done!")
      })
    })
  }, width = 800, height = 600)
  
  output$tweets <- renderDataTable({
    input$search_tweets
    if (input$search_tweets == 0) return()
    
    daily_tweet()
  })
  
  output$twitter_ts <- renderPlot({
    input$search_tweets
    if (input$search_tweets == 0) return()
    
    daily_tweet <- daily_tweet()
    raw_data <- daily_tweet[, list(tweets = length(tweet_text)), keyby = date]
    date_data <- data.table("date" = as.Date(min(raw_data$date):max(raw_data$date), origin = "1970-01-01"), key = "date")
    data <- merge(date_data, raw_data, all.x = TRUE)
    data[is.na(tweets), tweets := 0]
    ggplot(data, aes_string(x = "date", y = "tweets")) +
      geom_point() +
      geom_line() +
      scale_x_date("Date", labels = date_format("%m-%d")) +
      scale_y_continuous("Number of tweets", labels = comma) +
      theme(
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
      )
  }, width = 1024)
})
