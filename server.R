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
  raw_tweets <- eventReactive(input$search_tweets, {
    term <- input$term
    start_date <- input$period[1]
    end_date <- input$period[2]
    num_tweets <- input$num_tweets
    
    searchTwitter(term, since = as.character(start_date), until = as.character(end_date), lang = "en", n = num_tweets)
  })
  
  daily_tweet <- reactive({
    data <- raw_tweets()
    ## remove retweets
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
    if (input$search_tweets == 0) return()
    isolate({
      withProgress(message = "Please wait!", value = 0, {
        incProgress(5 / 100, detail = "Fetching tweets ...")
        raw_data <- raw_tweets()
        
        incProgress(50 / 100, detail = "Parsing tweets ...")
        if (length(raw_data) <= 0) {
          stop("No tweets found within specified date range.")
        } else {
          orig_tweets <- raw_data[!(sapply(raw_data, function(x) {x$getRetweeted()}))]
        }
        
        data_text <- sapply(orig_tweets, function(x) {iconv(x$getText(), to = "UTF-8")})
        data_corpus <- Corpus(VectorSource(data_text))
        tdm <- TermDocumentMatrix(data_corpus,
                                  control=list(
                                    removePunctuation = TRUE,
                                    removeNumbers = TRUE,
                                    tolower = TRUE,
                                    stemming = TRUE,
                                    stopwords = c(unlist(strsplit(input$term, " ")),
                                                  paste0(c("@", "#"), "smarter travel"),
                                                  stopwords("english"))
                                  ))
        m <- as.matrix(tdm)
        word_freqs <- sort(rowSums(m), decreasing = TRUE) 
        data <- data.frame(word = names(word_freqs), freq = word_freqs)
        
        incProgress(90 / 100, detail = "Creating word cloud ...")
        wordcloud(data$word, data$freq, min.freq = 3, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(8, 0.3))
      })
    })
  }, width = 800, height = 600)
  
  output$tweets <- renderDataTable({
    if (input$search_tweets == 0) return()
    isolate({daily_tweet()})
  })
  
  output$twitter_ts <- renderPlot({
    if (input$search_tweets == 0) return()
    isolate({
      daily_tweet <- daily_tweet()
      raw_data <- daily_tweet[, list(tweets = length(tweet_text)), keyby = date]
      date_data <- data.table("date" = as.Date(min(raw_data$date):max(raw_data$date), origin = "1970-01-01"), key = "date")
      data <- merge(date_data, raw_data, all.x = TRUE)
      data[is.na(tweets), tweets := 0]
      ggplot(data, aes_string(x = "date", y = "tweets")) +
        geom_point() +
        geom_line() +
        scale_x_date(labels = date_format("%m-%d")) +
        scale_y_continuous(labels = comma) +
        theme(
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15, vjust = 1)
        )
    })
  }, width = 1024)
})
