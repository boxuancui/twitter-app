library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  titlePanel(title = "Twitter App"),
  theme = shinytheme("journal"),
  tags$head(tags$script(src = "general.js")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("term", label = "Search tweets containing", value = "SmarterTravel"),
      dateRangeInput("period", label = "between", start = Sys.Date() - 7, end = Sys.Date(), max = Sys.Date()),
      sliderInput("num_tweets", label = "with no more than n tweets", min = 100, max = 5000, value = 1000, step = 100),
      actionButton("search_tweets", label="Search")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("word_cloud", height="768px")),
        tabPanel("Tweets", dataTableOutput("tweets")),
        tabPanel("Daily Volume", plotOutput("twitter_ts", height="768px"))
      )
    )
  )
))