library(shiny)

shinyUI(fluidPage(
  theme="bootstrap.min.css",
  titlePanel("Twitter App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("term", label="Search tweets containing", value="#BigData"),
      br(),
      dateRangeInput("period", label="between", start=Sys.Date()-3, end=Sys.Date()),
      br(),
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