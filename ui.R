library(shiny)

shinyUI(fluidPage(
  titlePanel("Twitter App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("term", label="Search tweets containing", value="#BigData"),
      dateRangeInput("period", label="between", start=Sys.Date()-3, end=Sys.Date()),
      br(),
      submitButton("Search")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("word_cloud")),
        tabPanel("Tweets", tableOutput("tweets"))
      )
    )
  )
))