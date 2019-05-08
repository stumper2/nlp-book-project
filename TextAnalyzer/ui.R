#install.packages("shinythemes")
library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  # Application title
  theme = shinytheme("flatly"),
  titlePanel("Text Analysis and Modeling"),
  navbarPage(
    "Text Modeling",
    tabPanel("Language", sidebarLayout(
      #Side bar for data inputs
      sidebarPanel(
        fileInput(
          "selection",
          "Choose a Text File to Analyze",
          multiple = FALSE,
          accept = c(".txt")
        ),
        selectInput(
          "number",
          "Choose a language model:",
          choices = c("Word Bigram",
                      "Word Trigram",
                      "Word Quadgram")
        ),
        hr(),
        actionButton("update", "Update")
      ),
      
      mainPanel(#"Language Probability Bar Plot",
                #plotOuput(), 
                # textOutput("contents"),
                plotOutput("lap_plot"),
                plotOutput("gt_plot")
                )
      )
    ),
    tabPanel("Era"),
    tabPanel("Sentiment", 
      sidebarLayout(
      # Side bar for data inputs
        sidebarPanel(
        selectInput(
          "sentiment",
          "Choose a Sentiment Analysis:",
          choices = c("positivity",
                      "emotion")
        ),
        hr(),
        actionButton("SentUpdate", "Update")
      ),
      
      mainPanel(
             plotOutput("phonePlot"), 
             plotOutput("plot")
             )
      )
  )
  )
))
