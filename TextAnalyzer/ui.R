#install.packages("shinythemes")
library(shiny)
library(shinythemes)

shinyUI(
  fluidPage(# Application title
  theme = shinytheme("flatly"),
  titlePanel("Text Analysis and Modeling"),
  navbarPage(
    "Text Modeling",
    tabPanel("Language", sidebarLayout(
      # Sidebar with a slider and selection inputs
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
        # selectInput(
        #   "Model",
        #   "Choose a smoothing method:",
        #   choices = c("Laplace Smoothing", 
        #               "Good Turing's Smoothing")
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
    tabPanel("Sentiment", plotOutput("phonePlot"), plotOutput("plot"))
  )
))
