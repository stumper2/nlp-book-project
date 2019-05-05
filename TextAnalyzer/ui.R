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
        hr(),
        actionButton("update", "Change")
      ),
      
      mainPanel("Language Probability Bar Plot",
                #plotOuput(),
                # textOutput("contents"),
                plotOutput("Lap_plot")
                # ,plotOutput("gt_plot"))
      ))),
      tabPanel("Era", sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          fileInput(
            "selection",
            "Choose a Text File to Analyze",
            multiple = FALSE,
            accept = c(".txt")
          ),
          selectInput(
            "language",
            "Choose a language:",
            choices = c("English",
                        "Spanish",
                        "Italian")
          ),
          hr(),
          actionButton("update", "Change")
        ),
        
        mainPanel("Era Probability Bar Plot") #,
        # plotOutput("Lap_plot")
      )),
      tabPanel("Sentiment", sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          fileInput(
            "selection",
            "Choose a Text File to Analyze",
            multiple = FALSE,
            accept = c(".txt")
          ),
          selectInput(
            "language",
            "Choose a language:",
            choices = c("English",
                        "Spanish",
                        "Italian")
          ),
          hr(),
          actionButton("update", "Change")
        ),
        
        mainPanel(
          "Era Probability Bar Plot",
          plotOutput("phonePlot"),
          plotOutput("plot")
        )))
  )))