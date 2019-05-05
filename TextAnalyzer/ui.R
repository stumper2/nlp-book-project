#install.packages("shinythemes")
library(shiny)
library(shinythemes)

shinyUI(
  fluidPage(# Application title
  theme = shinytheme("flatly"),
  titlePanel("Text Analysis and Modeling"),
  navbarPage(
    "Text Modeling",
    tabPanel("Analysis",
             sidebarLayout(
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
                 actionButton("update", "Change")
               ),
               
               mainPanel("Language Probability Bar Plot",
                         #plotOuput(), 
                         textOutput("contents"),
                         "Era Probability Bar Plot"#, plotOutput("")
               )
             )),
    
    tabPanel("Language", plotOutput("Lap_plot"), plotOutput("gt_plot")),
    tabPanel("Era"),
    tabPanel("Sentiment")
  )
))
