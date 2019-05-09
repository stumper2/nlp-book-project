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
      
      mainPanel(# textOutput("contents"),
                fluidRow(column(width = 6, plotOutput("lap_plot")), column(width = 6, plotOutput("gt_plot"))),
                fluidRow(column(width = 12, "Both graphs above are projections of probabilities of given languages. Given our limited resources and time, we are estimating whether the text will be English, Italian, or Spanish. The smoothing methods available are Laplace smoothing and Good Turings, but due to the probabilities ≈0, we leave the results in a natural log form. The most probable language is highlighted above in red."))
                )
      )
    ),
    
    tabPanel("Era", 
             sidebarPanel(
               fileInput(
                 "selection2",
                 "Choose a Text File to Analyze",
                 multiple = FALSE,
                 accept = c(".txt")
               ),
               selectInput(
                 "EraLanguage",
                 "Choose a language:",
                 choices = c("English",
                             "Spanish",
                             "Italian")
               ),
               hr(),
               actionButton("update2", "Update")
             ),
             mainPanel(
               plotOutput("era_plot")
             )
    ),
  
  tabPanel("Sentiment", sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      fileInput(
        "selection3",
        "Choose a Text File to Analyze",
        multiple = FALSE,
        accept = c(".txt")
      ),
      selectInput(
        "SentLanguage",
        "Choose a language:",
        choices = c("English",
                    "Spanish",
                    "Italian")
      ),
      selectInput(
        "sentiment",
        "Choose a Sentiment Analysis:",
        choices = c("positivity",
                    "emotion")
      ),
      hr(),
      actionButton("update3", "Update")
    ),
      mainPanel(
        fluidRow(column(width = 6, plotOutput("phonePlot")), column(width = 6,  plotOutput("plot"))),
        fluidRow(column(width = 12, "This tab analyzes the connotation of the text, specifically the connotation of each word. By tokenizing the document and comparing it to a sentiment dictionary, we are able to categorize words based on emotion, and more generally whether they are positive or negative. On the left is a graph representing the frequencies where as the right is a Word cloud categorizing the most frequent words by their connotations."))
      )
  ))
))
)
