#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(shinycssloaders)

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
                fluidRow(column(width = 6, withSpinner(plotOutput("lap_plot")), color = "blue"), column(width = 6, withSpinner(plotOutput("gt_plot"), color = "orange"))),
                hr(),
                fluidRow(column(width = 12, "This section will analyze the text to determine the most prevalent language given 2 different smoothing methods - Laplace smoothing and Good Turing’s smoothing. The possible languages are English, Spanish, and Italian. Due to the results of the smoothing being ≈0, the graphs are the ln(probability) with the smallest graph being the most likely. This will be highlighted in red.")),
                hr()
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
               withSpinner(plotOutput("era_plot"), type = 4, color = "black", color.background = "white"),
               fluidRow("This section will analyze the inputted text and determine which literary period it was written during. This assertion is based on a model created by literature found on Gutenberg.com. The text is compared to each language-specific period and the highest returning score represents the most probable selection, thus highlighted in red."),
               hr(),
               fluidRow(column(width = 4, strong("English Timeline:"), br(), "- Middle English Period (1066-1500)", br(), "- The Renaissance (1500-1600)", br(), "- The Neoclassical Period (1600-1785)", br(), "- The Romantic Period (1785-1832)", br(), "- The Victorian Period (1832-1901)", br(), "- The Modern Period (1901-Present)"), 
                        column(width = 4, strong("Spanish Timeline:"), br(), "- Renaissance (1400-1600)", br(), "- Baroque (1600-1700)", br(), "- Enlightenment (1700-1800)", br(), "- Romanticism (1800-1850)", br(), "- Realism (1850-1900)", br(), "- Modernism (1900-Present)"), 
                        column(width = 4, strong("Italian Timeline:"), br(), "- Medieval Period (1200-1400)", br(), "- Renaissance Period (1400-1550)", br(), "- Baroque Period (1550-1700)", br(), "- Classicism Period (1700-1815)", br(), "- Romanticism and Realism Period (1815-1915)", br(), "- Contemporary Period (1915-Present)"))
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
        choices = c("Positivity",
                    "Emotion")
      ),
      hr(),
      actionButton("update3", "Update")
    ),
      mainPanel(
        fluidRow(column(width = 12, "This tab analyzes the connotation of the text, specifically the connotation of each word. By tokenizing the document and comparing it to a sentiment dictionary, we are able to categorize words based on emotion, and more generally whether they are positive or negative. The first is a graph of the word frequencies sorted by sentiment where as the second is a Word cloud categorizing the most frequent words by their connotations.")),
        hr(),
        withSpinner(plotOutput("phonePlot"), type = 3, color = "orange", color.background = "white"),
        withSpinner(plotOutput("plot"), type = 3, color = "blue", color.background = "white")
      )
  ))
))
)
