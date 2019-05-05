# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Give the page a title
  titlePanel("Sentiment Modeling"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar with one input
    sidebarPanel(
      fileInput(
        "selection",
        "Choose a Text File to Analyze",
        multiple = FALSE,
        accept = c(".txt")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")
      
      plotOutput("plot")
    )
  )
)