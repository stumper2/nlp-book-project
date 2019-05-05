#install.packages("readtext")
library(shiny)
library(readtext)

shinyServer(function(input, output) {
  terms = eventReactive(input$update, {
    # Change when the "update" button is pressed...
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # testing for readble file
  output$contents = renderText ({
    inFile = input$selection

    if (is.null(inFile)) {
      "Unable to Read File"
    } 
  })
  
  #Laplace Graph
  output$lap_plot = renderPlot({
    
    input$update
    
    inFile = isolate(input$selection)
    
    langauge_viz((inFile)$datapath, isolate(input$number), 1) 
  })
  
  # Good Turings Graph
  output$gt_plot = renderPlot({

    input$update
    
    inFile = isolate(input$selection)
    
    langauge_viz((inFile)$datapath, isolate(input$number), 2)    
    
  })
  
  
  
  #Word clouds!
  output$phonePlot <- renderPlot({
    inFile = input$selection
    # Render a barplot
    it = sent_modeling(inFile$datapath, "English")
    ggplot(it) +
      geom_bar(aes(x =sentiment))
  })
  #Word clouds!
  output$plot <- renderPlot({
    create_wordcloud(it)
  })
})
