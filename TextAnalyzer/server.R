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
  
  output$contents = renderText ({
    inFile = input$selection
    
    if (is.null(inFile)) {
      "Unable to Read File"
    } else {
      (readtext(inFile$datapath)$text)
    }
  })
  #Laplace Graph
  output$Lap_plot = renderPlot({
    
    input$update
    
    inFile = isolate(input$selection)
    #why wont it take the data path?!?!?!
    langauge_viz((inFile)$datapath, isolate(input$number))
    
    ggplot(lap_df) +
      aes(x = Language, y = Prob, fill = Language) +
      labs(title = "Language projection using laplace smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )
    
    ggplot(gt_df) +
      aes(x = Language, y = Prob, fill = Language) +
      labs(title = "Language projection using Good Turings smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE )
  })
  #Good Turings Graph
  # output$gt_plot = renderPlot({
  #   
  #   inFile = input$selection
  #   langauge_viz(inFile$datapath, input$number)
  # 
  # })
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
