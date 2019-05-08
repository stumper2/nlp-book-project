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
    
    lap_df = language_df((inFile)$datapath, isolate(input$number), 1) 
    
    print("making laplace graph!")
      ggplot(lap_df) +
      aes(x = Language, y =-Prob, fill = Minimum) +
      labs(title = "Language projection using laplace smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
    
   
  })
  
  # Good Turings Graph
  output$gt_plot = renderPlot({

    input$update
    
    inFile = isolate(input$selection)
    
    gt_df = language_df((inFile)$datapath, isolate(input$number), 2)   
    
    print("making gt graph!")
    ggplot(gt_df) +
      aes(x = Language, y = -Prob, fill = Minimum) +
      labs(title = "Language projection using Good Turings smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
  })
  
  
  
  terms = eventReactive(input$SentUpdate, {
    # Change when the "update" button is pressed...
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Sentiment analysis...")
      })
    })
  })
  #chacheing the model so that we can use in both tgraphs
  re = reactive({
    input$update
    inFile = isolate(input$selection)
    input$SentUpdate
    sent_modeling(inFile$datapath, "english" , isolate(input$sentiment))
  })
  
  #Word clouds!
  output$phonePlot <- renderPlot({

    input$update
    input$SentUpdate
    
    it = re()
    
    ggplot(it) +
      aes(x = sentiment, fill = sentiment) +
      geom_bar() # + title and color and stuff
   
  })
  
  #Word clouds!
  output$plot <- renderPlot({
    
    input$update
    input$SentUpdate
    
    it = re()
    
    ggplot(it) + 
      aes(label = word, size = n, color = sentiment) +
      scale_size_area(max_size = 24) + 
      geom_text_wordcloud(area_corr = TRUE ,rm_outside = TRUE) +   
      theme_minimal() + 
      facet_wrap(~sentiment)
  })
})
