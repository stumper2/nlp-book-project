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
      labs(title = "Language projection using laplace smoothing", subtitle = "Where the smallest ln(probability) is the most likely", y = "ln(Probability)") +
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
      labs(title = "Language projection using Good Turings smoothing", subtitle = "Where the smallest ln(probability) is the most likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
  })
  

  
  
  #caching hte top lang
  # lang = reactive({
  #   input$update
  #   inFile = isolate(input$selection)
  #     lang_df = language_df((inFile)$datapath, isolate(input$number), 2)
  #     lang_df$Language[max(lang_df$Prob) == lang_df$Prob]
  # })
  
  # terms = eventReactive(input$Sentupdate, {
  #   # Change when the "update" button is pressed...
  #   # ...but not for anything else
  #   isolate({
  #     withProgress({
  #       setProgress(message = "Sentiment analysis...")
  #     })
  #   })
  # })
  #chacheing the model so that we can use in both tgraphs
  # word_sents = reactive({
  #   input$update
  #     inFile = isolate(input$selection)
  #     print("Calculating most probably language...")
  #     top_lang = lang()
  #  
  #   input$SentUpdate
  #   print("creating sentiment model using most probably language")
  #     sent_modeling(inFile$datapath, tolower(top_lang) , isolate(input$sentiment))
  # })
  # 
  #Word clouds!
  output$phonePlot <- renderPlot({
    
    input$update
    # input$SentUpdate
    inFile = isolate(input$selection)
    
    it = sent_modeling(inFile$datapath, tolower(input$SentLanguage), (input$sentiment))
    gr = ggplot(it) +
      aes(x = sentiment, fill = sentiment) +
      labs(x = "Sentiment", y = "Frequency") + 
      geom_bar() # + title and color and stuff
    
    if (isolate(input$sentiment) == "emotion") {
      gr + labs(title = "Graph of frequencies of emotionally implicit words", subtitle = "Grouped by respective connotations")
    } else {
      gr + labs(title = "Graph of frequencies in respect to Positive connotation", subtitle = "Grouped by positivity")
    }

   
  })
  
  #Word clouds!
  output$plot <- renderPlot({
    input$update
    inFile = isolate(input$selection)
    
    # input$Sentupdate
    
    it = sent_modeling(inFile$datapath, tolower(input$SentLanguage), (input$sentiment))
    
    ggplot(it) + 
      aes(label = word, size = n, color = sentiment) +
      scale_size_area(max_size = 24) + 
      geom_text_wordcloud(area_corr = TRUE ,rm_outside = TRUE) +   
      theme_minimal() 
  })
  

  
  output$era_plot = renderPlot({
    input$update
    
    inFile = isolate(input$selection)
    era_analysis(inFile$text, isolate(input$EraLanguage))
  })

})

