#install.packages("readtext")
library(shiny)
library(readtext)

shinyServer(function(input, output) {
  
  # testing for readble file
  output$contents = renderText ({
    inFile = input$selection

    if (is.null(inFile)) {
      "Unable to Read File"
    } 
  })

  #Laplace Graph
  output$lap_plot = renderPlot({
    
    validate (
      is_there_data((input$selection)$datapath) %then%
      need(input$update, "Press the update Button!")
    )
    
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
    
    validate (
      is_there_data((input$selection)$datapath) %then%
        need(input$update, "Press the update Button!")
    )

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
  

  #Word clouds!
  output$phonePlot <- renderPlot({

    validate (
      is_there_data((input$selection3)$datapath) %then%
        need(input$update3, "Press the update Button!")
    )
    
    input$update3
    
    inFile = isolate(input$selection3)

    input$SentUpdate
    it = sent_modeling(inFile$datapath, tolower(input$SentLanguage), tolower(input$sentiment))
    gr = ggplot(it) +
      aes(x = sentiment, fill = sentiment) +
      labs(x = "Sentiment", y = "Frequency") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_bar() # + title and color and stuff
    
    if (isolate(input$sentiment) == "emotion") {
      gr + labs(title = "Graph of frequencies of emotionally implicit words", subtitle = "Grouped by respective connotations")
    } else {
      gr + labs(title = "Graph of frequencies in respect to Positive connotation", subtitle = "Grouped by positivity")
    }
  })
  
  #Word clouds!
  output$plot <- renderPlot({
    validate (
      is_there_data((input$selection3)$datapath) %then%
        need(input$update3, "Press the update Button!")
    )

    input$update3
    inFile = isolate(input$selection3)

    it = sent_modeling(inFile$datapath, tolower(input$SentLanguage), tolower(input$sentiment))
    
    ggplot(it) + 
      aes(label = word, size = n, color = sentiment) +
      scale_size_area(max_size = 24) + 
      geom_text_wordcloud(rm_outside = TRUE) +   
      theme_minimal() 
  })
  
  output$era_plot = renderPlot({
    input$update2
  
    validate (
      is_there_data((input$selection2)$datapath) %then%
        need(input$update2, "Press the update Button!")
    )
    inFile = isolate(input$selection2)
    scores = era_analysis(inFile$datapath, isolate(input$EraLanguage))
    
    scores1 = scores %>%
      mutate(Maximum = ifelse(max(as.numeric(score)) == as.numeric(score), T, F)) 
    scores1$Maximum
    scores1%>%
      ggplot() +
      aes(x = document, y = format(round(as.numeric(score),4), nsmall = 2), fill = Maximum) +
      labs(x = "Era's", y = "Topic Scoring", title = "Topic Modeling based on Language specific Era") +
      geom_col() +
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
    

    
  })
})

