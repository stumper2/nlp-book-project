#install.packages("readtext")
library(shiny)
library(readtext)

shinyServer(function(input, output) {
  file1 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Loading file...")
        input$selection
      })
    })
  })
  
  file2 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Loading file...")
        input$selection2
      })
    })
  })
  
  file3 <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Loading file...")
        input$selection3
      })
    })
  })
  
  #Laplace Graph
  output$lap_plot = renderPlot({
    validate (
      is_there_data((input$selection)$datapath) %then%
        need(input$update, "Press the update Button!")
    )
    
    inFile = file1()
    
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
    
    inFile = file1()
    
    gt_df = language_df((inFile)$datapath, isolate(input$number), 2)   
    
    print("making gt graph!")
    ggplot(gt_df) +
      aes(x = Language, y = -Prob, fill = Minimum) +
      labs(title = "Language projection using Good Turings smoothing", subtitle = "Where the smallest ln(probability) is the most likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
  })
  
  output$era_plot = renderPlot({
    validate (
      is_there_data((input$selection2)$datapath) %then%
        need(input$update2, "Press the update Button!")
    )
    
    inFile = file2()
    
    scores = era_analysis(inFile$text, isolate(input$EraLanguage))
    
    scores %>%
      ggplot() +
      aes(x = document, y = format(round(as.numeric(score),2), nsmall = 2), fill = topic) +
      labs(x = "Era's", y = "Topic Scoring", title = "Topic Modeling based on Language specific Era") +
      geom_col()
  })
  
  #Word clouds!
  output$phonePlot <- renderPlot({
    validate (
      is_there_data((input$selection3)$datapath) %then%
        need(input$update3, "Press the update Button!")
    )
    
    inFile = file3()
    
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
    
    inFile = file3()
    
    it = withProgress({
      setProgress(message = "Processing file...",
      sent_modeling(inFile$datapath, tolower(input$SentLanguage), tolower(input$sentiment)))
    })
    
    ggplot(it) + 
      aes(label = word, size = n, color = sentiment) +
      scale_size_area(max_size = 24) + 
      geom_text_wordcloud(rm_outside = TRUE) +   
      theme_minimal() 
  })
})

