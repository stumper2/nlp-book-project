#install.packages("readtext")
library(shiny)
library(readtext)

shinyServer(function(input, output) {
  terms = reactive({
    # Change when the "update" button is pressed...
    input$update
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
      # ngram_evaluator_laplace(inFile, input$Model, input$number)
    }
  })
})
