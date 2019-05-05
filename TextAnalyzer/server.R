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
    }
  })
  #Laplace Graph
  output$Lap_plot = renderPlot({

    inFile = input$selection
    #why wont it take the data path?!?!?!
    langauge_viz((inFile)$datapath, input$number)
  })
  #Good Turings Graph
  output$gt_plot = renderPlot({
    
    inFile = input$selection
    langauge_viz(inFile$datapath, input$number)

  })
})
