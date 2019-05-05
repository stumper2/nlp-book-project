# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    it = sent_modeling()
    ggplot(it) +
      geom_bar(aes(x =sentiment))
  })

 output$plot <- renderPlot({
   create_wordcloud(it)
 })

}
# Run the application 
shinyApp(ui = ui, server = server)