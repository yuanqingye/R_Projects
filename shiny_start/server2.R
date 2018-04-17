server = function(input,output){
  output$hist = renderPlot({
    title = 'norm distribution'
    hist(rnorm(input$num),main = title)
    })
}

shinyServer(server)
