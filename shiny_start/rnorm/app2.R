library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Display datasets in R"),
  sidebarPanel(
    selectInput("select","please select a dataset",choices = c("iris","faithful","cars")),
    numericInput("obs", "Number of observations to view:", 10)
  ),
  mainPanel(
  verbatimTextOutput("summary"),  
  dataTableOutput("table")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$select,
           "iris" = iris,
           "faithful" = faithful,
           "cars" = cars)
  })
  
  output$summary = renderPrint({
    dt_name = datasetInput()
    summary(dt_name)
  })
  
  output$table <- renderDataTable({
    dt_name = datasetInput()
    obs = input$obs
    dt_name[1:obs,]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
