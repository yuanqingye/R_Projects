#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("dplyr")
library("shiny")
library("DT")

# create a summary table
summary_iris <- group_by(iris, Species) %>%
  summarise(Count = n())

ui <- fluidPage(
  dataTableOutput("summary")
  , dataTableOutput("drilldown")
)


server <- function(input, output){
  
  # display the data that is available to be drilled down
  output$summary <- DT::renderDataTable(summary_iris)
  
  # subset the records to the row that was clicked
  drilldata <- reactive({
    shiny::validate(
      need(length(input$summary_rows_selected) > 0, "Select rows to drill down!")
    )    
    
    # subset the summary table and extract the column to subset on
    # if you have more than one column, consider a merge instead
    # NOTE: the selected row indices will be character type so they
    #   must be converted to numeric or integer before subsetting
    selected_species <- summary_iris[as.integer(input$summary_rows_selected), ]$Species
    iris[iris$Species %in% selected_species, ]
  })
  
  # display the subsetted data
  output$drilldown <- DT::renderDataTable(drilldata())
}

shinyApp(ui, server)