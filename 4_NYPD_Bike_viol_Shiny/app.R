#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <- fluidPage(
  selectInput("Data/Processed/df_bike_violations_2023-08-05.csv", label = "Dataset", choices = ls("Dataset")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
