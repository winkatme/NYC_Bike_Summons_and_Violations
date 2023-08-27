#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

#df <-read.csv('Data/Processed/df_bike_violations_2023-08-05.csv')


ui <- fluidPage(
  tableOutput("summary")

)
server <- function(input, output, session) {
  df <- reactive({
    url = 'https://raw.githubusercontent.com/winkatme/NYC_Bike_Summons_and_Violations/main/Data/Processed/df_bike_violations_2023-08-05.csv'
    read.csv(url)
  })
  
  output$summary <- renderTable({
    summary(df())
  })
  
}

shinyApp(ui, server)

