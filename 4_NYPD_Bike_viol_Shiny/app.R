#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse, warn.conflicts = FALSE)


df_bike_violations<-read.csv('https://raw.githubusercontent.com/winkatme/NYC_Bike_Summons_and_Violations/main/Data/Processed/df_bike_violations_2023-08-05.csv')
df_bike_violations<-df_bike_violations |> 
  mutate(description = as.factor(description)) |> 
  mutate(violation_code = as.factor(violation_code)) |>
  mutate(veh_category = as.factor(veh_category)) |>
  mutate(city_nm = as.factor(city_nm)) |>
  mutate(rpt_owning_cmd = as.factor(rpt_owning_cmd)) |>
  mutate(violation_date = as.POSIXct(violation_date))
description_list = as.list(levels(df_bike_violations$description))



ui <- fluidPage(
  selectizeInput(inputId="description_list", label="Violation Description", choices = colnames(df_bike_violations)),
  #tableOutput("head")
  plotOutput("plot")

)
server <- function(input, output, session) {
  #df_bike_violations <- reactive({
  #  url = 'https://raw.githubusercontent.com/winkatme/NYC_Bike_Summons_and_Violations/main/Data/Processed/df_bike_violations_2023-08-05.csv'
  #  read.csv(url) 
  #})
  

  output$head <- renderTable({
    head(description_list)
  })
  
}

shinyApp(ui, server)
