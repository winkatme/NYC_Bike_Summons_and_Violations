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
library(mapview, warn.conflicts = FALSE)
#install.packages('leaflet')
library(leaflet)

df_bike_violations<-read.csv('https://raw.githubusercontent.com/winkatme/NYC_Bike_Summons_and_Violations/main/Data/Processed/df_bike_violations_2023-08-05.csv')

df_bike_violations<-df_bike_violations |> 
  mutate(description = as.factor(description)) |> 
  mutate(violation_code = as.factor(violation_code)) |>
  mutate(veh_category = as.factor(veh_category)) |>
  mutate(city_nm = as.factor(city_nm)) |>
  mutate(rpt_owning_cmd = as.factor(rpt_owning_cmd)) |>
  mutate(violation_date = as.POSIXct(violation_date)) |> 
  drop_na()


ui <- fluidPage(
  titlePanel('Bike/E-vehicle violations geomap'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="description_input", 
                  label="Violation Description", 
                  choices = c("Choose one" = "", levels(fct_infreq(df_bike_violations$description))), 
                  selected="OPER BICYCLE WITH MORE 1 EARPHONE",  
                  width='600px'),
      
      sliderInput(inputId = "distribution", 
                  label = "Dates",
                  min = as.Date("2016-01-24","%Y-%m-%d"),
                  max = as.Date("2016-04-02","%Y-%m-%d"),
                  value = c(as.Date("2016-02-01"), as.Date("2016-03-21"))
                  ),
      ),
    mainPanel(
      leafletOutput("map")
      #tableOutput("head")
    )
  )
)


server <- function(input, output, session) {

  # test - successful - just show head from input  
  #df_head <- reactive(df_bike_violations |> filter(.data$description==.env$input$description_input))
  # note: Since reactive turns it into a function, '()' are required after.
  #output$head <- renderTable(head(df_head()))
  
  # test - success, but just in html: show mapview of just IMPROPER TURN
  #m <- mapview(na.omit(df_bike_violations |> filter(description=="IMPROPER TURN")), xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
  #output$map <- renderLeaflet(m@map)
  
  # works in shiny:
  df_plot <- reactive(df_bike_violations |> filter(.data$description==.env$input$description_input)) 
  #m <- reactive(mapview(df_plot(), xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE))
  output$map <- renderLeaflet((mapview(df_plot(), xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)@map))
  
  
}

shinyApp(ui, server)
