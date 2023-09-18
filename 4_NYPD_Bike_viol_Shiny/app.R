library(shiny)
library(tidyverse, warn.conflicts = FALSE)
library(mapview, warn.conflicts = FALSE)
# install.packages('leaflet')
library(leaflet)

# import and quickly clean the data
df_bike_violations <- read.csv("https://raw.githubusercontent.com/winkatme/NYC_Bike_Summons_and_Violations/main/Data/Processed/df_bike_violations_2023-09-16.csv")

df_bike_violations <- df_bike_violations |>
  mutate(description = as.factor(description)) |>
  mutate(violation_code = as.factor(violation_code)) |>
  mutate(veh_category = as.factor(veh_category)) |>
  mutate(city_nm = as.factor(city_nm)) |>
  mutate(rpt_owning_cmd = as.factor(rpt_owning_cmd)) |>
  mutate(violation_date = as.POSIXct(violation_date)) |>
  select(-c(evnt_key, rpt_owning_cmd, x_coord_cd, y_coord_cd, location)) |>
  drop_na()


ui <- fluidPage(
  titlePanel(" NYC Bike/E-Vehicle Violations Geomap"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "description_input",
        label = "Violation Description, sorted in descending order of frequency",
        choices = c("Choose one" = "", levels(fct_infreq(df_bike_violations$description))),
        selected = "OPER BICYCLE WITH MORE 1 EARPHONE",
        width = "600px"
      ),
      
      ## To Do: A date slider input might look better. 
      # sliderInput(inputId = "distribution",
      #            label = "Dates",
      #            min = as.Date("2016-01-24","%Y-%m-%d"),
      #            max = as.Date("2016-04-02","%Y-%m-%d"),
      #            value = c(as.Date("2016-02-01"), as.Date("2016-03-21"))
      #            ),

      dateRangeInput("date_range", "Date range",
        start = "2018-01-07",
        end = "2023-03-30"
      ),
      htmlOutput("date_start_end_as_text")
    ),
    mainPanel(
      leafletOutput("map")
      # tableOutput("head")
    )
  )
)


server <- function(input, output, session) {
  # geomap:
  df_plot <- reactive(df_bike_violations |>
    filter(.data$description == .env$input$description_input) |>
    filter(.data$violation_date >= .env$input$date_range[1] & .data$violation_date <= .env$input$date_range[2]))

  output$map <- renderLeaflet((mapview(df_plot(), xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)@map))

  # update the dateRangeInput values  with date_min and date_max after choosing
  # a new violation from the selectInput drop down:
  date_min <- reactive(df_bike_violations |>
    filter(.data$description == .env$input$description_input) |>
    slice(which.min(.data$violation_date)) |>
    mutate(violation_date = as.Date(.data$violation_date)) |>
    pull(.data$violation_date))

  date_max <- reactive(df_bike_violations |>
    filter(.data$description == .env$input$description_input) |>
    slice(which.max(.data$violation_date)) |>
    mutate(violation_date = as.Date(.data$violation_date)) |>
    pull(.data$violation_date))

  observeEvent(input$description_input, {
    updateDateRangeInput(session, "date_range", start = date_min(), end = date_max())
  })

  # display min and max dates to user
  output$date_start_end_as_text <- renderUI({
    HTML(paste(
      "Earliest date for this violation: ", date_min(), "<br/>",
      "Most recent date for this violation: ", date_max()
    ))
  })
}

shinyApp(ui, server)
