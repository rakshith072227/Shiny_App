library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)  # Needed for date handling

ui <- fluidPage(
  titlePanel("Year-wise Average Pollutant Levels"),
  tableOutput("tb2")
)

server <- function(input, output, session) {
  
  # Load and clean data
  data1 <- reactive({
    read_csv("city_day.csv") %>% 
      clean_names()
  })
  
  # Add year, month, etc.
  data2 <- reactive({
    data1() %>% 
      mutate(
        year = year(date),
        month = month(date),
        day = day(date),
        week = week(date),
        weekend = wday(date, label = TRUE)
      )
  })
  
  # Transform and summarize
  data3 <- reactive({
    data2() %>%
      pivot_longer(cols = 3:14,   # Adjust if needed based on your column positions
                   names_to = "pollutants",
                   values_to = "values") %>%
      group_by(year, pollutants) %>% 
      summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
  })
  
  # Render table
  output$tb2 <- renderTable({
    data3()
  })
}

shinyApp(ui = ui, server = server)
