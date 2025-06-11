library(tidyverse)
library(janitor)
library(ggplot2)
library(shiny)
library(lubridate)

ui <- fluidPage(
  tableOutput("tb2")
)

server <- function(input, output, session) {
  data1 <- reactive({
    "city_day.csv" %>% 
    read_csv() %>% 
    clean_names() -> aqidf1
  })
  data2 <- reactive({
    data1() %>% 
      mutate(year = date %>% year(),
             month = date %>% month(),
             day = date %>% day(),
             week = date %>%  week(),
             weekend = date %>% wday(label = T)) -> aqidf2
  })
  data3 <- reactive({
    data2() %>%
      pivot_longer(cols = 3:14,  # make sure these are your pollutant columns
                   names_to = "pollutants",
                 values_to = "values")%>% 
      group_by(year, pollutants) %>% 
      summarise(mean_values = mean(values, na.rm = TRUE), .groups = "drop")
})
  output$tb2 <- renderTable({
    data3()
  })
  
}

shinyApp(ui, server)
