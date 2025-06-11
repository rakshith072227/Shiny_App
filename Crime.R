library(shiny)
library(tidyverse)
library(janitor)
ui <- fluidPage(
  tableOutput("tb1")
)

server <- function(input, output, session) {
  data1 <- reactive({
    "10_Property_stolen_and_recovered.csv" %>% 
      read_csv() %>% 
      clean_names() ->df1
  })
  data2 <- reactive({
    data1() %>% 
      rename("state_ut" = "area_name")
  })
  
  data3 <- reactive({
    data2() %>% 
      filter(group_name != "Total Property")
  })
  output$tb1 <- renderTable({
    data3() %>% 
      pull(cases_property_stolen) %>% 
      sum(na.rm = T) -> v
    paste0("Total")
  })
  
}

shinyApp(ui, server)