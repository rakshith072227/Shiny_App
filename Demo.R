library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  selectInput("species", "Select Species", choices = c("setosa", "versicolor", "virginica")),
  plotOutput("speciesplot")
)

server <- function(input, output, session) {
  
  output$speciesplot <- renderPlot({
    iris %>% 
      filter(Species == input$species) %>%   
      ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point(color = "darkblue") +
      labs(title =input$species)
  })
}

shinyApp(ui,server)
