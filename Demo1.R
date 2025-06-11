library(shiny)
library(tidyverse)

ui <- fluidPage(
  selectInput("species", "Select Species", choices = c("setosa", "versicolor", "virginica")),
  plotOutput("speciesplot"),
  tableOutput("speciestable")
)

server <- function(input, output, session) {
  
  # Reactive block to filter species-specific data
  speciesdata <- reactive({
    iris %>% 
      filter(Species == input$species)
  })
  
  # Plot rendered from filtered data
  output$speciesplot <- renderPlot({
    ggplot(speciesdata(), aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point(color = "darkblue") +
      labs(title = paste("Sepal Plot for", input$species))
  })
  
  # Table rendered from filtered data
  output$speciestable <- renderTable({
    speciesdata()
  })
}

shinyApp(ui, server)
