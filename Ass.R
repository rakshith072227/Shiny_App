library(shiny)
library(tidyverse)

# Load your dataset (adjust path or method as required)
# Assume pol_data is already available in your environment

ui <- fluidPage(
  titlePanel("Air Quality Trends Explorer (2015–2020)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select City", choices = unique(pol_data$City)),
      selectInput("pollutant", "Select Pollutant", choices = unique(pol_data$pollutant)),
      checkboxInput("show_all_cities", "Compare Across All Cities (for CO only)", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("trendPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$city, input$pollutant)
    
    if (input$show_all_cities && input$pollutant == "CO") {
      pol_data %>%
        filter(pollutant == "CO") %>%
        group_by(Year, City) %>%
        summarise(mean_value = mean(values, na.rm = TRUE), .groups = 'drop')
    } else {
      pol_data %>%
        filter(City == input$city, pollutant == input$pollutant) %>%
        group_by(Year) %>%
        summarise(mean_value = mean(values, na.rm = TRUE), .groups = 'drop')
    }
  })
  
  # Render plot
  output$trendPlot <- renderPlot({
    df <- filtered_data()
    
    if (input$show_all_cities && input$pollutant == "CO") {
      ggplot(df, aes(x = Year, y = mean_value, color = City)) +
        geom_line() +
        facet_wrap(~City, scales = "free_y") +
        labs(title = "CO Trends Across All Cities", y = "CO Level", x = NULL) +
        theme_minimal()
    } else {
      ggplot(df, aes(x = Year, y = mean_value)) +
        geom_line(color = "steelblue") +
        labs(title = paste("Trend of", input$pollutant, "in", input$city),
             y = paste(input$pollutant, "Level"),
             x = NULL) +
        theme_minimal()
    }
  })
  
  # Render table
  output$summaryTable <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
