library(shiny)

ui <- fluidPage(
  # Include custom font styles
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Georgia', serif;
        font-size: 18px;
        color: #2c3e50;
      }
      h1 {
        font-family: 'Arial', sans-serif;
        font-weight: bold;
      }
    "))
  ),
  
  # Top tagline
  tags$h1("Welcome to shiny world",
          style = "text-align: center; color: #2c3e50; font-weight: bold;"),
  
  fluidRow(
    column(12, h1("Rakshith Shiny App"))
  ),
  
  fluidRow(
    column(6, h1("Text 1")),
    column(2, h1("Text 2")),
    column(2, h1("Text 3")),
    column(2, h1("Text 4"))
  ),
  
  fluidRow(
    column(6, h3("Shiny is an R package that enables users to build interactive web applications directly from R. 
                It combines the computational power of R with the interactivity of a modern web interface.")),
    
    column(2, h3("Shiny is an R package that enables users to build interactive web applications directly from R. 
                It combines the computational power of R with the interactivity of a modern web interface.")),
    
    column(2, h3("Shiny is an R package that enables users to build interactive web applications directly from R. 
                It combines the computational power of R with the interactivity of a modern web interface.")),
    
    column(2, h3("Shiny is an R package that enables users to build interactive web applications directly from R. 
                It combines the computational power of R with the interactivity of a modern web interface."))
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
