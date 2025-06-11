library(shiny)

ui <- fluidPage(
  numericInput("num1", "A", value = 0),
  numericInput("num2", "B", value = 0),
  numericInput("num3", "C", value = 0),
  textOutput("multiplyResult")  # Fixed ID (no spaces)
)

server <- function(input, output, session) {
  output$multiplyResult <- renderText({
    product <- input$num1 * input$num2 * input$num3
    paste("Final Value:", product)
  })
}

shinyApp(ui = ui, server = server)
