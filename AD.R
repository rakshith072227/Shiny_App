library(shiny)

ui <- fluidPage(
  numericInput("num1", "A", value = 0),
  numericInput("num2", "B", value = 0),
  numericInput("num3", "C", value = 0),
  textOutput("sumResult")  # Output placeholder
)

server <- function(input, output, session) {
  output$sumResult <- renderText({
    sum <- input$num1 + input$num2 + input$num3
    paste("Sum is:", sum)
  })
}

shinyApp(ui = ui, server = server)
