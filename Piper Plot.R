# Save this as app.R and run it in RStudio

library(shiny)
library(smwrGraphs)

# Function to convert mg/L to meq/L
to_meq <- function(mgL, mol_weight, valence) {
  (mgL / mol_weight) * valence
}

ui <- fluidPage(
  titlePanel("Piper Diagram - Bengaluru City Water Samples"),
  sidebarLayout(
    sidebarPanel(
      helpText("Static sample dataset is used for Bengaluru water quality (mg/L)."),
      actionButton("plotBtn", "Generate Piper Diagram")
    ),
    mainPanel(
      plotOutput("piperPlot", height = "700px")
    )
  )
)

server <- function(input, output) {
  
  # Sample data
  bengaluru_data <- data.frame(
    Site = paste("Sample", 1:5),
    Ca = c(40, 35, 50, 60, 45),       
    Mg = c(12, 15, 10, 20, 18),
    Na = c(25, 30, 22, 28, 27),
    K  = c(2, 2.5, 2, 3, 2.8),
    Cl = c(30, 35, 28, 40, 33),
    SO4 = c(20, 25, 18, 22, 21),
    HCO3 = c(90, 100, 85, 110, 95)
  )
  
  # Reactive expression to generate meq/L data
  bengaluru_meq <- reactive({
    transform(bengaluru_data,
              Ca   = to_meq(Ca,   40.08, 2),
              Mg   = to_meq(Mg,   24.31, 2),
              NaK  = to_meq(Na + K, 22.99, 1),  # Na + K
              Cl   = to_meq(Cl,   35.45, 1),
              SO4  = to_meq(SO4,  96.06, 2),
              HCO3 = to_meq(HCO3, 61.02, 1)
    )
  })
  
  output$piperPlot <- renderPlot({
    input$plotBtn  # triggers reactivity
    isolate({
      data_meq <- bengaluru_meq()
      
      piperPlot(
        data_meq$Ca,
        data_meq$Mg,
        data_meq$NaK,
        data_meq$Cl,
        data_meq$SO4,
        data_meq$HCO3,
        Plot = list(name = bengaluru_data$Site),
        caption = "Piper Diagram – Bengaluru City Water Samples"
      )
    })
  })
}

shinyApp(ui = ui, server = server)
