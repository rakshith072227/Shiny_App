# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# Function to convert mg/L to meq/L
to_meq <- function(mgL, mol_weight, valence) {
  (mgL / mol_weight) * valence
}

# Sample water quality data for Bengaluru (mg/L)
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

# Convert to meq/L
bengaluru_meq <- transform(bengaluru_data,
                           Ca   = to_meq(Ca,   40.08, 2),
                           Mg   = to_meq(Mg,   24.31, 2),
                           NaK  = to_meq(Na + K, 22.99, 1),
                           Cl   = to_meq(Cl,   35.45, 1),
                           SO4  = to_meq(SO4,  96.06, 2),
                           HCO3 = to_meq(HCO3, 61.02, 1)
)

# UI and Server Definition for Piper Plot Integration
ui <- dashboardPage(
  dashboardHeader(title = "Ground Water Assessment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Piper Plot", tabName = "piper", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "piper",
              fluidRow(
                box(title = "Piper Diagram", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piperPlot", height = "700px"), type = 6)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  output$piperPlot <- renderPlot({
    piperPlot(
      bengaluru_meq$Ca,
      bengaluru_meq$Mg,
      bengaluru_meq$NaK,
      bengaluru_meq$Cl,
      bengaluru_meq$SO4,
      bengaluru_meq$HCO3,
      Plot = list(name = bengaluru_data$Site),
      caption = "Piper Diagram – Bengaluru City Water Samples"
    )
  })
}

shinyApp(ui, server)