# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs)
library(smwrBase)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

# Custom Color Palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# Sample Data with State Names
get_sample_data <- function() {
  tibble(
    LOCATION_NAME = c("Tamil Nadu", "Kerala", "Karnataka", "Andhra Pradesh", "Telangana",
                      "Maharashtra", "Gujarat", "Punjab", "Rajasthan", "Uttar Pradesh"),
    CA = runif(10, 20, 100),
    MG = runif(10, 10, 50),
    Sodium = runif(10, 30, 150),
    K = runif(10, 2, 10),
    CHLORIDE = runif(10, 50, 250),
    SULPHATE = runif(10, 40, 200),
    BICARBONATE = runif(10, 80, 300)
  )
}

# UI
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 60px;",
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 20px;")
    ),
    titleWidth = 500
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("magnifying-glass")),
      menuItem("Ground Water Chemistry", tabName = "chemistry", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(sprintf('
        .skin-blue .main-header .logo { background-color: %s; color: white; }
        .skin-blue .main-header .navbar { background-color: %s; }
        body { background-color: %s; color: %s; }
        .box { border-top-color: %s; }
      ', iisc_palette["primary"], iisc_palette["secondary"],
                              iisc_palette["background"], iisc_palette["text"],
                              iisc_palette["accent"])))
    ),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload GPKG File", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load Data", icon = icon("database"))
                ),
                box(title = "File Information", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )
      ),
      
      tabItem(tabName = "data_explore",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE, uiOutput("dataset_summary")),
                box(title = "Column Details", status = "success", solidHeader = TRUE, DTOutput("column_details"))
              ),
              fluidRow(
                box(title = "Data Preview", status = "warning", solidHeader = TRUE, DTOutput("data_preview")),
                box(title = "Data Statistics", status = "info", solidHeader = TRUE, uiOutput("data_statistics"))
              )
      ),
      
      tabItem(tabName = "chemistry",
              fluidRow(
                box(title = "Select Location(s)", status = "info", solidHeader = TRUE, width = 12,
                    pickerInput("location_filter", "Choose Location(s):",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(actions-box = TRUE, live-search = TRUE))
                )
              ),
              fluidRow(
                box(title = "Piper Plot", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data_storage <- reactiveValues(csv_data = get_sample_data(), data_loaded = FALSE)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    showNotification("Custom GPKG upload not yet handled in this version", type = "warning")
  })
  
  observe({
    req(data_storage$csv_data)
    updatePickerInput(session, "location_filter",
                      choices = unique(data_storage$csv_data$LOCATION_NAME),
                      selected = unique(data_storage$csv_data$LOCATION_NAME))
  })
  
  filtered_data <- reactive({
    req(data_storage$csv_data)
    if (!is.null(input$location_filter)) {
      data_storage$csv_data %>% filter(LOCATION_NAME %in% input$location_filter)
    } else {
      data_storage$csv_data
    }
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("File or Sample Data Loaded\n",
        "Number of Rows:", nrow(data_storage$csv_data), "\n",
        "Number of Columns:", ncol(data_storage$csv_data))
  })
  
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<h4>Dataset Characteristics</h4>",
      "<p><strong>Total Rows:</strong>", nrow(data_storage$csv_data), "</p>",
      "<p><strong>Total Columns:</strong>", ncol(data_storage$csv_data), "</p>"
    ))
  })
  
  output$column_details <- renderDT({
    req(data_storage$csv_data)
    column_info <- data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, function(x) class(x)[1]),
      Unique_Values = sapply(data_storage$csv_data, function(x) length(unique(x))),
      Missing_Values = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    )
    datatable(column_info, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        summarise(across(all_of(numeric_cols), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<h4>Numeric Column Statistics</h4><pre>", capture.output(print(stats)), "</pre>"))
    } else HTML("<p>No numeric columns found.</p>")
  })
  
  # Piper Plot
  output$piper_plot <- renderPlot({
    df <- filtered_data()
    
    required_cols <- c("CA", "MG", "Sodium", "K", "CHLORIDE", "SULPHATE", "BICARBONATE")
    if (!all(required_cols %in% colnames(df))) {
      showNotification("Required ions missing for Piper plot", type = "error")
      return(NULL)
    }
    
    to_meq <- function(mgL, mol_weight, valence) {
      (mgL / mol_weight) * valence
    }
    
    Ca <- to_meq(df$CA, 40.08, 2)
    Mg <- to_meq(df$MG, 24.31, 2)
    NaK <- to_meq(df$Sodium + df$K, 22.99, 1)
    Cl <- to_meq(df$CHLORIDE, 35.45, 1)
    SO4 <- to_meq(df$SULPHATE, 96.06, 2)
    HCO3 <- to_meq(df$BICARBONATE, 61.02, 1)
    
    sample_labels <- df$LOCATION_NAME
    
    piperPlot(Ca, Mg, NaK, Cl, SO4, HCO3,
              Plot = list(name = sample_labels),
              caption = "Piper Diagram – Groundwater Chemistry")
  })
}

# Run App
shinyApp(ui, server)