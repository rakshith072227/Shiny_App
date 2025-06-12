# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
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

# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# WQI Calculation Function
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }
  if ("NA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  }
  
  standards <- list(
    TDS = list(St = 1000, Wi = 0.121), EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152), SULPHATE = list(St = 250, Wi = 0.121),
    CHLORIDE = list(St = 250, Wi = 0.093), BICARBONATE = list(St = 500, Wi = 0.152),
    FLUORIDE = list(St = 1.2, Wi = 0.030), CA = list(St = 100, Wi = 0.060),
    MG = list(St = 50, Wi = 0.060), Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )
  
  param_names <- names(standards)
  for (param in param_names) {
    if (!param %in% names(water_sf)) {
      water_sf[[param]] <- 0
    } else {
      water_sf[[param]][is.na(water_sf[[param]])] <- 0
    }
  }
  
  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }
  
  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)
  
  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE
  )
  
  csv_path <- file.path(getwd(), "water_quality_output.csv")
  write.csv(st_drop_geometry(water_sf), csv_path, row.names = FALSE)
  return(water_sf)
}

# UI
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "sirpilogo white.avif", height = "30px", style = "margin-right: 10px;"),
      tags$img(src = "gdx_logo.png", height = "30px", style = "margin-right: 10px;"),
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 30px;")
    ),
    titleWidth = 650
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
      tags$style(HTML(sprintf(
        '.skin-blue .main-header .logo { background-color: %s; }
         .skin-blue .main-header .navbar { background-color: %s; }
         body { background-color: %s; color: %s; }
         .box { border-top-color: %s; }',
        iisc_palette["primary"], iisc_palette["secondary"],
        iisc_palette["background"], iisc_palette["text"],
        iisc_palette["accent"]
      )))
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
                box(title = "Filters", status = "info", solidHeader = TRUE, width = 12,
                    selectInput("state", "State", choices = NULL),
                    selectInput("district", "District", choices = NULL),
                    selectInput("block", "Block", choices = NULL)
                )
              ),
              fluidRow(
                box(title = "Piper Plot", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piper_plot", height = "600px"), type = 6)
                )
              ),
              fluidRow(
                box(title = "Chemical Composition", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("chem_plot", height = "600px"), type = 6)
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    water_sf <- calculate_wqi(input$gpkg_upload$datapath)
    data_storage$csv_data <- st_drop_geometry(water_sf)
    data_storage$sf_data <- water_sf
    
    updateSelectInput(session, "state", choices = unique(water_sf$STATE_UT))
    showNotification("GPKG loaded successfully!", type = "message")
  })
  
  observeEvent(input$state, {
    req(data_storage$sf_data)
    districts <- unique(data_storage$sf_data$DISTRICT[data_storage$sf_data$STATE_UT == input$state])
    updateSelectInput(session, "district", choices = districts)
  })
  
  observeEvent(input$district, {
    req(data_storage$sf_data)
    blocks <- unique(data_storage$sf_data$BLOCK[data_storage$sf_data$DISTRICT == input$district])
    updateSelectInput(session, "block", choices = blocks)
  })
  
  filtered_data <- reactive({
    df <- data_storage$sf_data
    req(input$state, input$district, input$block)
    df[df$STATE_UT == input$state & df$DISTRICT == input$district & df$BLOCK == input$block, ]
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("Rows:", nrow(data_storage$csv_data), "\nColumns:", ncol(data_storage$csv_data))
  })
  
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste0("<b>Total Rows:</b> ", nrow(data_storage$csv_data), "<br><b>Total Columns:</b> ", ncol(data_storage$csv_data)))
  })
  
  output$column_details <- renderDT({
    req(data_storage$csv_data)
    datatable(data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, class),
      Missing = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    ))
  })
  
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(head(data_storage$csv_data, 100))
  })
  
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    num_cols <- sapply(data_storage$csv_data, is.numeric)
    stats <- summary(data_storage$csv_data[, num_cols])
    HTML(paste0("<pre>", capture.output(print(stats)), "</pre>"))
  })
  
  output$piper_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    required_cations <- c("CA", "MG", "Sodium", "K")
    required_anions <- c("CHLORIDE", "SULPHATE", "BICARBONATE")
    
    if (all(required_cations %in% names(df)) && all(required_anions %in% names(df))) {
      cation_data <- df[, required_cations] %>% st_drop_geometry()
      anion_data <- df[, required_anions] %>% st_drop_geometry()
      
      cation_matrix <- as.matrix(cation_data)
      anion_matrix <- as.matrix(anion_data)
      
      piper(cations = cation_matrix, anions = anion_matrix,
            plot.symbols = list(pch = 21, col = "black", bg = "skyblue", cex = 1.2))
    } else {
      plot.new()
      text(0.5, 0.5, "Required ions not available for Piper plot")
    }
  })
  
  output$chem_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    selected <- df[, c("CA", "MG", "Sodium", "K", "CHLORIDE", "SULPHATE", "BICARBONATE")]
    selected_long <- pivot_longer(as.data.frame(selected), everything(), names_to = "Parameter", values_to = "Value")
    ggplot(selected_long, aes(x = Parameter, y = Value, fill = Parameter)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Chemical Composition", y = "Concentration (mg/L)") +
      scale_fill_manual(values = rep("#4FC3F7", length(unique(selected_long$Parameter))))
  })
}

# Run App
shinyApp(ui, server)
