library(shiny)
library(DT)
library(dplyr)

# Sample data frame to mimic inventory
initial_data <- data.frame(
  ItemID = 1:5,
  ItemName = c("Milk", "Cheese", "Butter", "Curd", "Paneer"),
  Category = c("Dairy", "Dairy", "Dairy", "Dairy", "Dairy"),
  Quantity = c(100, 50, 30, 60, 40),
  Unit = c("Litres", "Kg", "Kg", "Litres", "Kg"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Inventory Management System - MBA Food Business"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("itemName", "Item Name"),
      textInput("category", "Category"),
      numericInput("quantity", "Quantity", value = 1, min = 1),
      textInput("unit", "Unit (Kg/Litres/etc)"),
      actionButton("addItem", "Add Item", class = "btn-primary"),
      hr(),
      h4("Inventory Summary"),
      verbatimTextOutput("summary")
    ),
    
    mainPanel(
      h3("Current Inventory"),
      DTOutput("inventoryTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store inventory
  inventory <- reactiveVal(initial_data)
  
  # Add new item
  observeEvent(input$addItem, {
    new_id <- ifelse(nrow(inventory()) == 0, 1, max(inventory()$ItemID) + 1)
    new_item <- data.frame(
      ItemID = new_id,
      ItemName = input$itemName,
      Category = input$category,
      Quantity = input$quantity,
      Unit = input$unit,
      stringsAsFactors = FALSE
    )
    
    inventory(rbind(inventory(), new_item))
  })
  
  # Render Inventory Table
  output$inventoryTable <- renderDT({
    datatable(
      inventory(),
      editable = TRUE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  }, server = TRUE)
  
  # Update inventory on edit
  observeEvent(input$inventoryTable_cell_edit, {
    info <- input$inventoryTable_cell_edit
    updated <- inventory()
    updated[info$row, info$col] <- info$value
    inventory(updated)
  })
  
  # Display Summary
  output$summary <- renderPrint({
    inv <- inventory()
    total_items <- nrow(inv)
    total_quantity <- sum(as.numeric(inv$Quantity), na.rm = TRUE)
    low_stock <- inv %>% filter(Quantity < 20)
    
    cat("Total Unique Items:", total_items, "\n")
    cat("Total Quantity in Stock:", total_quantity, "\n")
    if (nrow(low_stock) > 0) {
      cat("\n⚠️ Items Low in Stock:\n")
      print(low_stock[, c("ItemName", "Quantity", "Unit")])
    } else {
      cat("\n✅ All items are sufficiently stocked.")
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)

