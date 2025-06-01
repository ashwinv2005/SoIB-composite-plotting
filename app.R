library(shiny)
library(tidyverse)
library(ggpubr)
library(glue)
library(ggrepel)

source("generate_composite_plot.R")

filter_columns <- c("India.Checklist.Common.Name", 
                    "IUCN.Category",
                    "WPA.Schedule",
                    "CMS.Appendix",
                    "Migratory.Status.Within.India",
                    "Habitat.Specialization",
                    "Diet.Guild",
                    "India.Endemic",
                    "Subcontinent.Endemic",
                    "Himalayas.Endemic",
                    "Endemic.Region",
                    "Order",
                    "Family",
                    "Breeding.Activity.Period",
                    "Non.Breeding.Activity.Period",
                    "Restricted.Islands",
                    "CITES.Appendix",
                    "SOIBv2.Long.Term.Status",
                    "SOIBv2.Current.Status",
                    "SOIBv2.Range.Status",
                    "SOIBv2.Priority.Status") 

# UI for the app
ui <- fluidPage(
  titlePanel("State of India's Birds: Trend Plotter"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("trend_type_input", "Trend Type", 
                  choices = c("Long-term" = "LTT", "Current" = "CAT"),
                  selected = "LTT"),
      
      selectInput("group_by_field", "Group by", 
                  choices = filter_columns, #colnames(data_main),
                  selected = "SOIBv2.Range.Status"),
      
      uiOutput("filter_ui"),
      
      verbatimTextOutput("filters_summary")
    ),
    
    mainPanel(
      # Image output for the plot
      imageOutput("trendPlot", height = "800px")
    )
  )
)

# Server logic for the app
server <- function(input, output, session) {
  
  # Dynamically generate the UI for filter inputs based on SoIB_main columns
  output$filter_ui <- renderUI({
    filter_inputs <- lapply(filter_columns, function(col_name) {
      choices <- c("All", sort(unique(data_main[[col_name]])))
      if (is.character(data_main[[col_name]])) {
        selectInput(
          inputId = paste0("filter_", col_name),
          label = paste("Filter by", col_name),
#          choices = c("All", unique(data_main[[col_name]])),
          choices = choices,
          selected = "All",
          multiple = TRUE
        )
      } else {
        NULL
      }
    })
    do.call(tagList, filter_inputs)
  })
  
  
  # Render the plot and return the image as output
  output$trendPlot <- renderImage({
    
    filters <- list()
    for (col in filter_columns) {
      val <- input[[paste0("filter_", col)]]
      if (!is.null(val) && !("All" %in% val)) {
        filters[[col]] <- val
      }
    }
    groupByField <- input$group_by_field
    trendType <- input$trend_type_input
    
    
    # Call the function to generate and save the plot
    image_path <- generate_composite_plot (trendType, filters, groupByField)
    
    # Validate image path
    if (!is.character(image_path) || length(image_path) != 1 || !file.exists(image_path)) {
      showNotification("Image file missing or invalid path returned by plot function.", type = "error")
      return(NULL)
    }
    
    list(
      src = image_path,
      contentType = "image/jpeg", # or "image/png" if that's what you're using
      width = 600,
      height = 450
    )    
  }, deleteFile = TRUE)
  
  # Output filter summary for display
  output$filters_summary <- renderPrint({
    selected_filters <- sapply(colnames(data_main), function(col_name) {
      input[[paste0("filter_", col_name)]]
    })
    selected_filters
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
