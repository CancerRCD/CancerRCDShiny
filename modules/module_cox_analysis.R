# UI Component
mod_cox_analysis_ui <- function(id) {
  ns <- NS(id)  # Namespace to avoid ID conflicts
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Cox Analysis"),
        selectInput(ns("target"), "Select Signature:", choices = NULL),
        selectInput(ns("survival_measure"), "Select Survival Measure:",
                    choices = c("Overall Survival" = "OS",
                                "Disease Specific Survival" = "DSS",
                                "Disease Free Interval" = "DFI",
                                "Progression Free Interval" = "PFI")),
        
        # New output to display selected Omic Feature and CTAB
        uiOutput(ns("selected_info")),  
        
        actionButton(ns("plot_button"), "Generate Plot", icon = icon("chart-line")),
        br(), br(),
        downloadButton(ns("download_plot"), "Download Plot", icon = icon("download"))
      ),
      mainPanel(
        # Envolver o plotOutput() com withSpinner() para mostrar carregamento
        withSpinner(plotOutput(ns("plot"), width = "800px", height = "800px"), 
                    type = 3, color = "#2c3e50", color.background = "#FFFFFF")  # Tipo do spinner e cor
      )
    )
  )
}

# Server Component
mod_cox_analysis_server <- function(id, Target) {  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    # Mapping function for Omic_Layer
    map_omic_layer <- function(omic_layer) {
      mapping <- list(
        "mRNA" = "mRNA",
        "Transcript" = "transcript",
        "Protein" = "protein",
        "Mutation" = "mutation",
        "CNV" = "cnv",
        "Methylation" = "methylation",
        "miRNA" = "miRNA"
      )
      
      if (!is.null(omic_layer) && omic_layer %in% names(mapping)) {
        return(mapping[[omic_layer]])  # Return mapped value
      } else {
        return("unknown")  # Default fallback
      }
    }
    
    # Populate the Signature dropdown with available options
    observe({
      req(Target())  # Ensure data is available
      data <- Target()
      
      # Create a named vector: names = Nomenclature, values = Signatures
      choices_named <- setNames(data$Signature, data$Nomenclature)
      
      # Update SelectInput to display Nomenclature but store Signature
      updateSelectInput(session, "target", choices = choices_named)
    })
    
    # Reactive expression to store user-selected parameters
    selected_inputs <- eventReactive(input$plot_button, {
      req(input$target, input$survival_measure)  # Ensure inputs are selected
      
      data <- Target()
      selected_signature <- input$target
      
      # Automatically retrieve the corresponding 'Omic feature'
      omic_layer <- data %>%
        filter(Signature == selected_signature) %>%
        pull('Omic feature') %>%
        unique() %>%
        .[1]  # Get the first unique value
      
      list(
        Gene = selected_signature, 
        data_type = map_omic_layer(omic_layer),
        measure = input$survival_measure
      )
    })
    
    # **New Output: Display Selected Omic Feature and CTAB**
    output$selected_info <- renderUI({
      req(input$target)
      
      data <- Target()
      selected_data <- data[data$Signature == input$target, ]
      
      if (nrow(selected_data) > 0) {
        omic_feature <- selected_data$'Omic feature'[1]
        
        HTML(paste0(
          "<p><strong>Omic Feature:</strong> ", omic_feature, "</p>"
        ))
      } else {
        HTML("<p style='color: red;'><strong>No data found for this signature.</strong></p>")
      }
    })
    
    # Function to generate the plot
    generate_plot <- reactive({
      req(selected_inputs())  # Ensure inputs are available
      
      vis_unicox_tree(
        Gene = selected_inputs()$Gene,
        measure = selected_inputs()$measure,
        data_type = selected_inputs()$data_type,
        threshold = 0.5,
        values = c("grey", "#E31A1C", "#377DB8")
      )
    })
    
    # Render the Cox plot
    output$plot <- renderPlot({
      generate_plot()
    }, width = 1200, height = 1200)  # Ensure rendering matches desired size
  
    # Download functionality for the generated plot
  output$download_plot <- downloadHandler(
    filename = function() {
      # Retrieve user selections
      target <- input$target  # Example: "P62LCKLIGAND"
      omic_layer <- input$omic_layer  # Example: "Protein"
      measure_type <- input$survival_measure  # Example: "OS"
      
      # Mapping correlation type for readability
      measure_map <- list(
        "OS" = "Overall Survival",
        "DSS" = "Disease Specific Survival",
        "DFI" = "Disease Free Interval",
        "PFI" = "Progression Free Interval"
      )
      
      # Convert correlation type to readable format
      measure_name <- measure_map[[measure_type]]
      
      # Construct a descriptive filename
      filename <- paste(
        "Cox_analysis_plot",
        target,
        omic_layer,
        measure_name,
        Sys.Date(),
        sep = "_"
      )
      
      # Ensure the filename is safe (remove spaces or special characters)
      filename <- gsub("[^A-Za-z0-9_]", "", filename)
      
      # Append the file extension
      paste0(filename, ".png")
    },
    content = function(file) {
      plot <- generate_plot()
      if (!is.null(plot)) {
        png(file, width = 1200, height = 1200)  # Adjust size
        print(plot)  # Save the radar plot
        dev.off()
      } else {
        showNotification("Não há gráfico para salvar.", type = "error")
      }
    }
  )
})
}

