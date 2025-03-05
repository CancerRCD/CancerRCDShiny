# UI Component
mod_tumor_normal_analysis_ui <- function(id) {
  ns <- NS(id)  # Namespace to avoid ID conflicts
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Tumor vs. Normal Analysis"),
        selectInput(ns("target"), "Select Signature:", choices = NULL),
        
        # New output to display selected Omic Feature and CTAB
        uiOutput(ns("selected_info")),  
        
        actionButton(ns("plot_button"), "Generate Plot", icon = icon("chart-line")),
        br(), br(),
        downloadButton(ns("download_plot"), "Download Plot", icon = icon("download"))
      ),
      mainPanel(
        # Envolver o plotOutput() com withSpinner() para mostrar carregamento
        withSpinner(plotOutput(ns("plot"), width = "1200px", height = "800px"), 
                    type = 3, color = "#2c3e50", color.background = "#FFFFFF")  # Tipo do spinner e cor
      )
    )
  )
}

# Server Component
mod_tumor_normal_analysis_server <- function(id, Target) {  
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
      return(mapping[[omic_layer]])  # Return mapped value
    }
    
    # Populate dropdowns with available options
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
      req(input$target)
      
      data <- Target()
      
      # Automatically retrieve 'Omic feature' and CTAB based on the selected Signature
      selected_data <- data[data$Signature == input$target, ]
      
      list(
        Gene = input$target, 
        Cancer = selected_data$CTAB[1],  # Automatically set Cancer Type
        data_type = map_omic_layer(selected_data$'Omic feature'[1])  # Automatically set Omic Layer
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
    
    # Reactive function to generate plot
    generate_plot <- reactive({
      req(selected_inputs())
      
      vis_toil_TvsN_cancer(
        Gene = selected_inputs()$Gene,  # FIXED: Using correct input
        Mode = "Violinplot",
        data_type = selected_inputs()$data_type,
        Show.P.value = TRUE,
        Show.P.label = TRUE,
        Method = "wilcox.test",
        values = c("#DF2020", "#DDDF21"),
        TCGA.only = FALSE,
        Cancer = selected_inputs()$Cancer
      )
    })
    
    # Render the tumor vs. normal plot
    output$plot <- renderPlot({
      generate_plot()
    }, width = 800, height = 800)  # Ensure correct rendering size
    
    
    # Download functionality for the generated plot
    output$download_plot <- downloadHandler(
      filename = function() {
        req(input$target, input$omic_layer)  # Ensure values are available
        paste0(
          "tumor_normal_plot_", 
          input$target, "_", 
          input$omic_layer, "_", 
          Sys.Date(), ".png"
        ) %>% gsub(" ", "_", .)  # Replace spaces with underscores for safety
      },
      content = function(file) {
        png(file, width = 8, height = 8, units = "in", res = 300)  # High-resolution output
        print(generate_plot())  # Save the plot
        dev.off()
      }
    )
  })
}