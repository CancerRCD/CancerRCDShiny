# UI Module
mod_search_your_target_ui <- function(id) {
  ns <- NS(id)  # Namespace to avoid ID conflicts
  
  tagList(
    h3("Search Your Gene"),
    textInput(ns("targetFilter"), "Enter Gene:", placeholder = "Type gene name here..."),
    uiOutput(ns("targetStatus"), style = "margin-top: 10px;"),
    DTOutput(ns("resultsTable")),
    downloadButton(ns("downloadData"), "Download Results", style = "margin-top: 20px;")
  )
}

# Server Module
mod_search_your_target_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Check if the manually entered gene exists in the Gene column
    target_exists <- reactive({
      req(dataset(), input$targetFilter != "")
      data <- dataset()
      gene_input <- trimws(toupper(input$targetFilter))  # Standardize input
      target_found <- any(toupper(data$Gene) == gene_input)
      return(target_found)
    })
    
    # Display the status of the target gene
    output$targetStatus <- renderUI({
      if (input$targetFilter == "") {
        return(NULL)
      }
      
      if (!target_exists()) {
        return(HTML("<p><strong style='color: red;'>❌ This gene is not in our database.</strong></p>"))
      } else {
        return(HTML("<p><strong style='color: green;'>✅ This gene is in our database. Go ahead!</strong></p>"))
      }
    })
    
    # Filter Target based on the target gene in the Signature column
    filtered_data <- reactive({
      req(input$targetFilter != "", target_exists())
      target_pattern <- paste0("\\b", trimws(toupper(input$targetFilter)), "\\b")
      filtered <- Target[grepl(target_pattern, toupper(Target$Signature)), ]
      return(filtered)
    })
    
    # Render the results table with the filtered data
    output$resultsTable <- renderDT({
      req(filtered_data())
      
      data <- filtered_data()
      
      # Torna a coluna Nomenclature clicável e realmente copiável
      data$Nomenclature <- sprintf(
        '<span style="cursor: pointer; color: #007bff;" 
            onclick="navigator.clipboard.writeText(`%s`).then(() => { 
            console.log(`Copied: %s`); 
            }).catch(err => console.error(`Copy failed`, err));">%s</span>', 
        data$Nomenclature, data$Nomenclature, data$Nomenclature
      )
      
      datatable(data, 
                escape = FALSE,  # Permite renderização de HTML na coluna Nomenclature
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
    })
    
    # Enable download of the filtered data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("Filtered_Results_", input$targetFilter, ".csv")
      },
      content = function(file) {
        data_to_download <- filtered_data()
        write.csv(data_to_download, file, row.names = FALSE)
      }
    )
    
  })
}
















# # UI Module
# mod_search_your_target_ui <- function(id) {
#   ns <- NS(id)  # Namespace to avoid ID conflicts
#   
#   tagList(
#     h3("Search Your Gene"),
#     textInput(ns("targetFilter"), "Enter Gene:", placeholder = "Type gene name here..."),
#     uiOutput(ns("targetStatus"), style = "margin-top: 10px;"),
#     DTOutput(ns("resultsTable")),
#     downloadButton(ns("downloadData"), "Download Results", style = "margin-top: 20px;")
#   )
# }
# 
# # Server Module
# mod_search_your_target_server <- function(id, dataset) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Check if the manually entered gene exists in the Gene column
#     target_exists <- reactive({
#       req(dataset(), input$targetFilter != "")
#       data <- dataset()
#       gene_input <- trimws(toupper(input$targetFilter))  # Standardize input
#       target_found <- any(toupper(data$Gene) == gene_input)
#       return(target_found)
#     })
#     
#     # Display the status of the target gene
#     output$targetStatus <- renderUI({
#       if (input$targetFilter == "") {
#         return(NULL)
#       }
#       
#       if (!target_exists()) {
#         return(HTML("<p><strong style='color: red;'>❌ This gene is not in our database.</strong></p>"))
#       } else {
#         return(HTML("<p><strong style='color: green;'>✅ This gene is in our database. Go ahead!</strong></p>"))
#       }
#     })
#     
#     # Filter Target based on the target gene in the Signature column
#     filtered_data <- reactive({
#       req(input$targetFilter != "", target_exists())
#       target_pattern <- paste0("\\b", trimws(toupper(input$targetFilter)), "\\b")
#       filtered <- Target[grepl(target_pattern, toupper(Target$Signature)), ]
#       return(filtered)
#     })
#     
#     # Render the results table with the filtered data
#     output$resultsTable <- renderDT({
#       req(filtered_data())
#       datatable(filtered_data(), 
#                 options = list(pageLength = 10, autoWidth = TRUE),
#                 rownames = FALSE)
#     })
#     
#     # Enable download of the filtered data
#     output$downloadData <- downloadHandler(
#       filename = function() {
#         paste0("Filtered_Results_", input$targetFilter, ".csv")
#       },
#       content = function(file) {
#         data_to_download <- filtered_data()
#         write.csv(data_to_download, file, row.names = FALSE)
#       }
#     )
#     
#   })
# }