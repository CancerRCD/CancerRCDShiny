# module_signature.R

# UI do módulo
mod_signature_ui <- function(id, signature_name) {
  ns <- NS(id)  # Namespace to prevent ID conflicts
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Step-by-Step Filtering"),
        uiOutput(ns("omicLayerFilterUI")),  # Omic Layer é fixo, mas dinâmico
        selectInput(ns("cancerFilter"), "2. Select Cancer Type:", choices = NULL),
        selectInput(ns("RCDFilter"), "3. Select RCD Types:", choices = NULL),
        selectInput(ns("signatureFilter"), "4. Select Signature:", choices = NULL),
        hr(),
        actionButton(ns("resetFilters"), "Reset Filters", icon = icon("redo")),
        br(), br(),
        downloadButton(ns("downloadData"), "Download Summary Results", icon = icon("download"))
      ),
      mainPanel(
        h3("Integrative Summary of Signature Analyses"),
        uiOutput(ns("summaryText")),  
        h3("Table results"),
        tableOutput(ns("selectedRowTable"))  
      )
    )
  )
}

# Server do módulo atualizado
mod_signature_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    # **Helper function to update filters with "No results available" logic**
    update_filter <- function(input_id, choices) {
      if (length(choices) == 0) {
        choices <- "No results available"
      }
      updateSelectInput(session, input_id, choices = choices, selected = NULL)
    }
    
    # **Dynamic Omic Layer Filter (fixed but dynamic)**
    output$omicLayerFilterUI <- renderUI({
      req(dataset())
      data <- dataset()
      omic_layer_value <- unique(na.omit(data$'Omic feature'))[1]  # Pega o primeiro valor único da coluna 'Omic feature'
      selectInput(ns("omicLayerFilter"), "1. Select Omic Layer:", choices = omic_layer_value, selected = omic_layer_value)
    })
    
    # **Initialize Cancer Type Filter**
    observe({
      req(dataset())
      data <- dataset()
      update_filter("cancerFilter", unique(na.omit(data$CTAB)))
    })
    
    # **Update RCD_types when Cancer Type is selected**
    observeEvent(input$cancerFilter, {
      req(dataset(), input$cancerFilter)
      data <- dataset()
      filtered <- data[data$CTAB == input$cancerFilter, ]
      
      update_filter("RCDFilter", unique(na.omit(filtered$'RCD form')))
      update_filter("signatureFilter", NULL)
    })
    
    # **Update Signature when RCD_types is selected**
    observeEvent(input$RCDFilter, {
      req(dataset(), input$cancerFilter, input$RCDFilter)
      data <- dataset()
      filtered <- data[data$CTAB == input$cancerFilter & 
                         data$'RCD form' == input$RCDFilter, ]
      
      update_filter("signatureFilter", unique(na.omit(filtered$Signature)))
    })
    
    # **Filtered Data**
    filteredData <- reactive({
      req(dataset(), input$cancerFilter, input$RCDFilter, input$signatureFilter)
      data <- dataset()
      filtered <- data[data$CTAB == input$cancerFilter & 
                         data$'RCD form' == input$RCDFilter & 
                         data$Signature == input$signatureFilter, ]
      
      if (nrow(filtered) > 0) {
        return(filtered[1, , drop = FALSE])  
      } else {
        return(NULL)
      }
    })
    
    # Display Summary without Bold Formatting
    output$summaryText <- renderUI({
      req(filteredData())
      row <- filteredData()
      HTML(paste0(
        "<p>Signature ", row[["Nomenclature"]], " is a ", row[["Molecular_Class"]], 
        " that interacts with ", row[["Interactions"]], ", playing a role in ", row[["RCD_types"]], ". ",
        
        "In ", row[["CTAB"]], ", the omic layer ", row[["'Omic feature'"]], 
        " of this signature is correlated with ", row[["Phenotype"]],
        " (rho = ", row[["Correlation_rho"]], ", p.adj = ", row[["Correlation_P_Adj"]], "). ",
        
        "Cox regression analysis indicates that ", row[["'Omic feature'"]], " is ", row[["Type_Cox_OS"]], 
        " (p = ", row[["p.value_Cox_OS"]], ") in Overall Survival (OS), ", row[["Type_Cox_DSS"]], 
        " (p = ", row[["p.value_Cox_DSS"]], ") in Disease-Specific Survival (DSS), ", row[["Type_Cox_DFI"]], 
        " (p = ", row[["p.value_Cox_DFI"]], ") in Disease-Free Interval (DFI), and ", row[["Type_Cox_PFI"]], 
        " (p = ", row[["p.value_Cox_PFI"]], ") in Progression-Free Interval (PFI) in ", row[["CTAB"]], " patients. ",
        
        "Additionally, survival analysis by patient groups shows that in OS, DSS, DFI, and PFI, the worst prognosis is ", 
        row[["Type_log_rank_OS"]], " (p = ", row[["p.value_log_rank_OS"]], "), ", 
        row[["Type_log_rank_DSS"]], " (p = ", row[["p.value_log_rank_DSS"]], "), ", 
        row[["Type_log_rank_DFI"]], " (p = ", row[["p.value_log_rank_DFI"]], "), and ", 
        row[["Type_log_rank_PFI"]], " (p = ", row[["p.value_log_rank_PFI"]], "), respectively. ",
        
        "Regarding the tumor microenvironment, this signature is associated with a ", 
        row[["microenvironment_classification"]], " profile and an immune phenotype classified as ", 
        row[["immune_classification"]], ".</p>", 
        
        "<p>For detailed statistical results, click 'Download'. To visualize the results, copy the signature name, open the 'Analysis and Plotting' dropdown menu, and select the desired analysis.</p>"
      ))
    })
    
    # Display filtered data in table
    output$selectedRowTable <- renderTable({
      req(filteredData())
      filteredData()
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
      filename = function() { paste0("selected_summary_", Sys.Date(), ".csv") },
      content = function(file) {
        data <- filteredData()
        data <- data %>% 
          mutate(Summary = HTML(paste0("<p>Signature ", data$Nomenclature, " summary...</p>")))
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # **Reset filters properly**
    observeEvent(input$resetFilters, {
      updateSelectInput(session, "cancerFilter", choices = unique(na.omit(dataset()$CTAB)), selected = NULL)
      updateSelectInput(session, "RCDFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "signatureFilter", choices = NULL, selected = NULL)
    })
  })
}