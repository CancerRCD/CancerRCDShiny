# module_all_signatures.R

# UI do módulo
mod_all_signatures_ui <- function(id, dataset) {
  ns <- NS(id)  # Namespace to evitar conflitos de ID
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Select Nomenclature"),
        selectInput(ns("signatureFilter"), "Select Nomenclature:", choices = NULL),
        hr(),
        br(), br(),
        downloadButton(ns("downloadData"), "Download Summary Results", icon = icon("download"))
      ),
      mainPanel(
        h3("Integrative Summary of Signature Analyses"),
        uiOutput(ns("summaryText")),  
        h3("Table Results"),
        tableOutput(ns("selectedRowTable"))  
      )
    )
  )
}

# Server do módulo
mod_all_signatures_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    # **Atualizar as opções do selectInput com todas as Signatures disponíveis**
    observe({
      req(dataset())
      data <- dataset()
      
      updateSelectInput(session, "signatureFilter", 
                        choices = unique(na.omit(data$Nomenclature)), 
                        selected = NULL)
    })
    
    # **Dados filtrados apenas pela Nomenclature selecionada**
    filteredData <- reactive({
      req(dataset(), input$signatureFilter)
      data <- dataset()
      filtered <- data[data$Nomenclature == input$signatureFilter, ]
      
      if (nrow(filtered) > 0) {
        return(filtered[1, , drop = FALSE])  
      } else {
        return(NULL)
      }
    })
    
    # **Texto de resumo da Nomenclature selecionada**
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
    
    # **Exibir os dados selecionados na tabela**
    output$selectedRowTable <- renderTable({
      req(filteredData())
      filteredData()
    })
    
    # **Permitir o download dos dados filtrados**
    output$downloadData <- downloadHandler(
      filename = function() { paste0("selected_summary_", Sys.Date(), ".csv") },
      content = function(file) {
        data <- filteredData()
        data <- data %>% 
          mutate(Summary = HTML(paste0("<p>Signature ", data$Nomenclature, " summary...</p>")))
        write.csv(data, file, row.names = FALSE)
      }
    )
    
  })
}
