# module_the_best_signature.R

# UI do módulo
mod_the_best_signature_ui <- function(id, dataset) {
  ns <- NS(id)  # Namespace para evitar conflitos de ID
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Step-by-Step Filtering"),
        selectInput(ns("omicLayerFilter"), "1. Select Omic Layer:", choices = NULL),  # Agora é dinâmico
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
        h3("Table Results"),
        tableOutput(ns("selectedRowTable"))  
      )
    )
  )
}

# Server do módulo
mod_the_best_signature_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    # **Função auxiliar para atualizar os filtros**
    update_filter <- function(input_id, choices) {
      if (length(choices) == 0) {
        choices <- "No results available"
      }
      updateSelectInput(session, input_id, choices = choices, selected = NULL)
    }
    
    # **Inicializa o filtro Omic Layer de forma dinâmica**
    observe({
      req(dataset())
      data <- dataset()
      
      # Atualiza o filtro 'Omic feature' com todas as opções disponíveis
      update_filter("omicLayerFilter", unique(na.omit(data$'Omic feature')))
      
      # Atualiza os outros filtros com valores nulos até que uma escolha seja feita
      update_filter("cancerFilter", NULL)
      update_filter("RCDFilter", NULL)
      update_filter("signatureFilter", NULL)
    })
    
    # **Atualiza o filtro Cancer Type baseado no Omic Layer selecionado**
    observeEvent(input$omicLayerFilter, {
      req(dataset(), input$omicLayerFilter)
      data <- dataset()
      filtered <- data[data$'Omic feature' == input$omicLayerFilter, ]
      
      # Atualiza o filtro Cancer Type com base no Omic Layer selecionado
      update_filter("cancerFilter", unique(na.omit(filtered$CTAB)))
      update_filter("RCDFilter", NULL)
      update_filter("signatureFilter", NULL)
    })
    
    # **Atualiza o filtro RCD Types baseado no Cancer Type selecionado**
    observeEvent(input$cancerFilter, {
      req(dataset(), input$omicLayerFilter, input$cancerFilter)
      data <- dataset()
      filtered <- data[data$'Omic feature' == input$omicLayerFilter &
                         data$CTAB == input$cancerFilter, ]
      
      # Atualiza o filtro RCD Types com base no Cancer Type selecionado
      update_filter("RCDFilter", unique(na.omit(filtered$'RCD form')))
      update_filter("signatureFilter", NULL)
    })
    
    # **Atualiza o filtro Signature baseado no RCD Type selecionado**
    observeEvent(input$RCDFilter, {
      req(dataset(), input$omicLayerFilter, input$cancerFilter, input$RCDFilter)
      data <- dataset()
      filtered <- data[data$'Omic feature' == input$omicLayerFilter &
                         data$CTAB == input$cancerFilter &
                         data$'RCD form' == input$RCDFilter, ]
      
      # Atualiza o filtro Signature com base no RCD Type selecionado
      update_filter("signatureFilter", unique(na.omit(filtered$Signature)))
    })
    
    # **Filtra os dados com base em todos os inputs**
    filteredData <- reactive({
      req(dataset(), input$omicLayerFilter, input$cancerFilter, input$RCDFilter, input$signatureFilter)
      data <- dataset()
      filtered <- data[data$'Omic feature' == input$omicLayerFilter &
                         data$CTAB == input$cancerFilter & 
                         data$'RCD form' == input$RCDFilter & 
                         data$Signature == input$signatureFilter, ]
      
      if (nrow(filtered) > 0) {
        return(filtered[1, , drop = FALSE])  
      } else {
        return(NULL)
      }
    })
    
    
    # **Exibe o resumo em HTML**
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
        " (p = ", row[["p.value_Cox_OS"]], ") in Overall Survival (OS)..."
      ))
    })
    
    # **Exibe os dados filtrados na tabela**
    output$selectedRowTable <- renderTable({
      req(filteredData())
      filteredData()
    })
    
    # **Download dos dados filtrados**
    output$downloadData <- downloadHandler(
      filename = function() { paste0("selected_summary_", Sys.Date(), ".csv") },
      content = function(file) {
        data <- filteredData()
        data <- data %>% 
          mutate(Summary = HTML(paste0("<p>Signature ", data$Nomenclature, " summary...</p>")))
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # **Reset de todos os filtros**
    observeEvent(input$resetFilters, {
      updateSelectInput(session, "omicLayerFilter", choices = unique(na.omit(dataset()$'Omic feature')), selected = NULL)
      updateSelectInput(session, "cancerFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "RCDFilter", choices = NULL, selected = NULL)
      updateSelectInput(session, "signatureFilter", choices = NULL, selected = NULL)
    })
  })
}
