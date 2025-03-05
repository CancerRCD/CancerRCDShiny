# mod_correlation_analysis.R

# UI Component
mod_correlation_analysis_ui <- function(id) {
  ns <- NS(id)  # Namespace para evitar conflitos de ID
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Correlation Analysis"),
        selectInput(ns("target"), "Select Signature:", choices = NULL),
        
        # New output to display selected Omic Feature and CTAB
        uiOutput(ns("selected_info")),  
        
        actionButton(ns("plot_button"), "Generate Plot", icon = icon("chart-line")),
        br(), br(),
        downloadButton(ns("download_plot"), "Download Plot", icon = icon("download"))
      ),
      mainPanel(
        # Envolver o plotOutput() com withSpinner() para mostrar carregamento
        withSpinner(plotOutput(ns("plot")), 
                    type = 3, color = "#2c3e50", color.background = "#FFFFFF")  # Tipo do spinner e cor
      )
    )
  )
}

# Server Component
mod_correlation_analysis_server <- function(id, Target) {  
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
    
    # Mapping function for correlation type based on Phenotype
    map_correlation_type <- function(phenotype) {
      mapping <- list(
        "MSI" = "msi",
        "TMB" = "tmb",
        "TSM" = "stemness"
      )
      return(mapping[[phenotype]])
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
    
    # Reactive function to get the corresponding mapped Omic Layer
    selected_omic_layer <- reactive({
      req(input$target, Target())  # Ensure inputs exist
      data <- Target()  # Call the reactive function properly
      
      # Find the 'Omic feature' (Omic Layer) that matches the selected Signature
      raw_omic_layer <- data$'Omic feature'[data$Signature == input$target]
      
      # If multiple matches, take the first
      if (length(raw_omic_layer) > 1) raw_omic_layer <- raw_omic_layer[1]
      
      # Apply the mapping function to get the correct mapped value
      mapped_omic_layer <- map_omic_layer(raw_omic_layer)
      
      return(mapped_omic_layer)
    })
    
    # Reactive function to determine correlation type based on Phenotype
    selected_correlation_type <- reactive({
      req(input$target, Target())
      data <- Target()
      phenotype <- data$Phenotype[data$Signature == input$target]
      if (length(phenotype) > 1) phenotype <- phenotype[1]
      return(map_correlation_type(phenotype))
    })
    
    # Reactive expression to store selected parameters
    selected_inputs <- eventReactive(input$plot_button, {
      req(input$target)
      list(
        Gene = input$target,
        data_type = selected_omic_layer(),
        correlation_type = selected_correlation_type()
      )
    })
    
    # **New Output: Display Selected Omic Feature and CTAB**
    output$selected_info <- renderUI({
      req(input$target)
      
      data <- Target()
      selected_data <- data[data$Signature == input$target, ]
      
      if (nrow(selected_data) > 0) {
        omic_feature <- selected_data$'Omic feature'[1]
        correlation_value <- selected_data$Phenotype[1]
        
        HTML(paste0(
          "<p><strong>Omic Feature:</strong> ", omic_feature, "</p>",
          "<p><strong>Correlation Analysis:</strong> ", correlation_value, "</p>"
        ))
      } else {
        HTML("<p style='color: red;'><strong>No data found for this signature.</strong></p>")
      }
    })
    
    
    # Reactive function to generate plot based on selection
    generate_plot <- reactive({
      req(selected_inputs())
      inputs <- selected_inputs()
      
      # Determine which correlation function to call
      p <- switch(inputs$correlation_type,
                  "msi" = vis_gene_msi_cor(Gene = inputs$Gene, cor_method = "spearman",
                                           data_type = inputs$data_type, Plot = TRUE),
                  "tmb" = vis_gene_tmb_cor(Gene = inputs$Gene, cor_method = "spearman",
                                           data_type = inputs$data_type, Plot = TRUE),
                  "stemness" = vis_gene_stemness_cor(Gene = inputs$Gene, cor_method = "spearman",
                                                     data_type = inputs$data_type, Plot = TRUE),
                  NULL)  # Se a opção não existir, retorna NULL
      
      # Verifica se p retornou dados
      if (is.null(p) || is.null(p$data) || nrow(p$data) == 0) {
        showNotification("Nenhum dado disponível para este gene e tipo de correlação.", type = "warning")
        return(NULL)
      }
      
      # Processar os dados de correlação
      pdata <- p$data %>%
        dplyr::mutate(cor = round(cor, digits = 3), p.value = round(p.value, digits = 3))
      
      # Transformar para formato wide
      df <- pdata %>%
        dplyr::select(cor, cancer) %>%
        tidyr::pivot_wider(names_from = cancer, values_from = cor)
      
      # Adicionar a assinatura gênica
      df$gene <- "Gene signature"
      df <- df %>%
        dplyr::relocate(gene, .before = everything())  # Garante que 'gene' seja a primeira coluna
      
      # **Solução: Remover colunas com NAs**
      df_clean <- df %>%
        dplyr::select(where(~ all(!is.na(.))))  # Remove colunas com pelo menos um NA
      
      # Verificar se há dados suficientes para plotar
      if (ncol(df_clean) < 3) {
        showNotification("Não há dados suficientes para gerar o radar plot. Tente outro gene ou tipo de correlação.", type = "error")
        return(NULL)
      }
      
      # Gerando o radar plot
      radar_plot <- ggradar::ggradar(
        df_clean[1, , drop = FALSE],  # Mantém o formato de dataframe
        font.radar = "sans",
        values.radar = c("-1", "0", "1"),
        grid.min = -1, grid.mid = 0, grid.max = 1,
        # Configuração do fundo e grid
        background.circle.colour = "white",
        gridline.mid.colour = "grey",
        # Polígonos
        group.line.width = 1,
        group.point.size = 3,
        group.colours = "#00AFBB"
      ) + theme(plot.title = element_text(hjust = .5))
      
      return(radar_plot)
    })
    
    # Renderiza o radar plot
    output$plot <- renderPlot({
      plot <- generate_plot()
      if (!is.null(plot)) print(plot)  # Evita erro caso seja NULL
    })
    
    # Download do radar plot com nome dinâmico
    output$download_plot <- downloadHandler(
      filename = function() {
        # Retrieve user selections
        target <- input$target  # Example: "P62LCKLIGAND"
        omic_layer <- input$omic_layer  # Example: "Protein"
        correlation_type <- input$correlation_type  # Example: "msi"
        
        # Mapping correlation type for readability
        correlation_map <- list(
          "msi" = "Microsatellite_Instability",
          "tmb" = "Tumor_Mutational_Burden",
          "stemness" = "Stemness"
        )
        
        # Convert correlation type to readable format
        correlation_name <- correlation_map[[correlation_type]]
        
        # Construct a descriptive filename
        filename <- paste(
          "Correlation_radar_plot",
          target,
          omic_layer,
          correlation_name,
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

