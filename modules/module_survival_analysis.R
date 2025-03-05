# mod_survival_analysis.R

library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(cowplot)

# UI Component
mod_survival_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Survival Analysis"),
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
        withSpinner(plotOutput(ns("plot"), width = "1200px", height = "800px"), 
                    type = 3, color = "#2c3e50", color.background = "#FFFFFF")  # Tipo do spinner e cor
      )
    )
  )
}

# Server Component
mod_survival_analysis_server <- function(id, Target) {  
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
      req(input$target, input$survival_measure)
      
      data <- Target()
      
      # Automatically retrieve 'Omic feature' and CTAB based on the selected Signature
      selected_data <- data[data$Signature == input$target, ]
      
      list(
        item = input$target, 
        TCGA_cohort = selected_data$CTAB[1],  # Automatically set Cancer Type
        profile = map_omic_layer(selected_data$'Omic feature'[1]),  # Automatically set Omic Layer
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
    
    # Reactive function to get survival data
    get_survival_data <- reactive({
      req(selected_inputs())  # Ensure inputs are available
      
      print("Fetching survival data...")  # Debugging step
      
      data <- tcga_surv_get(
        selected_inputs()$item,
        TCGA_cohort = selected_inputs()$TCGA_cohort,
        profile = selected_inputs()$profile,
        TCGA_cli_data = dplyr::full_join(
          load_data("tcga_clinical"), 
          load_data("tcga_surv"),
          by = "sample"
        )
      )
      
      print("Survival data retrieved:")
      print(head(data))  # Print first rows
      
      if (nrow(data) == 0) {
        stop("No survival data found for selected genes and cohort.")
      }
      
      return(data)
    })
  
    generate_plot <- reactive({
      req(get_survival_data())  # Ensure data is available
      data <- get_survival_data()  # Retrieve data
      inputs <- selected_inputs()  # Retrieve selected user inputs
      
      print(paste("Inputs received - Measure:", inputs$measure))
      
      # Ensure `inputs$measure` is valid
      if (is.null(inputs$measure) || !inputs$measure %in% c("OS", "DSS", "DFI", "PFI")) {
        stop("Error: Invalid or NULL survival measure selected.")
      }
      
      # Ensure `data` is properly loaded
      if (is.null(data) || nrow(data) == 0) {
        stop("Error: Data is NULL or empty before creating `surv_object`.")
      }
      
      print("Data structure before creating `surv_object`:")
      print(head(data))  
      print(colnames(data))  # Print all column names for verification
      
      # **Ensure `value_group` exists before `survfit()`**
      if ("value_group" %in% colnames(data)) {
        print("Column `value_group` already exists in `data`.")
      } else {
        print("Creating `value_group` column...")
        
        # Categorization logic
        if (inputs$profile == "mutation") {
          data$value_group <- ifelse(data$value == 0, "WT", "MT")
        } else if (inputs$profile %in% c("mRNA", "miRNA", "protein", "transcript", "methylation")) {
          data$value_group <- ifelse(data$value > median(data$value, na.rm = TRUE), "High", "Low")
        } else if (inputs$profile == "cnv") {
          data$value_group <- ifelse(data$value == 0, "Normal",
                                     ifelse(data$value < 0, "Deleted", "Duplicated"))
        } else {
          stop("Unknown profile type: ", inputs$profile)
        }
      }
      
      print("Unique values in `value_group` column:")
      print(unique(data$value_group))
      
      if (!"value_group" %in% colnames(data)) {
        stop("Error: `value_group` column is missing after creation step.")
      }
      
      if (length(unique(data$value_group)) < 2) {
        stop("Error: Survival analysis requires at least two groups in `value_group` (e.g., High/Low).")
      }
      
      # Create survival object
      surv_object <- tryCatch({
        Surv(time = data[[paste0(inputs$measure, ".time")]], event = data[[inputs$measure]])
      }, error = function(e) {
        stop("Error in creating `surv_object`: ", e$message)
      })
      
      print("Survival object created successfully!")  
      
      # Ensure `surv_object` is stored inside the dataset before `survfit()`
      data$surv_object <- surv_object
      
      # Fit the Kaplan-Meier survival model
      fit <- tryCatch({
        survfit(surv_object ~ value_group, data = data)
      }, error = function(e) {
        stop("Error in survfit: ", e$message)
      })
      
      print("Survfit object created successfully!")
      print(fit)  # Debugging: Print the fit object structure
      
      # Ensure `fit` is a valid `survfit` object
      if (!inherits(fit, "survfit")) {
        stop("Error: `fit` is not a valid survfit object before passing to ggsurvplot().")
      }
      
      # Debugging: Verify if `surv_object` is still accessible before plotting
      print("Verifying if `surv_object` exists before plotting...")
      if (!exists("surv_object")) {
        stop("Error: `surv_object` is missing before plotting!")
      }
      
      # Log-rank test for statistical significance
      surv_diff <- tryCatch({
        survdiff(surv_object ~ value_group, data = data)
      }, error = function(e) {
        warning("Error in log-rank test:", e$message)
        return(NULL)
      })
      
      # Extract p-value from the log-rank test
      p_val <- if (!is.null(surv_diff)) {
        1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
      } else {
        NA
      }
      
      print(paste("Log-rank test p-value:", p_val))
      
      # Define title based on selected survival measure
      title_text <- switch(inputs$measure,
                           "OS" = "Overall Survival",
                           "DSS" = "Disease Specific Survival",
                           "DFI" = "Disease Free Interval",
                           "PFI" = "Progression Free Interval",
                           "Overall Survival"
      )
      
      print("Generating ggsurvplot...")
      
      # Debugging: Check strata levels in `fit`
      print("Strata levels in fit object:")
      print(levels(summary(fit)$strata))
      
      # Explicitly isolate `fit` and `data` to prevent Shiny scoping issues
      fit_for_plot <- isolate(fit)
      data_for_plot <- isolate(data)
      
      print("Fit object before ggsurvplot:")
      print(fit_for_plot)
    
      # Generate Kaplan-Meier survival plot
      plot_km <- tryCatch({
        ggsurvplot(
          fit = fit_for_plot,  
          data = data_for_plot,
          conf.int = TRUE,  
          conf.int.alpha = 0.1,  
          conf.int.style = "step",  
          risk.table = TRUE,
          risk.table.col = "strata",  # Cor da tabela de risco por grupo
          risk.table.y.text = FALSE,  # Barras ao invés de nomes na tabela de risco
          surv.plot.height = 0.7,  # Ajustar altura do gráfico principal
          risk.table.height = 0.25,  # Explicit height to prevent layout issues
          ncensor.plot = TRUE,  
          ncensor.plot.height = 0.15,  
          ggtheme = theme_minimal(),  
          palette = c("#E69F00", "#56B4E9"),  
          linetype = 1,  
          surv.median.line = "hv",  
          pval = paste0("p = ", signif(p_val, 3)),  
          pval.coord = c(90, 0.2)  
        )
      }, error = function(e) {
        stop("Error in ggsurvplot: ", e$message)
      })
      
      print("ggsurvplot created successfully!")
      
      # Apply additional plot formatting
      plot_km$plot <- plot_km$plot + 
        theme(panel.grid = element_blank()) +  
        geom_hline(yintercept = 0, color = "black", size = 0.5) +  
        geom_vline(xintercept = 0, color = "black", size = 0.5) +
        labs(title = title_text) +  
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 25, vjust = 0.1))
      
      return(plot_km$plot)
    })
    
    # Render the survival plot
    output$plot <- renderPlot({
      generate_plot()
    }, width = 1200, height = 800)  # Ensure rendering matches desired size
    
    # Download functionality for the generated plot
    output$download_plot <- downloadHandler(
      filename = function() {
        target <- input$target  
        omic_layer <- input$omic_layer  
        measure_type <- input$survival_measure  
        
        measure_map <- list(
          "OS" = "Overall Survival",
          "DSS" = "Disease Specific Survival",
          "DFI" = "Disease Free Interval",
          "PFI" = "Progression Free Interval"
        )
        
        measure_name <- measure_map[[measure_type]]
        
        filename <- paste(
          "Survival_analysis_plot",
          target,
          omic_layer,
          measure_name,
          Sys.Date(),
          sep = "_"
        )
        
        filename <- gsub("[^A-Za-z0-9_]", "", filename)
        paste0(filename, ".png")
      },
      content = function(file) {
        plot <- generate_plot()
        if (!is.null(plot)) {
          png(file, width = 1200, height = 800)  
          print(plot)  
          dev.off()
        } else {
          showNotification("No plot available for download.", type = "error")
        }
      }
    )
  })
}
