# Definição da UI
ui <- tagList(
  # Tela de carregamento
  div(id = "loading_screen",
      style = "position: fixed; 
               top: 0; left: 0; 
               width: 100%; height: 100%; 
               background-color: white; 
               z-index: 9999; 
               display: flex; 
               flex-direction: column; 
               justify-content: center; 
               align-items: center;",
      h3("Loading CancerRCDShiny...", style = "color: #2c3e50; font-size: 24px;"),
      tags$style("
        @keyframes progress {
          0% { width: 0%; }
          100% { width: 100%; }
        }
      "),
      div(style = "width: 50%; background-color: #ddd; height: 10px; border-radius: 5px; overflow: hidden;",
          div(style = "height: 100%; width: 0%; background-color: #2c3e50; animation: progress 30s linear;")
      )
  ),
  
  # Script para remover a tela de carregamento após 30 segundos
  tags$script("
    setTimeout(function() {
      document.getElementById('loading_screen').style.display = 'none';
    }, 30000);
  "),
  
  # Interface principal do aplicativo
  navbarPage(
    title = div(
      "CancerRCDShiny: Integrated Multi-Optosis Model for Pan-Cancer Biomarker and Therapy Target Discovery",
    ),
    inverse = TRUE,
    header = tags$style(HTML("
      .navbar { background-color: #2c3e50; }
      .navbar .navbar-brand { color: white; font-size: 18px; }
      .navbar .navbar-nav > li > a { color: white; }
      .navbar .navbar-nav > li > a:hover { background-color: #1a242f; }
      .navbar .navbar-nav .active > a { background-color: #34495e; }
    ")),
    
    tabPanel("Home",
             fluidRow(
               column(12, align = "center",
                      h3("Welcome to CancerRCDShiny", style = "font-size: 32px;"),
                      p("Explore results from Integrated Multi-Optosis Model for Pan-Cancer Biomarker and Therapy Target Discovery.",
                        style = "font-size: 18px;"),
                      img(src = "Figure 3_HRF.png", style = "max-width: 80%; height: auto;")
               )
             ),
             hr(),
             fluidRow(
               column(12, align = "center",
                      tags$a(href = "manual.pdf", "Access the User Manual", target = "_blank")
               )
             )
    ),
    
    tabPanel("RCD information",
             fluidRow(
               column(12, align = "center",
                      div(style = "margin-top: 10px;"),  # Additional CSS-based spacing
                      img(src = "Figure 1_HRF.png", style = "max-width: 90%; height: auto;")
               )
             )
    ),
    
    tabPanel("Search Your Gene", mod_search_your_target_ui("search_your_target")),
    
    tabPanel("Nomenclature Identifier Interpreter", mod_nomenclature_ui("nomenclature")),
    
    navbarMenu("Multi-Omic Pan-Cancer Signatures",
               tabPanel("All Signatures", mod_all_signatures_ui("all_signatures")),
               tabPanel("CNV-Specific Signatures", mod_signature_ui("cnv_signature")),
               tabPanel("mRNA-Specific Signatures", mod_signature_ui("gene_signature")),
               tabPanel("Methylation-Specific Signatures", mod_signature_ui("methylation_signature")),
               tabPanel("Mutation-Specific Signatures", mod_signature_ui("mutation_signature")),
               tabPanel("miRNA-Specific Signatures", mod_signature_ui("mirna_signature")),
               tabPanel("Protein-Specific Signatures", mod_signature_ui("protein_signature")),
               tabPanel("Transcript-Specific Signatures", mod_signature_ui("transcript_signature"))
    ),
    
    navbarMenu("Top Sigantures",
               tabPanel("Omic Layers", mod_the_best_signature_ui("omic_layers")), 
               tabPanel("Clinical Meaningful", mod_the_best_signature_ui("clinical_meaningful")),
               tabPanel("Chimeric Antigen Receptor (CAR) Elements", mod_the_best_signature_ui("car_elements"))
    ),
    
    navbarMenu("Analysis and Plotting",
               tabPanel("Correlation Analysis", mod_correlation_analysis_ui("correlation_analysis")),
               tabPanel("Tumor vs Normal Analysis", mod_tumor_normal_analysis_ui("tumor_normal_analysis")),
               tabPanel("Cox Analysis", mod_cox_analysis_ui("cox_analysis")),
               tabPanel("Survival Analysis", mod_survival_analysis_ui("survival_analysis")),
               tabPanel("Immune Infiltrates Analysis", mod_infiltrates_analysis_ui("infiltrates_analysis"))
    ),
    
    tabPanel("Web Resources", mod_web_resources_ui("Web_resources")),
    tabPanel("Datasets to Download", mod_download_ui("download")),
    tabPanel("Developers", mod_developers_ui("Developers"))
  )
)
