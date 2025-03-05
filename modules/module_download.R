mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "margin-top: 50px; text-align: left;",
        h3("Datasets available for download"),
        p("Click the buttons below to download."),
        
        # Download buttons for each dataset with file format indicated
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1"), "Download Dataset_S1"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_full"), "Download Dataset_S2"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_mRNA"), "Download Dataset_S3_mRNA"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_Transcript_signatures"), "Download Dataset_S4_Transcript_signatures"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_miRNA"), "Download Dataset_S5_miRNA"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_Methylation"), "Download Dataset_S6_Methylation"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_Protein"), "Download Dataset_S7_Protein"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_Mutation"), "Download Dataset_S8_Mutation"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S1_CNV"), "Download Dataset_S9_CNV"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S10_CAR"), "Download Dataset_S10_CAR"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S11_omic_layers"), "Download Dataset_S11_omic_layers"))),
        fluidRow(column(12, align = "left", downloadButton(ns("Dataset_S12_meaningful"), "Download Dataset_S12_meaningful")))
    )
  )
}

mod_download_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    datasets <- list(
      "Dataset_S1" = "Dataset S1.xlsx",
      "Dataset_S1_full" = "Dataset_S2.tsv",
      "Dataset_S1_CNV" = "Dataset_S9_CNV.tsv",
      "Dataset_S1_Methylation" = "Dataset_S6_Methylation.tsv",
      "Dataset_S1_miRNA" = "Dataset_S5_miRNA.tsv",
      "Dataset_S1_mRNA" = "Dataset_S3_mRNA.tsv",
      "Dataset_S1_Mutation" = "Dataset_S8_Mutation.tsv",
      "Dataset_S1_Protein" = "Dataset_S7_Protein.tsv",
      "Dataset_S1_Transcript_signatures" = "Dataset_S4_Transcript.tsv",
      "Dataset_S10_CAR" = "Dataset_S10_CAR.tsv",
      "Dataset_S11_omic_layers" = "Dataset_S11_omic_layers.tsv",
      "Dataset_S12_meaningful" = "Dataset_S12_meaningful.tsv"
    )
    
    lapply(names(datasets), function(dataset_name) {
      output[[dataset_name]] <- downloadHandler(
        filename = function() { datasets[[dataset_name]] },  # Just the filename
        content = function(file) {
          source_path <- file.path("data", datasets[[dataset_name]])
          
          if (file.exists(source_path)) {
            success <- file.copy(source_path, file)
            if (!success) {
              showNotification(paste("Failed to copy file:", datasets[[dataset_name]]), type = "error")
            }
          } else {
            showNotification(paste("File not found:", datasets[[dataset_name]]), type = "error")
          }
        }
      )
    })
  })
}