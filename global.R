# ---------------------- Importação de Bibliotecas ----------------------
library(shiny)
library(UCSCXenaShiny)
library(UCSCXenaTools)
library(DT)
library(ggplot2)
library(rsconnect)
library(dplyr)
library(fmsb)
library(survminer)
library(survival)
library(ggradar)
library(tidyr)
library(devtools)
library(memoise) # Para cachear funções pesadas
library(ggsci)
library(gridtext)
library(cowplot)
library(readxl)
library(readr)
library(shinycssloaders)

# ---------------------- Configurações do Shiny ----------------------
options(shiny.maxRequestSize = 500*1024^2) # Permite uploads de até 500MB
options(shiny.sanitize.errors = FALSE) # Exibe mensagens de erro completas
options(shiny.autoreload = TRUE) # Recarrega o app automaticamente ao editar arquivos

# ---------------------- Importação de Módulos ----------------------
source("modules/module_search_your_target.R")
source("modules/module_nomenclature.R")
source("modules/module_all_signatures.R")
source("modules/module_signature.R")
source("modules/module_the_best_signature.R")
source("modules/module_correlation_analysis.R")
source("modules/module_tumor_normal_analysis.R")
source("modules/module_cox_analysis.R")
source("modules/module_survival_analysis.R")
source("modules/module_infiltrates_analysis.R")
source("modules/module_web_resources.R")
source("modules/module_download.R")
source("modules/module_developers.R")

# ---------------------- Função para carregar dados de forma segura ----------------------
load_rds <- memoise(function(filename) {
  file_path <- file.path("data", filename)
  if (file.exists(file_path)) {
    message("Carregando: ", filename)
    return(readRDS(file_path))
  } else {
    warning(paste("Arquivo não encontrado:", filename))
    return(NULL)
  }
})

# ---------------------- Carregamento dos Dados (Apenas uma Vez) ----------------------
search_your_target <- load_rds("search_your_target.rds")
Target <- load_rds("Dataset_S2.rds")
tcga_types <- read_excel("data/TCGA_Cancer_types.xlsx")

# Assinaturas multi-ômicas (cada uma carregada separadamente)
cnv_signature <- load_rds("Dataset_S9_CNV.rds")
gene_signature <- load_rds("Dataset_S3_mRNA.rds")
methylation_signature <- load_rds("Dataset_S6_Methylation.rds")
mutation_signature <- load_rds("Dataset_S8_Mutation.rds")
mirna_signature <- load_rds("Dataset_S5_miRNA.rds")
protein_signature <- load_rds("Dataset_S7_Protein.rds")
transcript_signature <- load_rds("Dataset_S4_Transcript.rds")

omic_layers <- load_rds("Dataset_S11_omic_layers.rds")
clinical_meaningful <- load_rds("Dataset_S12_meaningful.rds")
car_elements <- load_rds("Dataset_S10_CAR.rds")


