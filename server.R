server <- function(input, output, session) {
  session$allowReconnect(TRUE) # Permite reconexão automática
  
  # Criando reativos apenas onde for necessário
  search_your_target <- reactiveVal(search_your_target)
  Target <- reactiveVal(Target)
  cnv_signature <- reactiveVal(cnv_signature)
  gene_signature <- reactiveVal(gene_signature)
  methylation_signature <- reactiveVal(methylation_signature)
  mutation_signature <- reactiveVal(mutation_signature)
  mirna_signature <- reactiveVal(mirna_signature)
  protein_signature <- reactiveVal(protein_signature)
  transcript_signature <- reactiveVal(transcript_signature)
  omic_layers <- reactiveVal(omic_layers)
  clinical_meaningful <- reactiveVal(clinical_meaningful)
  car_elements <- reactiveVal(car_elements)
  
  # tcga_types <- data.frame(Cancer_abbreviation = "KIRP", Cancer_names = "Kidney Renal Papillary Cell Carcinoma")
  
  # Chamando os módulos corretamente
  mod_search_your_target_server("search_your_target", search_your_target)
  
  # Modificando para passar dataset corretamente para o módulo de nomenclatura
  # mod_nomenclature_server("nomenclature", Target, tcga_types)
  mod_nomenclature_server("nomenclature", tcga_types = tcga_types, Target = Target)
  
  
  # Chamando outros módulos de assinatura
  mod_all_signatures_server("all_signatures", Target)
  mod_signature_server("cnv_signature", cnv_signature)
  mod_signature_server("gene_signature", gene_signature)
  mod_signature_server("methylation_signature", methylation_signature)
  mod_signature_server("mutation_signature", mutation_signature)
  mod_signature_server("mirna_signature", mirna_signature)
  mod_signature_server("protein_signature", protein_signature)
  mod_signature_server("transcript_signature", transcript_signature)
  
  mod_the_best_signature_server("omic_layers", omic_layers)
  mod_the_best_signature_server("clinical_meaningful", clinical_meaningful)
  mod_the_best_signature_server("car_elements", car_elements)
  
  # Chamando outros módulos de análise
  mod_correlation_analysis_server("correlation_analysis", Target)
  mod_tumor_normal_analysis_server("tumor_normal_analysis", Target)
  mod_cox_analysis_server("cox_analysis", Target)
  mod_survival_analysis_server("survival_analysis", Target)
  mod_infiltrates_analysis_server("infiltrates_analysis", Target)
  
  # Chamando outros módulos gerais
  mod_web_resources_server("Web_resources")
  mod_download_server("download")
  mod_developers_server("Developers")
}
