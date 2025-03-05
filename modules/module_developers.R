# UI do módulo
mod_developers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Research Group Information", style = "text-align: center; margin-bottom: 20px;"),
    p("Our research group focuses on computational biology, bioinformatics, and multi-omics data analysis. 
           We develop statistical and machine learning approaches to understand complex biological systems, 
           with an emphasis on cancer genomics, molecular markers, and precision medicine.", 
      style = "max-width: 1000px; margin: 0 auto; text-align: justify;"),
    
    # Add spacing between group info and individual details
    br(), br(),  # Adds vertical space
    div(style = "margin-top: 50px;"),  # Additional CSS-based spacing
    
    fluidRow(
      column(3, align = "center",
             img(src = "HACN_Photo.jpg", height = "200px", width = "200px", style = "border-radius: 50%;"),
             h4("Higor Almeida Cordeiro Nogueira"),
             p("Researcher"),
             a("ResearchGate", href = "https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira", target = "_blank"),
             br(),
             a("LinkedIn", href = "https://linkedin.com/in/higor-almeida-950082255", target = "_blank")
      ),
      column(3, align = "center",
             img(src = "ESR_Photo.jpg", height = "200px", width = "200px", style = "border-radius: 50%;"),
             h4("Emanuell Rodrigues de Souza"),
             p("Researcher"),
             a("ResearchGate", href = "https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza", target = "_blank"),
             br(),
             a("LinkedIn", href = "http://www.linkedin.com/in/emanuell-rodrigues-de-souza-35b40a300", target = "_blank")
      ),
      column(3, align = "center",
             img(src = "ABG_Photo.jpg", height = "200px", width = "200px", style = "border-radius: 50%;"),
             h4("Ana Beatriz Garcia"),
             p("Researcher"),
             a("ResearchGate", href = "https://www.researchgate.net/profile/Ana-Beatriz-Garcia", target = "_blank"),
             br(),
             a("LinkedIn", href = "https://www.linkedin.com/in/ana-beatriz-garcia", target = "_blank")
      ),
      column(3, align = "center",
             img(src = "EMA_Photo.jpg", height = "200px", width = "200px", style = "border-radius: 50%;"),
             h4("Enrique Medina-Acosta"),
             p("Team Head"),
             a("ResearchGate", href = "https://www.researchgate.net/profile/Enrique-Medina-Acosta", target = "_blank"),
             br(),
             a("Google Scholar", href = "https://scholar.google.com/citations?user=XXXXXXX", target = "_blank") # Replace XXXXXXX with correct ID
      )
    )
  )
}


# Server do módulo (neste caso, vazio, pois não há lógica dinâmica)
mod_developers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sem lógica de servidor necessária
  })
}