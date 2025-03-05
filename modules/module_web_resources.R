# module_web_resources.R

# UI do módulo
mod_web_resources_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Useful Web Resources"),
    tags$ul(
      tags$li(tags$a(href = "#", onclick = "window.open('https://www.ensembl.org/biomart/', '_blank')", "BioMart")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://cancer.sanger.ac.uk/cosmic', '_blank')", "COSMIC")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://dgidb.org', '_blank')", "DGIdb")),
      tags$li(tags$a(href = "#", onclick = "window.open('http://www.zhounan.org/ferrdb/legacy/index.html', '_blank')", "FerrDb")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://www.gtexportal.org/', '_blank')", "GTEx Portal")),
      tags$li(tags$a(href = "#", onclick = "window.open('http://hamdb.scbdd.com/', '_blank')", "HAMdb")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://www.genome.jp/kegg/', '_blank')", "KEGG")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://myaidrive.com', '_blank')", "PDF AiDrive")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://precog.stanford.edu/', '_blank')", "Precog")),
      tags$li(tags$a(href = "#", onclick = "window.open('http://www.R-project.org', '_blank')", "R software package")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://navicell.vincent-noel.fr/pages/maps_rcd.html', '_blank')", "RCD map")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://genome.ucsc.edu/', '_blank')", "UCSC Genome browser")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://xena.ucsc.edu/', '_blank')", "UCSCXena")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://shiny.zhoulab.ac.cn/UCSCXenaShiny/#', '_blank')", "UCSCXenaShiny")),
      tags$li(tags$a(href = "#", onclick = "window.open('https://pcm2019.shinyapps.io/XDeathDB/', '_blank')", "XDeathDB"))
    )
  )
}

# Server do módulo (neste caso, vazio, pois não há lógica dinâmica)
mod_web_resources_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sem lógica de servidor necessária
  })
}
