saved_data_ui <- function(id, label = "saved data of single study or multiple study") {
  ns <- NS(id)
  tabPanel("Saved Data", value=id,
    sidebarLayout(
      sidebarPanel(
        p("Selected datasets"), helpIcon(ns('merge_select'), HELP.select.datasets),
	      textOutput(ns("selected")),
        actionButton(ns('delete'), 'Delete selected data', icon=icon("trash"), 
          class="btn-danger"),
	      tags$hr(),
        selectInput(ns('select_orthologous'), 'Ortholog file', 
                    c("homo sapiens (hs) vs mus musculus (mm)"="hs_mm_orth",
                      "homo sapiens (hs) vs rattus norvegicus (rn)"="hs_rs_orth",
                      "homo sapiens (hs) vs caenorhabditis elegans (ce)"="hs_ce_orth",
                      "homo sapiens (hs) vs drosophila melanogaster (dm)"="hs_dm_orth",
                      "caenorhabditis elegans (ce) vs drosophila melanogaster (dm)"="ce_dm_orth",
                      "Upload an orthologs file"="upload"),
                    selected = NULL),
        uiOutput(ns("upload_orthologous")),
        # fileInput(ns("orthologous"), 'Upload orthologous file',
        #   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        uiOutput(ns('reference')),
        actionButton(ns("merge"), 'Match and merge', icon=icon("rocket"))
      ),
      mainPanel(
        h3("List of saved data"),
        DT::dataTableOutput(ns("table")),
        hr(),
        h3("Orthologs file selected"),
        DT::dataTableOutput(ns("table_orth"))
      )
    )
  )
}
