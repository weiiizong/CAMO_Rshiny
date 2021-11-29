global_ui <- function(id, label = "Global") {
  ns <- NS(id)
  tabPanel("Genome-wide analysis", value=id,
           sidebarLayout(
             sidebarPanel(
               h2("Genome-wide c-scores & d-scores calculation"),
               #uiOutput(ns("compType")),
               ## Global ARS/ADS
               selectInput(ns("measure"), label = "Score type", 
                           choices = list("F-measure (recommended)" = "Fmeasure",
                                          "Youden" = "youden", 
                                          "Geometric mean" = "geo.mean")),
               
               numericInput(ns("permNumGlobal"), 
                            label = "Number of permutations", 
                            value = 100),

               actionButton(ns('ACS_ADS'), 'Genome-wide c-scores & d-scores', 
                            class="btn-success",icon = icon("play")), 
               
               hr(),
               actionButton(ns("plotGlobalMDS"),
                            'Genome-wide MDS plot',
                            class="btn-success",
                            icon = icon("play")),   
               tags$hr()
             ),
             mainPanel(
               tabsetPanel(tabPanel("C-scores & D-scores Table",
                                    h3("Genome-wide C-scores & D-scores Table"),
                                    textOutput(ns("Global_ACS_ADS_note")),
                                    DT::dataTableOutput(ns("globalACS_ADSTable"))),
                           tabPanel("MDS Plot",
                                    h3("MDS Plot"),
                                    plotOutput(ns("globalMdsFig"), height = 500))
               )
               
               #plotOutput(ns("ARS_DE"), height = 500, click = clickOpts("plot_click"),
               #hover = hoverOpts("plot_hover")),
               #verbatimTextOutput(ns("click_info")),
               #verbatimTextOutput(ns("hover_info")),
               #h3("KEGG pathway viewer"),
               #imageOutput(ns("KEGGtopo"), height = 500)))
               
             )
           )
  )
}