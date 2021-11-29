
pathway_ui <- function(id, label = "Aggregated Pathway") {
  ns <- NS(id)
  tabPanel("Pathway-based analysis", value=id,
           sidebarLayout(
             sidebarPanel(
               h2("Pathway c-scores & d-scores calculation"),
               selectInput(ns("measure"), label = "Score type", 
                           choices = list("F-measure (recommended)" = "Fmeasure",
                                          "Youden" = "youden", 
                                          "Geometric mean" = "geo.mean")),
               numericInput(ns("permNumPathway"), 
                            label = "Number of permutations", 
                            value = 100),
               br(),
               radioButtons(ns("select_pathwayfile"), label = "Pathway database",
                       choices = list("Upload a pathway list"="upload", 
                                      "Select from existing pathway lists"="exist"),
                       selected = "upload"),
               uiOutput(ns("exist_pathwayfile")),
               uiOutput(ns("upload_pathwayfile")),
               checkboxInput(ns("parallel"), 'Use parallel computation', FALSE),
               uiOutput(ns("para_cores")),
               
               ## pathway selection advanced setting
               bsCollapsePanel("Advanced settings for pathway selection",
                               NULL,
                               numericInput(ns("pathwaysizeLowerCut"), 
                                            label = "Minimum pathway size", value = 5),
                               numericInput(ns("pathwaysizeUpperCut"), 
                                            label = "Maximum pathway size", value = 200),
                               numericInput(ns("overlapsizeCut"), 
                                            label = "Minimum number of overlapping genes across studies should be greater than", value = 5),
                               numericInput(ns("medDECut"), 
                                            label = "Median number of overlapping DE genes across studies should be greater than", value = 3),
                               numericInput(ns("minDECut"), 
                                            label = "Minimum number of overlapping DE genes across studies should be greater than", value = 0),
                               numericInput(ns("qfisherCut"), 
                                            label = "Fisher meta-qvalue should smaller than", value = 0.05),
                               textInput(ns("topPathNnum"), 
                                            label = "If only top pathways in at least one study are considered (use this if meta-qvalue is too stringent), please input the number of top pathways.", value = ""),
                               style="primary"
               ),
               
               actionButton(ns('ACS_ADS'), 'Pathway c-scores & d-scores', 
                            class="btn-success", icon = icon("play")),
               
               ##tunning K
               tags$hr(),
               h2("Pathway clustering"),
               actionButton(ns('tuneK_ACS'), 
                            'Scree plot to select the number of pathway clusters K', 
                            class="btn-success",
                            icon = icon("play")),
               hr(),
               numericInput(ns("K_ACS"), 
                            label = "Optimal number of pathway clusters K", value = 1),
               selectInput(ns('select_hashtbfile'), 'Select a noun-pathway matrix for text mining', 
                           c("human_text"="human_text",
                             "worm_text"="worm_text",
                             "Upload a noun-pathway matrix file"="upload",
                             "Skip text mining"="no_textmining"),
                           selected = NULL),
               uiOutput(ns("upload_hashtbfile")),

               ## pathway clustering advanced setting
               bsCollapsePanel("Advanced settings for pathway clustering and co-membership heatmap", 
                               NULL, 
                               numericInput(ns("silCut"), "Silhouette index cutoff to control scatterness", 0.1),
                               #numericInput(ns("tmThres"), "FDR control for key words from text mining", 0.2),
                               numericInput(ns("comProbCut"), "Cutting level for co-membership probability", 0.7),
                               style="primary"
               ),
               actionButton(ns('pathclust_ACS'), 
                            'Pathway clustering', 
                            class="btn-success",
                            icon = icon("play")),
               hr(),
               h2("DE evidence plot"),
               uiOutput(ns("ACS_DEgroup")),
               actionButton(ns('plotACS_DE'), 
                            'DE evidence plot', 
                            class="btn-success",
                            icon = icon("play"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("C-scores & D-scores Table",
                          h3("Pathway C-scores"),
                          DT::dataTableOutput(ns("pathwayACS_Table")),
                          h3("Pathway D-scores"),
                          DT::dataTableOutput(ns("pathwayADS_Table"))),
                 tabPanel("Scree Plot",
                          h3("Scree plot to select the optimal K"),
                          imageOutput(ns("tuneKFig"), height = 500)),
                 tabPanel("Cluster MDS & Heatmap",
                          h3("MDS Plot"),
                          imageOutput(ns("mdsPathway"), height = 500),
                          h3("Clustering Heatmap"),
                          imageOutput(ns("heatmapPathway"),height = 500)),
                 tabPanel("Individual Cluster Information",
                          h3("Key words table & Co-membership heatmap"),
                          h5("Key words table: noun phrases in each pathway cluster"),
                          h5("Co-membership heatmap: probability of datasets being clustered together in each pathway cluster"),
                          uiOutput(ns("comemPlots"))),
                 tabPanel("DE Evidence Plot",
                          h3("DE Evidence Plot"),
                          numericInput(ns("numNearPath"),
                                       "Select the number of nearest pathways to display after clicking",
                                       3,min = 1, step = 1),
                          textOutput(ns("nearPathwayName")),
                          uiOutput(ns("clickedPathway")),
                          uiOutput(ns("ACS_DEplots")))
               )
             )
           )
  )
}
