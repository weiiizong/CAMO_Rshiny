indPathway_ui <- function(id, label = "Individual Pathway") {
  ns <- NS(id)
  tabPanel("Individual pathway analysis", value=id,
           sidebarLayout(
             sidebarPanel(
               h2("Individual pathway results browser"),
               h3("(i) Pathway-level c-scores & d-scores"),
               uiOutput(ns("selectPathway")),
               hr(),
               h3("(ii) Visualization of individual pathway"),
               uiOutput(ns("choosePathway")),
               checkboxInput(ns("browser_useACS"), 'Use c-scores to generate results', TRUE),
               checkboxGroupInput(ns('browserVisualizations'), 'Select visualizations to generate',
                                  c("mdsModel", "clustModel", "genePM", "keggView", "reactomeView")),
               #checkboxInput(ns("showKegg"), 'Generate and show Kegg topology plots of selected pathway(s)', TRUE),
               uiOutput(ns("KEGG_panel")),
               #checkboxInput(ns("showReactome"), 'Generate and show Reactome topology plots of selected pathway(s)', TRUE),
               uiOutput(ns("Reactome_panel")),
               actionButton(ns("showVisual"), 'Generate visulizations', class = "btn-success", icon = icon("play")),
               hr(),
               h3("(iii) KEGG community detection algorithm (recommend for human)"),
               uiOutput(ns('MD_chooseKEGGname')),
               h5("Select a study pair:"),
               fluidRow(
                 column(width = 3,
                        uiOutput(ns('MD_dataPair1'))
                 ),
                 column(width = 3, 
                        uiOutput(ns('MD_dataPair2'))
                 )
               ),
               textInput(ns("MD_KEGGspecies"), "KEGG organisms code", "hsa"),
               radioButtons(ns("MD_searchType"), "Choose a searching algorithm", choices = list("Simulated Annealing" = "SA", "Exhaustive Searching" = "Exhaustive"), selected = "Exhaustive"),
               radioButtons(ns("MD_geneType"), "Choose a module type", choices = list("Concordant" = "concordant", "Discordant" = "discordant"), selected = "discordant"),
               radioButtons(ns("MD_KEGG.dataGisTopologyG"), "Gene names in data matrix and KEGG topology plots are in same type?", choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE"),
               uiOutput(ns('MD_choosekeggGenes_dat_map')),
               
               bsCollapsePanel("Advanced settings for KEGG topological gene module detection",
                               NULL,
                               textInput(ns("MD_pathwayID"), "KEGG pathway ID without the organism prefix. If not provided, ID will be retrieved from KEGGREST."),
                               numericInput(ns("DE_PM_cut"), "Cutting level for posterior DE means", 0.2, min = 0),
                               numericInput(ns("minM"), "Minimum module size", 4, min = 1),
                               numericInput(ns("maxM"), "Maximum module size", 15, min = 1),
                               numericInput(ns("MD_P"), "Number of permutations", 1000, min = 1),
                               numericInput(ns("MD_cores"), "Number of parallel computing cores", 1, min = 1),
                               numericInput(ns("MD_seed"), "Random seed", 12345, min = 1),
                               textInput(ns("node_sep"), "Seperation notation for multiple genes in one node", "-"),
                               hr(),
                               p("Hyperparameters for Simulated Annealing:"),
                               numericInput(ns("reps_eachM"), "Number of searching repetitions at each module sizle", 1, min = 1),
                               numericInput(ns("topG_from_previous"), "Number of top module results stored as initials for next module sizle", 1, min = 1),
                               numericInput(ns("Tm0"), "Initial temperature", 10, min = 1),
                               numericInput(ns("mu"), "Temperature multiplier", 0.95),
                               numericInput(ns("epsilon"), "Final temperature", 1e-5),
                               numericInput(ns("N"), "Number of maximum annealing times", 3000, min = 1),
                               style="primary"
               ),
               actionButton(ns('tuneMD'), 'Elbow plot of -log10(p-value) for module detection', class="btn-success", icon = icon("play")),
               # p("Indicate module size(s) to plot"),
               textInput(ns("MD_whichToDraw"), "Select module sizes to show in KEGG maps", "all"),
               helpIcon(ns('which_modele'), "Please type the sizes of module of interests (check elbow plot) and use \",\" as separator (e.g. 1,2,3). Leave it as default if you want all of them."),
               actionButton(ns('plotMD'), 'KEGG topology plots with highlighted modules', class="btn-success", icon = icon("play")),
               hr(),
               h2("Save results for all pathways"),
               helpIcon(ns('individual_analysis'), "Results will be saved to the saving/working directory selected."),
               checkboxInput(ns("useADS"), 'Use c-scores to generate results', TRUE),
               #uiOutput(ns('selectVisualizations')),
               checkboxGroupInput(ns('selectVisualizations'), 'Select type(s) of visualization to generate and save',
                                  c("mdsModel", "clustModel", "genePM", "keggView", "reactomeView")),
               uiOutput(ns('keggViewSelect')),
               uiOutput(ns('reactomeViewSelect')),
               actionButton(ns('plotAll'), 'Run and Save', class="btn-success", icon = icon("play"))
             ),
             mainPanel(
               tabsetPanel(
               tabPanel("Pathway C-scores & D-scores Table",
                        h3("C-scores & D-scores Table of the Selected Pathway"),
                        textOutput(ns("Show_selected_pathway_name")),
                        textOutput(ns("Pathway_ACS_ADS_note")),
                        DT::dataTableOutput(ns("selectedPathwayACS_ADS_Table"))
               ),
               tabPanel("Visualizations",
                        h3("MDS, Clustering Heatmap, Posterior DE Heatmap, Topology Plot (only for KEGG and Reactome pathways)"),
                        uiOutput(ns("inputPathway"))
               ),
               tabPanel("KEGG topological module Detection - Elbow plot",
                        h3("Elbow plot to select module sizes"),
                        plotOutput(ns("MDelbowPlot"),width="auto",height="auto"),
                        textOutput(ns("MDbestSize"))
               ),
               tabPanel("KEGG topological module Detection - KEGG topology plots",
                        h3("KEGG topology plots with highlighted modules"),
                        uiOutput(ns("MDplots"))
               )
               )
             )
           )
  )
}
