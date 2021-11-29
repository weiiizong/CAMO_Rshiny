setting_ui <- function(id, label = "global settings") {
  ns <- NS(id)
  
  tabPanel("Setting", value=id,
           h2("Welcome to CAMO",align = "middle",style="primary"),
           tags$hr(),
           #HTML('<center><img src="pipeline.png" </center>'),
           fluidRow(
             img(src='pipeline.png',align="left"),
             p("CAMO is an interactive software with graphical user interface (GUI) 
               for any general-purpose concordant and discordant analysis of differential transcriptomic systems 
               implemented using R shiny. It performs threshold-free differential analysis to generate quantitative 
genome-wide and pathway level concordance/discordance scores (c-scores/d-scores). Based on the c-scores/d-scores, 
pathway-centric knowledge retrieval and topological subnetwork detection are performed with corresponding visualizations provided."), 
             p("Our tool is available for download on github: ",a(strong("CAMO."), href="https://github.com/CAMO/CAMO",target="_blank"), 
             "For detailed implementation of each tool, please refer to our ",a(strong("Tutorials."), href="https://github.com/metaOmicsCAMO/tutorial/
blob/master/CAMO_turtorial.pdf",target="_blank")), 
             p("CAMO is developed and maintained by ", a("Dr. George Tseng's group ",href="http://tsenglab.biostat.pitt.edu",target="_blank"),"(Department of Biostatistics, University of Pittsburgh) and ", a("Dr. Tianzhou Ma's group ",href="https://matianzhou.github.io",target="_blank"), "(Department of Epidemiology and Biostatistics, University of Maryland College Park)."),
             p("We recommend users to use R (>=3.5.0) to implement our tool. If you are using R 3.4, 
               you may encounter errors in installing dependencies of the modules. "),
             style="text-indent: 20px; font-size: 16px"),
           tags$hr(),
    mainPanel(
      h2("Session Information"),
      verbatimTextOutput(ns("urlText")),
      helpIcon("working_dir_help", "During the computation, some output files or images are automatically saved to this directory."),
      h2("Saving directory:", style="display:inline"),
      directoryInput(ns('directory'), label='select a directory')
    )
  )
}
