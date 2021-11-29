saved_data_server <- function(input, output, session) {
  
  ns <- NS("saved_data")
  
  DB <- reactiveValues(meta=meta(db), 
                       all_studies=DB.load(db, list.files(path=db@dir)))
  
  observeEvent(input$tabChange, {
    DB$all_studies <- DB.load(db, list.files(path=db@dir))
    DB$meta <- meta(db)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    DB$meta
  }))
  
  # observeEvent(input$orthologous, {
  #   if (!is.null(input$orthologous)) {
  #     inFile <- input$orthologous
  #     DB$full_ortholog <- read.csv(inFile$datapath, stringsAsFactors = F,
  #                                  header=T)
  #   }
  # })
  
  
  observeEvent(c(input$select_orthologous,input$orthologous),{
    path_old <- getwd()
    try({
      if(input$select_orthologous == "upload"){
        if(!is.null(input$orthologous)){
          inFile <- input$orthologous
          ext <- tools::file_ext(inFile$datapath)
          req(inFile)
          validate(need(ext == "csv", "Please upload a .csv"))
          DB$full_ortholog = read.csv(inFile$datapath, stringsAsFactors = F,
                                      header=T)
        }
      }else if(input$select_orthologous == "hs_mm_orth"){
        data(hs_mm_orth, package = "CAMO")
        DB$full_ortholog = hs_mm_orth
      }else if(input$select_orthologous == "hs_rn_orth"){
        data(hs_rn_orth, package = "CAMO")
        DB$full_ortholog = hs_rn_orth
      }else if(input$select_orthologous == "hs_ce_orth"){
        data(hs_ce_orth, package = "CAMO")
        DB$full_ortholog = hs_ce_orth
      }else if(input$select_orthologous == "hs_dm_orth"){
        data(hs_dm_orth, package = "CAMO")
        DB$full_ortholog = hs_dm_orth
      }else if(input$select_orthologous == "ce_dm_orth"){
        data(ce_dm_orth, package = "CAMO")
        DB$full_ortholog = ce_dm_orth
      }
      output$table_orth <- DT::renderDataTable(DT::datatable({
        DB$full_ortholog
      }))
    },session)
    setwd(path_old)
    done(session)
  })

  
  
  output$selected <- renderText({
    selected <- input$table_rows_selected
    if(length(selected) == 0)
      "You haven't select anything yet"
    else
      paste(rownames(meta(db)[selected,]), sep=", ")
    
  })
  
  observeEvent(input$delete, {
    selected <- input$table_rows_selected
    if(length(selected) != 0 & !is.null(meta(db))){
      selected <- rownames(meta(db)[selected,])
      DB.delete(db, selected)
      sendSuccessMessage(session, paste(selected, "deleted"))
      print(DB$meta)
      DB$meta <- meta(db)
      DB$all_studies <- DB.load(db, list.files(path=db@dir))
    }else{
      sendErrorMessage(session, "You haven't select anything yet")
    }
  })
  
  
  observeEvent(input$merge, {
    wait(session, "Match and merge")
    try({
      species <- sapply(1:length(DB$all_studies), function(x) 
        DB$all_studies[[x]]@species)
      #print(paste("all species:", species, sep=""))
      studyNames <- sapply(1:length(DB$all_studies), function(x) 
        DB$all_studies[[x]]@studyName)
      #print(paste("study names: ",studyNames, sep=""))
      mcmc.list <- lapply(1:length(DB$all_studies), function(x) 
        DB$all_studies[[x]]@MCMC)
      #print(paste("length of mcmc.list:", length(mcmc.list), sep=""))
      if(is.null(DB$full_ortholog)){
        data(hm_orth)
        DB$full_ortholog <- hm_orth
      }
      #print(paste("ref number :", which(species == input$reference)[1], sep=""))
      mcmc.merge.list <- CAMO::merge(mcmc.list, species = species,
                                     ortholog.db = DB$full_ortholog, 
                                     reference=which(species == input$reference)[1])
      names(mcmc.merge.list) = studyNames
      #print("finish merge")
      
      saveRDS(mcmc.merge.list,
              file=paste(DB.load.working.dir(db), 
                         "MergedDB.rds", sep="/"))
      saveRDS(species, 
              file=paste(DB.load.working.dir(db), 
                         "MergedSpecies.rds", sep="/"))
      saveRDS(studyNames, 
              file=paste(DB.load.working.dir(db), 
                         "MergedStudyNames.rds", sep="/"))
      
      message = paste("Data are successfully merged")
      sendSuccessMessage(session, message)
    },session)
    done(session)
  }, label="save study")
  
  
  ##########################
  # Render output/UI       #
  ##########################
  output$reference = renderUI({
    if(length(DB$all_studies) != 0){
      selectInput(ns('reference'), 'Reference species', 
                  as.character(unique(sapply(1:length(DB$all_studies), function(x) 
                    DB$all_studies[[x]]@species))),
                  selected=as.character(unique(sapply(1:length(DB$all_studies), 
                                                      function(x) DB$all_studies[[x]]@species))) [1])
      
    }
  })
  
  
  output$upload_orthologous = renderUI({
    if(input$select_orthologous == "upload"){
      fileInput(ns("orthologous"), 'Upload orthologs file (.csv)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      
    }
  })
}
