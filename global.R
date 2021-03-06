# This file will be executed prior to app startup to setup the necessary environment
  # if(!("CAMO" %in% installed)){
  #   stop("Please install CAMO R package first")
  # }

## following not necessary
library(preproc)
library(shiny)
library(shinyBS)
library(shinyjs)
library(Rcpp)
library(RcppArmadillo)
library(RcppGSL)
library(snowfall)
library(cvTools)
library(samr)
library(limma) # biocon
library(MASS)
library(ggrepel)
library(ggplot2)
library(gplots)
library(WGCNA)
library(tightClust)
library(ConsensusClusterPlus)
library(biomaRt)
library(KEGG.db)
library(KEGGREST)
library(KEGGgraph)
library(org.Hs.eg.db)
library(pathview)
library(cluster)
library(gridExtra)
library(grid)
library(pathview)
library(plotly)
library(igraph)
library(parallel)

###############
library(CAMO)  # main pkg

#Include all global functions
dir <- "global"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}
for (f in list.files(path=dir, pattern="*.cpp")) {
  sourceCpp(paste(dir, f, sep="/"))
}

# Create the directory for database prior to application startup
db <- new("Database", name="studies")

# Include all server modules
dir <- "server"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# Include all UI modules
dir <- "ui"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# Setting default working directory
tryCatch({
  DB.load.working.dir(db)
}, error=function(error){
  DB.set.working.dir(db, paste(getwd(), "data", sep="/"))
})

