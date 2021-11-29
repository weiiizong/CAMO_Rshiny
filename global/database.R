# Study class
setClass("Study1", representation(
  studyName="character",
  species="character",
  MCMC="matrix"))

setMethod("meta", signature("Study1"), function(object) {
  data.frame(species=object@species, studyName=object@studyName)
})


# Database class
setClass("Database",
         representation(
           name="character",
           dir="character",
           meta.file="character",
           working.path="character"
         ),
         prototype(
           dir="",
           meta.file="",
           working.path=""
         ),
         validity = function(object) {
           errors <- character()
           if (length(errors) == 0) TRUE else errors
         }
)

# custom contructor to set meta
setMethod("initialize", "Database",
          function(.Object, name) {
            .Object <- callNextMethod()
            .Object@dir      <- paste(DB.dir, name, sep="/")
            .Object@meta.file <- paste(".Database", name, "meta/meta", sep="/")
            .Object@working.path <- paste(".Database", name, "working/working", sep="/")
            if (!file.exists(.Object@dir)) dir.create(.Object@dir, recursive=T)
            dir.path <- paste(".Database", name, "meta",  sep="/")
            if (!file.exists(dir.path)) dir.create(dir.path, recursive=T)
            dir.path <- paste(".Database", name, "working",  sep="/")
            if (!file.exists(dir.path)) dir.create(dir.path, recursive=T)
            if (!file.exists(.Object@working.path))
              file.create(.Object@working.path, overwrite=F)
            studies <- c()
            studies <- DB.load(.Object, list.files(path=.Object@dir))
            print(paste("number of studies saved: " ,length(studies), sep=""))
            db.meta <- data.frame(
              "species"=character(0),
              "studyNames"=character(0)
            )
            if(length(studies) > 0) {
              db.meta <- lapply(studies, function(study_use) meta(study_use))
              db.meta <- do.call(rbind, db.meta)
            }
            DB.sync(.Object, db.meta)
            .Object
          }
)

# Return Database meta information as data.frame
setMethod("meta", signature("Database"), function(object) {
  readRDS(db@meta.file)
})

# write database meta data to file, should be called everytime when
# database is modified
DB.sync <- function(db, db.meta) {
  saveRDS(db.meta, file=db@meta.file)
}

# save x to db as file
DB.save <- function(db, study_use) {
  #  if(class(study) != "Study") stop("study must be Study")
  saveRDS(study_use, file=paste(db@dir, study_use@studyName, sep="/"))
  db.meta <- rbind(meta(db), meta(study_use))
  DB.sync(db, db.meta)
}

# load file from db
DB.load <- function(db, studies) {
  res <- c()
  for(study in studies) {
    res <- c(res, readRDS(paste(db@dir, study, sep='/')))
  }
  res
}

MergedDB.load <- function(db){
  if(file.exists(paste(DB.load.working.dir(db), "MergedDB.rds", sep="/"))){
    readRDS(paste(DB.load.working.dir(db), "MergedDB.rds", sep="/"))
  }else{
    return(NA)
  }
}

MergedSpecies.load <- function(db){
  if(file.exists(paste(DB.load.working.dir(db), 
                       "MergedSpecies.rds", sep="/"))){
    readRDS(paste(DB.load.working.dir(db), 
                  "MergedSpecies.rds", sep="/"))
  }else{
    return(NA)
  }
}

MergedStudyNames.load <- function(db){
  if(file.exists(paste(DB.load.working.dir(db), 
                       "MergedStudyNames.rds", sep="/"))){
    readRDS(paste(DB.load.working.dir(db), 
                  "MergedStudyNames.rds", sep="/"))
  }else{
    return(NA)
  }
}

MergedPM.load <- function(db){
  if(file.exists(paste(DB.load.working.dir(db), 
                       "MergedPM.rds", sep="/"))){
    readRDS(paste(DB.load.working.dir(db), 
                  "MergedPM.rds", sep="/"))
  }else{
    return(NA)
  }
}

# delete file from db
DB.delete <- function(db, studies) {
  file.remove(paste(db@dir, list.files(path=db@dir)[as.numeric(studies)], sep="/"))
  db.meta <- meta(db)
  db.meta <- db.meta[!(rownames(db.meta) %in% studies),]
  if(nrow(db.meta)>0){
    rownames(db.meta) = 1:nrow(db.meta)
  }
  DB.sync(db, db.meta)
}

# list all files in db
DB.ls <- function(db) {
  meta(db)$studyName
}

DB.set.working.dir <- function(db, path){
  file.con <- file(db@working.path)
  writeLines(path, file.con)
  close(file.con)
}

DB.load.working.dir <- function(db){
  file.con <- file(db@working.path)
  path <- readLines(file.con)
  close(file.con)
  if (length(path) == 0)
    stop(MSG.no.working.dir)
  else
    path
}

# 
# DEevid_ACS_plot_clicked <- function(ds1,ds2,DEevid1,DEevid2,ACSp,ADSp,clickedVec,
#                                     highlight.pathways = NULL,lb=0,ub=1,size.scale=4){
# 
#   P <- length(ACSp)
#   ACS_size=sapply(ACSp,ARS_to_size)
#   ADS_size=sapply(ADSp,ARS_to_size)
# 
#   if(!is.null(highlight.pathways)){
#     index = 1:P
#     index[-highlight.pathways] = ""
#   }else{
#     index = rep("",P)
#   }
#   # index=1:P
#   # index[ACS_size==1] <- ""
# 
#   # data_pos <- data.frame(ds1_score=ds1.pos,ds2_score=ds2.pos,
#   #                        ACS_size=ACS_size,index=index,clicked=clickedVec)
#   # 
#   # data_neg <- data.frame(ds1_score=ds1.neg,ds2_score=ds2.neg,
#   #                        ACS_size=ACS_size,index=index,clicked=clickedVec)
#   data_ACS <- data.frame(ds1_score=DEevid1,ds2_score=DEevid2,
#                          ACS_size=ACS_size,index=index,
#                          color_pos=clickedVec)
#   data_ADS <- data.frame(ds1_score=DEevid1,ds2_score=DEevid2,
#                          ADS_size=ADS_size,index=index,
#                          color_neg=clickedVec)
# 
# 
#   p_pos <-ggplot(data_ACS, aes(x=ds2_score, y=ds1_score, label=index)) + #label=index
#     #geom_text(size=10,parse=TRUE,color="black",hjust = -0.05,vjust=-0.05) +
#     geom_point(shape=16,size=(data_ACS$ACS_size*size.scale),color=data_ACS$color_pos) +
#     theme_bw() +
#     coord_fixed(ylim=c(lb,ub),xlim=c(lb,ub)) +
#     labs(x="",y="") +
#     scale_x_continuous(name="",breaks=seq(0,1,by=0.5),limits=c(0,1),
#                        labels=c("0","0.5","1")) +
#     scale_y_continuous(name="",breaks=seq(0,1,by=0.5),limits=c(0,1),
#                        labels=c("0","0.5","1")) +
#     #scale_color_manual(values=c("red", "grey"))+
#     theme(#legend.title = element_blank(),
#           axis.line = element_line(colour = "black"),
#           axis.text.x = element_text(size=60*size.scale, face="bold"),
#           axis.text.y = element_text(size=60*size.scale, face="bold"),
#           panel.border = element_blank(),
#           # plot.margin = unit(c(0,0,0,0),"cm"))
#           panel.grid.major = element_line(linetype = 'solid',#size = 2,
#                                           colour = "white"),
#           panel.grid.minor = element_line(linetype = 'solid',
#                                           colour = "white"),
#           panel.background = element_rect(fill = "#FFE3E0"))
# 
#   p_pos <- p_pos + annotate("text", x = (ub-0.15), y = lb, fontface=20,
#                             label = paste(ds2,sep=""),
#                             size=40*size.scale,colour="blue",hjust=0.6,vjust=0.1)
# 
#   p_pos <- p_pos + annotate("text", x = lb, y = (ub-0.15), fontface=20,
#                             label=paste(ds1,sep=""),
#                             size=40*size.scale,colour="blue",vjust=0,hjust=0.2)
# 
# 
#   p_neg <-ggplot(data_ADS, aes(x=ds1_score, y=ds2_score, label=index)) + #label=index
#     #geom_text(size=10,parse=TRUE,color="black",hjust = -0.05,vjust=-0.05) +
#     geom_point(shape=16,size=data_ADS$ADS_size*size.scale,color=data_ADS$color_neg) +
#     geom_text(size=8*size.scale,parse=TRUE,color="black",hjust = -0.05,vjust=-0.05) +
#     theme_bw() +
#     coord_fixed(ylim=c(lb,ub),xlim=c(lb,ub)) +
#     labs(x="",y="") +
#     scale_x_continuous(name="",breaks=seq(0,1,by=0.5),limits=c(0,1)) +
#     scale_y_continuous(name="",breaks=seq(0,1,by=0.5),limits=c(0,1)) +
#     #scale_color_manual(values=c("red", "grey"))+
#     theme(#legend.title = element_blank(),
#           axis.line = element_line(colour = "black"),
#           axis.text.x = element_text(size=60*size.scale, face = "bold"),
#           axis.text.y = element_text(size=60*size.scale, face = "bold"),
#           #plot.margin = unit(c(0,0,0,0),"cm")
#           panel.border = element_blank(),
#           panel.grid.major = element_line(linetype = 'solid',#size = 2,
#                                           colour = "white"),
#           panel.grid.minor = element_line(linetype = 'solid',
#                                           colour = "white"),
#           panel.background = element_rect(fill = "#DBE7E4"))
#           
#   p_neg <- p_neg + annotate("text", x = (ub-0.15), y = lb, fontface=2,
#                             label = paste(ds1,sep=""),
#                             size=40*size.scale,colour="blue",hjust=0.6,vjust=0.1)
#   p_neg <- p_neg + annotate("text", x = lb, y = (ub-0.15), fontface=2,
#                             label=paste(ds2,sep=""),
#                             size=40*size.scale,colour="blue",vjust=0,hjust=0.2)
# 
#   #ggsave(filename=paste("ACS_DE",ds1,"_",ds2,"_pos",".pdf",sep=""),p_pos,
#   #width = 10, height = 10)
# 
#   #ggsave(filename=paste("ACS_DE",ds2,"_",ds1,"_neg",".pdf",sep=""),p_neg,
#   #width = 10, height = 10)
# 
#   plist <- list(p_pos,p_neg)
#   return(plist)
# }
# 
# 
#  
# kegg_shiny = function(mcmc.merge.list,dataset.names,
#                       select.pathway.list,keggViewSelect = c(1,2)){
#   orig.path <- getwd()
#   pathway.name <- names(select.pathway.list)
#   if(sum(grepl("KEGG",pathway.name))==0) {
#     warning("No KEGG pathways")
#   } else{
#     #data("paths.hsa")
#     kegg_pathname <- unlist(as.list(KEGGPATHID2NAME)) ## KEGG pathway name <-> ID
#     dir.path <- paste("keggView",
#                       dataset.names[keggViewSelect[1]],
#                       dataset.names[keggViewSelect[2]],sep = "_")
#     if (!file.exists(dir.path)) dir.create(dir.path)
#     setwd(paste(orig.path,"/",dir.path,sep=""))
# 
#     kegg.pathway.name <- pathway.name[grep("KEGG",pathway.name)]
# 
#     K_KEGG <- length(kegg.pathway.name)
#     dat1 <- mcmc.merge.list[[keggViewSelect[1]]]
#     dat2 <- mcmc.merge.list[[keggViewSelect[2]]]
#     for(k in 1:K_KEGG){
#       print(paste("keggView",k,sep=":"))
#       keggk.name <- kegg.pathway.name[k]
#       print(keggk.name)
#       overlap.genes <- intersect(rownames(dat1),select.pathway.list[[keggk.name]])
#       signPM.mat <- cbind(apply(dat1[overlap.genes,],1,mean),
#                           apply(dat2[overlap.genes,],1,mean))
#       colnames(signPM.mat) <- dataset.names[keggViewSelect]
#       keggk.name1 <- gsub("KEGG ","",keggk.name)
#       #library(KEGG.db)
#       #xx <- unlist(as.list(KEGGPATHID2NAME))
#       pathwayID <- gsub("hsa","",names(paths.hsa)[which(paths.hsa==keggk.name1)])
#       res <- keggView(mat=signPM.mat,pathwayID)
#       if(grepl("/",keggk.name)){
#         keggk.name <- gsub("/","-",keggk.name)
#       }
#       hsaName <- paste("hsa",pathwayID,sep="")
#       file.rename(paste(hsaName,"..multi.png",sep=""),
#                   paste(keggk.name,".png",sep=""))
#       file.remove(paste(hsaName,".xml",sep=""))
#       file.remove(paste(hsaName,".png",sep=""))
#       #file.remove(paste(hsaName,"..png",sep=""))
#     }
#     setwd(orig.path)
#   }
# }

