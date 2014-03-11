script.dir <- function() {
  # from http://stackoverflow.com/a/16046056
  dirname(sys.frame(1)$ofile)
}

### Start with project dir, and helper functions
projectDir <- normalizePath(file.path(script.dir(), ".."))
getFullPath <- function(subpath){ file.path(projectDir, subpath) }

homeFolder <- path.expand("~")
dataFolder <- file.path(homeFolder, "Dropbox", "enhancer_predictions")
plotFolder <- file.path(dataFolder, "AdamPlots")

getFullPlotPath <- function(subpath){ file.path(plotFolder, subpath) }

exportAsTable <- function(df, file){ write.table(df,file=file,quote=FALSE, row.names=FALSE,sep="\t") }
clear <- function(save.vec=c()){ ls.vec <- ls(globalenv());del.vec <-setdiff(ls.vec,c(save.vec,"clear")); rm(list=del.vec,pos=globalenv())}
readInTable <- function(file) read.table(file=file,stringsAsFactors=FALSE,header=TRUE)


# setup libs
# install if needed from http://stackoverflow.com/a/4090208
list.of.packages <- c(
    "ggplot2",
    "ROCR", # http://cran.r-project.org/web/packages/ROCR/index.html
    "glmnet", # http://cran.r-project.org/web/packages/glmnet/glmnet.pdf
    "randomForest", #http://cran.at.r-project.org/web/packages/randomForest/randomForest.pdf
    "doParallel",
    "foreach",
    "mboost",
    "vcd", # mosaicpl
    "C50", # kuhn:411
    "mda", # fda, kuhn:362
    "gam",
    "gbm",
    "reshape2", # needed for melt
    "MASS",
    "devtools" # for github installs
    )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#install_github("harrysouthworth/gbm")

#load libs
lapply(list.of.packages, function(lib){
  library(lib, character.only=TRUE)
  })

calcNumCores <- function(){
  numCores <- detectCores()
  if(numCores > 8){
    numCores <- numCores / 2
  } else if(numCores == 1){
    numCores <- 1
  } else {
    numCores <- numCores - 1
  }
  cat("using", numCores, "cores")
  return(numCores)
}
#registerDoParallel(calcNumCores())
registerDoParallel(10)

## load in other libs
source(getFullPath("analysis/dataInput.R"))
source(getFullPath("analysis/predLib.R"))
source(getFullPath("analysis/runPredOnData.R"))
source(getFullPath("analysis/plotResults.R"))



## main analysis

main.heart <- function(){
  # heart
  heart.data.dir <- makeDir(getFullPath("data/heart/"))
  heart.mldata <- paste(heart.data.dir,"dataForPred.tab",sep="")
  heart.mlresults <- paste(heart.data.dir,"mlResults.tab",sep="")
  heart.plots.dir <- makeDir(getFullPlotPath("algoCompare/mouse/heart/"))
  heart.df <- cleanMouseHeart()
  exportAsTable(df=heart.df, file=heart.mldata)

  # run algorithms "trials" number of times -> save result
  heart.ml.df <- accumMlAlgos(df=heart.df, cols=getHeartCols(),
                              trials=30, resultFile=heart.mlresults)

  # exploritory analysis of hearts data
  exploritoryPlots(df=heart.df, cols=getHeartCols(), outdir=heart.plots.dir,msg="Heart Data -> explore")

  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=heart.ml.df, outdir = heart.plots.dir,msg="Heart data -> AW")

}

main.brain <- function(){
  # brain
  brain.data.dir <- makeDir(getFullPath("data/brain/"))
  brain.mldata <- paste(brain.data.dir,"dataForPred.tab",sep="")
  brain.mlresults <- paste(brain.data.dir,"mlResults.tab",sep="")
  brain.plots.dir <- makeDir(getFullPlotPath("algoCompare/mouse/brain/"))
  brain.df <- cleanMouseBrain()
  exportAsTable(df=brain.df, file=brain.mldata)


  # exploritory analysis of hearts data
  exploritoryPlots(df=brain.df, cols=getBrainCols(), outdir=brain.plots.dir,msg="Brain Data -> explore")

  # run algorithms "trials" number of times -> save result
  brain.ml.df <- accumMlAlgos(df=brain.df,cols=getBrainCols(), trials=30,resultFile=brain.mlresults)

  brain.ml.df <- accumMlAlgos(df=brain.df,cols=getBrainCols(),
                              trials=30,resultFile=brain.mlresults)

  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=brain.ml.df, outdir = brain.plots.dir,msg="Brain data -> AW")

}

main.forebrain <- function(){
  # forebrain
  forebrain.data.dir <- makeDir(getFullPath("data/forebrain/"))
  forebrain.mldata <- paste(forebrain.data.dir,"dataForPred.tab",sep="")
  forebrain.mlresults <- paste(forebrain.data.dir,"mlResults.tab",sep="")
  forebrain.plots.dir <- makeDir(getFullPlotPath("algoCompare/mouse/forebrain/"))
  forebrain.df <- cleanMouseForebrain()
  exportAsTable(df=forebrain.df, file=forebrain.mldata)

  # run algorithms "trials" number of times -> save result
  forebrain.ml.df <- accumMlAlgos(df=forebrain.df,cols=getForebrainCols(),
                                  trials=30,resultFile=forebrain.mlresults)

  # exploritory analysis of hearts data
  exploritoryPlots(df=forebrain.df, cols=getForebrainCols(), outdir=forebrain.plots.dir,msg="Forebrain Data -> explore")

  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=forebrain.ml.df, outdir = forebrain.plots.dir,msg="Forebrain data -> AW")
}

main.humanBrain <- function(){
  # forebrain
  brain.human.data.dir <- makeDir(getFullPath("data/human/brain/"))
  brain.human.mldata <- paste(brain.human.data.dir,"dataForPred.tab",sep="")
  brain.human.mlresults <- paste(brain.human.data.dir,"mlResults.tab",sep="")
  brain.human.plots.dir <- makeDir(getFullPlotPath("algoCompare/human/brain/"))
  brain.human.df <- cleanHumanBrain()
  exportAsTable(df=brain.human.df, file=brain.human.mldata)
  
  # run algorithms "trials" number of times -> save result
  human.brain.ml.df <- accumMlAlgos(df=brain.human.df,cols=getBrainColsHuman(),
                                  trials=30,resultFile=brain.human.mlresults)
  
  # exploritory analysis of hearts data
  exploritoryPlots(df=brain.human.df, cols=getBrainColsHuman(), outdir=brain.human.plots.dir,msg="Forebrain Data -> explore")
  
  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=human.brain.ml.df, outdir = brain.human.plots.dir,msg="Forebrain Human data -> AW")
}
main.humanHeart <- function(){
  # heart in human
  heart.human.data.dir <- makeDir(getFullPath("data/human/heart/"))
  heart.human.mldata <- paste(heart.human.data.dir,"dataForPred.tab",sep="")
  heart.human.mlresults <- paste(heart.human.data.dir,"mlResults.tab",sep="")
  heart.human.plots.dir <- makeDir(getFullPlotPath("algoCompare/human/heart/"))
  heart.human.df <- cleanHumanHeart()
  exportAsTable(df=heart.human.df, file=heart.human.mldata)
  
  # run algorithms "trials" number of times -> save result
  human.heart.ml.df <- accumMlAlgos(df=heart.human.df,cols=getHeartColsHuman(),
                                    trials=30,resultFile=heart.human.mlresults)
  
  # exploritory analysis of hearts data
  exploritoryPlots(df=heart.human.df, cols=getHeartColsHuman(), outdir=heart.human.plots.dir,msg="Foreheart Data -> explore")
  
  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=human.heart.ml.df, outdir = heart.human.plots.dir,msg="Foreheart Human data -> AW")
}

#main.humanHeart();main.humanBrain()
modelGBM <- function(){

  df.list <- list("forebrain"=cleanMouseForebrain(),
                  "brain"=cleanMouseBrain(),
                  "heart"=cleanMouseHeart() )

  dir.list <- list("forebrain"= makeDir(getFullPlotPath("gbm/mouse/forebrain"/)),
                   "brain"=makeDir(getFullPlotPath("gbm/mouse/brain/")),
                   "heart"=makeDir(getFullPlotPath("gbm/mouse/heart/")) )

  cols.list <- list("forebrain"=getForebrainCols(),
                    "brain"=getBrainCols(),
                    "heart"=getHeartCols() )

  for(tissue in c("heart", "forebrain", "brain")){
    runGbmOnDataSet(df=df.list[[tissue]],cols=cols.list[[tissue]],outdir=dir.list[[tissue]])
    runGbmTopFive(df=df.list[[tissue]],cols=cols.list[[tissue]],outdir=dir.list[[tissue]])
    gbmResults.df <- accumMlAlgosGbmTop5(df=df.list[[tissue]],cols=cols.list[[tissue]],
                                         trials=30,resultFile=paste(dir.list[[tissue]],"/gbmTop5compare.tab",sep=""),seed=412)
    plotMlresults(df=gbmResults.df, outdir = dir.list[[tissue]],msg=paste("Tissue =", tissue,"\ngbm top 5 vs. normal compare"))
    
  }
}


modelGBM.human <- function(){
  
  df.list <- list("brain"=cleanHumanBrain(),
                  "heart"=cleanHumanHeart() )
  
  dir.list <- list("brain"=makeDir(getFullPlotPath("gbm/human/brain/")),
                   "heart"=makeDir(getFullPlotPath("gbm/human/heart/")) )
  
  cols.list <- list("brain"=getBrainColsHuman(),
                    "heart"=getHeartColsHuman() )
  
  for(tissue in c("heart","brain")){
    runGbmOnDataSet(df=df.list[[tissue]],cols=cols.list[[tissue]],outdir=dir.list[[tissue]])
    runGbmTopFive(df=df.list[[tissue]],cols=cols.list[[tissue]],outdir=dir.list[[tissue]])
    gbmResults.df <- accumMlAlgosGbmTop5(df=df.list[[tissue]],cols=cols.list[[tissue]],
                                         trials=30,resultFile=paste(dir.list[[tissue]],"/gbmTop5compare.tab",sep=""),seed=412)
    plotMlresults(df=gbmResults.df, outdir = dir.list[[tissue]],msg=paste("Tissue =", tissue,"\ngbm top 4 vs. normal compare"))  }
}

main.modelGbmHumanMouse <- function(){
  cat("******************************** modelling gbm in human....\n")
  modelGBM.human()
  cat("done")
  cat("******************************** modeling gbm in mouse...\n")
  modelGBM()
  cat("done")
}


main <- function(){
  cat("******************************** running heart...\n")
  main.heart()

  cat("******************************** running forebrain...\n")
  main.forebrain()

  cat("******************************** assessing GBM model on entire dataset...\n")
  
  modelGBM()
  cat("******************************** done...\n")
}

runHuman <- function(){
  cat("******************************** beating human heart...\n")
  #main.humanHeart();
  cat("******************************** thinking brain...\n")
  main.humanBrain()
  cat("******************************** assessing GBM model on entire dataset...\n")
  
  modelGBM()
  cat("******************************** done...\n")
  
}




