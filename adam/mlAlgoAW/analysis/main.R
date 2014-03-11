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
    "reshape2", # needed for melt
    "MASS",
    "devtools" # for github installs
    )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if (!"gbm" %in% installed.packages()) {
  install_url("http://cran.r-project.org/src/contrib/Archive/gbm/gbm_2.0-8.tar.gz")
}

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
  heart.plots.dir <- makeDir(getFullPlotPath("heart/"))
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
  brain.plots.dir <- makeDir(getFullPath("plots/brain/"))
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

main.foreforebrain <- function(){
  # forebrain
  forebrain.data.dir <- makeDir(getFullPath("data/forebrain/"))
  forebrain.mldata <- paste(forebrain.data.dir,"dataForPred.tab",sep="")
  forebrain.mlresults <- paste(forebrain.data.dir,"mlResults.tab",sep="")
  forebrain.plots.dir <- makeDir(getFullPlotPath("forebrain/"))
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

modelGBM <- function(){

  df.list <- list("forebrain"=cleanMouseForebrain(),
                  "brain"=cleanMouseBrain(),
                  "heart"=cleanMouseHeart() )

  dir.list <- list("forebrain"= makeDir(getFullPath("plots/gbm/forebrain")),
                   "brain"=makeDir(getFullPath("plots/gbm/brain")),
                   "heart"=makeDir(getFullPath("plots/gbm/heart")) )

  cols.list <- list("forebrain"=getForebrainCols(),
                    "brain"=getBrainCols(),
                    "heart"=getHeartCols() )

  for(tissue in c("heart", "forebrain", "brain")){
    runGbmOnDataSet(df=df.list[[tissue]],cols=cols.list[[tissue]],outdir=dir.list[[tissue]])
  }
}

main <- function(){
  cat("******************************** running heart...\n")
  main.heart()

  cat("******************************** running forebrain...\n")
  main.foreforebrain()

  cat("******************************** done...\n")
}
