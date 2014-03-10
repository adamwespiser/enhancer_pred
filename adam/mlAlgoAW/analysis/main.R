script.dir <- function() {
  # from http://stackoverflow.com/a/16046056
  dirname(sys.frame(1)$ofile)
}
 
### Start with project dir, and helper functions
projectDir <- normalizePath(file.path(script.dir(), ".."))
getFullPath <- function(subpath){ file.path(projectDir, subpath) }
exportAsTable <- function(df, file){ write.table(df,file=file,quote=FALSE, row.names=FALSE,sep="\t") }
clear <- function(save.vec=c()){ ls.vec <- ls(globalenv());del.vec <-setdiff(ls.vec,c(save.vec,"clear")); rm(list=del.vec,pos=globalenv())}
readInTable <- function(file) read.table(file=file,stringsAsFactors=FALSE,header=TRUE)


# setup libs
# install.packages("ggplot2")  
# install.packages("ROCR") # http://cran.r-project.org/web/packages/ROCR/index.html
# install.packages("glmnet") # http://cran.r-project.org/web/packages/glmnet/glmnet.pdf  
# install.packages("randomForest") #http://cran.at.r-project.org/web/packages/randomForest/randomForest.pdf
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("mboost")
# install.packages("gbm")
# install.packages("vcd") # mosaicpl
# install.packages("C50") # kuhn:411
# install.packages("mda") # fda, kuhn:362
# install.packages("gam")
# install.packages("reshape2")
# install.packages("MASS")

#load libs
library(ggplot2)  
library(ROCR) # http://cran.r-project.org/web/packages/ROCR/index.html
library(glmnet) # http://cran.r-project.org/web/packages/glmnet/glmnet.pdf  
library(randomForest) #http://cran.at.r-project.org/web/packages/randomForest/randomForest.pdf
library(doParallel)
library(foreach)
library(mboost)
library(gbm)
library(vcd) # mosaicpl
library(C50) # kuhn:411
library(mda) # fda, kuhn:362
library(gam)
library(reshape2) # needed for melt
library(reshape2)
library(MASS)

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
  heart.plots.dir <- makeDir(getFullPath("plots/heart/"))
  heart.df <- cleanMouseHeart()
  exportAsTable(df=heart.df, file=heart.mldata)
  
  
  # exploritory analysis of hearts data
  exploritoryPlots(df=heart.df, cols=getHeartCols(), outdir=heart.plots.dir,msg="Heart Data -> explore")
  
  # run algorithms "trials" number of times -> save result
  heart.ml.df <- accumMlAlgos(df=heart.df,cols=getHeartCols(),
                              trials=30,resultFile=heart.mlresults)
  
  
  
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
  
  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=brain.ml.df, outdir = brain.plots.dir,msg="Brain data -> AW")
  
}

main.foreforebrain <- function(){
  # forebrain
  forebrain.data.dir <- makeDir(getFullPath("data/forebrain/"))
  forebrain.mldata <- paste(forebrain.data.dir,"dataForPred.tab",sep="")
  forebrain.mlresults <- paste(forebrain.data.dir,"mlResults.tab",sep="")
  forebrain.plots.dir <- makeDir(getFullPath("plots/forebrain/"))
  forebrain.df <- cleanMouseForebrain()
  exportAsTable(df=forebrain.df, file=forebrain.mldata)
  
  # exploritory analysis of hearts data
  exploritoryPlots(df=forebrain.df, cols=getForebrainCols(), outdir=forebrain.plots.dir,msg="Forebrain Data -> explore")
  
  # run algorithms "trials" number of times -> save result
  forebrain.ml.df <- accumMlAlgos(df=forebrain.df,cols=getForebrainCols(),
                                  trials=30,resultFile=forebrain.mlresults)
  
  # plot the results of each ml algo on the test/training divisions
  plotMlresults(df=forebrain.ml.df, outdir = forebrain.plots.dir,msg="Forebrain data -> AW") 
}

main <- function(){
  cat("and now for you, Mr.ScareCrow, \n")
  main.brain();
  
  cat("... one brain\n\n...and now for you, Mr. Lion")
  main.heart()
  cat("... one heart! now forebrain...")
  main.foreforebrain()
  cat("...and we are done!")
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


