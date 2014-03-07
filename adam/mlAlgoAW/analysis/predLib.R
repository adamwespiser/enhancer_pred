
#global assignment of project dir -> change to whatever in order to find plots/data/etc.. files


calcAUC <- function(prob, label){
  AUC <- NA
#  if(!identical(getLevels(label),2)){
#    return(NA)
#  }
  AUC <- try({
    (performance(prediction(predictions=prob, labels=label), "auc"))@y.values[[1]]
  })
  if ('try-error' %in% class(AUC)){
    NA
  } else {
    AUC
  }
}

getStatsFromGlmModel <- function(probs, y,knn=FALSE){
  if (TRUE == knn){ 
    pred <- as.numeric(probs) - 1 
  } else {
    pred <- rep(0,length(probs))
    pred[which(probs > 0.5)] <- 1
  }
  
  correct <- (pred == y)
  poly2 <- data.frame(trial=-1)
  poly2$TP <- length(which(correct & y ==1))
  poly2$TN <- length(which(correct & y ==0))  
  poly2$FP <- length(which(!correct & y ==0))  
  poly2$FN <- length(which(!correct & y ==1))  
  poly2$prec <- with(poly2, TP / (TP + FP))
  poly2$sens <- with(poly2, TP / (TP + FN))
  poly2$errorRate <-  1 - sum(correct)/length(correct)
  if (TRUE == knn){ 
    poly2$AUC <- 0
  } else {
    poly2$AUC <- calcAUC(prob=probs, label=y)
  }
  poly2
}


makeDir <- function(dir,recursiveCreate=TRUE){
  if (!file.exists(dir)){
    dir.create(path=dir,showWarnings=TRUE,recursive=recursiveCreate,mode="0755")
  }
  dir
}

getMemory <- function(){
  gettextf("%.2f Mb stored in memory",
           sum(sapply(unlist(ls(envir=.GlobalEnv)), 
                      function(x)object.size(get(x,envir=.GlobalEnv))))
           / (1000000))
}