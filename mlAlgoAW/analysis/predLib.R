
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

getStatsFromGlmModel <- function(probs, y, knn=FALSE){
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


saveFunArgs <- function(fnCall,verbose=TRUE,env=parent.frame(),
                        file="~/sandbox/objects.R",append=FALSE){

  fnCall <- standardise_call(fnCall)
  stopifnot(is.call(fnCall))

  if(identical(append,TRUE)){
  append.file <- file(file, open="a")
  } else {
    append.file <- file(file, open="w")


  }
  values <- as.character(fnCall[-1])
  variables <- names(fnCall)[-1]
  call.list <- as.list(fnCall)[-1]

  if(verbose){
    print(fnCall)
    print(paste0(variables, " = ", values, " #", sapply(fnCall[-1], typeof)))
  }

  dput(date(), file = append.file)
  dput(fnCall, file = append.file)

  for(i in which(variables != "")){
   # val.local <- ifelse(is.language(call.list[i][[1]]),eval(parse(text=values[i]), env),               call.list[i][[1]])

    if(is.language(call.list[i][[1]])){val.local <- eval(parse(text=values[i]), env)}else{val.local <-   call.list[i][[1]]}

    assign(variables[i], val.local, env)
    var.char <- variables[i]
    cat(paste(var.char, " = "),file=append.file)
    dput(eval(as.name(var.char),env), file=append.file)
  }

  cat(paste(fnCall[[1]],"(",paste0(variables, collapse=","), ")",sep=""),file=append.file)
  cat("\n\n\n", file=append.file)

}

testSaveFunArgs <- function(){
y<- 3
callExpr <- quote(runif(n=1 + y, min=dim(iris)[1], max=dim(iris)[1] + 1))
saveFunArgs(fnCall=callExpr,verbose=FALSE,
            file = "~/sandbox/objects2.R")

}












############################################






evalFunArgs <- function(fnCall,verbose=TRUE,env=parent.frame()){
  fnCall <- standardise_call(fnCall)
  stopifnot(is.call(fnCall))

  values <- as.character(fnCall[-1])
  variables <- names(fnCall)[-1]
  call.list <- as.list(fnCall)[-1]

  if(verbose){
    print(fnCall)
    print(paste0(variables, " = ", values, " #", sapply(fnCall[-1], typeof)))
  }
  for(i in which(variables != "")){
    val.local <- ifelse(is.language(call.list[i][[1]]),
                        eval(parse(text=values[i]), env),
                        call.list[i][[1]])
    assign(variables[i], val.local, env)
  }
}

#fnCall <- quote(read.csv("imp", header=one() * 4, sep=as.character(header)))
#evalFunArgs(fnCall)



#library(pryr)


standardise_call <- function(call, env = parent.frame()){
  stopifnot(is.call(call))
  fn <- eval(call[[1]], env)
  if(is.primitive(fn)) return(fn)
  match.call(fn, call)
}
modify_call <- function(call, new_args) {
  call <- standardise_call(call)
  nms <- names(new_args) %||% rep("", length(new_args))
  if (any(nms == "")) {
    stop("All new arguments must be named", call. = FALSE)
  }
  for(nm in nms) {
    call[[nm]] <- new_args[[nm]]
  }
  call
}


removeMaxFiles <- function(checkFile){
  # TODO figure out what is going on here...
  
  #mb.size <-  (file.info(checkFile)$size)/(1000 * 1000)
  #if (mb.size > 450){
  #  file.remove(checkFile)
  #}
}


applyGsubVec <- function(x,pattern,replacement){
  sapply(x,function(y)gsub(x=y, pattern=pattern, replacement=replacement))
  
}




