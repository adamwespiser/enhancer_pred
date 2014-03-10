run.glm.cross.weights <- function(form.cross, trainData, testData, weight.trainVec, y.test){
  cat("running run.glm.cross.weights\n")
  return(getStatsFromGlmModel(
    predict(glm(form.cross,
                data = trainData,
                family=binomial,
                weights = weight.trainVec),
            testData,
            type="response"),
    y.test))
}

run.glm.cross <- function(form.cross, trainData, testData,  y.test){
  cat("running run.glm.cross\n")
  return(getStatsFromGlmModel(
    predict(glm(form.cross,
                data = trainData,
                family=binomial),
            testData,
            type="response"),
    y.test))
}

run.lda <- function(trainVec.X, y.trainVec, test.X, y.test){
  cat("running run.lda\n")
  return(getStatsFromGlmModel(
    predict(lda(x=trainVec.X,
                grouping=y.trainVec),
            test.X,
            type="response")$x,
    y.test))
}

run.qda <- function(trainVec.X, y.trainVec, test.X, y.test){
  cat("running run.qda\n")
  return(getStatsFromGlmModel(
    predict(qda(x=trainVec.X,
                grouping=y.trainVec),
            test.X,
            type="response")$posterior[,2],
    y.test))
}

run.ridge.weight <- function(trainVec.X, y.trainVec, grid, weight.trainVec, scaleDistro, test.X, y.test){
  cat("running run.ridge.weight\n")
  return(getStatsFromGlmModel(
    predict(glmnet(trainVec.X,
                   y.trainVec,
                   family="binomial",
                   alpha=0,
                   lambda=grid,
                   weights=weight.trainVec,
                   standardize=scaleDistro),
            s=cv.glmnet(trainVec.X,
                        y.trainVec,
                        family="binomial",
                        type.measure="auc",
                        alpha=0,
                        standardize=scaleDistro)$lambda.min,
            newx=test.X,
            type="response"),
    y.test))
}

run.ridge <- function(trainVec.X, y.trainVec, grid, scaleDistro, test.X, y.test){
  cat("running run.ridge\n")
  return(getStatsFromGlmModel(
    predict(glmnet(trainVec.X,
                   y.trainVec,
                   family="binomial",
                   alpha=0,
                   lambda=grid,
                   standardize=scaleDistro),
            s=cv.glmnet(trainVec.X,
                        y.trainVec,
                        family="binomial",
                        type.measure="auc",
                        alpha=0,
                        standardize=scaleDistro)$lambda.min,
            newx=test.X,
            type="response"),
    y.test))
}

run.lasso.weight <- function(trainVec.X, y.trainVec, grid, weight.trainVec, scaleDistro, test.X, y.test){
  cat("running run.lasso.weight\n")
  return(getStatsFromGlmModel(
    predict(glmnet(trainVec.X,
                   y.trainVec,
                   family="binomial",
                   alpha=1,
                   lambda=grid,
                   weights=weight.trainVec,
                   standardize=scaleDistro),
            s=cv.glmnet(trainVec.X,
                        y.trainVec,
                        family="binomial",
                        type.measure="auc",
                        alpha=1,
                        standardize=scaleDistro)$lambda.min,
            newx=test.X,
            type="response"),
    y.test))
}

run.lasso <- function(trainVec.X, y.trainVec, grid, scaleDistro, test.X, y.test){
  cat("running run.lasso\n")
  return(getStatsFromGlmModel(
    predict(glmnet(trainVec.X,
                   y.trainVec,
                   family="binomial",
                   alpha=1,
                   lambda=grid,
                   standardize=scaleDistro),
            s=cv.glmnet(trainVec.X,
                        y.trainVec,
                        family="binomial",
                        type.measure="auc",
                        alpha=1,
                        standardize=scaleDistro)$lambda.min,
            newx=test.X,
            type="response"),
    y.test))
}

run.random.forest <- function(y.trainVec, trainData, testData,  cols, y.test){
  cat("running run.random.forest\n")
  return(getStatsFromGlmModel(as.numeric(
    predict(
      randomForest(
        y=factor(y.trainVec),
        x = trainData[, cols]),
      testData[,cols])) - 1,
    y.test))
}

run.random.forest.bag <- function(y.trainVec, trainData, testData,  cols, y.test){
  cat("running run.random.forest.bag\n")
  return(getStatsFromGlmModel(as.numeric(
    predict(
      randomForest(
        y=factor(y.trainVec),
        x=trainData[,cols],
        mtry=length(cols),importance=TRUE),
      testData[,cols])) - 1,
    y.test))
}

run.gam <- function(form, trainData, testData,  y.test){
  cat("running run.gam\n")
  return(getStatsFromGlmModel(
    predict(
      gam(formula(form),
          data = trainData,
          family = binomial),
      testData,
      type="response"),
    y.test))
}

run.glm.boost <- function(form.boost, trainData, testData,  y.test){
  cat("running run.glm.boost\n")
  return(getStatsFromGlmModel(
    predict(
      glmboost(formula(form.boost),
               data = trainData,
               family = Binomial(link="logit")),
      testData,
      type="response"),
    y.test))
}

run.gam.boost <- function(form.boost, trainData, testData,  y.test){
  cat("running run.gam.boost\n")
  return(getStatsFromGlmModel(
    predict(
      gamboost(formula(form.boost),
               data = trainData,
               family = Binomial(link="logit")),
      testData,
      type="response"),
    y.test))
}

run.gbm.boost.depth2 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.boost.depth2\n")
  return(getStatsFromGlmModel(
    predict(
      gbm(formula(form),
          data = trainData,
          distribution="bernoulli",
          cv.folds=7,
          n.trees=1000,
          interaction.depth=2,
          verbose=FALSE),
      testData,
      type="response",
      n.trees=1000),
    y.test))
}

run.gbm.boost.depth4 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.boost.depth4\n")
  return (getStatsFromGlmModel(
    predict(
      gbm(formula(form), 
          data = trainData, 
          distribution="bernoulli", 
          cv.folds=7,
          n.trees=1000, 
          interaction.depth=4,
          verbose=FALSE), 
      testData, 
      type="response", 
      n.trees = 1000),
    y.test))
}

run.gbm.id1.cv3 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.id1.cv3\n")
  return(getStatsFromGlmModel(
    predict(gbm1 <- gbm(formula(form),
                        data = trainData,
                        distribution="bernoulli",
                        cv.folds=3,
                        n.trees=10000,
                        interaction.depth=1,
                        verbose=FALSE,
                        shrinkage=0.001),
            testData,
            type="response",
            n.trees=gbm.perf(gbm1,method="cv",plot.it = FALSE)),
    y.test))
}

run.gbm.id2.cv3 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.id2.cv3\n")
  return(getStatsFromGlmModel(
    predict(gbm2 <- gbm(formula(form),
                        data = trainData,
                        distribution="bernoulli",
                        cv.folds=3,
                        n.trees=10000,
                        interaction.depth=2,
                        verbose=FALSE,
                        shrinkage=0.001),
            testData,
            type="response",
            n.trees=gbm.perf(gbm2,method="cv",plot.it = FALSE)),
    y.test))
}

run.gbm.id3.cv3 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.id3.cv3\n")
  return(getStatsFromGlmModel(
    predict(gbm3 <- gbm(formula(form),
                        data = trainData,
                        distribution="bernoulli",
                        cv.folds=3,
                        n.trees=10000,
                        interaction.depth=3,
                        verbose=FALSE,
                        shrinkage=0.001),
            testData,
            type="response",
            n.trees=gbm.perf(gbm3,method="cv",plot.it = FALSE)),
    y.test))
}

run.gbm.id4.cv3 <- function(form, trainData, testData,  y.test){
  cat("running run.gbm.id4.cv3\n")
  return(getStatsFromGlmModel(
    predict(gbm4 <- gbm(formula(form),
                        data = trainData,
                        distribution="bernoulli",
                        cv.folds=3,
                        n.trees=10000,
                        interaction.depth=4,
                        verbose=FALSE,
                        shrinkage=0.001),
            testData,
            type="response",
            n.trees=gbm.perf(gbm4,method="cv",plot.it = FALSE)),
    y.test))
}

run.C5.0.boost <- function(form.bost, trainData, testData,  y.test){
  cat("running run.C5.0.boost\n")
  return(getStatsFromGlmModel(
    predict(
      C5.0(formula(form.boost),
           data = trainData,
           trials=100),
      newdata=testData,
      type="prob",
      trials=5)[,2],
    y.test))
}

run.flex.disc.anyl <- function(form, trainData, testData,  y.test){
  cat("running flex.disc.anyl\n")
  return(getStatsFromGlmModel(
    predict(
      fda(formula(form),
          data=trainData),
      newdata=testData,
      type="posterior")[,2],
    y.test))
}


runMlAlgosListVersion <- function(df,cols){
  #   cols <- names(df)[8:22]
  trainVec <- sample(seq_along(df$label), 0.7*dim(df)[1])
  trainVecForGlm <- trainVec
  trainVec.X <- as.matrix(df[trainVec,cols])
  test.X <- as.matrix(df[-trainVec,cols])
  y.trainVec <- df$label[trainVec]
  y.test <- df$label[-trainVec]
  y.freq0 <- length(which(y.trainVec == 0))/length(y.trainVec)
  y.freq1 <- length(which(y.trainVec == 1))/length(y.trainVec)
  weight.trainVec <<- ifelse(y.trainVec== 1, 1/y.freq1, 1/y.freq0)
  grid = 10 ^ seq(10, -2, length = 100)
  scaleDistro <- TRUE
  
  form <- paste("label ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  #hierarchical model -> params fitted seperately for each species
  form.species <-  paste("label ~  original_species/( ",do.call(paste, c(as.list(cols), sep=" + ")),")")
  glm.main <- glm(form , data=df[trainVec,], family = binomial)
  glm.main.stats = getStatsFromGlmModel(predict(glm.main,df[-trainVec,], type="response"), as.numeric(df$label[-trainVec]) - 1)
  
  form.cross <- paste("label ~", do.call(paste, c(as.list(do.call(paste, c(expand.grid(cols, cols), sep=":"))), sep=" + ")))
  
  form.boost <- paste("label_factor ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  # form.boost <- paste("label_factor ~   original_species/(", do.call(paste, c(as.list(cols), sep=" + ")),")")
  
  df$label_factor <- factor(df$label)
  
  # use this formula
  
  trainData <- df[trainVec,]
  testData <- df[-trainVec,]
  
  algo.list <- list(
    glm.cross.weights.stats = run.glm.cross.weights(form.cross, trainData, testData, weight.trainVec, y.test),
    glm.cross.stats = run.glm.cross(form.cross, trainData, testData, y.test),
    lda.stats = run.lda(trainVec.X, y.trainVec, test.X, y.test),
    qda.stats = run.qda(trainVec.X, y.trainVec, test.X, y.test),
    ridge.weight.stats = run.ridge.weight(trainVec.X, y.trainVec, grid, weight.trainVec, scaleDistro, test.X, y.test),
    ridge.stats = run.ridge(trainVec.X, y.trainVec, grid, scaleDistro, test.X, y.test),
    lasso.weight.stats = run.lasso.weight(trainVec.X, y.trainVec, grid, weight.trainVec, scaleDistro, test.X, y.test),
    lasso.stats = run.lasso(trainVec.X, y.trainVec, grid, scaleDistro, test.X, y.test),
    random.forest = run.random.forest(y.trainVec, trainData, testData, cols, y.test),
    random.forest.bag = run.random.forest.bag(y.trainVec, trainData, testData, cols, y.test),
    gam = run.gam(form, trainData, testData, y.test),
    glm.boost = run.glm.boost(form.boost, trainData, testData, y.test),
    gam.boost = run.gam.boost(form.boost, trainData, testData, y.test),
    gbm.boost.depth2 = run.gbm.boost.depth2(form, trainData, testData, y.test),
    gbm.boost.depth4 = run.gbm.boost.depth4(form, trainData, testData, y.test),
    gbm.id1.cv3 = run.gbm.id1.cv3(form, trainData, testData, y.test),
    gbm.id2.cv3 = run.gbm.id2.cv3(form, trainData, testData, y.test),
    gbm.id3.cv3 = run.gbm.id3.cv3(form, trainData, testData, y.test),
    gbm.id4.cv3 = run.gbm.id4.cv3(form, trainData, testData, y.test),
    C5.0.boost = run.C5.0.boost(form.bost, trainData, testData, y.test),
    flex.disc.anyl = run.flex.disc.anyl(form, trainData, testData, y.test)
  ) # END for algo.list
  
  
  #df.out    <- rbind(glm.main.stats, glm.cross.weights.stats,glm.cross.stats,lda.stats,qda.stats,ridge.weight.stats,ridge.stats,lasso.weight.stats,lasso.stats,glm.boost,gam.boost)
  #df.out$algo <- c("glm.main.stats","glm.cross.weights.stats" , "glm.cross.stats" ,"lda","qda","ridge.weight.stats","ridge.stats","lasso.weight.stats","lasso.stats","glm.boost","gam.boost")
  df.out <- do.call(rbind,lapply(names(algo.list), function(x){algo.list[[x]]$algo <- x; algo.list[[x]]} ))
  
  df.out
}



accumMlAlgos <- function(df,cols,trials,resultFile,seed=412){
  if (missing(trials)){
    trials <- 30
  }
  
  # set the random seed -> make sure training set consistent between runs...
  set.seed(seed)
  
  ########################################## START DELETE ME ###############
  #TODO: delete me
  runMlAlgosListVersion(df,cols)
  stop("aborting before trials; remove me (purcaro)")
  ########################################## END DELETE ME ###############
  
  collect.df <- do.call(rbind,foreach(i = 1:trials) %dopar% {
    print(paste("trial = " ,i,sep=""))
    
    tryCatch({df.out <- runMlAlgosListVersion(df,cols)
              df.out$trial <- i
              #df.accum <- rbind(df.out, df.accum)},
              df.out},
             error = function(e) {
               print(e)
               #stop(e)
             }, finally=print(""))
  })
  #
  if(!missing(resultFile)){
    exportAsTable(collect.df, resultFile)
  }
  collect.df
}

# optimize gbm

getBestGBMparams <- function(df,cols, outdir){
  trainVec <- sample(seq_along(df$label), 0.7*dim(df)[1])
  trainVecForGlm <- trainVec
  trainVec.X <- as.matrix(df[trainVec,cols])
  test.X <- as.matrix(df[-trainVec,cols])
  y.trainVec <- df$label[trainVec]
  y.test <- df$label[-trainVec]
  y.freq0 <- length(which(y.trainVec == 0))/length(y.trainVec)
  y.freq1 <- length(which(y.trainVec == 1))/length(y.trainVec)
  weight.trainVec <<- ifelse(y.trainVec== 1, 1/y.freq1, 1/y.freq0)
  grid = 10 ^ seq(10, -2, length = 100)
  scaleDistro <- TRUE
  form <- paste("label ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  form.species <-  paste("label ~  original_species/( ",do.call(paste, c(as.list(cols), sep=" + ")),")")
  form.cross <- paste("label ~", do.call(paste, c(as.list(do.call(paste, c(expand.grid(cols, cols), sep=":"))), sep=" + ")))
  form.boost <- paste("label_factor ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  # form.boost <- paste("label_factor ~   original_species/(", do.call(paste, c(as.list(cols), sep=" + ")),")")
  df$label_factor <- factor(df$label)
  
  
  gbm1 <-  gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3,
               n.trees=10000, interaction.depth=1,verbose=FALSE)
  
  model1.stats <- getStatsFromGlmModel(predict(gbm1,
                                               df[-trainVec,],
                                               type="response",n.trees=gbm.perf(gbm1,method="cv")),
                                       y.test)
  
  gbm1.sum <- summary(gbm1)
  gbm1.colRank <- as.numeric(unlist(sapply(cols, function(x)which( as.character((gbm1.sum)$var) == x))))
  
  cumsum(rev(summary(gbm1)$rel.inf))/sum(summary(gbm1)$rel.inf)
  
  ggplot(summary(gbm1), aes(x=var,y=rel.inf))+geom_histogram(stat="identity")+coord_flip()
  
  cum.rel.inf <- cumsum(rev(summary(gbm1)$rel.inf))/sum(summary(gbm1)$rel.inf)
  cols.model2 <- cols[-which(cum.rel.inf < 0.02)]
  form.model2 <- paste("label ~   ",do.call(paste, c(as.list(cols.model2), sep=" + ")))
  
  gbm2 <-  gbm(formula(form.model2), data = df[trainVec,], distribution="bernoulli",
               cv.folds=3,
               n.trees=10000,
               interaction.depth=2,verbose=FALSE)
  
  model2.stats <- getStatsFromGlmModel(predict(gbm2,
                                               df[-trainVec,],
                                               type="response",n.trees=gbm.perf(gbm2,method="cv")),
                                       y.test)
  
  
  ## OOB estimate is warned against -> going with cv to train on the number of trees
  
  gbm3 <-  gbm(formula(form), data = df[trainVec,], distribution="adaboost", cv.folds=3,
               n.trees=10000, interaction.depth=2,verbose=FALSE)
  
  model3.stats <- getStatsFromGlmModel(predict(gbm3,
                                               df[-trainVec,],
                                               type="response",n.trees=gbm.perf(gbm3,method="cv")),
                                       y.test)
  
  
  
  model1.stats <- getStatsFromGlmModel(predict(gbm1 <- gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3,
                                                           n.trees=4000, interaction.depth=1,verbose=FALSE,shrinkage=0.01),
                                               df[-trainVec,],
                                               type="response",n.trees=gbm.perf(gbm1,method="cv")),
                                       y.test)
  
  
  
  
}


runGbmOnDataSet <- function(df,cols,outdir){
  form <- paste("label ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  
  gbm.call <-  quote(gbm(formula(form), data = df, distribution="bernoulli", cv.folds=3,
                         n.trees=10000, interaction.depth=4, verbose=FALSE))
  gbm.fn <- eval(gbm.call[[1]],parent.frame())
  gbm.match <-  match.call(gbm.fn,gbm.call)
  gbm.fn.string <- paste("gbm(",do.call(paste,c(as.list(paste0(names(gbm.match),"=",as.character(gbm.match))[-1]),sep=", "  )), ")",sep="")
  
  gbm.model <- eval(gbm.call)
  gbm.model.sum <- summary(gbm.model)
  
  ggplot(gbm.model.sum, aes(x=var,y=rel.inf))+geom_histogram(stat="identity") + coord_flip()+
    ggtitle(gbm.fn.string)
  ggsave(paste(outdir,"gbmRelImp.pdf",sep="/"))
  
  pdf(paste(outdir,"gbmError.pdf",sep="/"),width=7,height=7)
  gbm.perf(gbm.model)
  dev.off()
  
  write(gbm.fn.string,file=paste(outdir,"gbmCall-Short.txt",sep="/"))
  
  
  fullCallFile <- file( paste(outdir,"gbmCall-fullSpecify.txt",sep="/"))
  saveFunArgs(fnCall = gbm.match,verbose=FALSE,    file =paste(outdir,"gbmCall-fullSpecify.txt",sep="/"))
  removeMaxFiles(paste(outdir,"gbmCall-fullSpecify.txt",sep="/"))
  close(fullCallFile)
}


# eval errors w/ test case

irisExample <- function(){
  train = sample(seq_along(iris[,1]),0.7*dim(iris)[1])
  iris$label <- ifelse(iris$Species == "setosa", 1, 0)
  form <- "label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
  glm(formula = form, family = binomial, data = iris[train, ])
  
  glm(formula = form, family = binomial, data = iris, subset = train)
}

