
runMlAlgos <- function(df,cols){
  #   cols <- names(df)[8:22]
  # don't use -> 
  # TODO -> figure out exactly why glm has problems resolving subset=train -> eval in wrong envir?
  return(0)
  train <<- sample(seq_along(df$label), 0.7*dim(df)[1])
  train.X <- as.matrix(df[train,cols])
  test.X <- as.matrix(df[-train,cols])
  y.train <- df$label[train]
  y.test <- df$label[-train]
  y.freq0 <- length(which(y.train == 0))/length(y.train)
  y.freq1 <- length(which(y.train == 1))/length(y.train)
  weight.train <<- ifelse(y.train== 1, 1/y.freq1, 1/y.freq0) 
  grid = 10 ^ seq(10, -2, length = 100)
  scaleDistro <- TRUE
  
  form <- paste("label ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  glm.main <- glm(form , data=df, family = binomial,subset=train)
  glm.main.stats = getStatsFromGlmModel(predict(glm.main,df[-train,], type="response"), as.numeric(df$label[-train]) - 1) 
  
  
  
  form.cross <- paste("label ~", do.call(paste, c(as.list(do.call(paste, c(expand.grid(cols, cols), sep=":"))), sep=" + ")))
  # use this formula
  glm.cross.weights.stats <- getStatsFromGlmModel(predict(glm(form.cross, data = df[train,], family=binomial, weights = weight.train),
                                                          df[-train,], type="response"), y.test) 
  
  glm.cross.stats <- getStatsFromGlmModel(predict(glm(form.cross, data = df, family=binomial,subset = train) ,
                                                  df[-train,], type="response"), y.test) 
  
  
  lda.stats <- getStatsFromGlmModel(predict(lda(x=train.X,grouping=y.train),test.X,type="response")$x,y.test)
  qda.stats <- getStatsFromGlmModel(predict(qda(x=train.X,grouping=y.train),test.X,type="response")$posterior[,2],y.test)
  
  
  ridge.weight.stats <- getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=0, lambda=grid,weights=weight.train,standardize=scaleDistro),
                                                      s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                                      newx=test.X, type="response")  , 
                                              y.test)
  ridge.stats <- getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=0, lambda=grid,standardize=scaleDistro),
                                               s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                               newx=test.X, type="response")  , 
                                       y.test)
  lasso.weight.stats <- getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=1, lambda=grid,weights=weight.train,standardize=scaleDistro),
                                                      s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                                      newx=test.X, type="response")  , 
                                              y.test)
  
  lasso.stats <- getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=1, lambda=grid,standardize=scaleDistro),
                                               s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                               newx=test.X, type="response")  , 
                                       y.test)
  
  random.forest <- getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.train),x=df[train,cols]),df[-train,cols])) - 1,
                                         y.test)
  
  random.forest.bag <- getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.train),x=df[train,cols],mtry=length(cols),importance=TRUE),
                                                                df[-train,cols])) - 1,
                                         y.test)
  
  form.boost <- paste("label_factor ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  
  df$label_factor <- factor(df$label)
  
  glm.boost <- getStatsFromGlmModel(predict(
    glmboost(formula(form.boost), data = df[train,], family = Binomial(link="logit")), 
    df[-train,], 
    type="response"),
    y.test)
  
  gam.boost <- getStatsFromGlmModel(predict(
    gamboost(formula(form.boost), data = df[train,], family = Binomial(link="logit")), 
    df[-train,], 
    type="response"),
    y.test)
  
  
  
  df.out    <- rbind(glm.main.stats, glm.cross.weights.stats,glm.cross.stats,lda.stats,qda.stats,ridge.weight.stats,ridge.stats,lasso.weight.stats,lasso.stats,glm.boost,gam.boost)
  df.out$algo <- c("glm.main.stats","glm.cross.weights.stats" , "glm.cross.stats" ,"lda","qda","ridge.weight.stats","ridge.stats","lasso.weight.stats","lasso.stats","glm.boost","gam.boost")
  df.out
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
  
  algo.list <- list(glm.cross.weights.stats = getStatsFromGlmModel(predict(glm(form.cross, data = df[trainVec,], family=binomial, weights = weight.trainVec),
                                                                           df[-trainVec,], type="response"), y.test) ,
                    
                    glm.cross.stats = getStatsFromGlmModel(predict(glm(form.cross, data = df[trainVec,], family=binomial ) ,
                                                                   df[-trainVec,], type="response"), y.test) ,
                    
                    
                    lda.stats = getStatsFromGlmModel(predict(lda(x=trainVec.X,grouping=y.trainVec),test.X,type="response")$x,y.test),
                    qda.stats = getStatsFromGlmModel(predict(qda(x=trainVec.X,grouping=y.trainVec),test.X,type="response")$posterior[,2],y.test),
                    
                    
                    ridge.weight.stats = getStatsFromGlmModel( predict(glmnet(trainVec.X,y.trainVec,family="binomial", alpha=0, lambda=grid,weights=weight.trainVec,standardize=scaleDistro),
                                                                       s=cv.glmnet(trainVec.X,y.trainVec,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                                                       newx=test.X, type="response")  , 
                                                               y.test),
                    ridge.stats = getStatsFromGlmModel( predict(glmnet(trainVec.X,y.trainVec,family="binomial", alpha=0, lambda=grid,standardize=scaleDistro),
                                                                s=cv.glmnet(trainVec.X,y.trainVec,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                                                newx=test.X, type="response")  , 
                                                        y.test),
                    lasso.weight.stats = getStatsFromGlmModel( predict(glmnet(trainVec.X,y.trainVec,family="binomial", alpha=1, lambda=grid,weights=weight.trainVec,standardize=scaleDistro),
                                                                       s=cv.glmnet(trainVec.X,y.trainVec,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                                                       newx=test.X, type="response")  , 
                                                               y.test),
                    
                    lasso.stats = getStatsFromGlmModel( predict(glmnet(trainVec.X,y.trainVec,family="binomial", alpha=1, lambda=grid,standardize=scaleDistro),
                                                                s=cv.glmnet(trainVec.X,y.trainVec,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                                                newx=test.X, type="response")  , 
                                                        y.test),
                    
                    random.forest = getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.trainVec),x=df[trainVec,cols]),df[-trainVec,cols])) - 1,
                                                          y.test),
                    
                    random.forest.bag = getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.trainVec),x=df[trainVec,cols],mtry=length(cols),importance=TRUE),
                                                                                  df[-trainVec,cols])) - 1,
                                                               y.test),
                    
                    
                    gam = getStatsFromGlmModel(predict(
                      gam(formula(form), data = df[trainVec,], family = binomial), 
                      df[-trainVec,], 
                      type="response"),
                      y.test) , 
                    
                    
                    glm.boost = getStatsFromGlmModel(predict(
                      glmboost(formula(form.boost), data = df[trainVec,], family = Binomial(link="logit")), 
                      df[-trainVec,], 
                      type="response"),
                      y.test),
                    
                    gam.boost = getStatsFromGlmModel(predict(
                      gamboost(formula(form.boost), data = df[trainVec,], family = Binomial(link="logit")), 
                      df[-trainVec,], 
                      type="response"),
                      y.test), 
                    
                    gbm.boost.depth4 = getStatsFromGlmModel(predict(
                      gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=7,
                          n.trees=1000, interaction.depth=4,verbose=FALSE), 
                      df[-trainVec,], 
                      type="response",n.trees=1000),
                      y.test),
                    
                    gbm.boost.depth2 = getStatsFromGlmModel(predict(
                        gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=7, 
                            n.trees=1000, interaction.depth=2,verbose=FALSE), 
                        df[-trainVec,], 
                        type="response",n.trees=1000),
                        y.test),
                   
                    
                    gbm.id1.cv3 = getStatsFromGlmModel(predict(gbm1 <- gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3, 
                                                             n.trees=10000, interaction.depth=1,verbose=FALSE,shrinkage=0.001), 
                                                 df[-trainVec,], 
                                                 type="response",n.trees=gbm.perf(gbm1,method="cv")),
                                         y.test),
                    
                    gbm.id2.cv3 = getStatsFromGlmModel(predict(gbm2 <- gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3, 
                                                                           n.trees=10000, interaction.depth=2,verbose=FALSE,shrinkage=0.001), 
                                                               df[-trainVec,], 
                                                               type="response",n.trees=gbm.perf(gbm2,method="cv")),
                                                       y.test),
                    
                    gbm.id3.cv3 = getStatsFromGlmModel(predict(gbm3 <- gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3, 
                                                                           n.trees=10000, interaction.depth=3,verbose=FALSE,shrinkage=0.001), 
                                                               df[-trainVec,], 
                                                               type="response",n.trees=gbm.perf(gbm3,method="cv")),
                                                       y.test),
                    
                    gbm.id4.cv3 = getStatsFromGlmModel(predict(gbm4 <- gbm(formula(form), data = df[trainVec,], distribution="bernoulli", cv.folds=3, 
                                                                           n.trees=10000, interaction.depth=4,verbose=FALSE,shrinkage=0.001), 
                                                               df[-trainVec,], 
                                                               type="response",n.trees=gbm.perf(gbm4,method="cv")),
                                                       y.test),
                    
                    
                    
                    C5.0.boost = getStatsFromGlmModel(predict(
                       C5.0(formula(form.boost), data = df[trainVec,],trials=100), 
                            newdata=df[-trainVec,], type="prob",trials=5)[,2],
                       y.test),
                    
                    
                    flex.disc.anyl = getStatsFromGlmModel(predict(
                      fda(formula(form), data=df[trainVec,]),
                            newdata=df[-trainVec,], type="posterior")[,2],
                      y.test)
                    
  ) # END for algo.list
  
  
  #df.out    <- rbind(glm.main.stats, glm.cross.weights.stats,glm.cross.stats,lda.stats,qda.stats,ridge.weight.stats,ridge.stats,lasso.weight.stats,lasso.stats,glm.boost,gam.boost)
  #df.out$algo <- c("glm.main.stats","glm.cross.weights.stats" , "glm.cross.stats" ,"lda","qda","ridge.weight.stats","ridge.stats","lasso.weight.stats","lasso.stats","glm.boost","gam.boost")
  df.out <- do.call(rbind,lapply(names(algo.list), function(x){algo.list[[x]]$algo <- x; algo.list[[x]]} ))
  
  df.out
}



accumMlAlgos <- function(df,cols,trials,resultFile,seed=42){
  if (missing(trials)){
    trials <- 30
  }
  
  # set the random seed -> make sure training set consistent between runs...
  set.seed(seed)
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


# eval errors w/ test case

irisExample <- function(){
  train = sample(seq_along(iris[,1]),0.7*dim(iris)[1])
  iris$label <- ifelse(iris$Species == "setosa", 1, 0)
  form <- "label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
  glm(formula = form, family = binomial, data = iris[train, ])
  
  glm(formula = form, family = binomial, data = iris, subset = train)
}

