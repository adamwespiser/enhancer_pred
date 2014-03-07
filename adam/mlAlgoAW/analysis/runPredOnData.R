
runMlAlgos <- function(df,cols){
  #   cols <- names(df)[8:22]
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
    glmboost(formula(form.boost), data = df, family = Binomial(link="logit")), 
    df[-train,], 
    type="response"),
    y.test)
  
  gam.boost <- getStatsFromGlmModel(predict(
    gamboost(formula(form.boost), data = df, family = Binomial(link="logit")), 
    df[-train,], 
    type="response"),
    y.test)
  
  
  
  df.out    <- rbind(glm.main.stats, glm.cross.weights.stats,glm.cross.stats,lda.stats,qda.stats,ridge.weight.stats,ridge.stats,lasso.weight.stats,lasso.stats,glm.boost,gam.boost)
  df.out$algo <- c("glm.main.stats","glm.cross.weights.stats" , "glm.cross.stats" ,"lda","qda","ridge.weight.stats","ridge.stats","lasso.weight.stats","lasso.stats","glm.boost","gam.boost")
  df.out
}


runMlAlgosListVersion <- function(df,cols){
  #   cols <- names(df)[8:22]
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
  
  form.boost <- paste("label_factor ~   ",do.call(paste, c(as.list(cols), sep=" + ")))
  
  df$label_factor <- factor(df$label)
  
  # use this formula
  
  algo.list <- list(glm.cross.weights.stats = getStatsFromGlmModel(predict(glm(form.cross, data = df[train,], family=binomial, weights = weight.train),
                                                                           df[-train,], type="response"), y.test) ,
                    
                    glm.cross.stats = getStatsFromGlmModel(predict(glm(form.cross, data = df, family=binomial,subset = train) ,
                                                                   df[-train,], type="response"), y.test) ,
                    
                    
                    lda.stats = getStatsFromGlmModel(predict(lda(x=train.X,grouping=y.train),test.X,type="response")$x,y.test),
                    qda.stats = getStatsFromGlmModel(predict(qda(x=train.X,grouping=y.train),test.X,type="response")$posterior[,2],y.test),
                    
                    
                    ridge.weight.stats = getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=0, lambda=grid,weights=weight.train,standardize=scaleDistro),
                                                                       s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                                                       newx=test.X, type="response")  , 
                                                               y.test),
                    ridge.stats = getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=0, lambda=grid,standardize=scaleDistro),
                                                                s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=0,standardize=scaleDistro)$lambda.min,
                                                                newx=test.X, type="response")  , 
                                                        y.test),
                    lasso.weight.stats = getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=1, lambda=grid,weights=weight.train,standardize=scaleDistro),
                                                                       s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                                                       newx=test.X, type="response")  , 
                                                               y.test),
                    
                    lasso.stats = getStatsFromGlmModel( predict(glmnet(train.X,y.train,family="binomial", alpha=1, lambda=grid,standardize=scaleDistro),
                                                                s=cv.glmnet(train.X,y.train,family="binomial",type.measure="auc", alpha=1,standardize=scaleDistro)$lambda.min,
                                                                newx=test.X, type="response")  , 
                                                        y.test),
                    
                    random.forest = getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.train),x=df[train,cols]),df[-train,cols])) - 1,
                                                          y.test),
                    
                    random.forest.bag = getStatsFromGlmModel( as.numeric(predict(randomForest(y=factor(y.train),x=df[train,cols],mtry=length(cols),importance=TRUE),
                                                                                  df[-train,cols])) - 1,
                                                               y.test),
                    
                    
                    glm.boost = getStatsFromGlmModel(predict(
                      glmboost(formula(form.boost), data = df, family = Binomial(link="logit")), 
                      df[-train,], 
                      type="response"),
                      y.test),
                    
                    gam.boost = getStatsFromGlmModel(predict(
                      gamboost(formula(form.boost), data = df, family = Binomial(link="logit")), 
                      df[-train,], 
                      type="response"),
                      y.test), 
                    
                    gbm.boost.depth4 = getStatsFromGlmModel(predict(
                      gbm(formula(form), data = df[train,], distribution="bernoulli", cv.folds=7,
                          n.trees=1000, interaction.depth=4,verbose=FALSE), 
                      df[-train,], 
                      type="response",n.trees=1000),
                      y.test),
                    
                    gbm.boost.depth2 = getStatsFromGlmModel(predict(
                        gbm(formula(form), data = df[train,], distribution="bernoulli", cv.folds=7, 
                            n.trees=1000, interaction.depth=2,verbose=FALSE), 
                        df[-train,], 
                        type="response",n.trees=1000),
                        y.test),
                   
                    C5.0.boost = getStatsFromGlmModel(predict(
                       C5.0(formula(form.boost), data = df[train,],trials=100), 
                            newdata=df[-train,], type="prob",trials=5)[,2],
                       y.test),
                    
                    
                    flex.disc.anyl = getStatsFromGlmModel(predict(
                      fda(formula(form), data=df[train,]),
                            newdata=df[-train,], type="posterior")[,2],
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
    
    tryCatch({df.out <-runMlAlgosListVersion(df,cols)
              df.out$trial <- i
              #df.accum <- rbind(df.out, df.accum)},
              df.out},
             error = function(e) print("error in Log Reg routine"), finally=print(""))
  })
  # 
  if(!missing(resultFile)){
    exportAsTable(collect.df, resultFile)
  }
  collect.df
}





