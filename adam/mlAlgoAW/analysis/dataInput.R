homeFolder <- path.expand("~")
dataFolder <- file.path(homeFolder, "Dropbox", "enhancer_predictions")
mouse.heart.table <<- file.path(dataFolder, "mouse_heart_master_table_raw_scores.tab")
mouse.brain.table <<- file.path(dataFolder, "mouse_brain_master_table_raw_scores.tab")
mouse.forebrain.table <<- file.path(dataFolder, "mouse_brain_master_table_raw_scores.forebrain_positive_and_negative.txt")
human.brain.table <<- file.path(dataFolder, "human_brain_master_table_raw_scores.tab")
human.heart.table <<- file.path(dataFolder, "human_heart_master_table_raw_scores.tab")
mouse.test.forebrain  <- file.path(dataFolder, "mouse_test_forebrain_master_table_raw_scores.tab")
mouse.test.heart  <- file.path(dataFolder, "mouse_test_heart_master_table_raw_scores.tab")



cleanMouseHeart <- function(mouse.file=mouse.heart.table){
  heart.df <- read.csv(mouse.file,sep="\t")
  df.names.heart <- c("chr","start","end", "name", "label", "original_species", "tissue_possitive", "P300_vs_input",
                "H3K27ac_vs_input", "H3k04me1_vs_input","H3k04me3_vs_input","H3k09ac","H3k27ac","H3k27ac_vs_input",
                "H3k27me3","H3k36me3","H3k4me1","H3k4me3","H3k79me2","P300_vs_input2","pol2_vs_input","dgfNarrowPeak")
  
  origCols <- names(heart.df)
  names(heart.df) <- df.names.heart
  
  #negative is 0
  #positive is 1
  heart.df$label <- as.numeric(heart.df$label) -1
  heart.df
}

getHeartCols <- function(){
  df.names.heart <- c("chr","start","end", "name", "label", "original_species", "tissue_possitive", "P300_vs_input",
                      "H3K27ac_vs_input", "H3k04me1_vs_input","H3k04me3_vs_input","H3k09ac","H3k27ac","H3k27ac_vs_input",
                      "H3k27me3","H3k36me3","H3k4me1","H3k4me3","H3k79me2","P300_vs_input2","pol2_vs_input","dgfNarrowPeak")
  cols <- df.names.heart[8:22]
  if(!identical(unique(length(cols)), length(cols))){
    stop("there are duplicate column names -> aborting")
  }
  cols
}



getBrainCols <- function(){
  df.names.brain <-  c("chr", "start", "end", "name", "label", "original_species", 
                       "tissue_positive", "P300_vInpt_fbrain", 
                       "P300_mbrain", "H3K27ac_vInptt_fbrain", "P300_vInpt_fbrain2", 
                       "H3K27ac_vInpt_cbellum", "H3K27me3_vInpt_cbellum", 
                       "H3k4me1_vInpt_cbellum", "H3k4me3_vInpt_cbellum", "H3k27ac_vInpt_cortex", 
                       "H3k4me1_vInpt_cortex", "H3k4me3_vInpt_cortex", "H3k04me1_vInpt_wbrain", 
                       "H3k04me3_vInpt_wbrain", "H3k09me3_vInpt_wbrain",  "H3k27ac_vInpt_wbrain", 
                       "H3k27me3_vInpt_wbrain",  "H3k36me3_vInpt_wbrain", "Pol2_vInpt_wbrain")
  cols <- df.names.brain[8:25]
  if(!identical(unique(length(cols)), length(cols))){
    stop("there are duplicate column names -> aborting")
  }
  cols
  
}

cleanMouseBrain <- function(mouse.file=mouse.brain.table){
  df.names.brain <-  c("chr", "start", "end", "name", "label", "original_species", 
                       "tissue_positive", "P300_vInpt_fbrain", 
                       "P300_mbrain", "H3K27ac_vInptt_fbrain", "P300_vInpt_fbrain2", 
                       "H3K27ac_vInpt_cbellum", "H3K27me3_vInpt_cbellum", 
                       "H3k4me1_vInpt_cbellum", "H3k4me3_vInpt_cbellum", "H3k27ac_vInpt_cortex", 
                       "H3k4me1_vInpt_cortex", "H3k4me3_vInpt_cortex", "H3k04me1_vInpt_wbrain", 
                       "H3k04me3_vInpt_wbrain", "H3k09me3_vInpt_wbrain",  "H3k27ac_vInpt_wbrain", 
                       "H3k27me3_vInpt_wbrain",  "H3k36me3_vInpt_wbrain", "Pol2_vInpt_wbrain")
  
  
  
  brain.df <- read.csv(mouse.file,sep="\t")
  origCols <- names(brain.df)
  names(brain.df) <- df.names.brain
  
  #negative is 0brain.df
  #positive is 1
  brain.df$label <- as.numeric(brain.df$label) -1
  brain.df
}


getForebrainCols <- function(){
  df.names.brain <-  c("chr", "start", "end", "name", "label", "original_species", 
                       "tissue_positive", "P300_vInpt_fbrain", 
                       "P300_mbrain", "H3K27ac_vInptt_fbrain", "P300_vInpt_fbrain2", 
                       "H3K27ac_vInpt_cbellum", "H3K27me3_vInpt_cbellum", 
                       "H3k4me1_vInpt_cbellum", "H3k4me3_vInpt_cbellum", "H3k27ac_vInpt_cortex", 
                       "H3k4me1_vInpt_cortex", "H3k4me3_vInpt_cortex", "H3k04me1_vInpt_wbrain", 
                       "H3k04me3_vInpt_wbrain", "H3k09me3_vInpt_wbrain",  "H3k27ac_vInpt_wbrain", 
                       "H3k27me3_vInpt_wbrain",  "H3k36me3_vInpt_wbrain", "Pol2_vInpt_wbrain")
  df.names.brain[8:25]
  
}

cleanMouseForebrain <- function(mouse.file=mouse.forebrain.table){
  df.names.forebrain <-  c("chr", "start", "end", "name", "label", "original_species", 
                       "tissue_positive", "P300_vInpt_fbrain", 
                       "P300_mbrain", "H3K27ac_vInptt_fbrain", "P300_vInpt_fbrain2", 
                       "H3K27ac_vInpt_cbellum", "H3K27me3_vInpt_cbellum", 
                       "H3k4me1_vInpt_cbellum", "H3k4me3_vInpt_cbellum", "H3k27ac_vInpt_cortex", 
                       "H3k4me1_vInpt_cortex", "H3k4me3_vInpt_cortex", "H3k04me1_vInpt_wbrain", 
                       "H3k04me3_vInpt_wbrain", "H3k09me3_vInpt_wbrain",  "H3k27ac_vInpt_wbrain", 
                       "H3k27me3_vInpt_wbrain",  "H3k36me3_vInpt_wbrain", "Pol2_vInpt_wbrain")
  
  
  
  forebrain.df <- read.csv(mouse.file,sep="\t")
  origCols <- names(forebrain.df)
  names(forebrain.df) <- df.names.forebrain
  
  #negative is 0brain.df
  #positive is 1
  forebrain.df$label <- as.numeric(forebrain.df$label) -1
  forebrain.df
}


# TODO, use reduce or for loop to write R version of lisp's 'dotimes' function...

cleanColsHumanBrain <- function(cols){
  
  as.character(unlist(applyGsubVec(applyGsubVec(applyGsubVec(applyGsubVec(cols,
  "human_brain_intersect_E0",""),".with_scores.bed.scores_only",""),"[6-9][0-9].",""),"\\.","_")))
}

getBrainColsHuman <- function(human.file=human.brain.table){
  brain.df <- read.csv(human.file,sep="\t")
  cols <- cleanColsHumanBrain(colnames(brain.df))[8:72]
  if(!identical(unique(length(cols)), length(cols))){
    stop("there are duplicate column names -> aborting")
  }
  cols
}

cleanHumanBrain <- function(human.file=human.brain.table){
  
  brain.df <- read.csv(human.file,sep="\t")
  origCols <- names(brain.df)
  names(brain.df) <- cleanColsHumanBrain(origCols)
  
  #negative is 0brain.df
  #positive is 1
  brain.df$label <- as.numeric(brain.df$label) -1
  brain.df
}

cleanColsHumanHeart <- function(cols){  
  as.character(unlist(applyGsubVec(applyGsubVec(applyGsubVec(cols,
       "human_heart_intersect_E[0-1][0-9][0-9]\\.",""),"\\.with_scores.bed.scores_only",""),"\\.","_")))
}

getHeartColsHuman <- function(human.file=human.heart.table){
  heart.df <- read.csv(human.file,sep="\t")
  cols <- cleanColsHumanHeart(colnames(heart.df))[8:38]
  if(!identical(unique(length(cols)), length(cols))){
    stop("there are duplicate column names -> aborting")
  }
  cols
}

cleanHumanHeart <- function(human.file=human.heart.table){
  
  heart.df <- read.csv(human.file,sep="\t")
  origCols <- names(heart.df)
  names(heart.df) <- cleanColsHumanHeart(origCols)
  
  #negative is 0heart.df
  #positive is 1
  heart.df$label <- as.numeric(heart.df$label) -1
  heart.df
}
