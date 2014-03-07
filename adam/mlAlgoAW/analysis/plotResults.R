


plotMlresults <- function(df, outdir,msg=""){
  
  ggplot(df,  aes(algo,AUC))+geom_boxplot() + 
     coord_flip() + theme_bw()+ xlab("learning algorithm") + ylab("AUC")+
     ggtitle(paste("test set error for classification algorithm",msg,sep="\n")) + 
     theme(panel.grid.major.x= element_line(colour = "grey"))+
     theme(panel.grid.major.y= element_line(colour = "black")) 
  ggsave(file=paste(outdir,"modelComparison-AUC.pdf"),height=7,width=7)
  
  ggplot(df,  aes(algo,2*(prec*sens)/(prec+sens)))+geom_boxplot() + 
    coord_flip() + theme_bw()+ xlab("learning algorithm") + ylab("F1 = 2*(prec*sens)/(prec + sens) where range={0,1}")+
    ggtitle(paste("test set error for classification algorithm",msg,sep="\n")) + 
    theme(panel.grid.major.x= element_line(colour = "grey"))+
    theme(panel.grid.major.y= element_line(colour = "black")) 
  ggsave(file=paste(outdir,"modelComparison-F1.pdf",sep=""),height=7,width=9)
  
  ggplot(df,  aes(algo,(TP + TN)/( TP + TN + FP + FN  )))+geom_boxplot() + 
    coord_flip() + theme_bw()+ xlab("learning algorithm") + ylab("accuracy = (TP + TN)/( TP + TN + FP + FN  )")+
    ggtitle(paste("test set error for classification algorithm",msg,sep="\n")) + 
    theme(panel.grid.major.x= element_line(colour = "grey"))+
    theme(panel.grid.major.y= element_line(colour = "black")) 
  ggsave(file=paste(outdir,"modelComparison-accuracy.pdf",sep=""),height=7,width=9)
  
  ggplot(df,  aes(algo,(TN)/(TN + FP)))+geom_boxplot() + 
    coord_flip() + theme_bw()+ xlab("learning algorithm") + ylab("true negative rate = (TN)/(TN + FP)")+
    ggtitle(paste("test set error for classification algorithm",msg,sep="\n")) + 
    theme(panel.grid.major.x= element_line(colour = "grey"))+
    theme(panel.grid.major.y= element_line(colour = "black")) 
  ggsave(file=paste(outdir,"modelComparison-TNR.pdf",sep=""),height=7,width=9)
  
  
}


exploritoryPlots <- function(df, cols, outdir, msg=""){
  
  df$tissueExpr <- ifelse(df$label == 0, "negative", "positive")
  melt.df <- melt(df[c(c("original_species", "tissueExpr"),cols)], id.vars=c("tissueExpr", "original_species")) 
 
  pdf(paste(outdir, "explore-Cov.pdf", sep="/"))
  mosaicplot(~ original_species + tissueExpr, data = df, color=2:3,main=paste("area ~ count",msg,sep="\n"))
  dev.off()
  
  
  ggplot(melt.df, aes(x=log(value), fill=variable)) + geom_density(alpha=I(0.4)) + 
    facet_wrap(tissueExpr ~ original_species) + theme_bw() +
    ggtitle(msg)
  ggsave(file=paste(outdir, "explore-densityPlots.pdf", sep="/"))
  
  ggplot(melt.df, aes(x=variable, y = log(value),fill=tissueExpr)) + geom_boxplot(alpha=I(0.4)) + 
    facet_grid(. ~ original_species) + theme_bw() + coord_flip()+
    ggtitle(msg)
  ggsave(file=paste(outdir, "explore-boxPlotLog-humVmos.pdf", sep="/"))
  
  ggplot(melt.df, aes(x = variable, y = value, fill = tissueExpr)) + geom_boxplot(alpha=I(0.4)) + 
    facet_grid(. ~ original_species) + theme_bw() + coord_flip()+
    ggtitle(msg)
  ggsave(file=paste(outdir, "explore-boxPlot-humVmos.pdf", sep="/"))
  
  ggplot(melt.df, aes(x = variable, y = log(value), fill = tissueExpr)) + 
    geom_boxplot(alpha=I(0.4)) + theme_bw() + coord_flip()+
    ggtitle(msg)
  ggsave(file=paste(outdir, "explore-boxPlotLog.pdf", sep="/"))
  
  ggplot(melt.df, aes(x = variable, y = value, fill = tissueExpr)) + 
    geom_boxplot(alpha=I(0.4)) + theme_bw() + coord_flip()+
    ggtitle(msg)
  ggsave(file=paste(outdir, "explore-boxPlot-humVmos.pdf", sep="/"))
  
  pdf(paste(outdir, "explore-Corr.pdf", sep="/"))
  heatmap(cor(df[cols]), main=paste("PearsonCorr\n",msg,sep=""),margins=c(10,10))
  dev.off()
  
  pdf(paste(outdir, "explore-Cov.pdf", sep="/"))
   heatmap(cov(df[cols]), main=paste("PearsonCorr\n",msg,sep=""),margins=c(10,10))
  dev.off()
  

  
}


