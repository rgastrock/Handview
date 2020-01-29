source('R/shared.R')

getGroupAwarenessScores <- function(groups = c('30implicit','30explicit','cursorjump','handview')){
  
  # create empty dataframe to store all info:
  allscores <- data.frame()
  
  # loop through participants:
  for (group in groups) {
    
    # get localization data for participant:
    df <- read.csv(sprintf('data/%s_ppawareness_scores.csv',group), stringsAsFactors=FALSE)
    
    # concatenate all dataframes into one:
    if (prod(dim(allscores)) == 0) {
      allscores <- df
    } else {
      allscores <- rbind(allscores, df)
    }
    
  }
  
  # return dataframe with localization info for all participants in the group:
  return(allscores)
  
}

plotAwareness <- function(groups=c('30implicit', '30explicit', 'cursorjump', 'handview'), target='inline') {
  #for future reference: might also want to plot only low and high - this will merge scores 0 and 1
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9_awareness.svg', width=8, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  df <- getGroupAwarenessScores()
  
  par(mfrow=c(1,2),mar=c(4,4,2,.3))
  
  # groups <- c('30implicit', '30explicit', 'cursorjump', '60implicit', '60explicit')
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  colors <- c(impcol, expcol, cujcol, hancol)
  
  awareness <- array(data=NA, dim=c(length(groups),4))
  
  for (groupno in 1:length(groups)) {
    
    group <- groups[groupno]
    groupdf <- df[which(df$group == group),]
    
    group_awareness <- c()
    
    for (awareness_score in c(0:3)) {
      
      group_awareness <- c(group_awareness, (length(which(groupdf$points == awareness_score))))
      
    }
    
    awareness[groupno,] <- group_awareness
    
  }
  
  awareness <- as.matrix(awareness)
  rownames(awareness) <- c('NI','I','CJ','HV')
  colnames(awareness) <- c(1:4)
  
  # barplot(awareness,main="group awareness score counts",xlab="group",ylab="count",col=colors,beside=TRUE,legend=groups,border=NA,args.legend=c(x='topleft',border=NA))
  barplot(t(awareness[,c(1,2,3,4)]),main="Awareness Scores by Group",xlab="Group",ylab="Participant Count",col=rep(alpha(colors,0.6),each=4),beside=TRUE,border=TRUE,ylim=range(pretty(c(0, awareness))))
  axis(2, at=seq(0,25,5))
  
  # dot plot...
  #plot(c(-100,-100),c(-100,-100),type='n',asp=1,col=rgb(0,0,0,0),main="average trajectories", xlab="group", ylab="condition",xlim=c(0, 16*length(groups)), ylim=c(0, 16*nconditions), yaxt='n', xaxt='n',axes=F)  
  #par(las=1, mar=c(4,6,2,1) )
  plot(-1000,-1000,type='n',asp=1,col=rgb(0,0,0,0),main='Dot Plot of Scores',xlab='Score',ylab='',ylim=c(0,6),xlim=c(-1,4),yaxt='n',xaxt='n',axes=F)

  for (groupno in 1:length(groups)) {
    a <- awareness[groupno,]
    a <- a / sum(a)
    points(c(0,1,2,3),rep(length(groups)-groupno+1,4),cex=sqrt(a)*10,col=(alpha(colors[groupno],0.6)),pch=16)
  }

  axis(2, at=c(length(groups):1), labels=c('NI','I','CJ','HV'))
  axis(1, at=c(0,3), labels=c('low','high'))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}