source('R/shared.R')

# Analysis and Plots-----

loadRAE <- function(group, baselinecorrect = TRUE){
  
  NoCdat <- read.csv(file = sprintf('data/%s_nocursor.csv', group), stringsAsFactors = F)
  
  if (baselinecorrect == TRUE){
    #this method of baseline correction is different from the older one (see previous section)
    #values are close to old version, but different by a few decimal places
    #because old version takes median of all exclusive from all target angles
    #this takes median of values for each angle, corrects for baseline, then takes the mean of all 3 angle values
    #note that we take the mean at the end for this new version, opposed to median in the first
    NoCdat$exclusive <- NoCdat$exclusive - NoCdat$aligned
    NoCdat$inclusive <- NoCdat$inclusive - NoCdat$aligned
    NoCdat <- NoCdat[,-3] #remove aligned column
    
    exc <- aggregate(as.formula('exclusive ~ participant'), NoCdat, median, na.rm = TRUE)
    inc <- aggregate(as.formula('inclusive ~ participant'), NoCdat, median, na.rm = TRUE)
    NoCdat <- cbind(exc,inc)
    NoCdat <- NoCdat[-3]
    return(NoCdat)
  } else if (baselinecorrect == FALSE){
    NoCdat <- NoCdat[,-5]
    aligned <- aggregate(as.formula('aligned ~ participant'), NoCdat, median, na.rm = TRUE)
    exc <- aggregate(as.formula('exclusive ~ participant'), NoCdat, median, na.rm = TRUE)
    NoCdat <- cbind(aligned, exc)
    NoCdat <- NoCdat[,-3]
    return(NoCdat)
  }
  
  
}

#when using older version of No Cursor data baseline correction, take not of function names before running this
getEIGroupConfidenceInterval <- function(groups = c('30explicit', '30implicit', 'cursorjump', 'handview'), type){
  for (group in groups){
    # get the confidence intervals for each trial of each group
    #data <- df
    #data <- getGroupReachAftereffects(group = group)
    data <- loadRAE(group = group, baselinecorrect = TRUE)
    datat<- t(data)
    newdata<- datat[-c(1),]
    #data1 <- as.matrix(data[2:dim(data)[2]])
    
    confidence <- data.frame()
    
    cireaches <- unlist(newdata[1, ])
    cireaches <- as.numeric(cireaches)
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    
    cireaches <- unlist(newdata[2,])
    cireaches <- as.numeric(cireaches)
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    write.csv(confidence, file=sprintf('data/%s_CI_reachaftereffects.csv',group), row.names = F)
  }
}

#plot containing reach aftereffects for all groups
#inline means it will plot here in R Studio
plotGroupReachAfterEffects <- function(groups=c('30implicit', '30explicit', 'cursorjump', 'handview'), target='inline') {
  
  styles <- getStyle()
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_reachaftereffects.svg', width=8, height=6, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupAftereffects <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  #ylims=c(-.35*max(styles$rotation),max(styles$rotation)+(.35*max(styles$rotation))) #as -.1 and .2 before
  ylims=c(-.35*max(styles$rotation),max(styles$rotation)+(.35*max(styles$rotation))+4) #+4 added here to fit in individual points
  plot(NA, NA, xlim = c(0,2.6), ylim = ylims, #c(-5,35),
       xlab = "Strategy Use", ylab = "Angular Deviation of Hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #abline(h = c(0,0), col = rgb(0.5,0.5,0.5), lty = 2) 
  axis(1, at=c(0.75, 1.75), labels=c('Without Strategy', 'With Strategy'), cex.axis=0.85) #tick marks for x axis
  axis(2, at = c(0, 10, 20, 30), cex.axis=0.85) #tick marks for y axis
  
  
  for (group in groups) {
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_CI_reachaftereffects.csv',group))
    colourscheme <- getColourScheme(group=group)
    #take only exclusive first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    
    polygon(x = c(c(0.75:1.75), rev(c(0.75:1.75))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupAftereffects[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    stuff<-(meanGroupAftereffects[[group]])
    lines(x=c(0.75,1.75), y = c(stuff[1], stuff[2]), col=col, lty=1)
  }
  
  
  #add individual data points?
  
  #styles <- getStyle()
  Xrep <- 0.10
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    data <- loadRAE(group = group, baselinecorrect = TRUE)
    datat<- t(data)
    newdata<- datat[-c(1),]
    newdata <- as.numeric(newdata[1,])
    
    X <- rep((Xrep),length(newdata))
    Y <- c(newdata)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)
    
    Xrep <- Xrep + 0.15
  }
 
  Xrep <- 1.95
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    data <- loadRAE(group = group, baselinecorrect = TRUE)
    datat<- t(data)
    newdata<- datat[-c(1),]
    newdata <- as.numeric(newdata[2,])
    
    X <- rep((Xrep),length(newdata))
    Y <- c(newdata)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)
    
    Xrep <- Xrep + 0.15
    
  } 

  #add legend
  legend(0.10,44.5,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,lwd=5,bty='n',cex=0.85)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotReachAfterEffects <- function(target='inline'){
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig3A_reachaftereffects.svg', width=10, height=7, system_fonts=list(sans='Arial'), pointsize=14)
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  
  
  layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1,1))
  #layout(matrix(c(1,2), nrow=1, ncol=2, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: RAEs for all groups, Without and With Strategy
  plotGroupReachAfterEffects()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('A', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  
  # # # # # # # # # #
  # panel B: individual participants in the Without Strategy Trials
  # bootstrap method for better visualization, but is close enough to the t-distribution
  # essentially, we want to show that we are confident that the mean for each group lies here
  # and if any CIs overlap mean of Non Instructed, that means they are not different
  ylims=c(-.35*max(styles$rotation),max(styles$rotation)+(.35*max(styles$rotation))+4) #as -.1 and .2 before
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='Without Strategy',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('B', side=3, outer=FALSE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('B', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) 
  
  blockdefs <- list(c(1,3))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    # get the confidence intervals for each trial of each group
    #data <- df
    #data <- getGroupReachAftereffects(group = group)
    data <- loadRAE(group = group, baselinecorrect = TRUE)
    datat<- t(data)
    newdata<- datat[-c(1),]
    newdata <- as.numeric(newdata[1,])
    
    X <- rep((groupno-(1/3)),length(newdata))
    Y <- c(newdata)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)#as.character(styles$color_trans[groupno]))
    if (group == '30implicit'){
      abline(h = c(0,mean(c(newdata))), col = col, lty = 2)
    }
    
    meandist <- getConfidenceInterval(data=c(newdata), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    #include density dist?
    # #grab the density distribution from list
    # #X will be vertical, Y will be the distribution
    # DX <- meandist$density$x
    # #then we just scale the plot
    # DY <- meandist$density$y / max(meandist$density$y) / 2.5
    # 
    # #mostly for the polygon
    # #without these, there will be a space between the solid line and point
    # #With these, the space is also shaded now
    # DX <- c(DX[1], DX, DX[length(DX)])
    # DY <- c(0,     DY, 0)
    # 
    # polygon(x=DY+groupno, y=DX, border=FALSE, col=col) #as.character(styles$color_trans[groupno]))
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    points(x=groupno,y=mean(c(newdata)),pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
  }
  axis(side=1, at=c(1,2,3,4),labels=c('NI','I','CJ','HV'))
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  
  # # # # # # # # # #
  # panel C: individual participants in the With Strategy Trials
  # bootstrap method for better visualization, but is close enough to the t-distribution
  # essentially, we want to show that we are confident that the mean for each group lies here
  # and if any CIs overlap mean of Non Instructed, that means they are not different
  ylims=c(-.35*max(styles$rotation),max(styles$rotation)+(.35*max(styles$rotation))+4)
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='With Strategy',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('B', side=3, outer=FALSE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('C', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) 
  
  blockdefs <- list(c(1,3))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    # get the confidence intervals for each trial of each group
    #data <- df
    #data <- getGroupReachAftereffects(group = group)
    data <- loadRAE(group = group, baselinecorrect = TRUE)
    datat<- t(data)
    newdata<- datat[-c(1),]
    newdata <- as.numeric(newdata[2,])
    
    X <- rep((groupno-(1/3)),length(newdata))
    Y <- c(newdata)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)#as.character(styles$color_trans[groupno]))
    if (group == '30implicit'){
      abline(h = c(0,mean(c(newdata))), col = col, lty = 2)
    }
    
    meandist <- getConfidenceInterval(data=c(newdata), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    #include density dist?
    # #grab the density distribution from list
    # #X will be vertical, Y will be the distribution
    # DX <- meandist$density$x
    # #then we just scale the plot
    # DY <- meandist$density$y / max(meandist$density$y) / 2.5
    # 
    # #mostly for the polygon
    # #without these, there will be a space between the solid line and point
    # #With these, the space is also shaded now
    # DX <- c(DX[1], DX, DX[length(DX)])
    # DY <- c(0,     DY, 0)
    # 
    # polygon(x=DY+groupno, y=DX, border=FALSE, col=col) #as.character(styles$color_trans[groupno]))
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    points(x=groupno,y=mean(c(newdata)),pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
  }
  axis(side=1, at=c(1,2,3,4),labels=c('NI','I','CJ','HV'))
  axis(side=2, at=c(0,10,20,30),labels=c('0','10','20','30'),cex.axis=0.85)
  
  if (target == 'svg') {
    dev.off()
  }
  
}


# Statistics-----

getNoC4ANOVA <- function(styles) {
  
  diffgroup      <- c()
  participant    <- c()
  session       <- c()
  reachdeviation <- c()
  
  for (groupno in c(1:length(styles$group))) {
    
    # keeping count of unique participants (this is the start of the counter; I fix for unique IDs later on):
    startingID <- 1
    
    group <- styles$group[groupno] #loop through groups from styles data frame
    
    df <- loadRAE(group = group, baselinecorrect = FALSE) #no baseline correction
    
    # we need to know the number of participants to replicate some values:
    N <- dim(df)[1]
    
    for (currentsession in c('aligned','exclusive')) {
      
      #agegroup        <- c(agegroup, rep(thisagegroup, N))
      #instructed      <- c(instructed, rep(thisinstructed, N))
      # these create the values inside cells for the df to be created
      diffgroup       <- c(diffgroup, rep(group, N))
      participant     <- c(participant, df$participant)#c(participant, c(startingID : (startingID + N - 1)))
      session         <- c(session, rep(currentsession, N))
      reachdeviation  <- c(reachdeviation, df[,currentsession])
      
    }
    
    startingID <- startingID + N #for counter to continue
    
  }
  
  # put it in a data frame:
  NoCaov <- data.frame(diffgroup, participant, session, reachdeviation)
  # make participant numbers uniqe per group: (will look like - group.ppno)
  #NoCaov$participant <- sprintf('%s.%d', diffgroup, NoCaov$participant)
  
  # need to make certain columns as factors for ANOVA:
  NoCaov$diffgroup <- as.factor(NoCaov$diffgroup)
  NoCaov$session <- as.factor(NoCaov$session)
  
  return(NoCaov)
}

NoCANOVA <- function() {
  
  styles <- getStyle()
  
  NoC4aov <- getNoC4ANOVA(styles)                      
  
  # for ez, case ID should be a factor:
  NoC4aov$participant <- as.factor(NoC4aov$participant)
  NoCursorAOV <- ezANOVA(data=NoC4aov, wid=participant, dv=reachdeviation, within=session, between=diffgroup,type=3, return_aov = TRUE)
  #No Mauchly's and Sphericity Corrections because session has just 2 levels
  print(NoCursorAOV[1]) #so that it doesn't print the aov object as well
  
}

#post-hoc
NoCursorComparisonMeans <- function(){
  
  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(reachdeviation ~ session * diffgroup, data=NoC4aov), factors=c('diffgroup', 'session'), atx='session'))
  
  
  #main effects of group and block. interaction effect as well (only makes sense to look into interaction)
  #need to use afex (Analysis of Factorial Experiments) because this acccepts input from ezANOVA(), while phia does not
  
  
  #library(emmeans) #changed from lsmeans
  
  styles <- getStyle()
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  NoC4aov <- getNoC4ANOVA(styles)
  secondAOV <- aov_ez("participant","reachdeviation",NoC4aov,within="session",between="diffgroup")
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using lsmeans
  
  
  #cellmeans <- emmeans(secondAOV,specs=c('diffgroup','block'))
  cellmeans <- lsmeans(secondAOV$aov,specs=c('diffgroup','session'))
  print(cellmeans)
}
#tukey would be an inappropriate test > will always deafult to sidak
#so I am choosing to set sidak as default, since it is less conservative than the bonferroni
#bonferroni and sidak will not change any of the results
NoCursorComparisons <- function(method='sidak'){
  styles <- getStyle()
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  NoC4aov <- getNoC4ANOVA(styles) 
  secondAOV <- aov_ez("participant","reachdeviation",NoC4aov,within="session",between="diffgroup")
  
  EX_alvsEX_ex <- c(1,0,0,0,-1,0,0,0)
  IM_alvsIM_ex <- c(0,1,0,0,0,-1,0,0)
  CJ_alvsCJ_ex <- c(0,0,1,0,0,0,-1,0)
  HV_alvsHV_ex <- c(0,0,0,1,0,0,0,-1)
  
  
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  #we use implicit as a reference and compare all groups to it
  #compare cursor jump and hand view as well?
  
  
  contrastList <- list('AL.Instr vs. ROT.Instr'=EX_alvsEX_ex, 'AL.Non-Instr vs. ROT.Non-Instr'=IM_alvsIM_ex, 'AL.Cursor Jump vs. ROT.Cursor Jump'=CJ_alvsCJ_ex,
                       'AL.Handview vs. ROT.Handview'=HV_alvsHV_ex)
  comparisons<- contrast(lsmeans(secondAOV$aov,specs=c('diffgroup','session')), contrastList, adjust=method)
  
  print(comparisons)
}

getNoCurComparisonEffSize <- function(method = 'sidak'){
  comparisons <- NoCursorComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(angular deviation of hand) accounted for 
  #by the difference between group1 and group2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}
#effectsize is interpreted as % of variance in angular deviation of hand accounted for by difference in session
#So ANOVA has everything significant (main effects of group and session, interaction of group and session). 
#So it is possible that effect of session can be happening for some groups but not the others
#hence we do the planned comparison test, where we compare whether sessions differed for each group
#and they do. so the possibility is ruled out, and at the same time we show that all groups learned/adapted






getRAE4ANOVA <- function(styles) {
  
  styles <- getStyle()
  
  diffgroup      <- c()
  participant    <- c()
  strategy       <- c()
  reachdeviation <- c()
  
  for (groupno in c(1:length(styles$group))) {
    
    # keeping count of unique participants (this is the start of the counter; I fix for unique IDs later on):
    startingID <- 1
    
    group <- styles$group[groupno] #loop through groups from styles data frame
    
    df <- loadRAE(group = group, baselinecorrect = TRUE) #with baseline correction
    
    # we need to know the number of participants to replicate some values:
    N <- dim(df)[1]
    
    for (strategyuse in c('exclusive','inclusive')) {
      
      #agegroup        <- c(agegroup, rep(thisagegroup, N))
      #instructed      <- c(instructed, rep(thisinstructed, N))
      # these create the values inside cells for the df to be created
      diffgroup       <- c(diffgroup, rep(group, N))
      participant     <- c(participant, df$participant)#c(participant, c(startingID : (startingID + N - 1)))
      strategy         <- c(strategy, rep(strategyuse, N))
      reachdeviation  <- c(reachdeviation, df[,strategyuse])
      
    }
    
    startingID <- startingID + N #for counter to continue
    
  }
  
  # put it in a data frame:
  RAEaov <- data.frame(diffgroup, participant, strategy, reachdeviation)
  # make participant numbers uniqe per group: (will look like - group.ppno)
  #deleted the line below because I've anonymized participant names in uploaded file already
  #RAEaov$participant <- sprintf('%s.%d', diffgroup, RAEaov$participant)
  
  # need to make certain columns as factors for ANOVA:
  RAEaov$diffgroup <- as.factor(RAEaov$diffgroup)
  RAEaov$strategy <- as.factor(RAEaov$strategy)
  
  return(RAEaov)
  
}

RAEANOVA <- function() {
  
  styles <- getStyle()
  
  RAE4aov <- getRAE4ANOVA(styles)
  
  RAE4aov$participant <- as.factor(RAE4aov$participant)
  RAEaov <- ezANOVA(data=RAE4aov, wid=participant, dv=reachdeviation, within=strategy, between=diffgroup,type=3, return_aov = TRUE)
  #No Mauchly's and Sphericity Correction because strategy only has 2 levels
  print(RAEaov[1])
}

#post-hoc

RAEComparisonMeans <- function(){
  
  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(reachdeviation ~ session * diffgroup, data=NoC4aov), factors=c('diffgroup', 'session'), atx='session'))
  
  
  #main effects of group and block. interaction effect as well (only makes sense to look into interaction)
  #need to use afex (Analysis of Factorial Experiments) because this acccepts input from ezANOVA(), while phia does not
  
  
  #library(emmeans) #changed from lsmeans
  
  styles <- getStyle()
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  RAE4aov <- getRAE4ANOVA(styles)
  secondAOV <- aov_ez("participant","reachdeviation",RAE4aov,within="strategy",between="diffgroup")
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using lsmeans
  
  
  #cellmeans <- emmeans(secondAOV,specs=c('diffgroup','block'))
  cellmeans <- lsmeans(secondAOV$aov,specs=c('diffgroup','strategy'))
  print(cellmeans)
}
#tukey would be an inappropriate test > will always deafult to sidak
#so I am choosing to set sidak as default, since it is less conservative than the bonferroni
#bonferroni and sidak will not change any of the results
RAEComparisons <- function(method='sidak'){
  styles <- getStyle()
  blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  RAE4aov <- getRAE4ANOVA(styles) 
  secondAOV <- aov_ez("participant","reachdeviation",RAE4aov,within="strategy",between="diffgroup")
  
  EX_wovsEX_w <- c(1,0,0,0,-1,0,0,0)
  IM_wovsIM_w <- c(0,1,0,0,0,-1,0,0)
  CJ_wovsCJ_w <- c(0,0,1,0,0,0,-1,0)
  HV_wovsHV_w <- c(0,0,0,1,0,0,0,-1)
  
  
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  #we use implicit as a reference and compare all groups to it
  #compare cursor jump and hand view as well?
  
  
  contrastList <- list('WO.Instr vs. WITH.Instr'=EX_wovsEX_w, 'WO.Non-Instr vs. WITH.Non-Instr'=IM_wovsIM_w, 'WO.Cursor Jump vs. WITH.Cursor Jump'=CJ_wovsCJ_w,
                       'WO.Handview vs. WITH.Handview'=HV_wovsHV_w)
  comparisons<- contrast(lsmeans(secondAOV$aov,specs=c('diffgroup','strategy')), contrastList, adjust=method)
  
  print(comparisons)
}

getRAEComparisonEffSize <- function(method = 'sidak'){
  comparisons <- RAEComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(angular deviation of hand) accounted for 
  #by the difference between group1 and group2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}
#effect size is interpreted as % of variance in angular deviation of hand accounted for by difference in strategy use