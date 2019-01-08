source('R/shared.R')

# Analysis and Plots-----

getGroupConfidenceInterval <- function(groups = c('30explicit', '30implicit', 'cursorjump', 'handview'), type, location){
  for (group in groups){
    # get the confidence intervals for each trial of each group
    data <- read.csv(sprintf('data/%s_learningcurves.csv',group),stringsAsFactors=FALSE)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
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
      
    }
    write.csv(confidence, sprintf('data/%s_CI_learningcurves.csv',group), row.names = F)
  }
}

#plot containing learning curves for all groups
#inline means it will plot here in R Studio
plotLearningCurves <- function(groups=c('30implicit', '30explicit', 'cursorjump', 'handview'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_learningcurve.svg', width=7, height=4, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-10,40), 
       xlab = "Trial", ylab = "Angular Deviation of Hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0,30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(0, 10, 20, 30)) #tick marks for y axis
  
  
  for (group in groups) {
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_CI_learningcurves.csv',group))
    
    colourscheme <- getColourScheme(group=group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1)
  }
  
  #add legend
  legend(50,16,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,bty='n',cex=1)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#can also plot blocked learning curves
plotBlockedLearningCurves <- function(target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig1_blockedlearningcurves.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  #styles <- getNCMposterStyle()
  colourscheme <- getColourScheme()
  groups = c('30implicit','30explicit','cursorjump','handview')
  rotations = c(30,30,30,30)
  colors = c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']], colourscheme[['handview']][['S']])
  linestyles = c(1,1,1,1)
  labels <- c('Non-instructed','Instructed','Cursor Jump','Hand View')
  
  
  styles <- data.frame(groups,rotations,colors,linestyles,labels)
  colnames(styles) <- c('group','rotation','color','linestyle','label')
  
  par(mar=c(4,4,0.5,0.2))
  
  ylims=c(-.2*max(styles$rotation),max(styles$rotation)+(.2*max(styles$rotation)))
  plot(c(-1,36),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(-1,36),ylim=ylims,xlab='Trial',ylab="Angular Deviation of Hand (°)",xaxt='n',yaxt='n',bty='n')
  abline(h = c(0,30), col = 8, lty = 2) 
  
  for (groupno in c(1:length(styles$group))) {
    
    col <- colourscheme[[groupno]][['T']]
    group <- styles$group[groupno]
    curves <- read.csv(sprintf('data/%s_CI_learningcurves.csv',group), stringsAsFactors=FALSE)
    lower <- curves[,1]
    upper <- curves[,3]
    mid <- curves[,2]
    #curve <- lapply(data.frame(t(curves)),mean,na.rm=TRUE)
    
    lines(c(1:15),mid[1:15],col=as.character(styles$color[groupno]),lty=styles$linestyle[groupno],lw=2)
    polygon(x = c(c(1:15), rev(c(1:15))), y = c(lower[1:15], rev(upper[1:15])), border=NA, col=col)
    lines(c(21:35),mid[76:90],col=as.character(styles$color[groupno]),lty=styles$linestyle[groupno],lw=2)
    polygon(x = c(c(21:35), rev(c(21:35))), y = c(lower[76:90], rev(upper[76:90])), border=NA, col=col)
  }
  
  # axis(side=1, at=c(1,10,20,30))
  axis(side=1, at=c(1,5,10,25,30,35), labels=c('1','5','10','80','85','90'),cex.axis=0.85)
  axis(side=2, at=c(0,10,20,30),cex.axis=0.85)
  
  
  legend(17,12,styles$label,col=as.character(styles$color),lty=styles$linestyle,bty='n', cex=0.85)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}


# Statistics-----

getBlockedLearningCurves <- function(group, blockdefs) {
  #function reads in learningcurves csv file then creates an array with 3 columns for each participant.
  #Each column is the mean of the blocked trials (first three, second three, last 15)
  curves <- read.csv(sprintf('data/%s_learningcurves.csv',group), stringsAsFactors=FALSE)  
  
  #R <- dim(curves)[1] # not needed, checks if rows=90 (correct trial numbers)
  curves <- curves[,-1] #take away trial column
  N <- dim(curves)[2] #gets the number of columns (i.e., participants)
  
  blocked <- array(NA, dim=c(N,length(blockdefs))) #empty array where every participant will get 3 corresponding columns
  
  for (pp in c(1:N)) {
    
    for (block in c(1:length(blockdefs))) {
      
      blockdef <- blockdefs[[block]] #creates a list which specifies start trial of every block, and how many trials in total for this block
      blockstart <- blockdef[1] #either trial 1, 4, or 76
      blockend <- blockstart + blockdef[2] - 1 #either trial 3, 6, or 90
      samples <- curves[blockstart:blockend,pp] #gets corresponding reach angle per participant
      blocked[pp,block] <- mean(samples, na.rm=TRUE) #compute the mean for it and put it in array
      
    }
    
  }
  
  return(blocked)
  
}

getLearningCurves4ANOVA <- function(styles, blockdefs) {
  
  # set up vectors that will form a data frame for the ANOVA(s):
  #agegroup       <- c()
  #instructed     <- c()
  diffgroup      <- c()
  participant    <- c()
  block          <- c()
  reachdeviation <- c()
  
  
  
  for (groupno in c(1:length(styles$group))) {
    
    # keeping count of unique participants (this is the start of the counter; I fix for unique IDs later on):
    startingID <- 1
    
    group <- styles$group[groupno] #loop through groups from styles data frame
    
    # I only need groups for handview experiment
    # set up some basic descriptors that apply to the group:
    # if (substr(group, 1, 5) == 'aging') {
    #   thisagegroup <- 'older'
    # } else {
    #   thisagegroup <- 'younger'
    # }
    # thisinstructed <- grepl('explicit', group)
    
    # block the data:
    blocked <- getBlockedLearningCurves(group, blockdefs) #blocked reach angles from group
    
    
    # we need to know the number of participants to replicate some values:
    N <- dim(blocked)[1]
    
    for (blockno in c(1:length(blockdefs))) {
      
      #agegroup        <- c(agegroup, rep(thisagegroup, N))
      #instructed      <- c(instructed, rep(thisinstructed, N))
      # these create the values inside cells for the df to be created
      diffgroup       <- c(diffgroup, rep(group, N))
      participant     <- c(participant, c(startingID : (startingID + N - 1)))
      block           <- c(block, rep(blockno, N))
      reachdeviation  <- c(reachdeviation, blocked[,blockno])
      
    }
    
    startingID <- startingID + N #for counter to continue
    
  }
  
  # put it in a data frame:
  LCaov <- data.frame(diffgroup, participant, block, reachdeviation)
  # make participant numbers uniqe per group: (will look like - group.ppno)
  LCaov$participant <- sprintf('%s.%d', diffgroup, LCaov$participant)
  
  # need to make certain columns as factors for ANOVA:
  #LCaov$agegroup <- as.factor(LCaov$agegroup)
  #LCaov$instructed <- as.factor(LCaov$instructed)
  LCaov$diffgroup <- as.factor(LCaov$diffgroup)
  LCaov$block <- as.factor(LCaov$block)
  
  return(LCaov)
  
}

learningCurveANOVA <- function() {
  
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs)                      
  
  #looking into interaction below:
  #interaction.plot(LC4aov$diffgroup, LC4aov$block, LC4aov$reachdeviation)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  LC4aov$participant <- as.factor(LC4aov$participant)
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=reachdeviation, within=block,between=diffgroup,type=3, return_aov = TRUE) #which type of SS is appropriate?
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
}


#use a Tukey HSD (see R Tutorials 6)

learningcurveComparisonMeans <- function(){
  
  #can plot interaction just to eyeball it:
  #plot(interactionMeans(lm(reachdeviation ~ block * diffgroup, data=LC4aov), factors=c('diffgroup', 'block'), atx='block'))
  
  
  #main effects of group and block. interaction effect as well (only makes sense to look into interaction)
  #need to use afex (Analysis of Factorial Experiments) because this acccepts input from ezANOVA(), while phia does not
  
  
  #library(emmeans) #changed from lsmeans
  
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs) 
  secondAOV <- aov_ez("participant","reachdeviation",LC4aov,within="block",between="diffgroup")
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using lsmeans
  
  
  #cellmeans <- emmeans(secondAOV,specs=c('diffgroup','block'))
  cellmeans <- lsmeans(secondAOV$aov,specs=c('diffgroup','block'))
  print(cellmeans)
}

learningcurveComparisons <- function(method='Tukey'){
  styles <- getStyle()
  blockdefs <- list(c(1,3),c(4,3),c(76,15))
  
  LC4aov <- getLearningCurves4ANOVA(styles, blockdefs) 
  secondAOV <- aov_ez("participant","reachdeviation",LC4aov,within="block",between="diffgroup")
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  #we use implicit as a reference and compare all groups to it
  #compare cursor jump and hand view as well?
  
  EXvsIM <- c(1,-1,0,0,1,-1,0,0,1,-1,0,0)
  CJvsIM <- c(0,-1,1,0,0,-1,1,0,0,-1,1,0)
  HVvsIM <- c(0,-1,0,1,0,-1,0,1,0,-1,0,1)
  #CJvsHV <- c(0,0,1,-1,0,0,1,-1,0,0,1,-1) #remember to add this to the list
  #only explicit is significantly different
  
  #again compare all groups to implicit but in block 1 only
  EXvsIM_b1 <- c(1,-1,0,0,0,0,0,0,0,0,0,0)
  CJvsIM_b1 <- c(0,-1,1,0,0,0,0,0,0,0,0,0)
  HVvsIM_b1 <- c(0,-1,0,1,0,0,0,0,0,0,0,0)
  #only explicit is significantly different
  
  #all groups to implicit but in block 2 only
  EXvsIM_b2 <- c(0,0,0,0,1,-1,0,0,0,0,0,0)
  CJvsIM_b2 <- c(0,0,0,0,0,-1,1,0,0,0,0,0)
  HVvsIM_b2 <- c(0,0,0,0,0,-1,0,1,0,0,0,0)
  #only cursor jump differs
  
  contrastList <- list('Instr vs. Non-instr'=EXvsIM, 'Cursor Jump vs. Non-Instr'=CJvsIM, 'Hand View vs. Non-Instr'=HVvsIM,
                       'Block1: Instr vs. Non-instr'=EXvsIM_b1, 'Block1: Cursor Jump vs. Non-instr'=CJvsIM_b1, 'Block1: Hand View vs. Non-Instr'=HVvsIM_b1, 
                       'Block2: Instr vs. Non-instr'=EXvsIM_b2, 'Block2: Cursor Jump vs. Non-instr'=CJvsIM_b2, 'Block2: Hand View vs. Non-instr'=HVvsIM_b2)
  comparisons<- contrast(lsmeans(secondAOV$aov,specs=c('diffgroup','block')), contrastList, adjust=method)
  
  print(comparisons)
}