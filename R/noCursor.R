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
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_reachaftereffects.svg', width=4, height=6, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupAftereffects <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-0.1,1), ylim = c(-5,35), 
       xlab = "Strategy Use", ylab = "Angular Deviation of Hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Group Reach Aftereffects and Strategy Use',xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 30, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at=c(0, 1), labels=c('Without Strategy', 'With Strategy')) #tick marks for x axis
  axis(2, at = c(0, 10, 20, 30)) #tick marks for y axis
  
  
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
    
    polygon(x = c(c(0:1), rev(c(0:1))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupAftereffects[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    stuff<-(meanGroupAftereffects[[group]])
    lines(x=c(0,1), y = c(stuff[1], stuff[2]), col=col, lty=1)
  }
  
  #add legend
  legend(0.12,8,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,bty='n',cex=0.85)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

# Statistics-----
reachAftereffectsANOVA <- function() {
  
  # write code!
  
}