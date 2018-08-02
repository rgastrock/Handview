source('R/shared.R')

# Analysis and Plots-----

getGroupConfidenceInterval <- function(groups = c('30implicit','30explicit','cursorjump'), type = 't'){
  for (group in groups){
    # get the confidence intervals for each trial of each group
    data <- read.csv(file=sprintf('data/%s_learningcurves.csv',group), stringsAsFactors = F)
    ppcols <- unique(data$participant)
    #spread(data, the pp cols changed to rows, reachdev values to rows, specify how many ppcols to change)
    widedata <- spread(data, participant, reachdev)
    
    trialno <- widedata$trial
    data1 <- as.matrix(widedata[,2:dim(widedata)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(widedata$trial == trial), ]
      
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
    write.csv(confidence, file=sprintf('data/%s_CI_learningcurves.csv',group), row.names = F)
  }
}

#plot containing learning curves for all groups
#inline means it will plot here in R Studio
plotLearningCurves <- function(groups=c('30implicit', '30explicit', 'cursorjump'), target='inline') {
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_learningcurve.svg', width=7, height=4, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-10,40), 
       xlab = "Trial", ylab = "Angular Deviation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0,30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-10, 0, 15, 30, 40)) #tick marks for y axis
  
  
  for (group in groups) {
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_CI_learningcurves.csv',group))
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
  legend(50,10,legend=c('Implicit 30°','Instructed 30°','Cursor Jump'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']]),
         lty=1,bty='n')
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Statistics-----
learningCurveANOVA <- function() {
  
  # write code!
  
}