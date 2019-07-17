source('R/shared.R')

#Reach Aftereffects-----
plotGroupDistribution <- function(groups = c('30implicit','30explicit','cursorjump','handview'), target = 'inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig8_RAEdists.svg', width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(2,2))
  
  plot(NA, NA, xlim=c(0.5, 3.5), axes=F,
       ylab="Angular Deviation of Hand", xlab="Strategy",
       main='Reach Aftereffects and Strategy Use', bty='n', ylim=c(-10, 50), yaxt='n')
  axis(1, at=c(1.25, 2.75), labels=c('Without Strategy', 'With Strategy')) #tick marks for x axis
  axis(2, at = c(-10, 0, 10, 20, 30, 40, 50))
  
  for (group in groups){
    #pdf(file='data/testRAEgroupvioplots1.pdf') 
    #participants <- getGroupParticipants(group=group)
    #par(mfrow=c(2,2))#, mar=c(1,1,1,1))
    #for (pp in participants){
    data <- read.csv(sprintf('data/%s_nocursor.csv', group))
    data <- loadRAE(group=group, baselinecorrect=TRUE)
    data <- data[,-1]
    
    alldat <- stack(data)
    
    colourscheme <- getColourScheme(group=group)
    width <- .5
    #col <- colourscheme[[group]][['S']]
    
    #change x coordinates to change positions of each group (xccord, line (if with mean), vioplot)
    #add an average of .4 to each location
    if (group == '30implicit'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(alldat$ind))
      xcoord[alldat$ind=="exclusive"]<- 1#1.3
      xcoord[alldat$ind=="inclusive"]<- 2.5#2.7
      points(xcoord,alldat$values, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1,2.5), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      

      
      col <- colourscheme[[group]][['T']]
      vioplot(data$exclusive, col=col, horizontal=FALSE, at=1, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(data$inclusive, col=col, horizontal=FALSE, at=2.5, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == '30explicit'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(alldat$ind))
      xcoord[alldat$ind=="exclusive"]<- 1.2#1.6
      xcoord[alldat$ind=="inclusive"]<- 2.7#2.4
      points(xcoord,alldat$values, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.2,2.7), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      

      
      col <- colourscheme[[group]][['T']]
      vioplot(data$exclusive, col=col, horizontal=FALSE, at=1.2, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(data$inclusive, col=col, horizontal=FALSE, at=2.7, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == 'cursorjump'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(alldat$ind))
      xcoord[alldat$ind=="exclusive"]<- 1.4#1.6
      xcoord[alldat$ind=="inclusive"]<- 2.9#2.4
      points(xcoord,alldat$values, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.4,2.9), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      

      
      col <- colourscheme[[group]][['T']]
      vioplot(data$exclusive, col=col, horizontal=FALSE, at=1.4, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(data$inclusive, col=col, horizontal=FALSE, at=2.9, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == 'handview'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(alldat$ind))
      xcoord[alldat$ind=="exclusive"]<- 1.6#1.6
      xcoord[alldat$ind=="inclusive"]<- 3.1#2.4
      points(xcoord,alldat$values, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.6,3.1), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      

      
      col <- colourscheme[[group]][['T']]
      vioplot(data$exclusive, col=col, horizontal=FALSE, at=1.6, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(data$inclusive, col=col, horizontal=FALSE, at=3.1, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    }
    
    # xcoord <- rep(0, length(alldat$ind))
    # xcoord[alldat$ind=="exclusive"]<- 1.3#1.6
    # xcoord[alldat$ind=="inclusive"]<- 2.7#2.4
    
    
    #boxplot(data$angular_deviation ~ data$set, names = c('Without Strategy', 'With Strategy'),
    #boxwex=0.3, frame.plot=F, ylim=c(-10, 60))
    #axis(2, at = c(-10, 0, 10, 20, 30, 40, 50, 60))
    # xcoord <- rep(0, length(data$exclusive))
    # xcoord[data$exclusive]<- 1.6
    # xcoord[data$inclusive]<- 2.4
    #par(new=T)
    #maintitle <- sprintf('Reach Aftereffects: %s', group)

    # plot(xcoord, alldat$values, xlim=c(0.5, 3.5), axes=F, 
    #      ylab="Angular Deviation", xlab="Strategy", col = col,#rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
    #      main=maintitle, bty='n', ylim=c(-10, 60), yaxt='n')
    
    # points(xcoord,alldat$values, col = col)
    # 
    # meanexcludedata <- mean(data$exclusive, na.rm=T)
    # meanincludedata <- mean(data$inclusive, na.rm=T)
    # lines(x=c(1.3,2.7), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
    # 
    # axis(1, at=c(1.3, 2.7), labels=c('Without Strategy', 'With Strategy')) #tick marks for x axis
    # axis(2, at = c(-10, 0, 10, 20, 30, 40, 50, 60))
    # 
    # col <- colourscheme[[group]][['T']]
    # vioplot(data$exclusive, col=col, horizontal=FALSE, at=1.3, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left') #pchMed='-')
    # vioplot(data$inclusive, col=col, horizontal=FALSE, at=2.7, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left') #pchMed='-')

    #dev.off()
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#Localization----
getMeanPpDistribution <- function(group){
  
  data <- read.csv(file = sprintf('data/%s_loc_p3_AOV.csv', group))
  
  allppavg <- data.frame()
  
  #reachtype is rotated = 1 or aligned = 0
  #condition is passive = 1 or active = 0
  for (reachtype in c(0,1)) {
    
    for (condition in c(0,1)) {
      
      subdata <- data[(data$rotated_b == reachtype) & (data$passive_b == condition),]
      
      #get mean bias_deg across all handangle_deg per pp
      ppavg <- aggregate(subdata$bias_deg ~ subdata$participant, data= subdata, FUN = mean)
      colnames(ppavg) <- c('participant', 'meanshift')  
      
      outdf <- data.frame(ppavg$participant, group, reachtype, condition, ppavg$meanshift)
      colnames(outdf) <- c('participant', 'group', 'rotated', 'passive', 'meanshift')
      allppavg <- rbind(allppavg,outdf)
    } # end condition loop
    
  } # end reachtype loop
  
  return(allppavg)
} # end group loop

plotLocGroupDistribution <- function(groups = c('30explicit','30implicit','cursorjump','handview'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9_LOCdists.svg', width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
  }

  #par(mfrow=c(2,2), new=TRUE)
  
  plot(NA, NA, xlim=c(0.5, 3.5), axes=F,
       ylab="Localization Shift", xlab="Movement Type",
       main='Shifts in Active and Passive Localization', bty='n', ylim=c(-25, 10), yaxt='n')
  axis(1, at=c(1.25, 2.75), labels=c('Active', 'Passive')) #tick marks for x axis
  axis(2, at = c(-25, -20, -15, -10, -5, 0, 5, 10))
  
  for (group in groups){
    #pdf(file='data/testRAEgroupvioplots1.pdf') 
    #participants <- getGroupParticipants(group=group)
    #par(mfrow=c(2,2))#, mar=c(1,1,1,1))
    #for (pp in participants){
    meanlocshifts <- getMeanPpDistribution(group=group)
    colourscheme <- getColourScheme(group=group)
    width <- .5
    #col <- colourscheme[[group]][['S']]
    
    #change x coordinates to change positions of each group (xccord, line (if with mean), vioplot)
    #add an average of .4 to each location
    if (group == '30implicit'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(meanlocshifts))
      xcoord[meanlocshifts$passive == 0]<- 1#1.3
      xcoord[meanlocshifts$passive == 1]<- 2.5#2.7
      points(xcoord,meanlocshifts$meanshift, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1,2.5), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      
      actms <- meanlocshifts[meanlocshifts$passive == 0,]
      pasms <- meanlocshifts[meanlocshifts$passive == 1,]
      
      col <- colourscheme[[group]][['T']]
      vioplot(actms$meanshift, col=col, horizontal=FALSE, at=1, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(pasms$meanshift, col=col, horizontal=FALSE, at=2.5, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == '30explicit'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(meanlocshifts))
      xcoord[meanlocshifts$passive == 0]<- 1.2#1.3
      xcoord[meanlocshifts$passive == 1]<- 2.7#2.7
      points(xcoord,meanlocshifts$meanshift, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.2,2.7), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      
      actms <- meanlocshifts[meanlocshifts$passive == 0,]
      pasms <- meanlocshifts[meanlocshifts$passive == 1,]
      
      col <- colourscheme[[group]][['T']]
      vioplot(actms$meanshift, col=col, horizontal=FALSE, at=1.2, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(pasms$meanshift, col=col, horizontal=FALSE, at=2.7, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == 'cursorjump'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(meanlocshifts))
      xcoord[meanlocshifts$passive == 0]<- 1.4#1.3
      xcoord[meanlocshifts$passive == 1]<- 2.9#2.7
      points(xcoord,meanlocshifts$meanshift, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.4,2.9), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      
      actms <- meanlocshifts[meanlocshifts$passive == 0,]
      pasms <- meanlocshifts[meanlocshifts$passive == 1,]
      
      col <- colourscheme[[group]][['T']]
      vioplot(actms$meanshift, col=col, horizontal=FALSE, at=1.4, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(pasms$meanshift, col=col, horizontal=FALSE, at=2.9, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    } else if (group == 'handview'){
      col <- colourscheme[[group]][['S']]
      xcoord <- rep(0, length(meanlocshifts))
      xcoord[meanlocshifts$passive == 0]<- 1.6#1.3
      xcoord[meanlocshifts$passive == 1]<- 3.1#2.7
      points(xcoord,meanlocshifts$meanshift, col = col)
      
      # meanexcludedata <- mean(data$exclusive, na.rm=T)
      # meanincludedata <- mean(data$inclusive, na.rm=T)
      # lines(x=c(1.6,3.1), y = c(meanexcludedata, meanincludedata), lty=1, col=col)
      
      actms <- meanlocshifts[meanlocshifts$passive == 0,]
      pasms <- meanlocshifts[meanlocshifts$passive == 1,]
      
      col <- colourscheme[[group]][['T']]
      vioplot(actms$meanshift, col=col, horizontal=FALSE, at=1.6, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
      vioplot(pasms$meanshift, col=col, horizontal=FALSE, at=3.1, add=TRUE,lty=2, border=NA, drawRect=F, rectCol=col, lineCol=col, axes=F, side='left', wex=width) #pchMed='-')
    }
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}