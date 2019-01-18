source('R/shared.R')

#Reach Aftereffects-----
plotGroupDistribution <- function(groups = c('30implicit','30explicit','cursorjump','handview'), target = 'inline'){
  
  #but we can save plot as svg file
  # if (target=='svg') {
  #   svglite(file='doc/fig/Fig5_RAEdists.svg', width=7, height=4, pointsize=10, system_fonts=list(sans="Arial"))
  # }
  
  #par(mfrow=c(2,2))
  for (group in groups){
    #pdf(file='data/testRAEgroupvioplots1.pdf') 
    #participants <- getGroupParticipants(group=group)
    #par(mfrow=c(2,2))#, mar=c(1,1,1,1))
    #for (pp in participants){
    data <- read.csv(sprintf('data/%s_nocursor.csv', group))
    data <- loadRAE(group=group, baselinecorrect=TRUE)
    data <- data[,-1]
    
    alldat <- stack(data)
    
    xcoord <- rep(0, length(alldat$ind))
    xcoord[alldat$ind=="exclusive"]<- 1.6
    xcoord[alldat$ind=="inclusive"]<- 2.4
    
    
    #boxplot(data$angular_deviation ~ data$set, names = c('Without Strategy', 'With Strategy'),
    #boxwex=0.3, frame.plot=F, ylim=c(-10, 60))
    #axis(2, at = c(-10, 0, 10, 20, 30, 40, 50, 60))
    # xcoord <- rep(0, length(data$exclusive))
    # xcoord[data$exclusive]<- 1.6
    # xcoord[data$inclusive]<- 2.4
    #par(new=T)
    maintitle <- sprintf('Reach Aftereffects: %s', group)
    
    plot(xcoord, alldat$values, xlim=c(0.5, 3.5), axes=F, 
         ylab="Angular Deviation", xlab="Strategy", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
         main=maintitle, bty='n', ylim=c(-10, 60), yaxt='n')
    
    axis(1, at=c(1.3, 2.7), labels=c('Without Strategy', 'With Strategy')) #tick marks for x axis
    axis(2, at = c(-10, 0, 10, 20, 30, 40, 50, 60))
    
    vioplot(data$exclusive, col='#005de42f', horizontal=FALSE, at= 1, add=TRUE,lty=2, border=NA, pchMed='-')
    vioplot(data$inclusive, col='#005de42f', horizontal=FALSE, at=3, add=TRUE,lty=2, border=NA, pchMed='-')
    meanexcludedata <- mean(data$exclusive, na.rm=T)
    meanincludedata <- mean(data$inclusive, na.rm=T)
    lines(x=c(1.6,2.4), y = c(meanexcludedata, meanincludedata), lty=1, col='red')
    #dev.off()
  }
  
  #close everything if you saved plot as svg
  # if (target=='svg') {
  #   dev.off()
  # }
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
  # if (target=='svg') {
  #   svglite(file='doc/fig/Fig6_LOCdists.svg', width=7, height=4, pointsize=10, system_fonts=list(sans="Arial"))
  # }
  # 
  # par(mfrow=c(2,2), new=TRUE)
  for (group in groups){
    #pdf(file= sprintf('data/%s_locdist.pdf', group))
    meanlocshifts <- getMeanPpDistribution(group=group)
    par(mfrow = c(1,2))
    ycoord <- rep(0,length(meanlocshifts))
    ycoord[meanlocshifts$passive == 0] <- 1.6
    
    maintitle <- sprintf('Active: %s', group)
    
    plot(meanlocshifts$meanshift, ycoord, ylim=c(0.5, 3.5), axes=F,
         xlab="Localization Shifts (Deg.)", ylab='Active', col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
         main=maintitle, bty='n', xlim=c(-30, 20), yaxt='n')
    
    axis(1, at = c(-25,-20, -15, -10, -5, 0, 5, 10, 15, 20))
    actdata <- (meanlocshifts$meanshift[which(meanlocshifts$passive == 0)])
    vioplot(actdata, col='#005de42f', horizontal=TRUE, at= 1, add=TRUE,lty=2, border=NA)
    
    ycoord <- rep(0,length(meanlocshifts))
    ycoord[meanlocshifts$passive == 1] <- 1.6
    
    maintitle <- sprintf('Passive: %s', group)
    
    plot(meanlocshifts$meanshift, ycoord, ylim=c(0.5, 3.5), axes=F,
         xlab="Localization Shifts (Deg.)",ylab = 'Passive', col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
         main=maintitle, bty='n', xlim=c(-30, 20), yaxt='n')
    
    axis(1, at = c(-25,-20, -15, -10, -5, 0, 5, 10, 15, 20))
    pasdata <- (meanlocshifts$meanshift[which(meanlocshifts$passive == 1)])
    vioplot(pasdata, col='#005de42f', horizontal=TRUE, at= 1, add=TRUE,lty=2, border=NA)
    #dev.off()
  }
  #close everything if you saved plot as svg
  # if (target=='svg') {
  #   dev.off()
  # }
}