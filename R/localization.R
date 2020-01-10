source('R/shared.R')
source('R/noCursor.R')

# Analysis and Plots-----

kernelRegression <- function(x,y,width,interpoints) {
  
  output <- c()
  
  for (interpoint in interpoints) {
    
    w <- exp(-1 * ((x-interpoint)^2/(2 * width^2)))
    output <- c(output, sum(w*y)/sum(w))
    
  }
  
  return(output)
  
}

correctLocalizations <- function(df) {
  
  # this function corrects the tap angle, given the tap angle
  # we use the aligned data for a 'model' that predicts the errors of taps given a tap
  # and use that to correct both the aligned and rotated data
  
  # remove all nan values:
  df <- df[is.finite(df$hand_angle) & is.finite(df$tap_angle),]
  
  # remove taps too far away from the arc centre:
  df <- df[intersect(which((df$hand_angle - df$arc) > -20), which((df$hand_angle - df$arc) < 20)),]
  
  # remove outliers?
  # per participant and task
  participants <- unique(df$participant)
  
  row.idx <- c()
  
  for (p.id in participants) {
    
    for (isrot in c(0,1)) {
      
      for (ispas in c(0,1)) {
        
        task.idx <- which(df$participant == p.id & df$rotated == isrot & df$passive == ispas)
        
        ptask <- df[task.idx,]
        
        ang_dev <- ptask$hand_angle - ptask$tap_angle
        
        row.idx <- c(row.idx, task.idx[which(abs(ang_dev) <= (mean(ang_dev) + (3 * sd(ang_dev))))]) #removes outliers
        
      }
      
    }
    
  }
  
  df <- df[row.idx,]
  
  return(df)
  
}

getLocalizationConfidenceIntervals <- function(groups=c('30implicit','30explicit','cursorjump', 'handview')) {
  
  
  interpoints <- c(50,90,130)
  #interpoints <- seq(30,150,1)
  #iterations <- 1000 # ideally 10000 or more? no more iterations: bootstrap function I have is enough
  percentiles <- c(0.025, 0.5, 0.975) # this is not actually used...#it is needed in confint_kernelregression
  
  for (group in groups) {
    
    df <- read.csv(sprintf('data/%s_localization.csv',group),stringsAsFactors=FALSE)
    
    # remove taps where the hand location is further than 20 degrees from the arc centre
    df <- correctLocalizations(df)
    # throw out rows where either hand or tap is missing
    df <- df[is.finite(df$hand_angle) & is.finite(df$tap_angle),]
    
    participants <- c(unique(df$participant))
    
    KRbiases <- array(NA, dim=c(2, 2, length(interpoints), length(participants)))
    
    for (reachtype in c(0,1)) {
      
      for (condition in c(0,1)) {
        
        #print(sprintf('%s %d %d',group,reachtype,condition)) #took it out to remove printing in console
        
        # let's subset the dataframe to speed things up
        subdf <- df[(df$rotated == condition) & (df$passive == reachtype),]
        
        # and get a list of named lists of hand positions and localization biases premade
        # we can use this to bootstrap over participants
        # participants <- c(unique(subdf$participant))
        # pp.bias <- list()
        # pp.hand <- list()
        
        for (ppno in c(1:length(participants))) {
          
          p <- participants[ppno]
          
          tap  <- subdf[subdf$participant == p, 'tap_angle']
          hand <- subdf[subdf$participant == p, 'hand_angle']
          bias <- tap - hand

          # select non-outliers:
          bias.sd <- sd(bias)
          idx <- intersect(which(bias > (mean(bias) - (3 * bias.sd))), which(bias < (mean(bias) + (3 * bias.sd))))

          # remove outliers for every participant?
          bias <- bias[idx]
          hand <- hand[idx]

          # collect all the kernel regression interpolated points to later do confidence intervals on:
          KRbiases[reachtype+1,condition+1,,ppno] <- kernelRegression(x=hand,y=bias,width=15,interpoints=interpoints)
          
        }
        
        confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
        
        for (intpt in c(1:length(interpoints))) {
          
          #confint_kernelregression[intpt,2] <- mean(KRbiases[reachtype+1,condition+1,intpt,])
          confint_kernelregression[intpt,c(1,2,3)] <- getBSConfidenceInterval(data = KRbiases[reachtype+1,condition+1,intpt,], resamples = 1000)
          
        }
        
        # reachtype (0,1)
        # condition (0,1)
        
        # put the current combination in data frame:
        #this creates lower, mean, upper for aligned active, aligned passive, rotated active, rotated passive
        if (reachtype == 0 & condition == 0) {
          localizationCI <- data.frame(interpoints,confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
          colnames(localizationCI) <- c('angle',sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s%s',c('AL','RO')[condition+1],c('act','pas')[reachtype+1])))
        } else {
          tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
          colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s%s',c('AL','RO')[condition+1],c('act','pas')[reachtype+1]))
          localizationCI <- cbind(localizationCI, tempLocalizationCI)
        }
        
      }
      
      # now within reachtype, get a confidence interval on the difference between rotated and aligned:
      
      subdata <- KRbiases[reachtype+1,2,,] - KRbiases[reachtype+1,1,,]
      
      confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
      
      for (intpt in c(1:length(interpoints))) {
        
        #confint_kernelregression[intpt,2] <- mean(subdata[intpt,])
        confint_kernelregression[intpt,c(1,2,3)] <- getBSConfidenceInterval(data = subdata[intpt,], resamples = 1000)
        
      }
      
      #rotated minus aligned for both active and passive, hence the columns named act or pas (subtraction is for mean only)
      tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
      colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s',c('act','pas')[reachtype+1]))
      localizationCI <- cbind(localizationCI, tempLocalizationCI)
      
    }
    
    
    # now within reachtype, get a confidence interval on the difference between rotated and aligned:
    #this is act minus pas to get predicted consequences
    
    subdata <- (KRbiases[1,2,,] - KRbiases[1,1,,]) - (KRbiases[2,2,,] - KRbiases[2,1,,])
    
    confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
    
    for (intpt in c(1:length(interpoints))) {
      
      #confint_kernelregression[intpt,2] <- mean(subdata[intpt,])
      confint_kernelregression[intpt,c(1,2,3)] <- getBSConfidenceInterval(data = subdata[intpt,], resamples = 1000)
      
    }
    
    tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
    colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),'PredCons')
    localizationCI <- cbind(localizationCI, tempLocalizationCI)
    
    write.csv(localizationCI, sprintf('data/%s_localization_bCI.csv',group), row.names=FALSE)
    
  }
  
  
}


getLocalizationTdistributionConfidenceIntervals <- function(groups=c('30implicit','30explicit','cursorjump','handview')) {
  
  interpoints <- c(50,90,130)
  #interpoints <- seq(30,150,1)
  # iterations <- 10000 # ideally 10000 or more? no more iterations: use t-distribution
  percentiles <- c(0.025, 0.5, 0.975) # this is not actually used...#it is needed in confint_kernelregression
  
  for (group in groups) {
    
    df <- read.csv(sprintf('data/%s_localization.csv',group),stringsAsFactors=FALSE)
    
    # remove taps where the hand location is further than 20 degrees from the arc centre
    df <- correctLocalizations(df)
    # throw out rows where either hand or tap is missing
    df <- df[is.finite(df$hand_angle) & is.finite(df$tap_angle),]
    
    participants <- c(unique(df$participant))
    
    KRbiases <- array(NA, dim=c(2, 2, length(interpoints), length(participants)))
    
    for (reachtype in c(0,1)) {
      
      for (condition in c(0,1)) {
        
        #print(sprintf('%s %d %d',group,reachtype,condition)) #took it out to remove printing in console
        
        # let's subset the dataframe to speed things up
        subdf <- df[(df$rotated == condition) & (df$passive == reachtype),]
        
        # and get a list of named lists of hand positions and localization biases premade
        # we can use this to bootstrap over participants
        # participants <- c(unique(subdf$participant))
        # pp.bias <- list()
        # pp.hand <- list()
        
        for (ppno in c(1:length(participants))) {
          
          p <- participants[ppno]
          
          tap  <- subdf[subdf$participant == p, 'tap_angle']
          hand <- subdf[subdf$participant == p, 'hand_angle']
          bias <- tap - hand
          
          # select non-outliers:
          bias.sd <- sd(bias)
          idx <- intersect(which(bias > (mean(bias) - (3 * bias.sd))), which(bias < (mean(bias) + (3 * bias.sd))))
          
          # remove outliers for every participant?
          bias <- bias[idx]
          hand <- hand[idx]
          
          # collect all the kernel regression interpolated points to later do confidence intervals on:
          KRbiases[reachtype+1,condition+1,,ppno] <- kernelRegression(x=hand,y=bias,width=15,interpoints=interpoints)
          
        }
        
        confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
        
        for (intpt in c(1:length(interpoints))) {
          
          #confint_kernelregression[intpt,2] <- mean(KRbiases[reachtype+1,condition+1,intpt,])
          confint_kernelregression[intpt,c(1,2,3)] <- t.interval(KRbiases[reachtype+1,condition+1,intpt,]) 
          
        }
        
        # reachtype (0,1)
        # condition (0,1)
        
        # put the current combination in data frame:
        #this creates lower, mean, upper for aligned active, aligned passive, rotated active, rotated passive
        if (reachtype == 0 & condition == 0) {
          localizationCI <- data.frame(interpoints,confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
          colnames(localizationCI) <- c('angle',sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s%s',c('AL','RO')[condition+1],c('act','pas')[reachtype+1])))
        } else {
          tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
          colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s%s',c('AL','RO')[condition+1],c('act','pas')[reachtype+1]))
          localizationCI <- cbind(localizationCI, tempLocalizationCI)
        }
        
      }
      
      # now within reachtype, get a confidence interval on the difference between rotated and aligned:
      
      subdata <- KRbiases[reachtype+1,2,,] - KRbiases[reachtype+1,1,,]
      
      confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
      
      for (intpt in c(1:length(interpoints))) {
        
        #confint_kernelregression[intpt,2] <- mean(subdata[intpt,])
        confint_kernelregression[intpt,c(1,2,3)] <- t.interval(subdata[intpt,])
        
      }
      
      #rotated minus aligned for both active and passive, hence the columns named act or pas (subtraction is for mean only)
      tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
      colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),sprintf('%s',c('act','pas')[reachtype+1]))
      localizationCI <- cbind(localizationCI, tempLocalizationCI)
      
    }
    
    
    # now within reachtype, get a confidence interval on the difference between rotated and aligned:
    #this is act minus pas to get predicted consequences
    
    subdata <- (KRbiases[1,2,,] - KRbiases[1,1,,]) - (KRbiases[2,2,,] - KRbiases[2,1,,])
    
    confint_kernelregression <- matrix(data=NA,nrow=length(interpoints),ncol=length(percentiles))
    
    for (intpt in c(1:length(interpoints))) {
      
      #confint_kernelregression[intpt,2] <- mean(subdata[intpt,])
      confint_kernelregression[intpt,c(1,2,3)] <- t.interval(subdata[intpt,])
      
    }
    
    tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
    colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),'PredCons')
    localizationCI <- cbind(localizationCI, tempLocalizationCI)
    
    write.csv(localizationCI, sprintf('data/%s_localization_tCI.csv',group), row.names=FALSE)
    
  }
  
}


#plot containing active, passive, and predicted consequences for all groups
#inline means it will plot here in R Studio
plotLocalizationShift <- function(groups=c('30implicit', '30explicit', 'cursorjump','handview'), target='inline') {
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig6_localization.svg', width=8, height=3.5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #par(mfrow = c(1,3), mai=c(0.65,0.65,0.8,0.3))  #mai=c(0.65,0.3,0.8,0.3)) #added this to fix margins of plot
  par(mfrow = c(1,3), mar=c(4,4,5,0.25))
  
  for (reachtype.idx in c(0,1)){
    reachtype <- c('Active','Passive')[reachtype.idx+1]
    
    meanlocalizationshifts <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    #removed ylab for now to make space in poster (SCAPPS 2018)
    ylims=c(-.35*-15,-15+(.35*-15)) #as -.1 and .2 before; -15 is the constant here
    if (reachtype.idx == 0){
      plot(NA, NA, xlim = c(40,190), ylim = ylims, 
           xlab = "", ylab="Localization Shift (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf('%s Localization \n \n \n (Proprioception + Prediction)', reachtype),cex.main = 1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception + Prediction)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130),cex.axis=0.85) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
      #mtext('A', side=3, outer=TRUE, line=-1, adj=0, padj=1)
    } else if (reachtype.idx == 1){
      plot(NA, NA, xlim = c(40,190), ylim = ylims, 
           xlab = expression(paste("Hand Angle (",degree,")")), ylab="", frame.plot = FALSE, #frame.plot takes away borders; ylab coded as such to print degree symbol correctly
           main = sprintf('%s Localization \n \n \n (Proprioception)', reachtype),cex.main=1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130), cex.axis=0.85) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
    }
    #Active and Passive
    for (group in groups) {
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/%s_localization_tCI.csv',group))
      colourscheme <- getColourScheme()
      #take only exclusive first, last and middle columns of file
      if (reachtype.idx == 0){
        lower <- groupconfidence$act_p2.5
        upper <- groupconfidence$act_p97.5
        mid <- groupconfidence$act_p50
        
      } else if (reachtype.idx == 1){
        lower <- groupconfidence$pas_p2.5
        upper <- groupconfidence$pas_p97.5
        mid <- groupconfidence$pas_p50
      }
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      
      polygon(x = c(c(50,90,130), rev(c(50,90,130))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanlocalizationshifts[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    for (group in groups) {
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      stuff<-(meanlocalizationshifts[[group]])
      lines(x=c(50,90,130), y = c(stuff[1], stuff[2], stuff[3]), col=col, lty=1)
    }
    
    for (group in groups){

      col <- colourscheme[[group]][['S']]

      #add average dots and CIs
      localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx)),] #removed -1 in reachtype.idx
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]

      groupno <- which(groups == group)
      xloc <- 145 + (groupno*8)
      CI <- t.interval(shift)
      #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color_solid[groupno]), lty=styles$linestyle[groupno])
      lines(c(xloc, xloc), c(CI[3], CI[1]), col=col)
      lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=col)
      lines(c(xloc-1.5, xloc+1.5), c(CI[3], CI[3]), col=col)
      #lines(c(xloc-1.5, xloc+1.5), c(CI[2], CI[2]), col=col)
      points(xloc, mean(shift), col=col, pch=19)
    }
  }
  
  
  #Predicted Sensory Consequences
  meanlocalizationshifts <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  ylims=c(-.35*-15,-15+(.35*-15)) #as -.1 and .2 before; -15 is the constant here
  plot(NA, NA, xlim = c(40,190), ylim = ylims, 
       xlab = "", ylab="", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Predicted \n Sensory Consequences \n \n (Prediction)',cex.main=1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #mtext("(Prediction)", cex=1)
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at=c(50, 90, 130), cex.axis=0.85) #tick marks for x axis
  axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
  
  for (group in groups) {
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_localization_tCI.csv',group))
    #take only exclusive first, last and middle columns of file
    lower <- groupconfidence$PredCons_p2.5
    upper <- groupconfidence$PredCons_p97.5
    mid <- groupconfidence$PredCons_p50
    #lower <- groupconfidence[,8]
    #upper <- groupconfidence[,10]
    #mid <- groupconfidence[,9]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    
    polygon(x = c(c(50,90,130), rev(c(50,90,130))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanlocalizationshifts[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    stuff<-(meanlocalizationshifts[[group]])
    lines(x=c(50,90,130), y = c(stuff[1], stuff[2], stuff[3]), col=col, lty=1)
  }

  for (group in groups) {

    shifts <- list()
    col <-colourscheme[[group]][['S']]

    for (reachtype.idx in c(0,1)) {
      localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      shifts[[reachtype.idx+1]] <- shift
    }

    shift <- shifts[[1]] - shifts[[2]] #Active minus Passive

    groupno <- which(groups == group)
    xloc <- 145 + (groupno*8)
    CI <- t.interval(shift)
    #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color[groupno]), lty=styles$linestyle[groupno])
    lines(c(xloc, xloc), c(CI[3], CI[1]), col=col)
    lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=col)
    lines(c(xloc-1.5, xloc+1.5), c(CI[3], CI[3]), col=col)
    points(xloc, mean(shift), col=col, pch=19)

  }
  legend(40,-17,legend=c('Non-instructed','Instructed','Cursor Jump','Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,bty='n', cex=0.85)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotLocalizations <- function(target='inline'){
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig6_localization.svg', width=9, height=9, pointsize=14, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,4,0.1))
  
  
  
  layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Active, Passive, PredCons for all groups
  for (reachtype.idx in c(0,1)){
    reachtype <- c('Active','Passive')[reachtype.idx+1]
    
    meanlocalizationshifts <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    #removed ylab for now to make space in poster (SCAPPS 2018)
    ylims=c(-.7*-15,-15+(.7*-15)) #as -.1 and .2 before; -15 is the constant here
    if (reachtype.idx == 0){
      plot(NA, NA, xlim = c(40,140), ylim = ylims, 
           xlab = "", ylab="Localization Shift (°)", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf('%s Localization \n \n \n (Proprioception + Prediction)', reachtype),cex.main = 1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception + Prediction)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130),cex.axis=0.85) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
      mtext('A', side=3, outer=TRUE, line=-1, adj=0, padj=1)
    } else if (reachtype.idx == 1){
      plot(NA, NA, xlim = c(40,140), ylim = ylims, 
           xlab = expression(paste("Hand Angle (",degree,")")), ylab="", frame.plot = FALSE, #frame.plot takes away borders; ylab coded as such to print degree symbol correctly
           main = sprintf('%s Localization \n \n \n (Proprioception)', reachtype),cex.main=1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130), cex.axis=0.85) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
    }
    #Active and Passive
    #for (group in groups) {
    for (groupno in c(1:length(styles$group))){
      group <- styles$group[groupno]
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/%s_localization_tCI.csv',group))
      colourscheme <- getColourScheme()
      #take only exclusive first, last and middle columns of file
      if (reachtype.idx == 0){
        lower <- groupconfidence$act_p2.5
        upper <- groupconfidence$act_p97.5
        mid <- groupconfidence$act_p50
        
      } else if (reachtype.idx == 1){
        lower <- groupconfidence$pas_p2.5
        upper <- groupconfidence$pas_p97.5
        mid <- groupconfidence$pas_p50
      }
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      
      polygon(x = c(c(50,90,130), rev(c(50,90,130))), y = c(lower, rev(upper)), border=NA, col=col)
      
      meanlocalizationshifts[[group]] <- mid #use mean to fill in empty list for each group
    }
    
    for (groupno in c(1:length(styles$group))){
      group <- styles$group[groupno]
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      stuff<-(meanlocalizationshifts[[group]])
      lines(x=c(50,90,130), y = c(stuff[1], stuff[2], stuff[3]), col=col, lty=1)
    }

  }
  
  
  #Predicted Sensory Consequences
  meanlocalizationshifts <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  ylims=c(-.7*-15,-15+(.7*-15)) #as -.1 and .2 before; -15 is the constant here
  plot(NA, NA, xlim = c(40,140), ylim = ylims, 
       xlab = "", ylab="", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Predicted \n Sensory Consequences \n \n (Prediction)',cex.main=1, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #mtext("(Prediction)", cex=1)
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at=c(50, 90, 130), cex.axis=0.85) #tick marks for x axis
  axis(2, at = c(0, -5, -10, -15), cex.axis=0.85) #tick marks for y axis
  
  for (groupno in c(1:length(styles$group))){
    group <- styles$group[groupno]
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/%s_localization_tCI.csv',group))
    #take only exclusive first, last and middle columns of file
    lower <- groupconfidence$PredCons_p2.5
    upper <- groupconfidence$PredCons_p97.5
    mid <- groupconfidence$PredCons_p50
    #lower <- groupconfidence[,8]
    #upper <- groupconfidence[,10]
    #mid <- groupconfidence[,9]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    
    polygon(x = c(c(50,90,130), rev(c(50,90,130))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanlocalizationshifts[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (groupno in c(1:length(styles$group))){
    group <- styles$group[groupno]
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    stuff<-(meanlocalizationshifts[[group]])
    lines(x=c(50,90,130), y = c(stuff[1], stuff[2], stuff[3]), col=col, lty=1)
  }
  
  legend(40,-25,legend=c('Non-instructed','Instructed','Cursor Jump','Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,lwd=5,bty='n', cex=0.85)
  
  # # # # # # # # # #
  # panel B: individual participants in the Active Localization
  # bootstrap method for better visualization, but is close enough to the t-distribution
  # essentially, we want to show that we are confident that the mean for each group lies here
  # and if any CIs overlap mean of Non Instructed, that means they are not different
  ylims=c(-.7*-15,-15+(.7*-15)) #as -.1 and .2 before; -15 is the constant here
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='Active Localization',ylab='Localization Shift (°)',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('B', side=3, outer=FALSE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('B', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  #abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) 
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    # get the confidence intervals for each trial of each group
    
    #shift <- list()
    reachtype.idx <- 0
    
    
    localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
    localization <- localization[which(localization$passive_b == (reachtype.idx)),]
    localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
    shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
    
    
    
    
    # data <- loadRAE(group = group, baselinecorrect = TRUE)
    # datat<- t(data)
    # newdata<- datat[-c(1),]
    # newdata <- as.numeric(newdata[1,])
    
    X <- rep((groupno-(1/3)),length(shift))
    Y <- c(shift)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)#as.character(styles$color_trans[groupno]))
    # if (group == '30implicit'){
    #   abline(h = c(0,mean(c(newdata))), col = col, lty = 2)
    # }
    
    meandist <- getConfidenceInterval(data=c(shift), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
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
    # #include shaded distribution?
    # polygon(x=DY+groupno, y=DX, border=FALSE, col=col) #as.character(styles$color_trans[groupno]))
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    points(x=groupno,y=mean(c(shift)),pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
  }
  axis(side=1, at=c(1,2,3,4),labels=c('NI','I','CJ','HV'))
  axis(side=2, at=c(0,-5,-10,-15),labels=c('0','-5','-10','-15'))#,cex.axis=0.85)
  
  
  # # # # # # # # # #
  # panel C: individual participants in Passive Localization
  # bootstrap method for better visualization, but is close enough to the t-distribution
  # essentially, we want to show that we are confident that the mean for each group lies here
  # and if any CIs overlap mean of Non Instructed, that means they are not different
  ylims=c(-.7*-15,-15+(.7*-15)) #as -.1 and .2 before; -15 is the constant here
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='Passive Localization',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  #mtext('B', side=3, outer=FALSE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('C', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  #abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) 
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    # get the confidence intervals for each trial of each group
    
    #shift <- list()
    reachtype.idx <- 1
    
    
    localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
    localization <- localization[which(localization$passive_b == (reachtype.idx)),]
    localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
    shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
    
    
    
    
    # data <- loadRAE(group = group, baselinecorrect = TRUE)
    # datat<- t(data)
    # newdata<- datat[-c(1),]
    # newdata <- as.numeric(newdata[1,])
    
    X <- rep((groupno-(1/3)),length(shift))
    Y <- c(shift)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)#as.character(styles$color_trans[groupno]))
    # if (group == '30implicit'){
    #   abline(h = c(0,mean(c(newdata))), col = col, lty = 2)
    # }
    
    meandist <- getConfidenceInterval(data=c(shift), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
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
    # #include shaded distribution?
    # polygon(x=DY+groupno, y=DX, border=FALSE, col=col) #as.character(styles$color_trans[groupno]))
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    points(x=groupno,y=mean(c(shift)),pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
  }
  axis(side=1, at=c(1,2,3,4),labels=c('NI','I','CJ','HV'))
  axis(side=2, at=c(0,-5,-10,-15),labels=c('0','-5','-10','-15'))#,cex.axis=0.85)
  
  # # # # # # # # # #
  # panel D: individual participants in Predicted Sensory Consequences
  # bootstrap method for better visualization, but is close enough to the t-distribution
  # essentially, we want to show that we are confident that the mean for each group lies here
  # and if any CIs overlap mean of Non Instructed, that means they are not different
  ylims=c(-.7*-15,-15+(.7*-15)) #as -.1 and .2 before; -15 is the constant here
  plot(c(0,5),c(0,0),col=rgb(0.5,0.5,0.5),type='l',lty=2,xlim=c(0.5,4.5),ylim=ylims,xlab='Predicted Sensory Consequences',ylab='',xaxt='n',yaxt='n',bty='n',main='',font.main=1)
  
  
  mtext('D', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  #abline(h = c(0,30), col = rgb(0.5,0.5,0.5), lty = 2) 
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    # get the confidence intervals for each trial of each group
    
    
    shifts <- list()
    
    for (reachtype.idx in c(0,1)) {
      localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      shifts[[reachtype.idx+1]] <- shift
    }
    
    shift <- shifts[[1]] - shifts[[2]] #Active minus Passive
    
    
    X <- rep((groupno-(1/3)),length(shift))
    Y <- c(shift)
    colourscheme <- getColourScheme(group=group)
    col <- colourscheme[[group]][['T']]
    points(x=X,y=Y,pch=16,cex=1.5,col=col)#as.character(styles$color_trans[groupno]))
    # if (group == '30implicit'){
    #   abline(h = c(0,mean(c(newdata))), col = col, lty = 2)
    # }
    
    meandist <- getConfidenceInterval(data=c(shift), method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
    
    #include density dist
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
    # #include shaded distribution?
    # polygon(x=DY+groupno, y=DX, border=FALSE, col=col) #as.character(styles$color_trans[groupno]))
    
    col <- colourscheme[[group]][['S']]
    lines(x=rep(groupno,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
    #print(meandist$CI95)
    points(x=groupno,y=mean(c(shift)),pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
  }
  axis(side=1, at=c(1,2,3,4),labels=c('NI','I','CJ','HV'))
  axis(side=2, at=c(0,-5,-10,-15),labels=c('0','-5','-10','-15'))#,cex.axis=0.85)
  
  if (target == 'svg') {
    dev.off()
  }
  
}



# Statistics-----

getLocalization4ANOVA <- function(styles, shifts = FALSE) {
  
  LOCaov <- NA
  
  for (groupno in c(1:length(styles$group))){
    #startingID <- 1
    
    group <- styles$group[groupno]
    
    df <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F) #loc_p3 files correct for biases when generated
    #then we want the mean bias_deg for each pp
    df <- aggregate(bias_deg ~ participant * rotated_b * passive_b, data=df, FUN=mean)
    #below is basically a short-cut way of baseline correcting, when shifts = TRUE
    #it subtracts the aligned from the rotated
    #but will only do it if data is in correct order, because it compares first and second, second and third, etc.
    if (shifts) {
      df <- aggregate(bias_deg ~ participant * passive_b, data=df, FUN=diff)
    }
    dat <- data.frame(group,df)
    
    if (is.data.frame(LOCaov)) {
      LOCaov <- rbind(LOCaov, dat)
    } else {
      LOCaov <- dat
    }
    
  }
  #set relevant columns as factors:
  LOCaov$group <- as.factor(LOCaov$group)
  LOCaov$passive_b <- as.factor(LOCaov$passive_b)
  #LOCaov$rotated_b <- as.factor(LOCaov$rotated_b)
  if (!shifts) {
    LOCaov$rotated_b  <- as.factor(LOCaov$rotated_b) #when shifts != TRUE it still adds the session in
  }
  return(LOCaov)
}

localizationANOVA <- function(test = 'omnibus') {
  
  styles <- getStyle()
  
  if (test == 'omnibus'){
    LOC4aov <- getLocalization4ANOVA(styles)
    LOC4aov$participant <- as.factor(LOC4aov$participant)
    LocalizationAOV <- ezANOVA(data=LOC4aov, wid=participant, dv=bias_deg, within=c(rotated_b,passive_b), between=group, type=3, return_aov=TRUE) 
    #can remove: between=group, but adding it in gives a complete picture
    #note that for this we only care about main effect of rotated_b (session)
    
    print(LocalizationAOV[1])
  }
  
  if (test == 'shifts') {
    
    LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    LOC4aov$participant <- as.factor(LOC4aov$participant)
    LocalizationAOV <- ezANOVA(data=LOC4aov, wid=participant, dv=bias_deg, within=passive_b, between=group, type=3, return_aov=TRUE)
    
    print(LocalizationAOV[1])
    
  }
  
  if (test == 'passive') {
    LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    LOC4aov <- LOC4aov[which(LOC4aov$passive_b == 1),] #get just passive reaches (proprioceptive information only)
    LOC4aov$participant <- as.factor(LOC4aov$participant)
    LocalizationAOV <- ezANOVA(data=LOC4aov, wid=participant, dv=bias_deg, between=group, type=3, return_aov=TRUE)
    
    print(LocalizationAOV[1:2])
    
  }
  
  #added below just to see results in active
  if (test == 'active') {
    LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    LOC4aov <- LOC4aov[which(LOC4aov$passive_b == 0),] #get active reaches only
    LOC4aov$participant <- as.factor(LOC4aov$participant)
    LocalizationAOV <- ezANOVA(data=LOC4aov, wid=participant, dv=bias_deg, between=group, type=3, return_aov=TRUE)

    print(LocalizationAOV[1:2])

  }
}

#follow-up for significant ANOVA in active localization
activelocComparisonMeans <- function(){
  
  #library(emmeans) #changed from lsmeans
  
  styles <- getStyle()
  #blockdefs <- list('first'=c(1,3),'second'=c(4,3),'last'=c(76,15))
  
  LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
  LOC4aov <- LOC4aov[which(LOC4aov$passive_b == 0),] #get active reaches only
  LOC4aov$participant <- as.factor(LOC4aov$participant)
  secondAOV <- aov_ez("participant","bias_deg",LOC4aov,between="group")
  
  #nice(secondAOV, correction = 'none') #correction set to none since first AOV reveals no violation of sphericity
  #summary(secondAOV) #shows all results
  #run code above for figuring out df
  #output is the same
  #follow-ups using lsmeans
  
  cellmeans <- lsmeans(secondAOV$aov,specs=c('group'))
  print(cellmeans)
}

activelocComparisons <- function(method='sidak'){
  styles <- getStyle()
  
  
  LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
  LOC4aov <- LOC4aov[which(LOC4aov$passive_b == 0),] #get active reaches only
  LOC4aov$participant <- as.factor(LOC4aov$participant)
  secondAOV <- aov_ez("participant","bias_deg",LOC4aov,between="group")
  #based on cellmeans, confidence intervals and plots give us an idea of what contrasts we want to compare
  #we use implicit as a reference and compare all groups to it
  #compare cursor jump and hand view as well?
  
  #contrats will compare each group to implicit
  #add contrast comparing CJ to HV?
  EXvsIM <- c(1,-1,0,0)
  CJvsIM <- c(0,-1,1,0)
  HVvsIM <- c(0,-1,0,1)
  #CJvsHV <- c(0,0,1,-1) #remember to add this to the list

  
  
  contrastList <- list('Instr vs. Non-instr'=EXvsIM, 'Cursor Jump vs. Non-instr'=CJvsIM, 'Hand View vs. Non-Instr'=HVvsIM)#, 
                       #'Cursor Jump vs. Hand View'=CJvsHV)
  
  comparisons<- contrast(lsmeans(secondAOV$aov,specs=c('group')), contrastList, adjust=method)
  
  print(comparisons)
  
  #when including CJvsHV, only HVvsIM is significant. That is, CJvsHV is not significant.
  #not including CJvsHV, only HVvsIM is significant.
}

#effect size
getactivelocComparisonEffSize <- function(method = 'sidak'){
  comparisons <- activelocComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(active localization shift) accounted for 
  #by the difference between group1 and group2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#so next we ask whether Predicted Sensory COnsequences are affected by groups(explicit knowledge)?

getPredictedSensoryConsequences <- function(styles) {
  
  #first, combine all data from all groups
  alldf <- NA
  
  for (groupno in c(1:length(styles$group))){
    group <- styles$group[groupno]
    df <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F)
    
    if (is.data.frame(alldf)) {
      alldf <- rbind(alldf, df)
    } else {
      alldf <- df
    }
  }
  
  #then get the updates in predicted sensory consequences
  group       <- c()
  participant <- c()
  handangle   <- c()
  pred_update <- c()
  
  handangles <- unique(alldf$handangle_deg)
  groups <- unique(alldf$group)
  
  for (grp in groups) {
    
    participants <- unique(alldf$participant[which(alldf$group == grp)])
    
    for (pp in participants) {
      
      for (angle in handangles) {
        #get data needed
        subdf <- alldf[which(alldf$group == grp & alldf$participant == pp & alldf$handangle_deg == angle),]
        
        # get all the localization responses for this participant at this angle:
        AlAct <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 0)]
        AlPas <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 1)]
        RotAct <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 0)]
        RotPas <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 1)]
        
        # get the update in predicted sensory consequences:
        #first the difference of rotated and aligned in both active and passive
        #then the difference between active and passive to get UPSC
        UPSC <- (RotAct - AlAct) - (RotPas - AlPas)
        
        # store in new vectors:
        group <- c(group, grp)
        participant <- c(participant, pp)
        handangle <- c(handangle, angle)
        pred_update <- c(pred_update, UPSC)
      }
    }
  }
  
  alldf <- data.frame(group, participant, handangle, pred_update)
  alldf$group  <- factor(alldf$group, levels = c('30implicit', '30explicit', 'cursorjump', 'handview'))
  alldf$handangle   <- as.factor(alldf$handangle)
  alldf$participant <- as.character(alldf$participant) 
  
  return(alldf)
  
}

predictedConsequencesANOVA <- function() {
  styles <- getStyle()
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant * group, data=df, FUN=mean)
  df$participant <- as.factor(df$participant)
  PSC <- ezANOVA(data = df, wid=participant, dv=pred_update, between=group, type=3, return_aov=TRUE)
  print(PSC[1:2])
  
}

#we don't see a group effect in Pred Cons
#but our plot for Pred Cons makes it seem that hand view is almost at 0
#So we test each group and its difference from 0 (t-test)

predConsTtests <- function() {
  styles <- getStyle()
  #Hand view t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'handview'),]
  
  cat('Hand View group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$pred_update, mu=0))
  
  #Instructed t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30explicit'),]
  
  cat('Instructed group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$pred_update, mu=0))
  
  #Non-instructed t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30implicit'),]
  
  cat('Non-Instructed group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$pred_update, mu=0))
  
  #Cursor Jump t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'cursorjump'),]
  
  cat('Cursor Jump group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$pred_update, mu=0))
}

#run t-tests for passive localization as well
#this function is exactly the same as getPredictedSensoryConsequences
#but now we only want Passive reaches in Rotated minus Aligned
getPasLocShifts <- function(styles){
  
  #first, combine all data from all groups
  alldf <- NA
  
  for (groupno in c(1:length(styles$group))){
    group <- styles$group[groupno]
    df <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F)
    
    if (is.data.frame(alldf)) {
      alldf <- rbind(alldf, df)
    } else {
      alldf <- df
    }
  }
  
  #then get the shifts in proprioception
  group       <- c()
  participant <- c()
  handangle   <- c()
  prop_recal <- c()
  
  handangles <- unique(alldf$handangle_deg)
  groups <- unique(alldf$group)
  
  for (grp in groups) {
    
    participants <- unique(alldf$participant[which(alldf$group == grp)])
    
    for (pp in participants) {
      
      for (angle in handangles) {
        #get data needed
        subdf <- alldf[which(alldf$group == grp & alldf$participant == pp & alldf$handangle_deg == angle),]
        
        # get all the localization responses for this participant at this angle:
        AlAct <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 0)]
        AlPas <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 1)]
        RotAct <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 0)]
        RotPas <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 1)]
        
        # get the update in predicted sensory consequences:
        #first the difference of rotated and aligned in both active and passive
        #then the difference between active and passive to get UPSC
        PR <- RotPas - AlPas
        
        # store in new vectors:
        group <- c(group, grp)
        participant <- c(participant, pp)
        handangle <- c(handangle, angle)
        prop_recal <- c(prop_recal, PR)
      }
    }
  }
  
  alldf <- data.frame(group, participant, handangle, prop_recal)
  alldf$group  <- factor(alldf$group, levels = c('30implicit', '30explicit', 'cursorjump', 'handview'))
  alldf$handangle   <- as.factor(alldf$handangle)
  alldf$participant <- as.character(alldf$participant) 
  
  return(alldf)
} 
  
pasLocTtests <- function() {
  styles <- getStyle()
  
  #Hand view t-test
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'handview'),]
  
  cat('Hand View group proprioceptive recalibration compared to 0:\n')
  print(t.test(subdf$prop_recal, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$prop_recal, mu=0))
  
  #Instructed t-test
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30explicit'),]
  
  cat('Instructed group proprioceptive recalibration compared to 0:\n')
  print(t.test(subdf$prop_recal, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$prop_recal, mu=0))
  
  #Non-instructed t-test
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30implicit'),]
  
  cat('Non-Instructed group proprioceptive recalibration compared to 0:\n')
  print(t.test(subdf$prop_recal, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$prop_recal, mu=0))
  
  #Cursor Jump t-test
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'cursorjump'),]
  
  cat('Cursor Jump group proprioceptive recalibration compared to 0:\n')
  print(t.test(subdf$prop_recal, mu=0, alternative='less'))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdf$prop_recal, mu=0))
}

#Grouped Correlation Section -----
#Proprioceptive Recalibration
getPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  
  #newdf <- merge(df, df2, by='participant') #merge two df's together, according to participant
  #newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  newdf <- cbind(df, df2$reachdeviation)
  colnames(newdf) <- c('participant', 'group', 'prop_recal', 'reachdeviation')
  
  
  return(newdf)
}

plotPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7A_correlation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getPropExcData(styles)
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", ylab = 'No Cursor Reaches \n Without Strategy (°)', xlab = 'Shifts in Passive Localization (°)',
       bty='n', xlim= c(-30,20), ylim= c(-15,25), xaxt='n', yaxt='n', asp=1, cex=.85)
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20), cex=0.85)
  axis(side=1, at=c(-30,-20,-10,0,10,20), cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(reachdev ~ prop_recal)
  
  
  x <- seq(-28,7,.1) #min and max of pror_recal
  
  pred1 <- predict(mod1, newdata=data.frame(prop_recal=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$prop_recal, data$reachdeviation, pch=16, cex=1.5,
       col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(prop_recal)[1],range(prop_recal)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(2, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.121"))), col='#a6a6a6', bty='n', cex=.85)

  # legend(10,-8,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
  #        col=c(impcol,expcol,cujcol,hancol),
  #        pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}

#Predicted Sensory Consequences
getPredExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  
  #newdf <- merge(df, df2, by='participant') #merge two df's together, according to participant
  #newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  newdf <- cbind(df, df2$reachdeviation)
  colnames(newdf) <- c('participant', 'group', 'pred_update', 'reachdeviation')
  
  return(newdf)
  
  #mymod <- glm(reachdeviation ~ factor(group) + pred_update, data=newdf)
  #summary(mymod)
}

plotPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7B_correlation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getPredExcData(styles)
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", ylab = 'No Cursor Reaches \n Without Strategy (°)', xlab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-30,20), ylim= c(-15,25), xaxt='n', yaxt='n', asp=1, cex=.85)
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20), cex=0.85)
  axis(side=1, at=c(-30,-20,-10,0,10,20), cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(reachdev ~ pred_update)
  
  
  #x <- seq(-5,19,.1) #min and max of reachdev
  x <- seq(-9,12,.1) #min and max of pred_update
  
  #pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  pred1 <- predict(mod1, newdata=data.frame(pred_update=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$pred_update, data$reachdeviation, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(pred_update)[1],range(pred_update)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(2, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.089"))), col='#a6a6a6', bty='n', cex=.85)

  
  # legend(10,-8,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
  #        col=c(impcol,expcol,cujcol,hancol),
  #        pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

#GLM predicting RAE from both Prop_Recal and Pred_Update
getPropPredGLM <- function(){
  
  styles <- getStyle()
  propdf <- getPropExcData(styles)
  preddf <- getPredExcData(styles)
  
  newdf <- cbind(propdf, preddf$pred_update)
  colnames(newdf) <- c('participant', 'group', 'prop_recal', 'reachdeviation', 'pred_update')
  #newdf <- newdf[(newdf$group == 'handview'),]
  
  pred_update <- newdf$pred_update
  prop_recal <- newdf$prop_recal
  RAE <- newdf$reachdeviation
  
  mod1 <- glm(RAE ~ pred_update + prop_recal)
  print(summary(mod1))
  
  #effect size
  #we can also get squared semi-partial correlations as a measure of effect size
  # This is the contribution of one predictor to the DV, after controlling for the other predictor.
  mod1 <- lm(RAE ~ pred_update + prop_recal)
  getDeltaRsquare(mod1)
  
  #test for collinearity

  
  #vif(c(as.integer(pred_update), as.integer(prop_recal)))
  print(vif(newdf[c(3,5)]))
  #these are exactly the same from each other, given two predictors, but they are low
  #so there is no collinearity
  #hence we say that prop and pred are both contributing to RAE, but independently
  

}

#take this model and show predicted RAE and actual RAE
#if the model predicts the actual RAE well, then we know that predictions from the model are valid
#make a plot for predicted RAE and actual RAE
getPredActRAE <- function(){
  
  styles <- getStyle()
  propdf <- getPropExcData(styles)
  preddf <- getPredExcData(styles)
  
  newdf <- cbind(propdf, preddf$pred_update)
  colnames(newdf) <- c('participant', 'group', 'prop_recal', 'reachdeviation', 'pred_update')
  #newdf <- newdf[(newdf$group == 'handview'),]
  
  pred_update <- newdf$pred_update
  prop_recal <- newdf$prop_recal
  RAE <- newdf$reachdeviation
  
  mod1 <- glm(RAE ~ pred_update + prop_recal)
  
  # A + Bx + Cy = predicted RAE + error term
  #x is pred_update, y is prop_recal
  # A, B, C come from mod1
  A <- as.numeric(mod1[[1]][1])
  B <- as.numeric(mod1[[1]][2]) #B is pred_update coefficient
  C <- as.numeric(mod1[[1]][3]) #C is prop_recal coefficient
  
  for(participant in 1:length(newdf$participant)){
    pred <- newdf[participant,]$pred_update
    prop <- newdf[participant,]$prop_recal
    
    RAEpred <- A + (B*pred) + (C*prop)
    
    newdf$RAE_pred[participant] <- RAEpred
  }
  return(newdf)
}

#this plot below is essentially, the check for our model. We see that it undershoots data on the lower end, and overshoots those in the higher end.
#We can show this by running:
# data <- getPredActRAE()
# qqplot(data$RAE_pred, data$reachdeviation)
# segments(0, 0, 15.6, 15.6, col='#343434')

plotLinesPredActRAE <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7C_line_correlation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  data <- getPredActRAE()
  mod1 <- lm(data$RAE_pred ~ data$reachdeviation)
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Actual and Predicted Reach Aftereffects", xlab = 'Actual No Cursor Reaches \n Without Strategy (°)', ylab = 'Predicted No Cursor Reaches \n Without Strategy (°)',
       bty='n', ylim= c(-5,25), xlim= c(-5,25), xaxt='n', yaxt='n', asp=1, cex=.85)

  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-5,0,5,10,15,20), cex=0.85)
  axis(side=1, at=c(-5,0,5,10,15,20), cex=0.85)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$RAE_pred, pch=16, cex=.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #add line, need intercept and slope
  #create glm based on predicted RAE and Actual RAE
  # mod <- glm(data$RAE_pred ~ data$reachdeviation)
  # INT <- as.numeric(mod[[1]][1])
  # SLOPE <- as.numeric(mod[[1]][2])
  # abline(a=INT,b=SLOPE,col="#343434")
  
  # #Reg line
  # reglinex <- seq(range(pred)[1],range(pred)[2],.1)
  # abX <- range(reglinex)
  # abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  # lines(abX, abY, col='#343434')
  
  # # We can just plot the diagonal
  # diagonal is intercept at 0 and slope of 1
  segments(-5.3, -5.3, 20, 20, col=8)
  
  #we can plot distances from point to diagonal by specifying (x,y) for from and to
  segments(data$reachdeviation, data$RAE_pred, data$reachdeviation, data$reachdeviation, col= alpha(cols, .3))
  
  
  legend(13,-1,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(impcol,expcol,cujcol,hancol),
         pch=16,bty='o',cex=.6)
  
  
  #print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotPredActRAE <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7D_correlation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  data <- getPredActRAE()
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Actual and Predicted Reach Aftereffects", xlab = 'Actual No Cursor Reaches - Without Strategy (°)', ylab = 'Predicted No Cursor Reaches - Without Strategy (°)',
       bty='n', ylim= c(-5,20), xlim= c(-5,20), xaxt='n', yaxt='n', asp=1)
  
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-5,0,5,10,15,20))#, cex=0.85)
  axis(side=1, at=c(-5,0,5,10,15,20))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  # pred_update <- data$pred_update
  # reachdev <- data$reachdeviation
  # pred <- data$RAE_pred
  # act <- data$reachdeviation
  # mod1 <- lm(act ~ pred)
  # 
  # 
  # #x <- seq(-5,19,.1) #min and max of reachdev
  # x <- seq(0.2,15.6,.1) #min and max of RAE_pred
  # 
  # #pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # pred1 <- predict(mod1, newdata=data.frame(pred=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$RAE_pred, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #add line, need intercept and slope
  #create glm based on predicted RAE and Actual RAE
  # mod <- glm(data$RAE_pred ~ data$reachdeviation)
  # INT <- as.numeric(mod[[1]][1])
  # SLOPE <- as.numeric(mod[[1]][2])
  # abline(a=INT,b=SLOPE,col="#343434")
  
  # #Reg line
  # reglinex <- seq(range(pred)[1],range(pred)[2],.1)
  # abX <- range(reglinex)
  # abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  # lines(abX, abY, col='#343434')
  
  # # We can just plot the diagonal
  # diagonal is intercept at (0,0) and slope of 1
  segments(-5, -5, 20, 20, col='#343434')
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  #legend(12, 0, c(as.expression(bquote(""~ r^2 ~ "= 0.089"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(12,-3,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(impcol,expcol,cujcol,hancol),
         pch=16,bty='o',cex=.25)
  
  #print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotPropPredRelationships <- function(target='inline') {
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig7_correlation.svg', width=14, height=4, pointsize=14, system_fonts=list(sans='Arial'))
  }
  
  par(mfrow=c(1,3), mar=c(4,5,2,5))
  
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  
  
  # # # # # # # # # #
  # panel A: Prop Recal and RAE
  plotPropGroupCorrelations()
  #mtext('A', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('A', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  
  # # # # # # # # # #
  # panel B: PSC and RAE
  plotPredGroupCorrelations()
  #mtext('B', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('B', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  
  # # # # # # # # # #
  # panel C: Predicted and Actual RAE
  plotLinesPredActRAE()
  #mtext('C', side=3, outer=TRUE, at=c(0,1), line=-1, adj=0, padj=1)
  mtext('C', side=3, outer=FALSE, line=-1, adj=0, padj=1)
  
  if (target == 'svg') {
    dev.off()
  }
  
}


# Additional checks (Not included in Manuscript)----

# Given that both predictions and proprioception predict RAE independently,
# RAE ~ PSC: the residuals here should be predicted by Prop_Recal, and vice versa
# We then compare these with the original plots of the relationship between RAE ~ Prop and RAE ~ Pred

testPredResByProp <- function(){
  
  data <- getPredActRAE()
  
  RAE <- data$reachdeviation
  pred <- data$pred_update
  prop <- data$prop_recal
  
  mod1 <- glm(RAE ~ pred)
  mod1_Res <- resid(mod1)
  
  mod2 <- glm(mod1_Res ~ prop)
  print(summary(mod2))
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  
  plot(NA,NA, 
       xlim = c(-30,15), ylim = c(-15, 25),
       ylab = 'Residuals of RAE ~ PSC', xlab = 'Proprioception', main = 'Prediction Residuals and Proprioceptive Recalibration',
       bty='n', xaxt='n', yaxt='n')
  
  #CIs
  mod2A <- lm(mod1_Res ~ prop) #need lm function for it to work
  
  
  #x <- seq(-5,19,.1) #min and max of reachdev
  x <- seq(-28,7,.1) #min and max of prop
  
  #pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  pred1 <- predict(mod2A, newdata=data.frame(prop=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(prop, mod1_Res, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  #Reg line
  reglinex <- seq(range(prop)[1],range(prop)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod2$coefficients[2] + mod2$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add legend and r-squared
  legend(2, -8, c(as.expression(bquote(""~ r^2 ~ "= 0.2056"))), col='#a6a6a6', bty='n', cex=.85)
}

testPropResByPred <- function(){
  
  data <- getPredActRAE()
  
  RAE <- data$reachdeviation
  pred <- data$pred_update
  prop <- data$prop_recal
  
  mod1 <- glm(RAE ~ prop)
  mod1_Res <- resid(mod1)
  
  mod2 <- glm(mod1_Res ~ pred)
  print(summary(mod2))
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  
  plot(NA,NA, 
       xlim = c(-15,25), ylim = c(-15, 25),
       ylab = 'Residuals of RAE ~ Prop', xlab = "Predictions",  main = 'Proprioception Residuals and Predicted Sensory Consequences',
       bty='n', xaxt='n', yaxt='n')
  
  
  #CIs
  mod2A <- lm(mod1_Res ~ pred) #need lm function for it to work
  
  
  #x <- seq(-5,19,.1) #min and max of reachdev
  x <- seq(-9,12,.1) #min and max of prop
  
  #pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  pred1 <- predict(mod2A, newdata=data.frame(pred=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(pred, mod1_Res, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-10,0,10, 20))#, cex=0.85)
  
  #Reg line
  reglinex <- seq(range(pred)[1],range(pred)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod2$coefficients[2] + mod2$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add legend and r-squared
  legend(12, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.1794"))), col='#a6a6a6', bty='n', cex=.85)
}

#We can also get the correlation of the residual plots
#Both are significant
getPropResidByPredCorrelation <- function(){
  data <- getPredActRAE()
  
  RAE <- data$reachdeviation
  pred <- data$pred_update
  prop <- data$prop_recal
  
  mod1 <- glm(RAE ~ prop)
  mod1_Res <- resid(mod1)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(mod1_Res, pred))
  
}

getPredResidByPropCorrelation <- function(){
  data <- getPredActRAE()
  
  RAE <- data$reachdeviation
  pred <- data$pred_update
  prop <- data$prop_recal
  
  mod1 <- glm(RAE ~ pred)
  mod1_Res <- resid(mod1)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(mod1_Res, prop))
  
}

plotRelationships <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7E_correlationwres.svg', width=12, height=10, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,2))
  
  
  plotPropGroupCorrelations()
  testPropResByPred()
  plotPredGroupCorrelations()
  testPredResByProp()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#The idea is that If we take the residuals of Prop and RAE and predict them from Pred,
#The existence of a relationship here will show that the two are independent (This is already shown from Multiple regression and VIF analysis)
#This is because we take away one component, and still see a relationship, so that means that they were independent.

#Some other additional checks:
#show that some other variables with similar stats as Pas don't predict anything: 
#take the PSQ, shuffle them and subtract from Act, to get fake Pas scores. 
#These fake Pas scores should not correlate with residuals after predicting RAE with the real Act scores. 

testGetPSC <- function(styles) {
  
  #first, combine all data from all groups
  alldf <- NA
  
  for (groupno in c(1:length(styles$group))){
    group <- styles$group[groupno]
    df <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F)
    
    if (is.data.frame(alldf)) {
      alldf <- rbind(alldf, df)
    } else {
      alldf <- df
    }
  }
  
  #then get the updates in predicted sensory consequences
  group       <- c()
  participant <- c()
  handangle   <- c()
  act_loc     <- c()
  pas_loc     <- c()
  pred_update <- c()
  
  handangles <- unique(alldf$handangle_deg)
  groups <- unique(alldf$group)
  
  for (grp in groups) {
    
    participants <- unique(alldf$participant[which(alldf$group == grp)])
    
    for (pp in participants) {
      
      for (angle in handangles) {
        #get data needed
        subdf <- alldf[which(alldf$group == grp & alldf$participant == pp & alldf$handangle_deg == angle),]
        
        # get all the localization responses for this participant at this angle:
        AlAct <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 0)]
        AlPas <- subdf$bias_deg[which(subdf$rotated_b == 0 & subdf$passive_b == 1)]
        RotAct <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 0)]
        RotPas <- subdf$bias_deg[which(subdf$rotated_b == 1 & subdf$passive_b == 1)]
        
        # get the update in predicted sensory consequences:
        #first the difference of rotated and aligned in both active and passive
        #then the difference between active and passive to get UPSC
        UPSC <- (RotAct - AlAct) - (RotPas - AlPas)
        ACT <- RotAct - AlAct
        PAS <- RotPas - AlPas
        
        # store in new vectors:
        group <- c(group, grp)
        participant <- c(participant, pp)
        handangle <- c(handangle, angle)
        act_loc <- c(act_loc, ACT)
        pas_loc <- c(pas_loc, PAS)
        pred_update <- c(pred_update, UPSC)
      }
    }
  }
  
  alldf <- data.frame(group, participant, handangle, act_loc, pas_loc, pred_update)
  alldf$group  <- factor(alldf$group, levels = c('30implicit', '30explicit', 'cursorjump', 'handview'))
  alldf$handangle   <- as.factor(alldf$handangle)
  alldf$participant <- as.character(alldf$participant) 
  
  return(alldf)
  
}

getFakePas <- function(){
  
  styles <- getStyle()
  data <- testGetPSC(styles)
  
  data$pred_update <- sample(data$pred_update, replace = FALSE)
  data$fake_pas <- data$act_loc - data$pred_update
  
  subdata1 <- aggregate(fake_pas ~ participant*group, data=data, FUN=mean)
  subdata2 <- aggregate(act_loc ~ participant*group, data=data, FUN=mean)
  
  dat <- cbind(subdata2, subdata1$fake_pas)
  colnames(dat) <- c('participant', 'group', 'act_loc', 'fake_pas')
  
  #then we want to add exclusive angular deviations to the df above
  data2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  data2 <- data2[which(data2$strategy == 'exclusive'),]
  newdf <- cbind(dat, data2$reachdeviation)
  colnames(newdf) <- c('participant', 'group', 'act_loc', 'fake_pas', 'RAE')
  
  return(newdf)
}

getACTResidByFakePASCorrelation <- function(){
  styles <- getStyle()
  data <- getFakePas()
  
  RAE <- data$RAE
  ACT <- data$act_loc
  PAS <- data$fake_pas
  
  mod1 <- glm(RAE ~ ACT)
  mod1_Res <- resid(mod1)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(summary(mod1))
  print(cor.test(mod1_Res, PAS))
  
  #VIF of ACT and fake PAS?
  #print(vif(data[c(3,4)]))
  
  #or VIF of fake PAS and shuffled or fake PSQ
  #data$fPSQ <- ACT - PAS
  #print(vif(data[c(4,6)]))
  
  #or VIF of fake PAS and original PSQ
  dat2 <- testGetPSC(styles)
  dat2 <- aggregate(pred_update ~ participant*group, data=dat2, FUN=mean)
  data$PSQ <- dat2$pred_update
  print(vif(data[c(4,6)]))
}

#Can also check it from the other way around


getFakeAct <- function(){
  
  styles <- getStyle()
  data <- testGetPSC(styles)
  
  data$pred_update <- sample(data$pred_update, replace = FALSE)
  data$fake_act <- data$pas_loc + data$pred_update
  
  subdata1 <- aggregate(fake_act ~ participant*group, data=data, FUN=mean)
  subdata2 <- aggregate(pas_loc ~ participant*group, data=data, FUN=mean)
  
  dat <- cbind(subdata2, subdata1$fake_act)
  colnames(dat) <- c('participant', 'group', 'pas_loc', 'fake_act')
  
  #then we want to add exclusive angular deviations to the df above
  data2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  data2 <- data2[which(data2$strategy == 'exclusive'),]
  newdf <- cbind(dat, data2$reachdeviation)
  colnames(newdf) <- c('participant', 'group', 'pas_loc', 'fake_act', 'RAE')
  
  return(newdf)
}

getPASResidByFakeACTCorrelation <- function(){
  styles <- getStyle()
  data <- getFakeAct()
  
  RAE <- data$RAE
  ACT <- data$fake_act
  PAS <- data$pas_loc
  
  mod1 <- glm(RAE ~ PAS)
  mod1_Res <- resid(mod1)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(summary(mod1))
  print(cor.test(mod1_Res, ACT))
  
  #vif of ACT and PAS?
  #print(vif(data[c(3,4)]))
  
  #or VIF of fake ACT and shuffled or fake PSQ
  #data$fPSQ <- ACT - PAS
  #print(vif(data[c(4,6)]))
  
  #or VIF of fake ACT and original PSQ
  dat2 <- testGetPSC(styles)
  dat2 <- aggregate(pred_update ~ participant*group, data=dat2, FUN=mean)
  data$PSQ <- dat2$pred_update
  print(vif(data[c(4,6)]))
  
}

#We see that fake PAS scores do not correlate with the residuals of RAE ~ ACT,
# even if ACT is a significant predictor of RAE
#Same is true for the other way, fake ACT scores do not correlate with residuals of RAE ~ PAS,
# even if PAS is a significant predictor of RAE

#We can also look into the VIF of Active and Passive (ACT and PAS original scores)

getActPasCollinearity <- function(){
  
  styles <- getStyle()
  data <- testGetPSC(styles)
  
  #get active and passive scores
  subdata1 <- aggregate(act_loc ~ participant*group, data=data, FUN=mean)
  subdata2 <- aggregate(pas_loc ~ participant*group, data=data, FUN=mean)
  
  newdf <- cbind(subdata1, subdata2$pas_loc)
  colnames(newdf) <- c('participant', 'group', 'act_loc', 'pas_loc')
  
  print(vif(newdf[c(3,4)]))
  
}

# We see that VIF for ACT and PAS is high (vif = 3.37)

#But it is important to look into group/ manipulation effect.
#We can take the average PSQ and PAS, then subtract these from each individual PSQ or PAS score in each group
#Then we replot it and re-run the regression/correlation test
# If we see no effect by doing such a thing, then whatever we saw before could mostly be explained by group differences
# or the manipulation. This means that we are not only seeing the effects due to a higher sample size

testGroupEffect <- function(){
  
  styles <- getStyle()
  data <- testGetPSC(styles)
  
  #get active and passive scores
  subdata1 <- aggregate(act_loc ~ participant*group, data=data, FUN=mean)
  subdata2 <- aggregate(pas_loc ~ participant*group, data=data, FUN=mean)
  subdata3 <- aggregate(pred_update ~ participant*group, data=data, FUN=mean)
  
  dat <- cbind(subdata1, subdata2$pas_loc, subdata3$pred_update)
  
  
  #then we want to add exclusive angular deviations to the df above
  subdata2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  subdata2 <- subdata2[which(subdata2$strategy == 'exclusive'),]
  newdf <- cbind(dat, subdata2$reachdeviation)
  colnames(newdf) <- c('participant', 'group', 'act_loc', 'pas_loc', 'pred_update', 'RAE')
  
  groups <- unique(newdf$group)
  alldat <- data.frame()
  
  for (group in groups){
    subnewdf <- newdf[which(newdf$group == group),]
    
    meanACT <- mean(subnewdf$act_loc)
    meanPAS <- mean(subnewdf$pas_loc)
    meanPRED <- mean(subnewdf$pred_update)
    
    subnewdf$act_loc <- subnewdf$act_loc - meanACT
    subnewdf$pas_loc <- subnewdf$pas_loc - meanPAS
    subnewdf$pred_update <- subnewdf$pred_update - meanPRED
    
    if (prod(dim(alldat)) == 0){
      alldat <- subnewdf
    } else {
      alldat <- rbind(alldat, subnewdf)
    }
  }
  
  return(alldat)
}

#rerun the multiple regression
getMeanCorrectedGLM <- function(){
  
  data <- testGroupEffect()
  mod1 <- glm(RAE ~ pas_loc + pred_update, data = data)
  print(summary(mod1))
  
}

#lower AIC with original regression, so it is explaining data better (which makes sense because group effect is in this model)
#but is there a way to quantify this?

#compare original regression to this mean corrected regression
#if significant, this means that PAS and PSC contributing to RAE still holds,
#but group effects are explaining part of the data (i.e. there is a group effect)
#two models are non-nested, so we can use a coxtest (see help file for test)

#library(lmtest)
getOrigCorrRegComp <- function(){
  
  
  styles <- getStyle()
  propdf <- getPropExcData(styles)
  preddf <- getPredExcData(styles)
  
  newdf <- cbind(propdf, preddf$pred_update)
  colnames(newdf) <- c('participant', 'group', 'prop_recal', 'reachdeviation', 'pred_update')
  #newdf <- newdf[(newdf$group == 'handview'),]
  
  pred_update <- newdf$pred_update
  prop_recal <- newdf$prop_recal
  RAE <- newdf$reachdeviation
  
  lm1 <- glm(RAE ~ pred_update + prop_recal)
  
  
  data <- testGroupEffect()
  lm2 <- glm(RAE ~ pas_loc + pred_update, data = data)
  
  coxtest(lm1, lm2)
}

#The idea of the Cox test is the following: if the first model contains the correct set of regressors, 
#then a fit of the regressors from the second model to the fitted values from first model should have no further explanatory value. 
#But if it has, it can be concluded that model 1 does not contain the correct set of regressors.

#Hence, to compare both models the fitted values of model 1 are regressed on model 2 and vice versa. 
#A Cox test statistic is computed for each auxiliary model which is asymptotically standard normally distributed.

#Since values are significant, then model 1 has some additional explanatory value on model 2: see second result below

#we replot and test for PAS
plotMeanCorrectedPropCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7F_PropMeanCorrectedCorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- testGroupEffect()
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", ylab = 'No Cursor Reaches - Without Strategy (°)', xlab = 'Shifts in Corrected Passive Localization (°)',
       bty='n', xlim= c(-30,15), ylim= c(-15,25), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$pas_loc
  reachdev <- data$RAE
  mod1 <- lm(reachdev ~ prop_recal)
  
  
  x <- seq(-21.54,11.58,.1) #min and max of pror_recal
  
  pred1 <- predict(mod1, newdata=data.frame(prop_recal=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$pas_loc, data$RAE, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(prop_recal)[1],range(prop_recal)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(2, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.056"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(5,-7,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(impcol,expcol,cujcol,hancol),
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getMeanCorrectedRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- testGroupEffect()
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$RAE, dat$pas_loc))
  
}
# So it's still sigificant (p=0.02, r = -0.24), but this is less so than original data (r^2 went from .12 to .06).
# Now, we do the same for PSQ

plotMeanCorrectedPredCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7G_PredMeanCorrectedCorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- testGroupEffect()
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol,hancol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", ylab = 'No Cursor Reaches - Without Strategy (°)', xlab = 'Shifts in Corrected Predictions (°)',
       bty='n', xlim= c(-15,25), ylim= c(-15,25), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred <- data$pred_update
  reachdev <- data$RAE
  mod1 <- lm(reachdev ~ pred)
  
  
  x <- seq(-6.59,14.33,.1) #min and max of pred
  
  pred1 <- predict(mod1, newdata=data.frame(pred=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$pred_update, data$RAE, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(pred)[1],range(pred)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(2, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.018"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(5,-7,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(impcol,expcol,cujcol,hancol),
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getMeanCorrectedRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- testGroupEffect()
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$RAE, dat$pred_update))
  
}
# So it's NOT sigificant (p=0.21, r = -0.13), and r^2 went from .089 to .018.
# Non-significance means that any effect we've previously seen in the regression was due to the group manipulation.
# That is, at least one group could be the cause that a relationship was seen. See Pre Group Correlation Section

#What if we leave out the Hand View group, do we still see such a relationship for both Prop and Pred?
plotNoHVPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7H_NoHVPropcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getPropExcData(styles)
  data <- data[-which(data$group == 'handview'),]
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", ylab = 'No Cursor Reaches - Without Strategy (°)', xlab = 'Shifts in Passive Localization (°)',
       bty='n', xlim= c(-30,15), ylim= c(-15,25), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(reachdev ~ prop_recal)
  
  
  x <- seq(-28.17,4.95,.1) #min and max of pror_recal
  
  pred1 <- predict(mod1, newdata=data.frame(prop_recal=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$prop_recal, data$reachdeviation, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(prop_recal)[1],range(prop_recal)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(2, -2, c(as.expression(bquote(""~ r^2 ~ "= 0.081"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(5,-5,legend=c('Non-instructed','Instructed','Cursor Jump'),
         col=c(impcol,expcol,cujcol),
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getNoHVRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getPropExcData(styles)
  dat <- dat[-which(dat$group == 'handview'),]
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}
# So for Prop without HV, correlation is still significant (p = 0.03, r = -0.28, r^2 = 0.08).

plotNoHVPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7I_NoHVPredcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getPredExcData(styles)
  data <- data[-which(data$group == 'handview'),]
  
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  cols <- c(expcol,impcol,cujcol)[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", ylab = 'No Cursor Reaches - Without Strategy (°)', xlab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-15,25), ylim= c(-15,25), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=2, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=1, at=c(-10,0,10, 20))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(reachdev ~ pred_update)
  
  
  #x <- seq(-5,19,.1) #min and max of reachdev
  x <- seq(-8.97,11.95,.1) #min and max of pred_update
  
  #pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  pred1 <- predict(mod1, newdata=data.frame(pred_update=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$pred_update, data$reachdeviation, pch=16, cex=1.5,
         col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(pred_update)[1],range(pred_update)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, 0, c(as.expression(bquote(""~ r^2 ~ "= 0.043"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-5,legend=c('Non-instructed','Instructed','Cursor Jump'),
         col=c(impcol,expcol,cujcol),
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getNoHVRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getPredExcData(styles)
  dat <- dat[-which(dat$group == 'handview'),]
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

# So for Pred without HV, correlation is NOT significant (p = 0.11, r = -0.21, r^2 = 0.04). This may suggest that HV
# was leading to the significance in relationship. BUT running Correlation for just HV shows that:
# HV Pred and RAE relationship is non-existent (p = 0.99, r = -0.003, r^2 = 0.00001)


#Per Group Correlation section----
getHVPredExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == 'handview'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == 'handview'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getHVRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getHVPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

plotHVPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_HVcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getHVPredExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(pred_update ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$pred_update, pch=16, cex=1.5,
         col= alpha(hancol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.00001"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Hand View',
         col=hancol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getCJPredExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == 'cursorjump'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == 'cursorjump'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getCJRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getCJPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

plotCJPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_CJcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getCJPredExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(pred_update ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$pred_update, pch=16, cex=1.5,
         col= alpha(cujcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.00007"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Cursor Jump',
         col=cujcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getNIPredExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == '30implicit'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == '30implicit'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getNIRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getNIPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

plotNIPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_NIcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getNIPredExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(pred_update ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$pred_update, pch=16, cex=1.5,
         col= alpha(impcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.091"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Non-Instructed',
         col=impcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getIPredExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == '30explicit'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == '30explicit'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getIRAEPredCorrelation <- function(){
  styles <- getStyle()
  dat <- getIPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$pred_update))
  
}

plotIPredGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_Icorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getIPredExcData(styles)
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Predicted Sensory Consequences", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Predictions (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  pred_update <- data$pred_update
  reachdev <- data$reachdeviation
  mod1 <- lm(pred_update ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$pred_update, pch=16, cex=1.5,
         col= alpha(expcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.091"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Instructed',
         col=expcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotPGPredCorr <- function(){
  par(mfrow = c(2,2))
  
  plotHVPredGroupCorrelations()
  plotCJPredGroupCorrelations()
  plotNIPredGroupCorrelations()
  plotIPredGroupCorrelations()
}

getPGPredCorr <- function(){
  
  cat('Hand View group - Predictions and Reach Aftereffects:\n')
  getHVRAEPredCorrelation()
  
  cat('Cursor Jump group - Predictions and Reach Aftereffects:\n')
  getCJRAEPredCorrelation()
  
  cat('Instructed group - Predictions and Reach Aftereffects:\n')
  getIRAEPredCorrelation()
  
  cat('Non Instructed group - Predictions and Reach Aftereffects:\n')
  getNIRAEPredCorrelation()
  
}

#do the same for proprioception

getHVPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == 'handview'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == 'handview'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getHVRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getHVPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}

plotHVPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_HVPropcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getHVPropExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Proprioception (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(prop_recal ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$prop_recal, pch=16, cex=1.5,
         col= alpha(hancol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.2481"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Hand View',
         col=hancol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getCJPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == 'cursorjump'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == 'cursorjump'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getCJRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getCJPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}

plotCJPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_CJPropcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getCJPropExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Proprioception (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(prop_recal ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$prop_recal, pch=16, cex=1.5,
         col= alpha(cujcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.1167"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Cursor Jump',
         col=cujcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getNIPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == '30implicit'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == '30implicit'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getNIRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getNIPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}

plotNIPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_NIPropcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getNIPropExcData(styles)
  colourscheme <- getColourScheme()
  #expcol <- colourscheme[['30explicit']][['S']]
  impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Proprioception (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(prop_recal ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$prop_recal, pch=16, cex=1.5,
         col= alpha(impcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.0294"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Non-Instructed',
         col=impcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

getIPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  subdf <- df[(df$group == '30explicit'),]
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  subdf2 <- df2[(df2$diffgroup == '30explicit'),]
  
  newdf <- merge(subdf, subdf2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

getIRAEPropCorrelation <- function(){
  styles <- getStyle()
  dat <- getIPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(cor.test(dat$reachdeviation, dat$prop_recal))
  
}

plotIPropGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig7J_IPropcorrelation.svg', width=5, height=5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  styles <- getStyle()
  #plot change in localization on y, and Without Strategy on X
  #data points will be coloured according to groups
  #one regression line
  #still need to separate points by group
  data <- getIPropExcData(styles)
  colourscheme <- getColourScheme()
  expcol <- colourscheme[['30explicit']][['S']]
  #impcol <- colourscheme[['30implicit']][['S']]
  #cujcol <- colourscheme[['cursorjump']][['S']]
  #hancol <- colourscheme[['handview']][['S']]
  #cols <- hancol[unclass(data$group)] #order matters, because of levels in group
  plot(NA, NA, main="Reach Aftereffects and Proprioceptive Recalibration", xlab = 'No Cursor Reaches - Without Strategy (°)', ylab = 'Shifts in Proprioception (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,15), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))#, cex=0.85)
  axis(side=2, at=c(-30,-20,-10,0,10))#, cex=0.85)
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(prop_recal ~ reachdev)
  
  
  # x <- seq(-5,19,.1)
  # 
  # pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  # 
  # polyX <- c(x,rev(x))
  # polyY <- c(pred1[,2], rev(pred1[,3]))
  # polygon(polyX, polyY, col='#dadada', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$prop_recal, pch=16, cex=1.5,
         col= alpha(expcol, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -10, c(as.expression(bquote(""~ r^2 ~ "= 0.083"))), col='#a6a6a6', bty='n', cex=1)
  
  legend(15,-25,legend='Instructed',
         col=expcol,
         pch=16,bty='o',cex=.25)
  
  print(summary(mod1))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

plotPGPropCorr <- function(){
  par(mfrow = c(2,2))
  
  plotHVPropGroupCorrelations()
  plotCJPropGroupCorrelations()
  plotNIPropGroupCorrelations()
  plotIPropGroupCorrelations()
}

getPGPropCorr <- function(){
  
  cat('Hand View group - Propriocetive Recalibration and Reach Aftereffects:\n')
  getHVRAEPropCorrelation()
  
  cat('Cursor Jump group - Propriocetive Recalibration and Reach Aftereffects:\n')
  getCJRAEPropCorrelation()
  
  cat('Instructed group - Propriocetive Recalibration and Reach Aftereffects:\n')
  getIRAEPropCorrelation()
  
  cat('Non Instructed group - Propriocetive Recalibration and Reach Aftereffects:\n')
  getNIRAEPropCorrelation()
  
}

# Winsorized correlation and incorporating Group to model ----
#If a spurious correlation exists, one solution would be to run a robust correlation test
#Winsorized correlation sets the values at the tails to a certain percentile value (20% is the default)
#This way, extreme values do not affect the data
#requires package:

#library(WRS2)

getRAEPropWinCorr<- function(){
  styles <- getStyle()
  dat <- getPropExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(wincor(dat$reachdeviation, dat$prop_recal))
  
}

#Correlation becomes r = -0.53, p = 0!

getRAEPredWinCorr <- function(){
  styles <- getStyle()
  dat <- getPredExcData(styles)
  #plot(dat$reachdeviation, dat$prop_recal)
  print(wincor(dat$reachdeviation, dat$pred_update))
  
}

#Correlation becomes r = -0.36, p = 0.0007!



#Code below is unnecessary, but tries to incorporate group into model (unsure if it is correct)
#car package is required here
#library(car)

getPropPredGroupGLM <- function(){

  #read in data
  styles <- getStyle()
  propdf <- getPropExcData(styles)
  preddf <- getPredExcData(styles)

  newdf <- cbind(propdf, preddf$pred_update)
  colnames(newdf) <- c('participant', 'group', 'prop_recal', 'reachdeviation', 'pred_update')

  #set variables for easier referencing
  pred_update <- newdf$pred_update
  prop_recal <- newdf$prop_recal
  RAE <- newdf$reachdeviation
  group <- newdf$group


  #dummy coding is automatically done in R (can use model.matrix to show this)
  #res <- model.matrix(~group, data = newdf)

  #ANOVA is a special case of regression, where all predictors are categorical, so we can use the same thing here
  #can pull out the Anova table, Anova from car takes care of unbalanced designs automatically (anova from base does not)

  #which mod to use?
  mod1<-lm(RAE ~ prop_recal + pred_update + group,data=newdf)
  #mod1<-lm(RAE ~ prop_recal + pred_update + group + prop_recal*pred_update*group,data=newdf)
  print(Anova(mod1))

  #to interpret contrasts of categorical variable run line below:
  summary(mod1)

  #Prop, Pred, Group are all significant (Running the longer model will give all interactions, and these are not significant)
  #So there is a group effect.
  #Looking into contrasts, we see that both handview and cursor jump are significant at alpha=.05

}