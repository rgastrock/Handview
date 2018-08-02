source('R/shared.R')

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

getLocalizationConfidenceIntervals <- function(groups=c('30implicit','30explicit','cursorjump')) {
  
  
  interpoints <- c(50,90,130)
  #interpoints <- seq(30,150,1)
  # iterations <- 10000 # ideally 10000 or more? no more iterations: use t-distribution
  percentiles <- c(0.025, 0.5, 0.975) # this is not actually used...#it is needed in confint_kernelregression
  
  for (group in groups) {
    
    df <- read.csv(sprintf('data/%s_localization.csv',group),stringsAsFactors=FALSE)
    
    # correct using 3rd order polynomial
    # and removing taps where the hand location is further than 20 degrees from the arc centre
    # df <- correctLocalizationsP3(df)
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
          confint_kernelregression[intpt,c(1,2,3)] <- getConfidenceInterval(data = KRbiases[reachtype+1,condition+1,intpt,], resamples = 1000) #took out xbar in t.interval function
          
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
        confint_kernelregression[intpt,c(1,2,3)] <- getConfidenceInterval(data = subdata[intpt,], resamples = 1000) #took out xbar in t.interval function
        
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
      confint_kernelregression[intpt,c(1,2,3)] <- getConfidenceInterval(data = subdata[intpt,], resamples = 1000)
      
    }
    
    tempLocalizationCI <- data.frame(confint_kernelregression[,1],confint_kernelregression[,2],confint_kernelregression[,3])
    colnames(tempLocalizationCI) <- sprintf(c('%s_p2.5','%s_p50','%s_p97.5'),'PredCons')
    localizationCI <- cbind(localizationCI, tempLocalizationCI)
    
    write.csv(localizationCI, sprintf('data/%s_localization_bCI.csv',group), row.names=FALSE)
    
  }
  
  
}


getLocalizationTdistributionConfidenceIntervals <- function(groups=c('30implicit','30explicit','cursorjump')) {
  
  interpoints <- c(50,90,130)
  #interpoints <- seq(30,150,1)
  # iterations <- 10000 # ideally 10000 or more? no more iterations: use t-distribution
  percentiles <- c(0.025, 0.5, 0.975) # this is not actually used...#it is needed in confint_kernelregression
  
  for (group in groups) {
    
    df <- read.csv(sprintf('data/%s_localization.csv',group),stringsAsFactors=FALSE)
    
    # correct using 3rd order polynomial
    # and removing taps where the hand location is further than 20 degrees from the arc centre
    # df <- correctLocalizationsP3(df)
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
          confint_kernelregression[intpt,c(1,2,3)] <- t.interval(KRbiases[reachtype+1,condition+1,intpt,]) #took out xbar in t.interval function
          
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
plotLocalizationShift <- function(groups=c('30implicit', '30explicit', 'cursorjump'), target='inline') {
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4_localization.svg', width=7, height=4, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  par(mfrow = c(1,3))
  
  for (reachtype.idx in c(0,1)){
    reachtype <- c('Active','Passive')[reachtype.idx+1]
    
    meanlocalizationshifts <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(30,150), ylim = c(2,-20), 
         xlab = "Hand Angle (°)", ylab = "Shift in Localization (°)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf('%s Localization', reachtype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at=c(50, 90, 130)) #tick marks for x axis
    axis(2, at = c(0, -5, -10, -15)) #tick marks for y axis
    
    #Active and Passive
    for (group in groups) {
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/%s_localization_tCI.csv',group))
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
  }
  
  
  #Predicted Sensory Consequences
  meanlocalizationshifts <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(30,150), ylim = c(2,-20), 
       xlab = "Hand Angle (°)", ylab = "Shift in Localization (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Predicted Consequences', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at=c(50, 90, 130)) #tick marks for x axis
  axis(2, at = c(0, -5, -10, -15)) #tick marks for y axis
  
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
  
  legend(15,-20,legend=c('Implicit 30°','Strategy 30°','Cursor Jump'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']]),
         lty=1,bty='n')
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

# Statistics-----
reachAftereffectsANOVA <- function() {
  
  # write code!
  
}