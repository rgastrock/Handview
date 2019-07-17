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
    svglite(file='doc/fig/Fig4_localization.svg', width=8, height=3.5, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  par(mfrow = c(1,3), mai=c(0.65,0.3,0.8,0.3)) #added this to fix margins of plot
  
  for (reachtype.idx in c(0,1)){
    reachtype <- c('Active','Passive')[reachtype.idx+1]
    
    meanlocalizationshifts <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    #removed ylab for now to make space in poster (SCAPPS 2018)
    if (reachtype.idx == 0){
      plot(NA, NA, xlim = c(30,175), ylim = c(2,-17), 
           xlab = "", ylab="", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf('Shifts in \n %s Localization \n \n (Proprioception + Prediction)', reachtype),cex.main = 1.35, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception + Prediction)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130)) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15)) #tick marks for y axis
    } else if (reachtype.idx == 1){
      plot(NA, NA, xlim = c(30,175), ylim = c(2,-17), 
           xlab = expression(paste("Hand Angle (",degree,")")), ylab="", frame.plot = FALSE, #frame.plot takes away borders; ylab coded as such to print degree symbol correctly
           main = sprintf('Shifts in \n %s Localization \n \n (Proprioception)', reachtype),cex.main=1.35, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130)) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15)) #tick marks for y axis
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
      xloc <- 155 + (groupno*4)
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
  plot(NA, NA, xlim = c(30,175), ylim = c(2,-17), 
       xlab = "", ylab="", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Shifts in Predicted \n Sensory Consequences \n \n (Prediction)',cex.main=1.35, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #mtext("(Prediction)", cex=1)
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
    
    shift <- shifts[[1]] - shifts[[2]]
    
    groupno <- which(groups == group)
    xloc <- 155 + (groupno*4)
    CI <- t.interval(shift)
    #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color[groupno]), lty=styles$linestyle[groupno])
    lines(c(xloc, xloc), c(CI[3], CI[1]), col=col)
    lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=col)
    lines(c(xloc-1.5, xloc+1.5), c(CI[3], CI[3]), col=col)
    points(xloc, mean(shift), col=col, pch=19)
    
  }
  legend(30,-15,legend=c('Non-instructed','Instructed','Cursor Jump','Hand View'),
         col=c(colourscheme[['30implicit']][['S']],colourscheme[['30explicit']][['S']],colourscheme[['cursorjump']][['S']],colourscheme[['handview']][['S']]),
         lty=1,bty='n')
  
  #close everything if you saved plot as svg
  if (target=='svg') {
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
  # if (test == 'active') {
  #   LOC4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
  #   LOC4aov <- LOC4aov[which(LOC4aov$passive_b == 0),] #get just passive reaches (proprioceptive information only)
  #   LOC4aov$participant <- as.factor(LOC4aov$participant)
  #   LocalizationAOV <- ezANOVA(data=LOC4aov, wid=participant, dv=bias_deg, between=group, type=3, return_aov=TRUE)
  #   
  #   print(LocalizationAOV[1:2])
  #   
  # }
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
  alldf$group  <- as.factor(alldf$group)
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
  alldf$group  <- as.factor(alldf$group)
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

#Correlation Section -----
getPropExcData <- function(styles){
  
  #get data for prop-recal and get mean across target angles for each pp
  df <- getPasLocShifts(styles)
  df <- aggregate(prop_recal ~ participant*group, data=df, FUN=mean)
  
  #then we want to add exclusive angular deviations to the df above
  df2 <- getRAE4ANOVA(styles)
  #we want only exclusive data
  df2 <- df2[which(df2$strategy == 'exclusive'),]
  
  newdf <- merge(df, df2, by='participant') #merge two df's together, according to participant
  newdf <- newdf[,-c(4:5)] #removes these columns to avoid duplication
  
  return(newdf)
}

plotGroupCorrelations <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_correlation.svg', width=5, height=5, pointsize=10, system_fonts=list(sans="Arial"))
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
  plot(NA, NA, main="Relationship between RAEs and Shifts in Passive Localization", xlab = 'Hand Deviation During Reaches with No Cursor - Without Strategy (°)', ylab = 'Shifts in Passive Localization (°)',
       bty='n', xlim= c(-10,25), ylim= c(-30,10), xaxt='n', yaxt='n')
  #add dashed lines at 0
  abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = 0, col = 8, lty = 2) #creates vertical dashed lines through x =  0
  # this puts tick marks exactly where we want them:
  axis(side=1, at=c(-10,0,10,20))
  axis(side=2, at=c(-30,-20,-10,0,10))
  
  
  #library(car)
  #scatterplot(data$prop_recal~data$reachdeviation, data=data)
  
  #CIs
  prop_recal <- data$prop_recal
  reachdev <- data$reachdeviation
  mod1 <- lm(prop_recal ~ reachdev)
  
  
  x <- seq(-5,19,.1)
  
  pred1 <- predict(mod1, newdata=data.frame(reachdev=x), interval='confidence')
  
  polyX <- c(x,rev(x))
  polyY <- c(pred1[,2], rev(pred1[,3]))
  polygon(polyX, polyY, col='#a6a6a6', border=NA)
  
  #add in data points of all pp's
  points(data$reachdeviation, data$prop_recal, pch=16, cex=1.5,
       col= alpha(cols, 0.6)) #library(scales) needed for alpha to work
  
  #Reg line
  reglinex <- seq(range(reachdev)[1],range(reachdev)[2],.1)
  abX <- range(reglinex)
  abY <- abX * mod1$coefficients[2] + mod1$coefficients[1]
  lines(abX, abY, col='#343434')
  
  #add in r-squared value to plot
  #this is just the value from mod1 under multiple R squared
  #as of now, I add this value in manually below
  
  #add legend and r-squared
  legend(12, -20, c(as.expression(bquote(""~ r^2 ~ "= 0.121"))), col='#a6a6a6', bty='n', cex=1)

  legend(15,-25,legend=c('Non-instructed','Instructed','Cursor Jump', 'Hand View'),
         col=c(impcol,expcol,cujcol,hancol),
         pch=16,bty='o',cex=0.50)
  
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
