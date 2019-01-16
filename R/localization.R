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
    svglite(file='doc/fig/Fig4_localization.svg', width=7, height=4, pointsize=14, system_fonts=list(sans="Arial"))
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
      plot(NA, NA, xlim = c(30,150), ylim = c(2,-17), 
           xlab = "", ylab="", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf('Shifts in \n %s Localization \n \n (Proprioception + Prediction)', reachtype),cex.main = 1.35, xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      #mtext("(Proprioception + Prediction)", cex = 1)
      abline(h = 0, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at=c(50, 90, 130)) #tick marks for x axis
      axis(2, at = c(0, -5, -10, -15)) #tick marks for y axis
    } else if (reachtype.idx == 1){
      plot(NA, NA, xlim = c(30,150), ylim = c(2,-17), 
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
  }
  
  
  #Predicted Sensory Consequences
  meanlocalizationshifts <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(30,150), ylim = c(2,-17), 
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

#omnibus ANOVA test
#note that rotated means session (aligned or rotated) and passive is the movement type (active or passive)
#there is a main effect of rotated_b suggesting change in localization after rotation training
#have the full version and we just care about rotated_b main effect

#shifts ANOVA test
#main effect of group and passive_b(movement type)
#this suggests a difference between groups and a difference between active and passive movement type
#however there is no interaction

#proprioceptive recalibration affected by groups(explicit knowledge)?
#proprioception ANOVA test (basically a One-way ANOVA)
#No difference between groups
#if I run the active version there is a difference (probably handview) - do a post-hoc, if we want this
#these basically confirm the plots we do for active and passive

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
  
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant * group, data=df, FUN=mean)
  df$participant <- as.factor(df$participant)
  PSC <- ezANOVA(data = df, wid=participant, dv=pred_update, between=group, type=3, return_aov=TRUE)
  print(PSC[1:2])
  
}

#We find that groups do not differ in predicted sensory consequences
#This does not make sense to me, in active there is a difference
#but when we isolate prop and pred, there are none
#Anyway we don't really care about Active anyway - so proceed

#so we don't see a group effect in Pred Cons
#but our plot makes it seem that hand view is lower than other groups
#So now we test each group and its difference from 0 (t-test)

predConsTtests <- function() {
  #Hand view t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'handview'),]
  
  cat('Hand View group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  
  #Instructed t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30explicit'),]
  
  cat('Instructed group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  
  #Non-instructed t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == '30implicit'),]
  
  cat('Non-Instructed group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  
  #Cursor Jump t-test
  df <- getPredictedSensoryConsequences(styles)
  df <- aggregate(pred_update ~ participant*group, data=df, FUN=mean)
  subdf <- df[which(df$group == 'cursorjump'),]
  
  cat('Cursor Jump group predicted sensory consequences compared to 0:\n')
  print(t.test(subdf$pred_update, mu=0, alternative='less'))
  
}

#So only hand view was not significant
#Ho: mu greater than or equal to 0; Ha: mu less than 0
#suggests that mean predicted sensory consequences for hand view group was equal or greater than 0
#So no group effect on Predicted Sensory Consequences, but only hand view group does not significantly differ from 0


