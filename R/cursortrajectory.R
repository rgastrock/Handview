source('R/shared.R')

getAverageTrajectories <- function(groups=c('30implicit', '30explicit', 'cursorjump', 'handview')) {
  
  # define which trials we want
  trialRanges <- data.frame(matrix(data=c(c('aligned', 'rotated', 'rotated'),c(43,1,88),c(3,3,3)),ncol=3))
  colnames(trialRanges) <- c('condition','start','number')
  
  # splined trajectories should all have an equal amount of interpolated points
  npoints <- 50
  
  # loop through groups:
  for (group in groups) {
    
    # we store the dataframes in this list:
    groupaveragetrajectories <- getGroupAverageTrajectories(group,trialRanges=trialRanges,npoints=npoints)
    
    save(groupaveragetrajectories,file=sprintf('data/%s_trajectories.dat',group))
    
  }
  
  # when reading in the dat files, we want to know what's in them:
  write.csv(trialRanges, file='data/trialRanges.csv',quote=FALSE,row.names=FALSE)
  
  # return findings
  #return(TRUE)
  
}

getGroupAverageTrajectories <- function(group,trialRanges,npoints) {
  
  participants <- getGroupParticipants(group)
  
  # make an array that holds the relevant info
  # this is multi-dimensional too
  # 3 rows for x, 3 for y, then for all participants
  GroupReachTrajectories <- array(data=NA, dim=c(length(participants),nrow(trialRanges),npoints,2))
  
  # loop through participants:
  for (pp.id in 1:length(participants)) {
    
    # get each participants' after effects and store in dataframe:
    GroupReachTrajectories[pp.id,,,] <- getParticipantReachTrajectories(participants[pp.id],trialRanges,npoints)
    
  }
  
  # return the after effects dataframe:
  return(GroupReachTrajectories)
  
}

getParticipantReachTrajectories <- function(participant,trialRanges,npoints) {
  #print(participant)
  pdf <- read.csv('data/participants_files.csv',stringsAsFactors=FALSE)
  
  folder <- pdf[pdf$ID==participant, 'folder']
  # version <- pdf[pdf$ID==participant, 'version']
  
  #we want an array that has 2 dimensions. These are the x and y values for end of aligned, start of rotated, end of rotated (3 rows)
  #viewing it will probably only show the x values, cause it is 2 dimensional, flip through View()
  centralParticipantReachTrajectories <- array(data=NA, dim=c(nrow(trialRanges),npoints,2))
  
  for (row.id in 1:nrow(trialRanges)) {
    
    condition <- trialRanges$condition[row.id]
    basefilename <- pdf[pdf$ID==participant,sprintf('%s_train',condition)]
    filename <- sprintf('data/%s/%s/%s',folder,participant,basefilename)
    
    # read the data
    columnnames <- c("trial", "target", "Xcursor","Ycursor","Xrobot","Yrobot","XscreenO","YscreenO","Xhome","Yhome","XTarget","Ytarget","blocknumber","rotation","time","trialselected","sampleselected","sampleinterpolated","maxvelocity")
    reachdf <- read.table(filename,stringsAsFactors=FALSE);
    
    if (dim(reachdf)[2] == 19) {
      colnames(reachdf) <- columnnames
    } else if (dim(reachdf)[2] == 20) {
      colnames(reachdf) <- c(columnnames, 'unsure')
    }
    
    # fix, so that origin is (0,0), at least for the robot coordinates that we use here:
    reachdf$Yrobot = reachdf$Yrobot + 8.5
    
    start <- as.numeric(levels(trialRanges$start))[trialRanges$start]
    number <- as.numeric(levels(trialRanges$number))[trialRanges$number]
    # print(start[row.id])
    # print(number[row.id])
    trials <- c(start[row.id]:(start[row.id]+number[row.id]-1))
    
    rowParticipantReachTrajectories <- array(data=NA, dim=c(max(as.numeric(levels(trialRanges$number))[trialRanges$number]),npoints,2))
    
    # loop through trials
    # only when they exist and are selected, do we get a splined trajectory
    for (trial.id in 1:length(trials)) {
      
      trialno <- trials[trial.id]
      
      if (trialno %in% unique(reachdf$trial)) {
        
        trialdf <- reachdf[reachdf$trial == trialno,]
        
        if (any(trialdf$trialselected == 1)) {
          
          # if the trial seems OK, get an interpolated version, and store in the array:
          SpTr <- getSplineTrajectory(trialdf,npoints=npoints)
          rowParticipantReachTrajectories[trial.id, 1:npoints, 1:2] <- SpTr
          
          # this never happens, which means that the spline is not even done for many participants
          if (any(is.nan(SpTr))) {
            cat(sprintf('NANs for participant: %s in trial %d\n', participant, trialno))
          }
          
        }
        
      }
      
    }
    
    centralParticipantReachTrajectories[row.id,,] <- apply(rowParticipantReachTrajectories, c(2,3), mean, na.rm=TRUE)
    # print(dim(centra))
  }
  
  # return a full array:
  return(centralParticipantReachTrajectories)
  
}

getSplineTrajectory <- function(trialdf,npoints=npoints) {
  
  # we'll use the robot coordinates, not the cursor coordinates
  XX <- trialdf[trialdf$sampleselected == 1,'Xrobot']
  YY <- trialdf[trialdf$sampleselected == 1,'Yrobot']
  TT <- trialdf[trialdf$sampleselected == 1,'time']
  angle <- trialdf[1,'target']
  
  # rotate so the target is straight to the right, somewhere on the line X=0 (better for averaging and plotting)
  trajectory <- rotateTrajectory(XX,YY,(-1*angle))
  XX <- trajectory[,1]
  YY <- trajectory[,2]
  
  # spline npoints on the trajectory, so we have an equal number of samples for every reach
  newX <- spline(TT, XX, n=npoints)$y
  newY <- spline(TT, YY, n=npoints)$y
  
  if (any(!is.finite(c(newX,newY)))) {
    cat('beware the NaNs!\n')
  }
  
  # combine into an array:
  coordinates <- array(data=NA, dim=c(npoints,2))
  coordinates[,1] <- newX
  coordinates[,2] <- newY
  
  return(coordinates)
  
}

plotAverageTrajectories <- function(target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig4A_AverageTrajectories.svg', width=8, height=6, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow=c(1,1))
  
  groups <- c('30implicit', '30explicit', 'cursorjump','handview')
  conditions <- c()
  
  #colors <- c(rgb(.7,.7,.7),rgb(0,0,0),rgb(1,0,0),rgb(.7,.7,.7),rgb(0,0,0))
  colors <- getColourScheme(groups)

  trialRanges <- read.csv('data/trialRanges.csv')
  
  npoints <- 50
  nconditions <- nrow(trialRanges)
  # how do we know npoints? for now this is, unfortunately, a magic number...
  averageGroupTrajectories <- array(data=NA, dim=c(length(groups),nconditions,npoints,2))
  
  plot(c(-100,-100),c(-100,-100),type='n',asp=1,col=rgb(0,0,0,0),main="Average trajectories", xlab="Group", ylab="Cursor Training",xlim=c(0, 16*length(groups)), ylim=c(0, 16*nconditions), yaxt='n', xaxt='n',axes=F)
  
  for (group.id in 1:length(groups)) {
    
    group <- groups[group.id]
    color <- colors[[group.id]][['S']]
    
    load(sprintf('data/%s_trajectories.dat',group))
    
    # loads: groupaveragetrajectories
    
    meantrajectory <- apply(groupaveragetrajectories, c(2,3,4), median, na.rm=TRUE)
    
    for (condition.id in 1:nconditions) {
      
      condition <- as.character(trialRanges[condition.id,'condition'])
      if (group.id == 1) {
        conditions <- c(conditions, condition)
      }
      
      
      #print(condition)
      X <- meantrajectory[condition.id,,1]
      Y <- meantrajectory[condition.id,,2]
      
      # first rotated to desired orientation:
      hand <- rotateTrajectory(X,Y,90)
      X <- hand[,1]
      Y <- hand[,2]
      
      # then translate to fit on the plot nicely
      Xmod <- ((group.id-1) * 16) + 8
      if (condition.id == 1){
        Ymod <- (((condition.id-1) * 16) + 4) + 32
      } else if (condition.id == 3){
        Ymod <- (((condition.id-1) * 16) + 4) - 32
      } else {
        Ymod <- ((condition.id-1) * 16) + 4
      }
      
      X <- X + Xmod
      Y <- Y + Ymod
      
      lines(X,Y,type='l',col=color, lty=1, lw=2)
      
      if (condition == 'rotated') {
        
        unrotate <- -30
        
        cursor <- rotateTrajectory(hand[,1],hand[,2],unrotate)
        X <- cursor[,1] #+ Xmod
        Y <- cursor[,2] #+ Ymod
        # we only show the part where cursor has jumped (1/3 of distance from home to target, but dependent on participant movement)
        if (group == 'cursorjump') {
          
          dist <- sqrt(X^2 + Y^2)
          idx <- which(dist > 4)
          
          
          newX <- X[idx] + Xmod
          newY <- Y[idx] + Ymod
          
          lines(newX,newY,type='l',col=color, lty=2, lw=2)
          
        } else {
          newX <- X + Xmod
          newY <- Y + Ymod
          
          lines(newX,newY,type='l',col=color, lty=2, lw=2)
          
        }
      }
      
      # draw in home and target position
      points(c(0,0)+Xmod,c(0,12)+Ymod,col=rgb(0,0,0),bg=rgb(1,1,1,0),cex=2)
      
    }
    
  }
  
  axis(1, at=(c(1:length(groups))*16) - 8, labels=c('Non-Instructed', 'Instructed','CursorJump', 'HandView'))
  axis(2, at=(c(1:nconditions)*16) - 8, labels=c('Late Adaptation','Early Adaptation','Aligned'))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#do all confidence ellipses AND/OR do each individual one but faintly
plotAllTrajectories <- function(target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4_AllTrajectories.svg', width=8, height=6, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow=c(1,1))
  
  groups <- c('30implicit', '30explicit', 'cursorjump','handview')
  #groups <- c('handview')
  conditions <- c()
  
  #colors <- c(rgb(.7,.7,.7),rgb(0,0,0),rgb(1,0,0),rgb(.7,.7,.7),rgb(0,0,0))
  colors <- getColourScheme(groups)
  
  trialRanges <- read.csv('data/trialRanges.csv')
  
  npoints <- 50
  nconditions <- nrow(trialRanges)
  # how do we know npoints? for now this is, unfortunately, a magic number...
  averageGroupTrajectories <- array(data=NA, dim=c(length(groups),nconditions,npoints,2))
  
  plot(c(-100,-100),c(-100,-100),type='n',asp=1,col=rgb(0,0,0,0),main="Individual and Average Trajectories", xlab="Group", ylab="Cursor Training",xlim=c(0, 16*length(groups)), ylim=c(0, 16*nconditions), yaxt='n', xaxt='n',axes=F)
  
  for (group.id in 1:length(groups)) {
    
    group <- groups[group.id]
    ngroup <- length(getGroupParticipants(group=group))
    color <- colors[[group.id]][['T']]
    
    load(sprintf('data/%s_trajectories.dat',group))
    
    # loads: groupaveragetrajectories
    
    #meantrajectory <- apply(groupaveragetrajectories, c(2,3,4), median, na.rm=TRUE)
    for (ppno in 1:ngroup){
      for (condition.id in 1:nconditions) {
        
        condition <- as.character(trialRanges[condition.id,'condition'])
        if (group.id == 1) {
          conditions <- c(conditions, condition)
        }
        
        
        #print(condition)
        X <- groupaveragetrajectories[ppno,condition.id,,1]
        Y <- groupaveragetrajectories[ppno,condition.id,,2]
        
        
        # first rotated to desired orientation:
        hand <- rotateTrajectory(X,Y,90)
        X <- hand[,1]
        Y <- hand[,2]
        
        # then translate to fit on the plot nicely
        Xmod <- ((group.id-1) * 16) + 8
        if (condition.id == 1){
          Ymod <- (((condition.id-1) * 16) + 4) + 32
        } else if (condition.id == 3){
          Ymod <- (((condition.id-1) * 16) + 4) - 32
        } else {
          Ymod <- ((condition.id-1) * 16) + 4
        }
        
        X <- X + Xmod
        Y <- Y + Ymod
        
        lines(X,Y,type='l',col= alpha(color, 0.1), lty=1, lw=2)
        
        if (condition == 'rotated') {
          
          unrotate <- -30
          
          cursor <- rotateTrajectory(hand[,1],hand[,2],unrotate)
          X <- cursor[,1] #+ Xmod; scaling factors removed, used later on
          Y <- cursor[,2] #+ Ymod
          # we only show the part where cursor has jumped (1/3 of distance from home to target, but dependent on participant movement)
          if (group == 'cursorjump') {
            
            dist <- sqrt(X^2 + Y^2)
            idx <- which(dist > 4)
            
            newX <- X[idx] + Xmod
            newY <- Y[idx] + Ymod
            
            lines(newX,newY,type='l',col=alpha(color, 0.1), lty=2, lw=2)
            
          } else {
            newX <- X + Xmod
            newY <- Y + Ymod
            
            lines(newX,newY,type='l',col=alpha(color, 0.1), lty=2, lw=2)
            
          }
        }
        
        # draw in home and target position
        points(c(0,0)+Xmod,c(0,12)+Ymod,col=rgb(0,0,0),bg=rgb(1,1,1,0),cex=2)
        
      }
    }
    
    
  }
  
  #then add the mean on top
  for (group.id in 1:length(groups)) {
    
    group <- groups[group.id]
    color <- colors[[group.id]][['S']]
    
    load(sprintf('data/%s_trajectories.dat',group))
    
    # loads: groupaveragetrajectories
    
    meantrajectory <- apply(groupaveragetrajectories, c(2,3,4), median, na.rm=TRUE)
    
    for (condition.id in 1:nconditions) {
      
      condition <- as.character(trialRanges[condition.id,'condition'])
      if (group.id == 1) {
        conditions <- c(conditions, condition)
      }
      
      
      #print(condition)
      X <- meantrajectory[condition.id,,1]
      Y <- meantrajectory[condition.id,,2]
      
      # first rotated to desired orientation:
      hand <- rotateTrajectory(X,Y,90)
      X <- hand[,1]
      Y <- hand[,2]
      
      # then translate to fit on the plot nicely
      Xmod <- ((group.id-1) * 16) + 8
      if (condition.id == 1){
        Ymod <- (((condition.id-1) * 16) + 4) + 32
      } else if (condition.id == 3){
        Ymod <- (((condition.id-1) * 16) + 4) - 32
      } else {
        Ymod <- ((condition.id-1) * 16) + 4
      }
      
      X <- X + Xmod
      Y <- Y + Ymod
      
      lines(X,Y,type='l',col=color, lty=1, lw=2)
      
      if (condition == 'rotated') {
        
        unrotate <- -30
        
        cursor <- rotateTrajectory(hand[,1],hand[,2],unrotate)
        X <- cursor[,1] #+ Xmod
        Y <- cursor[,2] #+ Ymod
        
        if (group == 'cursorjump') {
          
          dist <- sqrt(X^2 + Y^2)
          idx <- which(dist > 4)
          
          
          newX <- X[idx] + Xmod
          newY <- Y[idx] + Ymod
          
          lines(newX,newY,type='l',col=color, lty=2, lw=2)
          
        } else {
          newX <- X + Xmod
          newY <- Y + Ymod
          
          lines(newX,newY,type='l',col=color, lty=2, lw=2)
          
        }
      }
      # draw in home and target position
      points(c(0,0)+Xmod,c(0,12)+Ymod,col=rgb(0,0,0),bg=rgb(1,1,1,0),cex=2)
      
    }
    
  }
  
  axis(1, at=(c(1:length(groups))*16) - 8, labels=c('Non-Instructed', 'Instructed','CursorJump', 'HandView'))
  axis(2, at=(c(1:nconditions)*16) - 8, labels=c('Late Adaptation','Early Adaptation','Aligned'))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#function below checks each individual trajectory plotted for a specified group
#check for handview since it seems like certain people there are deviating by 60 degrees, to know what is going on:
plotINDTrajectories <- function(target='inline') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/SuppFig4B_INDTrajectories.svg', width=8, height=6, pointsize=10, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow=c(1,1))
  
  #groups <- c('30implicit', '30explicit', 'cursorjump','handview')
  groups <- c('handview')
  conditions <- c()
  
  #colors <- c(rgb(.7,.7,.7),rgb(0,0,0),rgb(1,0,0),rgb(.7,.7,.7),rgb(0,0,0))
  colors <- getColourScheme(groups)
  
  trialRanges <- read.csv('data/trialRanges.csv')
  
  npoints <- 50
  nconditions <- nrow(trialRanges)
  # how do we know npoints? for now this is, unfortunately, a magic number...
  averageGroupTrajectories <- array(data=NA, dim=c(length(groups),nconditions,npoints,2))
  
  
  
  for (group.id in 1:length(groups)) {
    
    group <- groups[group.id]
    ngroup <- length(getGroupParticipants(group=group))
    color <- colors[[4]][['S']]
    
    load(sprintf('data/%s_trajectories.dat',group))
    
    # loads: groupaveragetrajectories
    
    #meantrajectory <- apply(groupaveragetrajectories, c(2,3,4), median, na.rm=TRUE)
    for (ppno in 1:ngroup){
      plot(c(-100,-100),c(-100,-100),type='n',asp=1,col=rgb(0,0,0,0),main="Individual and Average Trajectories", xlab="Group", ylab="Cursor Training",xlim=c(0, 16*length(groups)), ylim=c(0, 16*nconditions), yaxt='n', xaxt='n',axes=F)
      
      for (condition.id in 1:nconditions) {
        
        condition <- as.character(trialRanges[condition.id,'condition'])
        if (group.id == 1) {
          conditions <- c(conditions, condition)
        }
        
        
        #print(condition)
        X <- groupaveragetrajectories[ppno,condition.id,,1]
        Y <- groupaveragetrajectories[ppno,condition.id,,2]
        
        
        # first rotated to desired orientation:
        hand <- rotateTrajectory(X,Y,90)
        X <- hand[,1]
        Y <- hand[,2]
        
        # then translate to fit on the plot nicely
        Xmod <- ((group.id-1) * 16) + 8
        if (condition.id == 1){
          Ymod <- (((condition.id-1) * 16) + 4) + 32
        } else if (condition.id == 3){
          Ymod <- (((condition.id-1) * 16) + 4) - 32
        } else {
          Ymod <- ((condition.id-1) * 16) + 4
        }
        
        X <- X + Xmod
        Y <- Y + Ymod
        
        lines(X,Y,type='l',col= alpha(color, 0.9), lty=1, lw=2)
        
        if (condition == 'rotated') {
          
          unrotate <- -30
          
          cursor <- rotateTrajectory(hand[,1],hand[,2],unrotate)
          X <- cursor[,1] #+ Xmod; scaling factors removed, used later on
          Y <- cursor[,2] #+ Ymod
          # we only show the part where cursor has jumped (1/3 of distance from home to target, but dependent on participant movement)
          if (group == 'cursorjump') {
            
            dist <- sqrt(X^2 + Y^2)
            idx <- which(dist > 4)
            
            newX <- X[idx] + Xmod
            newY <- Y[idx] + Ymod
            
            lines(newX,newY,type='l',col=alpha(color, 0.9), lty=2, lw=2)
            
          } else {
            newX <- X + Xmod
            newY <- Y + Ymod
            
            lines(newX,newY,type='l',col=alpha(color, 0.9), lty=2, lw=2)
            
          }
        }
        
        # draw in home and target position
        points(c(0,0)+Xmod,c(0,12)+Ymod,col=rgb(0,0,0),bg=rgb(1,1,1,0),cex=2)
        #axis(1, at=(c(1:length(groups))*16) - 8, labels=c('Non-Instructed', 'Instructed','CursorJump', 'HandView'))
        axis(2, at=(c(1:nconditions)*16) - 8, labels=c('Late Adaptation','Early Adaptation','Aligned'))
      }
    }
    
    
  }
  
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}
