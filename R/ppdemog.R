getDemographics <- function(type = c('all','grouped'), groups = c('30explicit','30implicit','cursorjump','handview')){
  #funtion grabs relevant demographic info for methods
  
  #read in data file
  ndat <- read.csv(file = 'data/participant_demographics.csv')
  
  #can grab info from all groups (whole dataset) of from each group
  if (type == 'all'){
    oldest <- max(ndat$age)
    youngest <- min(ndat$age)
    meanage <- mean(ndat$age)
    sdage <- sd(ndat$age)
    malenum <- length(which(ndat$sex == 'M'))
    femalenum <- length(which(ndat$sex == 'F'))
    
    alldemog <- c(oldest, youngest, meanage, sdage, malenum, femalenum)
    names(alldemog) <- c('oldest', 'youngest', 'mean_age', 'sd_age', 'males', 'females')
    return(alldemog)
  } else if (type == 'grouped'){
    alldemog <- data.frame()
    for (group in groups){
      
      oldest <- max(ndat$age[which(ndat$group == group)])
      youngest <- min(ndat$age[which(ndat$group == group)])
      meanage <- mean(ndat$age[which(ndat$group == group)])
      sdage <- sd(ndat$age[which(ndat$group == group)])
      malenum <- length(which(ndat$sex == 'M' & ndat$group == group))
      femalenum <- length(which(ndat$sex == 'F' & ndat$group == group))
      
      groupdemog <- data.frame(group,oldest, youngest, meanage, sdage, malenum, femalenum)
      colnames(groupdemog) <- c('group','oldest', 'youngest', 'mean_age', 'sd_age', 'males', 'females')
      
      if (prod(dim(alldemog)) == 0){
        alldemog <- groupdemog
        row.names(alldemog) <- NULL
      } else {
        alldemog <- rbind(alldemog, groupdemog)
        row.names(alldemog) <- NULL
      }
    }
    return(alldemog)
  }
  
}

source('R/shared.R')

#Deleted Trials from Selection-----
#note that this is dependent on raw data (in Explicit Project, which is not shared on github)
#function getParticipantTaskData is reliant on different directory, so I changed path to make this work here
countGroupDeletedTrials<- function (group, session, task) {
  #session: aligned or rotated, task: train or nocursor
  #get list of participants in group
  #go through each pp data, and grabe unselected trials
  #divide by total trials to get a percentage
  pplist <- getGroupParticipants(group = group)
  delsum <- data.frame()
  for (pp in pplist){
    data <- getParticipantTaskData(group = group , id = pp, session = session , task = task)
    total <- length(unique(data$trial))
    
    subdf <- data[which(data$trialselected_bool == 0),]
    deleted <- length(unique(subdf$trial))
    percentdeleted<- (deleted/total)*100
    del_trial <- data.frame(deleted, total, percentdeleted)
    colnames(del_trial) <- c('deleted_trials', 'total_trials', 'percent_deleted')
    
    if (prod(dim(delsum)) == 0){
      delsum <- del_trial
      row.names(delsum) <- NULL
    } else {
      delsum <- rbind(delsum, del_trial)
      row.names(delsum) <- NULL
    }
    
  }
  #return(delsum)
  #code above does it for each pp, but we want whole group
  groupdeleted <- sum(delsum$deleted_trials)
  grouptotal <- sum(delsum$total_trials)
  grouppercent <- (groupdeleted/grouptotal)*100
  
  groupdelsum <- data.frame(group, groupdeleted, grouptotal, grouppercent)
  colnames(groupdelsum) <- c('group', 'deleted_trials_group', 'total_trials_group', 'percent_deleted_group')
  return(groupdelsum)
}

countAllGroupDeletedTrials <- function(groups=c('30explicit','30implicit','cursorjump','handview'), session, task){
  #I just have this to combine all info from all groups, previous function was getting long
  alldeletion <- data.frame()
  for (group in groups){
    deletion <- countGroupDeletedTrials(group=group, session=session, task=task)
    
    if (prod(dim(alldeletion)) == 0){
      alldeletion <- deletion
      row.names(alldeletion) <- NULL
    } else {
      alldeletion <- rbind(alldeletion, deletion)
      row.names(alldeletion) <- NULL
    }
  }
  return(alldeletion)
}

#now we can write a function that does it for both sessions, and both tasks
countTotalDeletions <- function(){
  #possible combinations for session and task are:
  #aligned train, aligned nocursor
  #rotated train, rotated nocursor
  
  cat('Aligned Train:\n')
  df <- countAllGroupDeletedTrials(session = 'aligned', task = 'train')
  deleted <- sum(df$deleted_trials_group)
  total <- sum(df$total_trials_group)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Aligned No Cursor:\n')
  df <- countAllGroupDeletedTrials(session = 'aligned', task = 'nocursor')
  deleted <- sum(df$deleted_trials_group)
  total <- sum(df$total_trials_group)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Rotated Train:\n')
  df <- countAllGroupDeletedTrials(session = 'rotated', task = 'train')
  deleted <- sum(df$deleted_trials_group)
  total <- sum(df$total_trials_group)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
  
  cat('Rotated No Cursor:\n')
  df <- countAllGroupDeletedTrials(session = 'rotated', task = 'nocursor')
  deleted <- sum(df$deleted_trials_group)
  total <- sum(df$total_trials_group)
  percentage <- (deleted/total)*100
  newdf <- data.frame(deleted, total, percentage)
  print(newdf)
}