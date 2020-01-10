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

#we can also do this individually for each participant
countParticipantDeletedTrials<- function (groups=c('30explicit','30implicit','cursorjump','handview'), session, task) {
  #session: aligned or rotated, task: train or nocursor
  #get list of participants in group
  #go through each pp data, and grabe unselected trials
  #divide by total trials to get a percentage
  delsum <- data.frame()
  for (group in groups){
    pplist <- getGroupParticipants(group = group)
    
    for (pp in pplist){
      data <- getParticipantTaskData(group = group , id = pp, session = session , task = task)
      total <- length(unique(data$trial))
      
      subdf <- data[which(data$trialselected_bool == 0),]
      deleted <- length(unique(subdf$trial))
      percentdeleted<- (deleted/total)*100
      del_trial <- data.frame(group, deleted, total, percentdeleted)
      colnames(del_trial) <- c('group','deleted_trials', 'total_trials', 'percent_deleted')
      
      if (prod(dim(delsum)) == 0){
        delsum <- del_trial
        row.names(delsum) <- NULL
      } else {
        delsum <- rbind(delsum, del_trial)
        row.names(delsum) <- NULL
      }
    }
  
  }
  
  return(delsum)
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

#we can have a statistial test ready for any difference in percentages
#Kruskal-Wallis test is basically similar to an Anova, but for nonparametric data
# Null hypothesis is that the percentages are not different from each other
# Thus, a p-value greater than .05 would mean no differences across groups
# Result will always be the same, because Kruskal-wallis is a test on ranks. Each group will only get one rank, and this is also the summed ranks.
getKWTotalDeletions <- function(){
  #possible combinations for session and task are:
  #aligned train, aligned nocursor
  #rotated train, rotated nocursor
  
  cat('Aligned Train:\n')
  df <- countParticipantDeletedTrials(session = 'aligned', task = 'train')
  print(kruskal.test(deleted_trials ~ group, data = df))

  
  cat('Aligned No Cursor:\n')
  df <- countParticipantDeletedTrials(session = 'aligned', task = 'nocursor')
  print(kruskal.test(deleted_trials ~ group, data = df))
  
  cat('Rotated Train:\n')
  df <- countParticipantDeletedTrials(session = 'rotated', task = 'train')
  print(kruskal.test(deleted_trials ~ group, data = df))
  
  cat('Rotated No Cursor:\n')
  df <- countParticipantDeletedTrials(session = 'rotated', task = 'nocursor')
  print(kruskal.test(deleted_trials ~ group, data = df))
}
#only aligned train is significant. This shouldn't be a problem, probably just connected to how cursor jump group was collected
#Cursor jump was an older paradigm, one of the first few that was done when experiment started


#Localization outlier correction deletion----
getGroupDeletedLocalizationTrials <- function(group) {
  
  delsum <- c()
  
  
  df <- read.csv(sprintf('data/%s_localization.csv',group),stringsAsFactors=FALSE)
  
  # remove taps where the hand location is further than 20 degrees from the arc centre
  #df <- correctLocalizations(df)
  
  
  
  # remove all nan values:
  df <- df[is.finite(df$hand_angle) & is.finite(df$tap_angle),]
  
  # which ones are further than 20 degrees from arc centre
  #subdfbelow <- df[which((df$hand_angle - df$arc < -20)),]
  #subdfabove <- df[which((df$hand_angle - df$arc > 20)),]
  # remove taps too far away from the arc centre:
  subdf <- df[intersect(which((df$hand_angle - df$arc) > -20), which((df$hand_angle - df$arc) < 20)),]
  
  # remove outliers?
  # per participant and task
  participants <- unique(df$participant)
  
  
  for (p.id in participants) {
    
    for (isrot in c(0,1)) {
      
      for (ispas in c(0,1)) {
        #total number of trials if +/- 20 was not removed
        tot <- which(df$participant == p.id & df$rotated == isrot & df$passive == ispas)
        uncleaned_total_trials <- length(tot)
        
        #then we gather trials based off of 3SD cutoff
        #subset data frame to current iteration in for loops
        task.idx <- which(subdf$participant == p.id & subdf$rotated == isrot & subdf$passive == ispas)
        #get these rows
        ptask <- subdf[task.idx,]
        
        ang_dev <- ptask$hand_angle - ptask$tap_angle
        
        row.idx <- task.idx[which(abs(ang_dev) <= (mean(ang_dev) + (3 * sd(ang_dev))))] #removes outliers/keep these rows
        
        #track trials removed
        out.idx <- task.idx[which(abs(ang_dev) > (mean(ang_dev) + (3 * sd(ang_dev))))]
        
        total_trials <- length(row.idx)
        deleted_trials <- length(out.idx)
        percentdeleted<- (deleted_trials/total_trials)*100
        removed_taps <- uncleaned_total_trials - total_trials
        percent_removed <- (removed_taps/uncleaned_total_trials)*100
        
        total_removed <- removed_taps + deleted_trials
        total_percent_removed <- (total_removed/uncleaned_total_trials)*100
        
        del_trial <- data.frame(group, isrot, ispas, uncleaned_total_trials, total_trials, removed_taps, percent_removed, deleted_trials, percentdeleted, total_removed, total_percent_removed)
        colnames(del_trial) <- c('group','rotated', 'passive', 'uncleaned_trials', 'cleaned_trials', 'removed_taps', 'percent_removed', 'deleted_clean_trials', 'percent_clean_deleted', 'total_removed', 'total_percent_removed')
        
        
        if (prod(dim(delsum)) == 0){
          delsum <- del_trial
          row.names(delsum) <- NULL
        } else {
          delsum <- rbind(delsum, del_trial)
          row.names(delsum) <- NULL
        }
      }
      
    }
    
  }
  
  return(delsum)
}

#use the previous function to combine across all groups
combineGroupDeletedLocalizationTrials <- function(groups=c('30explicit','30implicit','cursorjump','handview')){
  alldat <- c()
  
  for (group in groups){
    df <- getGroupDeletedLocalizationTrials(group=group)
    
    if (prod(dim(alldat)) == 0){
      alldat <- df
      row.names(alldat) <- NULL
    } else {
      alldat <- rbind(alldat, df)
      row.names(alldat) <- NULL
    }
  }
  return(alldat)
}

#we perform the kruskal-wallis test for the dataset from the above function. This has all data from each participant, across all trial types
#This helps us test for an overall difference. Per group, we just compare total removed trials, from total uncleaned trials
getKWLocTotalDeletions <- function(){
  
  cat('All Localization Data:\n')
  df <- combineGroupDeletedLocalizationTrials()
  print(kruskal.test(total_removed ~ group, data = df))
}
#Doing an overall test makes it significant (p=.04). So let's try to test t according to task_type
getKWTaskTotalDeletions <- function(){
  
  cat('All Localization Data:\n')
  df <- getAllGroupDeletedLocTrials()
  print(kruskal.test(total_removed ~ task_type, data = df))
}
#Running the test according to task type become not significant (p=.09). Thus, significance from overall test is probably due to group differences in removed trials.
#But since we have percentages across all groups, we can look at each group's removed percentages (getTotalGroupDeletedLocTrials)
#Numbers don't show much of a cause for concern. 


#Now that we have all infor for each group, we want percentage of trials removed when it was over +/- 20 degrees and the SD cutoff
# for aligned-active, aligned-passive, rotated-active, rotated-passive, across all participants
# Then we do this for all groups.

getAllGroupDeletedLocTrials <- function(groups=c('30explicit','30implicit','cursorjump','handview')){
  alldatgroup <- c()
  for (group in groups){
    dat <- getGroupDeletedLocalizationTrials(group=group)
    
    AlAct <- dat[which(dat$rotated == 0 & dat$passive == 0),]
    AlPas <- dat[which(dat$rotated == 0 & dat$passive == 1),]
    RotAct <- dat[which(dat$rotated == 1 & dat$passive == 0),]
    RotPas <- dat[which(dat$rotated == 1 & dat$passive == 1),]
    
    #Aligned Active
    task_type <- 'Aligned_Act'
    #+/-20 deg
    uncleaned_total <- sum(AlAct$uncleaned_trials)
    removed_trials <- sum(AlAct$removed_taps)
    group_percent_removed <- (removed_trials/uncleaned_total)*100
    #SD cutoff
    cleaned_total <- sum(AlAct$cleaned_trials)
    deleted_clean <- sum(AlAct$deleted_clean_trials)
    group_cleaned_removed <- (deleted_clean/cleaned_total)*100
    #total
    total_removed <- removed_trials + deleted_clean
    total_percent_removed <- (total_removed/uncleaned_total)*100
    
    newdf_AlAct <- data.frame(group, task_type, uncleaned_total, removed_trials, group_percent_removed, cleaned_total, deleted_clean, group_cleaned_removed, total_removed, total_percent_removed)
    
    
    #Aligned Passive
    task_type <- 'Aligned_Pas'
    #+/-20 deg
    uncleaned_total <- sum(AlPas$uncleaned_trials)
    removed_trials <- sum(AlPas$removed_taps)
    group_percent_removed <- (removed_trials/uncleaned_total)*100
    #SD cutoff
    cleaned_total <- sum(AlPas$cleaned_trials)
    deleted_clean <- sum(AlPas$deleted_clean_trials)
    group_cleaned_removed <- (deleted_clean/cleaned_total)*100
    
    #total
    total_removed <- removed_trials + deleted_clean
    total_percent_removed <- (total_removed/uncleaned_total)*100
    
    newdf_AlPas <- data.frame(group, task_type, uncleaned_total, removed_trials, group_percent_removed, cleaned_total, deleted_clean, group_cleaned_removed, total_removed, total_percent_removed)
    
    
    #Rotated Active
    task_type <- 'Rotated_Act'
    #+/-20 deg
    uncleaned_total <- sum(RotAct$uncleaned_trials)
    removed_trials <- sum(RotAct$removed_taps)
    group_percent_removed <- (removed_trials/uncleaned_total)*100
    #SD cutoff
    cleaned_total <- sum(RotAct$cleaned_trials)
    deleted_clean <- sum(RotAct$deleted_clean_trials)
    group_cleaned_removed <- (deleted_clean/cleaned_total)*100
    
    #total
    total_removed <- removed_trials + deleted_clean
    total_percent_removed <- (total_removed/uncleaned_total)*100
    
    newdf_RotAct <- data.frame(group, task_type, uncleaned_total, removed_trials, group_percent_removed, cleaned_total, deleted_clean, group_cleaned_removed, total_removed, total_percent_removed)
    
    #Rotated Passive
    task_type <- 'Rotated_Pas'
    #+/-20 deg
    uncleaned_total <- sum(RotPas$uncleaned_trials)
    removed_trials <- sum(RotPas$removed_taps)
    group_percent_removed <- (removed_trials/uncleaned_total)*100
    #SD cutoff
    cleaned_total <- sum(RotPas$cleaned_trials)
    deleted_clean <- sum(RotPas$deleted_clean_trials)
    group_cleaned_removed <- (deleted_clean/cleaned_total)*100
    
    #total
    total_removed <- removed_trials + deleted_clean
    total_percent_removed <- (total_removed/uncleaned_total)*100
    
    newdf_RotPas <- data.frame(group, task_type, uncleaned_total, removed_trials, group_percent_removed, cleaned_total, deleted_clean, group_cleaned_removed, total_removed, total_percent_removed)
    
    
    alldat <- rbind(newdf_AlAct, newdf_AlPas, newdf_RotAct, newdf_RotPas)
    
    if (prod(dim(alldatgroup)) == 0){
      alldatgroup <- alldat
      row.names(alldatgroup) <- NULL
    } else {
      alldatgroup <- rbind(alldatgroup, alldat)
      row.names(alldatgroup) <- NULL
    }
  }
  
  
  return(alldatgroup)
}

#We can further summarize this into total deletion percentages per group (across all tasks)
getTotalGroupDeletedLocTrials <- function(){
  
  dat <- getAllGroupDeletedLocTrials()
  groups <- unique(dat$group)
  delsum <- c()
  
  for (group in groups){
    subdat <- dat[which(dat$group == group),]
    uncleaned_taps <- sum(subdat$uncleaned_total)
    removed_taps <- sum(subdat$removed_trials)
    percent_removed <- (removed_taps/uncleaned_taps)*100
    
    cleaned_taps <- sum(subdat$cleaned_total)
    deleted_clean <- sum(subdat$deleted_clean)
    percent_deleted <- (deleted_clean/cleaned_taps)*100
    
    total_removed <- removed_taps + deleted_clean
    total_percent_deleted <- (total_removed/uncleaned_taps)*100
    
    delgroup <- data.frame(group, uncleaned_taps, removed_taps, percent_removed, cleaned_taps, deleted_clean, percent_deleted, total_removed, total_percent_deleted)
    
    if (prod(dim(delsum)) == 0){
      delsum <- delgroup
      row.names(delsum) <- NULL
    } else {
      delsum <- rbind(delsum, delgroup)
      row.names(delsum) <- NULL
    }
  }
  
  return(delsum)
}

# Then a grand total summary for all
getTotalDeletedLocTrials <- function(){
  delsum <- getTotalGroupDeletedLocTrials()
  #then we add in a total across all groups at the end
  uncleaned_taps <- sum(delsum$uncleaned_taps)
  removed_taps <- sum(delsum$removed_taps)
  percent_removed <- (removed_taps/uncleaned_taps)*100
  
  cleaned_taps <- sum(delsum$cleaned_taps)
  deleted_clean <- sum(delsum$deleted_clean)
  percent_deleted <- (deleted_clean/cleaned_taps)*100
  
  grandtotal <- 'total'
  
  totdel <- data.frame(grandtotal, uncleaned_taps, removed_taps, percent_removed, cleaned_taps, deleted_clean, percent_deleted)
  
  return(totdel)
}
