library('ez')
library('tidyr')
# library('svglite')

# colorPalette <- function() {
#   
#   # return a set of colors that adhere to lab standards
#   
# }

#create a list containing the colourscheme per group

colourscheme <- list()

colourscheme[['30implicit']] <- list('S'='#ff8200ff', # pure orange
                                     'T'='#ff82002f')    #2f gives a lighter shade of the color

colourscheme[['30explicit']] <- list('S'='#e51636ff', #vivid/york red
                                     'T'='#e516362f')

colourscheme[['cursorjump']] <- list('S'='#c400c4ff', #strong magenta
                                     'T'='#c400c42f')

colourscheme[['handview']] <-   list('S'='#005de4ff', #pure blue
                                     'T'='#005de42f')

# Generic Functions -----

t.interval <- function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  #same as getConfidenceInterval, but assumes a normal t-distribution
  
  z <- qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar <- mean(data, na.rm = TRUE)
  sdx <- sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar, xbar + z * sdx)) 
  
}

getBSConfidenceInterval <- function(data, resamples) {
  
  data <- data[which(is.finite(data))] #need is.finite due to NA values
  
  #bootstrap to 95% CI with replacement (done when normal t-distribution is not assumed)
  #so multiplies data times 1000 and replaces the values
  samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
  #apply mean function to this new matrix
  BS <- apply(samplematrix, c(1), FUN=mean) #use mean instead of median (we mainly use mean for analyses, even though median is robust to outliers)
  #95% confidence that data falls within range
  #2.5% to 97.5%, with 50% being the median mean, which is close to actual mean
  return(quantile(BS, probs = c(0.025, 0.50, 0.975)))
  
}

getParticipantTaskData <- function(group, id, session, task) {
  
  all_part <- read.csv(file = "C:/Users/Raphael/Documents/Science/Explicit/data/participants_files.csv")
  condition <- sprintf('%s_%s', session, task) #builds up which condition to look in the file
  filename <- as.character(all_part[all_part$ID == id, condition]) #get specified particiant id, session, task
  
  filepath <- sprintf('data/%s/%s/%s', group, id, filename) #creates the file path relative to current directory
  
  #fix headers
  if (task %in% c("activetap", "passivetap")){
    df <- read.table(file = filepath, header = TRUE) #these files already have headers
  } 
  
  if (task %in% c("train", "nocursor")) {
    df <- read.table(file = filepath) #these files need headers to be added
    # count the number of columns
    CountCol <- ncol(df)
    
    
    # 19 columns, use these as headers
    if (CountCol == 19) {
      colnames(df) <- c("trial", "targetangle_deg", "cursorx_cm", "cursory_cm", 
                        "robotx_cm", "roboty_cm", "homescreenx_px", "homescreeny_px",
                        "homerobotx_cm", "homeroboty_cm", "targetx_cm", "targety_cm",
                        "blocknumber", "rotated_bool", "time_ms", "trialselected_bool",
                        "sampleselected_bool", "sampleinterpolated_bool", "maxvelocity_idx")
    }
    
    # 20 columns, use these as headers (only unsure heading was added)
    if (CountCol == 20) {
      colnames(df) <- c("trial", "targetangle_deg", "cursorx_cm", "cursory_cm", 
                        "robotx_cm", "roboty_cm", "homescreenx_px", "homescreeny_px",
                        "homerobotx_cm", "homeroboty_cm", "targetx_cm", "targety_cm",
                        "blocknumber", "rotated_bool", "time_ms", "trialselected_bool",
                        "sampleselected_bool", "sampleinterpolated_bool", "maxvelocity_idx", "unsure")
    }
    
    
  }
  #need to set origin at (0,0)
  df$robotx_cm <- df$robotx_cm - df$homerobotx_cm
  df$roboty_cm <- df$roboty_cm - df$homeroboty_cm
  #get only trialselected_bool in data with a value of 1
  #df<- df[df$trialselected_bool==1,] #might not even need this code
  return(df)
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}

getTrialReachAngleAt <- function(trialdf, location ='maxvel') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=1)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected_bool'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected_bool == 1,'robotx_cm']
  Y <- trialdf[trialdf$sampleselected_bool == 1,'roboty_cm']
  MV <- trialdf[trialdf$sampleselected_bool == 1,'maxvelocity_idx']
  angle <- trialdf[1,'targetangle_deg']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  #reachangle[1,2] <- angle
  
  return(reachangle)
  
}




