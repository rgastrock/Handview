library('ez')
library('svglite')
library('phia')
library('afex')
library('lsmeans')
library('vioplot')
library('scales')
library('lsr')
library('tools')
library('SMCL')

# Generic Functions -----
getStyle <- function() {
  
  
  # I don't use these for my plots, but helpful function to organize data into format for ANOVA.
  # Colors removed because not necessary to complete other ANOVA functions.
  groups    =  c('30implicit', 
                 '30explicit', 
                 'cursorjump', 
                 'handview')
  rotations =  c(30,
                 30,          
                 30,               
                 30)
  # solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
  #                  rgb(255, 128, 0,   255, max = 255), 
  #                  rgb(136, 0,   238, 255, max = 255),
  #                  rgb(136, 153, 255, 255, max = 255))
  # 
  # transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
  #                  rgb(255, 128, 0,   47,  max = 255), 
  #                  rgb(136, 0,   238, 47,  max = 255),
  #                  rgb(136, 153, 255, 47,  max = 255))
  # 
  # linestyles = c(2,
  #                1,
  #                2,
  #                1)
  labels <-    c('non-instructed',
                 'instructed',
                 'cursorjump',
                 'handview')
  
  
  styles <- data.frame(groups,rotations,labels, stringsAsFactors = FALSE) #added stringsasfactors to not create levels
  colnames(styles) <- c('group','rotation','label')
  
  return(styles)
  
}

getColourScheme <- function(groups = c('30explicit', '30implicit', 'cursorjump', 'handview')){
  #create a list containing the colourscheme per group
  for (group in groups){
    colourscheme <- list()
    
    colourscheme[['30implicit']] <- list('S'='#ff8200ff', # pure orange
                                         'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['30explicit']] <- list('S'='#e51636ff', #vivid/york red
                                         'T'='#e516362f')
    
    colourscheme[['cursorjump']] <- list('S'='#c400c4ff', #strong magenta
                                         'T'='#c400c42f')
    
    colourscheme[['handview']] <-   list('S'='#005de4ff', #pure blue
                                         'T'='#005de42f')
    
  }
  return(colourscheme)
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

getGroupParticipants <- function(group) {
  
  all_part <- read.csv(file = "D:/Documents/Science/Explicit/data/participants_files.csv")
  #return all participant ID's for whichever group specified
  participants_grouped <- as.vector(all_part$ID[which(all_part$folder == group)]) 
  return (participants_grouped)
  
}

#to avoid having to change filenames all the time
getParticipantTaskData <- function(group, id, session, task) {
  
  all_part <- read.csv(file = "D:/Documents/Science/Explicit/data/participants_files.csv")
  condition <- sprintf('%s_%s', session, task) #builds up which condition to look in the file
  filename <- as.character(all_part[all_part$ID == id, condition]) #get specified particiant id, session, task
  
  #check whether file extension is csv or txt
  #note that library('tools') needs to be loaded
  ext <- file_ext(filename)
  
  filepath <- sprintf('D:/Documents/Science/Explicit/data/%s/%s/%s', group, id, filename) #creates the file path relative to current directory
  
  #if file extension is csv (from PySelector), do this:
  if (ext %in% "csv"){
    # if (task %in% c("activetap", "passivetap")){
    #   df <- read.table(file = filepath, header = TRUE) #not used right now, but will be helpful once PySelector deals with localization data as well
    # }
    
    if (task %in% c("train", "nocursor")){
      df <- read.csv(file = filepath)
      #will always have 20 columns so assign these names:
      colnames(df) <- c("trial", "targetangle_deg", "cursorx_cm", "cursory_cm", 
                        "robotx_cm", "roboty_cm", "homescreenx_px", "homescreeny_px",
                        "homerobotx_cm", "homeroboty_cm", "targetx_cm", "targety_cm",
                        "blocknumber", "rotated_bool", "time_ms", "trialselected_bool",
                        "maxvelocity_idx", "sampleselected_bool", "sampleinterpolated_bool", "unsure")
      
      #homerobot x and y is currently in meters, convert to cm
      df$homerobotx_cm <- df$homerobotx_cm * 100
      df$homeroboty_cm <- df$homeroboty_cm * 100
      #need to set origin at (0,0)
      df$robotx_cm <- df$robotx_cm - df$homerobotx_cm
      df$roboty_cm <- df$roboty_cm - df$homeroboty_cm
      #get only trialselected_bool in data with a value of 1
      #df<- df[df$trialselected_bool==1,] #might not even need this code
    }
    
  }
  
  #when dealing with data selected from MATLAB GUI:
  if (ext %in% "txt"){
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
      
      #need to set origin at (0,0)
      df$robotx_cm <- df$robotx_cm - df$homerobotx_cm
      df$roboty_cm <- df$roboty_cm - df$homeroboty_cm
      #get only trialselected_bool in data with a value of 1
      #df<- df[df$trialselected_bool==1,] #might not even need this code 
    }
  }
  
  return(df)
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

t.interval <- function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  #same as getConfidenceInterval, but assumes a normal t-distribution
  
  z <- qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar <- mean(data, na.rm = TRUE)
  sdx <- sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar, xbar + z * sdx)) 
  
}

#below is a function that integrates the two functions above
#it does the same thing, but the one below is helpful for plotting bootstrapped means for individual data
getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,hi)))
    }
    
  }
  
}
