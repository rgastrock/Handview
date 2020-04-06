library(svglite)
#Generate Data Frames----

getTrialType <- function(session){
  
  if (session == 'familiarization'){
    #Familiarization Session
    #Create data frame containing values
    trial <- c(0:35)
    t_align <- rep(1, 18)
    align <- matrix(t_align, nrow = 18, ncol = 1)
    t_loc <- rep(2, 18)
    loc <- matrix(t_loc, nrow = 18, ncol = 1)
    task <- rbind(align, loc)
    fam_df <- data.frame(trial, task)
    return(fam_df)
  } else if (session == 'aligned'){
    #Aligned Session
    #Create data frame containing values
    trial <- c(0:323)
    
    #1 means aligned cursor
    #2 means active and passive localization
    #3 means no cursor
    
    align1 <- matrix(rep(1,45), nrow = 45, ncol = 1)
    loc1 <- matrix(rep(2,18), nrow = 18, ncol = 1)
    align2 <- matrix(rep(1,9), nrow = 9, ncol = 1) #because other trial sets are shorter
    nocur <- matrix(rep(3,9), nrow = 9, ncol = 1)
    
    
    task <- rbind(align1, loc1, align2, loc1, align2, nocur,#first cycle, repeated 4 times
                  align2, loc1, align2, loc1, align2, nocur,
                  align2, loc1, align2, loc1, align2, nocur,
                  align2, loc1, align2, loc1, align2, nocur)
    aligned_df <- data.frame(trial, task)
    return(aligned_df)
  } else if (session == 'rotated'){
    #Rotated Session
    #Create data frame containing values
    trial <- c(0:635)
    
    #1 means rotated cursor
    #2 means active and passive localization
    #3 means no cursor - exclude
    #4 means no cursor - include
    
    align1 <- matrix(rep(1,90), nrow = 90, ncol = 1)
    loc1 <- matrix(rep(2,18), nrow = 18, ncol = 1)
    align2 <- matrix(rep(1,30), nrow = 30, ncol = 1) #because other trial sets are shorter
    nocurexc <- matrix(rep(3,9), nrow = 9, ncol = 1)
    nocurinc <- matrix(rep(4,9), nrow = 9, ncol = 1)
    
    task_EI <- rbind(align1, loc1, align2, loc1, align2, nocurexc, nocurinc,#first cycle, repeated 4 times
                     align2, loc1, align2, loc1, align2, nocurinc, nocurexc,
                     align2, loc1, align2, loc1, align2, nocurexc, nocurinc,
                     align2, loc1, align2, loc1, align2, nocurinc, nocurexc)
    rot_df_EI <- data.frame(trial, task_EI)
    
    #can also do IE
    # task_IE <- rbind(align1, loc1, align2, loc1, align2, nocurinc, nocurexc,#first cycle, repeated 4 times
    #                  align2, loc1, align2, loc1, align2, nocurexc, nocurinc,
    #                  align2, loc1, align2, loc1, align2, nocurinc, nocurexc,
    #                  align2, loc1, align2, loc1, align2, nocurexc, nocurinc)
    # rot_df_IE <- data.frame(trial, task_IE)
    return(rot_df_EI)
  }
}

#Generate Plots-----
plotExpSched <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_expsched.svg', width=8.5, height=4, pointsize=14, system_fonts=list(sans="Arial"))#width is 7, height is 2.25, pointsize is 6
  }
  #8,5,14
  #9.5, 3.5, 10
  # Can also plot familiarization
  # #Familiarization Session
  # #X and Y for rect
  # X <- c(17, 35)
  # Y <- c(-30, 30)
  # plot(0:35, seq(-30,30,length.out=36), type = "n", axes = FALSE,## no axes
  #      xlab = 'Trial', ylab = 'Angular Deviation of Hand (°)', main = 'Familiarization Session') 
  # lim <- par("usr")
  # rect(X[1], Y[1], X[2], Y[2], border = "grey", col = "grey")
  # axis(side=1, at=c(1,17,35)) ## add axes back
  # axis(side=2, at=c(-30,0,30))
  # lines(c(0:17), rep(0,18))
  
  #combine aligned and rotated
  par(mar=c(2,1,2,1), mfrow = c(2,1))
  
  #Aligned Session
  #might need to specify different X and Y for each trial
  aligned_df <- getTrialType(session='aligned')
  
  X1 <- c(45, 62)
  X2 <- c(72,89)
  X3 <- c(117,134)
  X4 <- c(144,161)
  X5 <- c(189,206)
  X6 <- c(216,233)
  X7 <- c(261,278)
  X8 <- c(288,305)
  
  X1nc <- c(99,107)
  X2nc <- c(171,179)
  X3nc <- c(243,251)
  X4nc <- c(315,323)
  
  X1c <- c(0,44)
  X2c <- c(63,71)
  X3c <- c(90,98)
  X4c <- c(108,116)
  X5c <- c(135,143)
  X6c <- c(162,170)
  X7c <- c(180,188)
  X8c <- c(207,215)
  X9c <- c(234,242)
  X10c <- c(252,260)
  X11c <- c(279,287)
  X12c <- c(306,314)
  
  Y <- c(0, 1)
  
  plot(c(1:length(aligned_df$trial)), seq (0,30, length.out = length(aligned_df$trial)), type = 'n', axes = FALSE,
       xlab = '', ylab = '', main = 'Aligned Session',
       xlim = c(0,636), ylim = c(-0.2,1.2), cex.main=.65, cex.lab=.65)
  
  #set variables for colours
  drkbl <- "#1261A0"
  lgtbl <- "#3895D3"
  drkgr <- "#36802D"
  grcur <- "#dadada"
  midgr <- "#77ab59"
  lgtgr <- "#c9df8a"
  
  #localization
  rect(X1[1], Y[1], X1[2], Y[2], border = drkbl, col = drkbl)#blue diff shade
  rect(X2[1], Y[1], X2[2], Y[2], border = lgtbl, col = lgtbl)#light blue
  rect(X3[1], Y[1], X3[2], Y[2], border = drkbl, col = drkbl)
  rect(X4[1], Y[1], X4[2], Y[2], border = lgtbl, col = lgtbl)
  rect(X5[1], Y[1], X5[2], Y[2], border = drkbl, col = drkbl)
  rect(X6[1], Y[1], X6[2], Y[2], border = lgtbl, col = lgtbl)
  rect(X7[1], Y[1], X7[2], Y[2], border = drkbl, col = drkbl)
  rect(X8[1], Y[1], X8[2], Y[2], border = lgtbl, col = lgtbl)
  
  #nocursor
  rect(X1nc[1], Y[1], X1nc[2], Y[2], border = drkgr, col = drkgr)#dark green
  rect(X2nc[1], Y[1], X2nc[2], Y[2], border = drkgr, col = drkgr)
  rect(X3nc[1], Y[1], X3nc[2], Y[2], border = drkgr, col = drkgr)
  rect(X4nc[1], Y[1], X4nc[2], Y[2], border = drkgr, col = drkgr)
  
  #cursor trials
  rect(X1c[1], Y[1], X1c[2], Y[2], border = grcur, col = grcur)#pale yellow was FFFF99, now its light greay
  rect(X2c[1], Y[1], X2c[2], Y[2], border = grcur, col = grcur)
  rect(X3c[1], Y[1], X3c[2], Y[2], border = grcur, col = grcur)
  rect(X4c[1], Y[1], X4c[2], Y[2], border = grcur, col = grcur)
  rect(X5c[1], Y[1], X5c[2], Y[2], border = grcur, col = grcur)
  rect(X6c[1], Y[1], X6c[2], Y[2], border = grcur, col = grcur)
  rect(X7c[1], Y[1], X7c[2], Y[2], border = grcur, col = grcur)
  rect(X8c[1], Y[1], X8c[2], Y[2], border = grcur, col = grcur)
  rect(X9c[1], Y[1], X9c[2], Y[2], border = grcur, col = grcur)
  rect(X10c[1], Y[1], X10c[2], Y[2], border = grcur, col = grcur)
  rect(X11c[1], Y[1], X11c[2], Y[2], border = grcur, col = grcur)
  rect(X12c[1], Y[1], X12c[2], Y[2], border = grcur, col = grcur)
  
  # lines(c(0:44), rep(0.1,45), lwd=3)
  # lines(c(63:71), rep(0.1,9), lwd=3)
  # lines(c(90:98), rep(0.1,9), lwd=3)
  # lines(c(108:116), rep(0.1,9), lwd=3)
  # lines(c(135:143), rep(0.1,9), lwd=3)
  # lines(c(162:170), rep(0.1,9), lwd=3)
  # lines(c(180:188), rep(0.1,9), lwd=3)
  # lines(c(207:215), rep(0.1,9), lwd=3)
  # lines(c(234:242), rep(0.1,9), lwd=3)
  # lines(c(252:260), rep(0.1,9), lwd=3)
  # lines(c(279:287), rep(0.1,9), lwd=3)
  # lines(c(306:314), rep(0.1,9), lwd=3)
  
  
  axis(side=1, at=c(1,45,107,179,251,324), cex.axis=.65) ## add axes back
  # axis(side=2, at=c(0.1,0.9), labels=c('0','30'))
  #Ncols <- ceiling(6/4) #6 labels, 4 rows
  
  legend(350,1.3,legend=c('Cursor Training', 'No Cursor', 'No Cursor: Without Strategy', 'No Cursor: With Strategy','Active Localization','Passive Localization',''),
         col=c(grcur,drkgr,midgr,lgtgr,drkbl,lgtbl,'#ffffff'),
         #text.col=c("#000000","#76BA1B","#4C9A2A","#A4DE02",drkbl,lgtbl),
         lty=1,bty='n',cex=.65,lwd=5, ncol=2)
  
  
  
  #Rotated Session
  #might need to specify different X and Y for each trial
  rot_df_EI <- getTrialType(session='rotated')
  
  X1 <- c(90,107)
  X2 <- c(138,155)
  X3 <- c(234,251)
  X4 <- c(282,299)
  X5 <- c(378,395)
  X6 <- c(426,443)
  X7 <- c(522,539)
  X8 <- c(570,587)
  
  X1nc_e <- c(186,195)#plus1
  X1nc_i <- c(195,203)
  X2nc_e <- c(330,339)#plus1
  X2nc_i <- c(339,347)
  X3nc_e <- c(474,483)#plus1
  X3nc_i <- c(483,491)
  X4nc_e <- c(618,627)#plus1
  X4nc_i <- c(627,635)
  
  X1c <- c(0,89)
  X2c <- c(108,137)
  X3c <- c(156,185)
  X4c <- c(204,233)
  X5c <- c(252,281)
  X6c <- c(300,329)
  X7c <- c(348,377)
  X8c <- c(396,425)
  X9c <- c(444,473)
  X10c <- c(492,521)
  X11c <- c(540,569)
  X12c <- c(588,617)
  
  Y <- c(0, 1)
  
  plot(c(1:length(rot_df_EI$trial)), seq (0,30, length.out = length(rot_df_EI$trial)), type = 'n', axes = FALSE,
       xlab = 'Trial', ylab = '', main = 'Rotated Session',
       xlim = c(0,635), ylim = c(-0.2,1.2), cex.main=.65, cex.lab=.65)#.45
  
  #localization
  rect(X1[1], Y[1], X1[2], Y[2], border = drkbl, col = drkbl)#diff blue shade
  rect(X2[1], Y[1], X2[2], Y[2], border = lgtbl, col = lgtbl)#light blue
  rect(X3[1], Y[1], X3[2], Y[2], border = drkbl, col = drkbl)
  rect(X4[1], Y[1], X4[2], Y[2], border = lgtbl, col = lgtbl)
  rect(X5[1], Y[1], X5[2], Y[2], border = drkbl, col = drkbl)
  rect(X6[1], Y[1], X6[2], Y[2], border = lgtbl, col = lgtbl)
  rect(X7[1], Y[1], X7[2], Y[2], border = drkbl, col = drkbl)
  rect(X8[1], Y[1], X8[2], Y[2], border = lgtbl, col = lgtbl)
  
  #nocursor
  rect(X1nc_e[1], Y[1], X1nc_e[2], Y[2], border = midgr, col = midgr) #mid green
  rect(X1nc_i[1], Y[1], X1nc_i[2], Y[2], border = lgtgr, col = lgtgr)#lighter green
  rect(X2nc_e[1], Y[1], X2nc_e[2], Y[2], border = lgtgr, col = lgtgr)
  rect(X2nc_i[1], Y[1], X2nc_i[2], Y[2], border = midgr, col = midgr)
  rect(X3nc_e[1], Y[1], X3nc_e[2], Y[2], border = midgr, col = midgr)
  rect(X3nc_i[1], Y[1], X3nc_i[2], Y[2], border = lgtgr, col = lgtgr)
  rect(X4nc_e[1], Y[1], X4nc_e[2], Y[2], border = lgtgr, col = lgtgr)
  rect(X4nc_i[1], Y[1], X4nc_i[2], Y[2], border = midgr, col = midgr)
  
  #cursor trials
  rect(X1c[1], Y[1], X1c[2], Y[2], border = grcur, col = grcur)#pale yellow
  rect(X2c[1], Y[1], X2c[2], Y[2], border = grcur, col = grcur)
  rect(X3c[1], Y[1], X3c[2], Y[2], border = grcur, col = grcur)
  rect(X4c[1], Y[1], X4c[2], Y[2], border = grcur, col = grcur)
  rect(X5c[1], Y[1], X5c[2], Y[2], border = grcur, col = grcur)
  rect(X6c[1], Y[1], X6c[2], Y[2], border = grcur, col = grcur)
  rect(X7c[1], Y[1], X7c[2], Y[2], border = grcur, col = grcur)
  rect(X8c[1], Y[1], X8c[2], Y[2], border = grcur, col = grcur)
  rect(X9c[1], Y[1], X9c[2], Y[2], border = grcur, col = grcur)
  rect(X10c[1], Y[1], X10c[2], Y[2], border = grcur, col = grcur)
  rect(X11c[1], Y[1], X11c[2], Y[2], border = grcur, col = grcur)
  rect(X12c[1], Y[1], X12c[2], Y[2], border = grcur, col = grcur)#alternatively, FCF38E
  
  # lines(c(0:89), rep(0.9,90), lwd=3)
  # lines(c(108:137), rep(0.9,30), lwd=3)
  # lines(c(156:185), rep(0.9,30), lwd=3)
  # lines(c(204:233), rep(0.9,30), lwd=3)
  # lines(c(252:281), rep(0.9,30), lwd=3)
  # lines(c(300:329), rep(0.9,30), lwd=3)
  # lines(c(348:377), rep(0.9,30), lwd=3)
  # lines(c(396:425), rep(0.9,30), lwd=3)
  # lines(c(444:473), rep(0.9,30), lwd=3)
  # lines(c(492:521), rep(0.9,30), lwd=3)
  # lines(c(540:569), rep(0.9,30), lwd=3)
  # lines(c(588:617), rep(0.9,30), lwd=3)
  
  
  axis(side=1, at=c(1,90,203,347,491, 635), cex.axis=.65) ## add axes back
  # axis(side=2, at=c(0.1,0.9), labels=c('0','30'))
  #Angular Deviation of Hand (°)
  # mtext('Angular Deviation of Hand (°)', side = 2, outer = TRUE, line=-2, cex = 1)
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}