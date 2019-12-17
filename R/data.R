getPreprocessedData <- function(check=TRUE,participantsOnly=FALSE) {
  
  # Checks if data files are available locally. If not, it will download the files.
  cat('Making sure pre-processed data is available locally:\n')
  
  data_files <- c('30explicit_CI_learningcurves.csv'      =   'https://osf.io/3mzcn/download',
                  '30explicit_CI_reachaftereffects.csv'   =   'https://osf.io/zxdq9/download',
                  '30explicit_learningcurves_long.csv'    =   'https://osf.io/k9dmr/download',
                  '30explicit_learningcurves_wide.csv'    =	  'https://osf.io/xjcw3/download',
                  '30explicit_loc_p3_AOV.csv'	            =	  'https://osf.io/jhp4t/download',
                  '30explicit_localization.csv'           =		'https://osf.io/vqpe8/download',
                  '30explicit_localization_tCI.csv'       =		'https://osf.io/7z4sw/download',
                  '30explicit_nocursor.csv'			          =   'https://osf.io/db4yc/download',
                  '30explicit_ppawareness_scores.csv'     =	  'https://osf.io/472wb/download',
                  
                  '30implicit_CI_learningcurves.csv'      =   'https://osf.io/63c8j/download',
                  '30implicit_CI_reachaftereffects.csv'   =   'https://osf.io/jsu75/download',
                  '30implicit_learningcurves_long.csv'    =   'https://osf.io/7dutr/download',
                  '30implicit_learningcurves_wide.csv'    =   'https://osf.io/35uma/download',
                  '30implicit_loc_p3_AOV.csv'             =   'https://osf.io/prkgw/download',
                  '30implicit_localization.csv'           =   'https://osf.io/exnpw/download',
                  '30implicit_localization_tCI.csv'       =   'https://osf.io/vd7zh/download',
                  '30implicit_nocursor.csv'               =   'https://osf.io/wprmc/download',
                  '30implicit_ppawareness_scores.csv'     =   'https://osf.io/2t7us/download',

                  'cursorjump_CI_learningcurves.csv'      =   'https://osf.io/vqcwk/download',
                  'cursorjump_CI_reachaftereffects.csv'   =   'https://osf.io/vpsb9/download',
                  'cursorjump_learningcurves_long.csv'    =   'https://osf.io/rhca7/download',
                  'cursorjump_learningcurves_wide.csv'    =   'https://osf.io/ruyka/download',
                  'cursorjump_loc_p3_AOV.csv'             =   'https://osf.io/9qgne/download',
                  'cursorjump_localization.csv'           =   'https://osf.io/65arn/download',
                  'cursorjump_localization_tCI.csv'       =   'https://osf.io/qs2cn/download',
                  'cursorjump_nocursor.csv'               =   'https://osf.io/bhuvk/download',
                  'cursorjump_ppawareness_scores.csv'     =   'https://osf.io/5xm4f/download',
                  
                  'handview_CI_learningcurves.csv'        =   'https://osf.io/5t4su/download',
                  'handview_CI_reachaftereffects.csv'     =   'https://osf.io/b28sy/download',
                  'handview_learningcurves_long.csv'      =   'https://osf.io/k5yvn/download',
                  'handview_learningcurves_wide.csv'      =   'https://osf.io/9wdpb/download',
                  'handview_loc_p3_AOV.csv'               =   'https://osf.io/8dy5t/download',
                  'handview_localization.csv'             =   'https://osf.io/uxrz2/download',
                  'handview_localization_tCI.csv'         =   'https://osf.io/3vh2x/download',
                  'handview_nocursor.csv'                 =   'https://osf.io/s2kfj/download',
                  'handview_ppawareness_scores.csv'       =   'https://osf.io/gx7mz/download',
                  
                  'participant_demographics.csv'          =   'https://osf.io/jc2ug/download'
                  
  )
  
  if (participantsOnly) {
    
    data_files <- c('participant_demographics.csv'        =   'https://osf.io/jc2ug/download')
    
  }
  
  for (filename in names(data_files)) {
    
    folderfilename <- sprintf('data/%s',filename)
    
    if (!check | !file.exists(folderfilename)) {
      
      url = as.character(data_files[filename])
      
      cat(sprintf("Downloading: '%s' from '%s'\n", filename, url))
      
      df <- read.csv(url(url),stringsAsFactors=FALSE)
      
      write.csv(df,folderfilename,row.names=FALSE,quote=FALSE)
      
    } else {
      
      cat(sprintf("File exists: '%s', not downloading.\n", filename))
      
    }
    
  }
  
}