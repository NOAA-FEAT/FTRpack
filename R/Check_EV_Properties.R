#' Check_EV_Properties.R
#'
#' @description Uses COM objects to run Echoview and get transducer properties.  These properties are 
#' saved to a .csv file in the Data_Compile_dir of the DirNameFile excel spreadsheet. The name defaults
#' to TransducerConfigCheck(SurveyName).csv
#'
#' @param SurveyName name of survey as in excel file
#' @param DirNameFile excel file name of paths
#' @param variables - variables to export
#' @param database is an optional argument for export format.  Default is database (1).  Spreadsheet =0
#' @param sheet is an optional argument to denote which sheet of the DirNameFile
#' want to use. This defaults to the first sheet if not set. 
#' @examples
#'  \dontrun{
#' Check_EV_Properties(SurveyName1in, DirNameFile,variables,sheet=case)
#'}
#' @export
#' 
Check_EV_Properties<-function(SurveyName, DirNameFile,variables,sheet=case){
     ### Uses COM objects to run Echoview and get transducer properties
     #Adapted from Beth Phillip's script 2023

     # # Location of EV files to check transducer settings
     DirTable <- readxl::read_excel(DirNameFile,sheet)
     DirTableSurvey<- subset(DirTable, Survey == SurveyName)
     DirTableSurvey[] <- lapply(DirTableSurvey, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset
     BaseJudgePath <- as.character(DirTableSurvey$Base_Path)
     ExportPath <- DirTableSurvey$Data_Compile_Dir
     #EVdirs <-DirTableSurvey$Orig_EV_Dir
     EVdirs<-BaseJudgePath
     expected_ducerdepth <- DirTableSurvey$DucerDepth #expected transducer vertical offset for each vessel

     # Make directory that data will be saved in if it doesn't exist
     dir.create(file.path(ExportPath), showWarnings = FALSE)
     
     #########################
     # list the EV files to update transducer properties
     EVfile.list <- list.files(paste0(EVdirs), pattern=".EV$", ignore.case = TRUE)
     
     ###############################################
     #   Open Original EV file to get settings     #
     ###############################################
     
     ducer_location.list = list()
     ducer_locations.list = list()


     for (i in EVfile.list){
       # EV filename
       var=variables  #reset in case it was changed
       EVfileNames <- file.path(EVdirs, i)
       EvName <- strsplit(i, split = '*.EV')[[1]]
       Tr=strsplit(EvName,"x")[[1]][2]  #get transect number
       EVfileName <- file.path(EVdirs, i)
       print(EVfileName)
       
       # create COM connection between R and Echoview
       EVApp <- COMCreate("EchoviewCom.EvApplication")
       EVApp$Minimize()  #Minimize EV file to run in background
       
       # open EV file
       EVfile <- EVApp$OpenFile(EVfileName)
       Obj <- EVfile[["Variables"]]
       if (is.null(Obj$FindByName(var))==TRUE){
            var<-paste0(var,' (1)')  #try adding (1) to the variable if it doesn't work the first time
       }
       varac <- Obj$FindByName(var)$AsVariableAcoustic()
       # access transducer properties
       transducerobj <- EVfile[["Transducers"]]
       no_trans <- transducerobj[["Count"]]
       no_trans_list <-vector(mode="list", length=no_trans)
       
       #get transducer depth (vertical offset), and x- y- offset settings
       #for survey purposes, just look at 38 kHz
       #for (k in 1:length(no_trans_list)){
       k=2 #38 kHz
         # get offset settings for each transducer
         transducerobj_props <- EVfile[["Transducers"]]$Item(k-1)
         Freq <- transducerobj_props[["Name"]]
         x_offset <- transducerobj_props[["AlongshipOffset"]] 
         y_offset <- transducerobj_props[["AthwartshipOffset"]] 
         z_offset <- transducerobj_props[["VerticalOffset"]] 
         flag <- ifelse(z_offset==expected_ducerdepth,"N","Y")
         ducer_location.list[[k]] <- c(EvName,Freq,x_offset,y_offset,z_offset,flag)
       #}
       #combine settings for each transducer
       ducer_location = do.call(rbind, ducer_location.list)
       #add transducer settings for transect to rest of the transects
       ducer_locations.list[[i]] <- ducer_location 
       #add more values of interest
       ducer_locations.list[i][[1]][7]<-varac[["Properties"]][["Analysis"]][["Excludebelow"]]
       ducer_locations.list[i][[1]][8]<-varac[["Properties"]][["Analysis"]][["BadDataHasVolume"]]
       TDmode<-varac[["Properties"]][["Grid"]][["TimeDistanceMode"]]
       if(TDmode==2){TDmodech<-"GPSNMi"}
       if(TDmode==3){TDmodech<-"VesselLogNMi"}
       ducer_locations.list[i][[1]][9]<-TDmodech
       varaccal<-varac[["Properties"]][["Calibration"]]
       ducer_locations.list[i][[1]][10]<-varaccal$Get("SoundSpeed",1)
       ducer_locations.list[i][[1]][11]<-varaccal$Get("Ek5TsGain",1)
       ducer_locations.list[i][[1]][12]<-varaccal$Get("EK60SaCorrection",1)
     # Close EV file
     EVApp$CloseFile(EVfile)
     
     # Quit echoview
     EVApp$Quit()
     
     } 
     
     # Bind all rows from transects and transducer settings into a data frame
     ducer_locations = as.data.frame(do.call(rbind, ducer_locations.list))
     
     #Change column names of data frame
     names(ducer_locations)[1] <- "Transect"
     names(ducer_locations)[2] <- "Transducer name"
     names(ducer_locations)[3] <- "x_offset"
     names(ducer_locations)[4] <- "y_offset"
     names(ducer_locations)[5] <- "z_offset"
     names(ducer_locations)[6] <- "Flag"
     names(ducer_locations)[7] <- "Exclude_below_line"
     names(ducer_locations)[8] <- "Include_volume_nodata_samples"
     names(ducer_locations)[9] <- "TimeDistanceMode"
     names(ducer_locations)[10] <- "SoundSpeed"
     names(ducer_locations)[11] <- "TsGain"
     names(ducer_locations)[12] <- "SaCorrection"
     ducer_locations
     
     write.csv(ducer_locations, paste0(ExportPath,"/TransducerConfigCheck",SurveyName,".csv"),row.names = FALSE)
}