#' repath_EVdir_flex.R
#'
#' @description Function to take a directory of EV files and reset the path.
#' This works for EV files that have an ek5 sub-directory too.
#' The ek5 subdirectory must be named 'ek5 EV'.  The EK5_Dir_Flag must be
#' set appropriately in the excel file.
#' Written by RT 2022, modified 2024.
#' Requires an excel file input that says what the paths are
#' Each row is a survey (survey_name), and each column is a path or file name
#'
#' @param SurveyName name of survey as in excel file
#' @param DirNameFile excel file name of paths
#' Excel file has the following columns (with header names)
#' Cal_File: calibration file name
#' Base_Path:
#' Orig_EV_Dir:
#' Raw_Dir
#' EK5_Dir_Flag
#' EK5_Raw_Dir
#' @param ni is an optional argument for which file to start with (default ni=1 is the first file).
#' @param sheet is an optional argument to denote which sheet of the DirNameFile
#'
#' @examples
#'  \dontrun{
#' DirNameFile="C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_Ek80_conv_updated/Directory Structure EK60 EK80 conversion updated.xlsx"
#' SurveyName='2019_US'
#' repath_EVdir_flex(SurveyName,DirNameFile, ni, sheet)
#'}
#' @export
repath_EVdir_flex <- function(SurveyName, DirNameFile, ni=1,sheet,...) {

###
# uses functions from
# RDCOMClient
# readxl

  ###############################################
  #           Set Paths                         #
  ###############################################

  DirTable <- readxl::read_excel(DirNameFile, sheet)
  DirTableSurvey<- subset(DirTable, Survey == SurveyName)
  DirTableSurvey[] <- lapply(DirTableSurvey, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset
  BasePath<-DirTableSurvey$Base_Path
  BaseJudgePath<-DirTableSurvey$Orig_EV_Dir

  #calibration
  if (DirTableSurvey$Mult_Cal_Flag==0){  #multiple calibration flag off
       CALfileName<-DirTableSurvey$Cal_File
       CALfileName_EK5<-DirTableSurvey$EK5_Cal_File
     }else{  #multiple calibrations for the survey, look in the excel sheet
       CALsheetname<-paste0(SurveyName,"_cal")
       CALsheet<-readxl::read_excel(DirNameFile, sheet=CALsheetname)
       CALfileName_EK5<-DirTableSurvey$EK5_Cal_File
       CALfileName<-NA
     }
  #location of .raw files
  RAWdir<-DirTableSurvey$Raw_Dir
  RAWdir2<-DirTableSurvey$Raw_Dir2 #possible second raw directory -used in 2011 for .ek6 files
  RAWdir_EK5<-DirTableSurvey$EK5_Raw_Dir

  #Get ek_flag
  EK_flag<-DirTableSurvey$EK5_Dir_Flag

  #location of transect EV files
  Transectdir<-DirTableSurvey$Orig_EV_Dir
  TransectEK5dir<-file.path(DirTableSurvey$Orig_EV_Dir, 'ek5 EV')

  ###############################################
  #         Update paths in Echoview            #
  ###############################################

  # list the EV files
  #EVfile.list <- list.files(file.path(Transectdir), pattern="*.(?i)EV$")
  EVfileEK5.list<- list.files(file.path(TransectEK5dir), pattern="*.(?i)EV$")
  EVfile.list<-setdiff(list.files(file.path(Transectdir), pattern="*.(?i)EV$"), list.dirs(file.path(Transectdir),recursive = FALSE, full.names = FALSE)) #don't include directories
  # create COM connection between R and Echoview
  EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")

  if (EK_flag==1){
       EVlists<-list(EVfile.list,EVfileEK5.list)
       Transectlistdir<-list(Transectdir,TransectEK5dir)
       Callist<-list(CALfileName, CALfileName_EK5)
       RAWlist<-list(RAWdir,RAWdir_EK5)
  }else{
       EVlists<-list(EVfile.list)
       Transectlistdir<-list(Transectdir)
       Callist<-list(CALfileName)
       RAWlist<-list(RAWdir)
  }
  #Will need another case in here if ever have multiple calibrations as well as using ek5 files as well as .raw files
  ## ------------- start loop
  for (j in 1:length(EVlists)){  #put back to j=1
       Thisfile.list<-EVlists[[j]]
       ThisTransectdir<-Transectlistdir[[j]]
       ThisCal<-Callist[[j]]
       ThisRAW<-RAWlist[[j]]
       ni<-1 #reset ni
       nfiles=length(Thisfile.list)  #number of EV files
       for (i in Thisfile.list[ni:nfiles]){
         ni=which(Thisfile.list==i)
         print(ni)
         EVfileName <- file.path(ThisTransectdir, i)
         print(EVfileName)
         Transect<-getTnumv2(EVfileName)
         # open EV file
         EVfile <- EVApp$OpenFile(EVfileName)

         #set the first fileset object
         filesetObj <- EVfile[["Filesets"]]$Item(0)  #RAW

         # set data properties object and input new datafiles folder location
         evPropObj <- EVfile[["Properties"]]
         evPropObj[["DataPaths"]]$Clear()
         #evPropObj[["DataPaths"]]$Add(file.path(ThisRAW))
         evPropObj[["DataPaths"]]$Insert(file.path(RAWdir2),0) #better to do it like this where the new datapath is above the default data path. #add in second data path
         evPropObj[["DataPaths"]]$Insert(file.path(ThisRAW),0) #better to do it like this where the new datapath is above the default data path.
         print(paste("data path:", evPropObj[["DataPaths"]]$Item(0)))
         # set calibration file
         if (DirTableSurvey$Mult_Cal_Flag==0){  #multiple calibration flag off, Calfilename is already set above
         }else{  #multiple calibrations for the survey, look in the excel sheet
              Bounds<-as.matrix(CALsheet[,c("x_low_bound","x_up_bound")])
              for (ri in 1:nrow(Bounds)){
                   if (between(Transect,Bounds[ri,1],Bounds[ri,2])){  #If the transect is within the bounds of that row
                        CALfileName<-as.character(CALsheet[ri,3])
                        ThisCal<-CALfileName
                   }
              }
         }
         add.calibration <- filesetObj$SetCalibrationFile(ThisCal)
         print(paste("calibration added: ",add.calibration))

         # save EV file
         EVfile$Save()

         #close EV file
         EVApp$CloseFile(EVfile)

       }
  }
  ##########################################

  EVApp$Quit()  #quit echoview
}
