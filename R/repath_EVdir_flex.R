#' repath_EVdir_flex.R
#'
#' @description Function to take a directory of EV files and reset the path.
#' This works for EV files that have just an EK80 fileset, or that have the EK60 file set
#' as the first fileset, and the EK80 as the second fileset.
#' May want to eventually convert to calling "fileset1" and "fileset2" instead of EK60 and EK80.
#' Written by RT 2022.
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
#' EK80_Cal_File
#' EK80_Raw_Dir
#'
#' @param ni is an optional argument for which file to start with (default ni=1 is the first file).
#' @param sheet is an optional argument to denote which sheet of the DirNameFile
#' 
#' @examples
#'  \dontrun{
#' DirNameFile="C:/rthomas/R/Rcode/EK60_EK80 conversion/EK60_Ek80_conv_updated/Directory Structure EK60 EK80 conversion updated.xlsx"
#' SurveyName='2019_US'
#' repath_EVdir(SurveyName,DirNameFile)
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
       CALfileName_EK80<-DirTableSurvey$EK80_Cal_File
     }else{  #multiple calibrations for the survey, look in the excel sheet
       CALsheetname<-paste0(SurveyName,"_cal")
       CALsheet<-readxl::read_excel(DirNameFile, sheet=CALsheetname)
     }
  #location of .raw files
  RAWdir<-DirTableSurvey$Raw_Dir
  RAWdir_EK80<-DirTableSurvey$EK80_Raw_Dir

  #location of transect EV files
  Transectdir<-DirTableSurvey$Orig_EV_Dir

  ###############################################
  #         Update paths in Echoview            #
  ###############################################

  # list the EV files
  EVfile.list <- list.files(file.path(Transectdir), pattern="*.(?i)EV$")

  # create COM connection between R and Echoview
  EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")
  nfiles=length(EVfile.list)  #number of EV files

  ## ------------- start loop
  for (i in EVfile.list[ni:nfiles]){
    ni=which(EVfile.list==i)
    print(ni)
    EVfileName <- file.path(Transectdir, i)
    print(EVfileName)
    Transect<-getTnumv2(EVfileName)

    # open EV file
    EVfile <- EVApp$OpenFile(EVfileName)

    #set the first fileset object
    filesetObj <- EVfile[["Filesets"]]$Item(0)  #RAW

    #######
    # EK60
    #######

    # set data properties object and input new datafiles folder location
    evPropObj <- EVfile[["Properties"]]
    evPropObj[["DataPaths"]]$Clear()
    evPropObj[["DataPaths"]]$Add(file.path(RAWdir))
    print(paste("data path:", evPropObj[["DataPaths"]]$Item(1)))
    # set calibration file
    if (DirTableSurvey$Mult_Cal_Flag==0){  #multiple calibration flag off, Calfilename is already set above
    }else{  #multiple calibrations for the survey, look in the excel sheet
         Bounds<-as.matrix(CALsheet[,c("x_low_bound","x_up_bound")])
         for (ri in 1:nrow(Bounds)){
              if (between(Transect,Bounds[ri,1],Bounds[ri,2])){  #If the transect is within the bounds of that row
                   CALfileName<-as.character(CALsheet[ri,3])
              }
         }
    }
    add.calibration <- filesetObj$SetCalibrationFile(CALfileName)
    print(paste("calibration added: ",add.calibration))

    # save EV file
    EVfile$Save()

    #close EV file
    EVApp$CloseFile(EVfile)

  }
  ##########################################

  EVApp$Quit()  #quit echoview
}
