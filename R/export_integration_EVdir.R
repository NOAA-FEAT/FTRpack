#'export_integration_EVdir
#'
#' @description function to export integration from variables in a set of EV files.
#' There are two excel files for inputs (paths, parameters to export).  The exports
#' go into a directory that is automatically named by date.  This directory is in the
#' export directory listed in the DirNameFile.  The EVfile is not saved in the process.
#' This works for EV files that have an ek5 sub-directory too.
#' The ek5 subdirectory must be named 'ek5 EV'
#'
#' @param SurveyName name of survey as in excel file
#' @param DirNameFile excel file name of paths
#' @param variables - variables to export
#' @param paramnamefile optional excel file name of parameters to export or not export.
#' The column ToExport is parameters to export.  The column NoExport is parameters to not export
#' @param ni is an optional argument for which file to start with (default ni=1 is the first file).
#' This defaults to 1 (all EV files)
#' @param gdepth is an optional argument for the cell depth.  gdepth defaults to 10 m
#' @param database is an optional argument for export format.  Default is database (1).  Spreadsheet =0
#' @param sheet is an optional argument to denote which sheet of the DirNameFile
#' want to use. This defaults to the first sheet if not set.
#' @param datedir  Flag to state whether to create and use a separate date
#' sub-directory when redoing exports, so originals don't get overwritten. This
#' defaults to using a date sub-directory (datedir=1 is default; make date directory.
#' datedir=0 does not create a sub-directory)
#'
#' @export
export_integration_EVdir <- function(Survey_Name, DirNameFile,variables,paramnamefile=NA,ni=1,gdepth=10,database=1,sheet,datedir=1,...) {

  ###############################################
  #----------------  INPUT  --------------------#
  ###############################################

  DirTable <- readxl::read_excel(DirNameFile,sheet)
  DirTableSurvey<- subset(DirTable, Survey == Survey_Name)
  DirTableSurvey[] <- lapply(DirTableSurvey, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset
  DirTableSurveyExcepts<- readxl::read_excel(DirNameFile,sheet='Exceptions')
  DirTableSurveyExcepts<-subset(DirTableSurveyExcepts, Survey == Survey_Name)
  DirTableSurveyExcepts[] <- lapply(DirTableSurveyExcepts, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

  #setup folders to read in data
  BasePath<-DirTableSurvey$Base_Path
  #CONV_EV<-DirTableSurvey$Post_EV_Dir
  GPSdistX<-DirTableSurveyExcepts$GPS_dist_x
  #location of transect EV files
  CONV_EV<-DirTableSurvey$Post_EV_Dir
  CONV_EV_EK5<-file.path(DirTableSurvey$Post_EV_Dir, 'ek5 EV')
  EK_flag<-DirTableSurvey$EK5_Dir_Flag

  CONV_exportbase<-DirTableSurvey$Export_Dir
  if (datedir==0){
       CONV_export<-CONV_exportbase #don't use a date sub-directory
       suppressWarnings(dir.create(file.path(CONV_export)))
  }else{
       # Create a separate date sub-directory when redoing exports, so originals don't get overwritten
       date_exportdir<-format(Sys.time(),"%Y%m%d")
       suppressWarnings(dir.create(file.path(CONV_exportbase,date_exportdir)))
       CONV_export<-file.path(CONV_exportbase, date_exportdir)
       suppressWarnings(dir.create(file.path(CONV_export)))
  }

  vars <- data.frame(variables,stringsAsFactors = FALSE)
  vars_EV<-data.frame(DirTableSurvey$EK5_var,stringsAsFactors = FALSE)  #variable name for EK5 data
  colnames(vars_EV)<-c("variables")  #give same name to column name of EV variable data frame
  listvars<-list(vars,vars_EV)

  # for(v in 1:nrow(vars)){
  #   var <- vars$variables[v]
  #   suppressWarnings(dir.create(file.path(CONV_export,var)))
  # }


  #list the EV files to integrate
  #EVfile.list <- list.files(file.path(CONV_EV), pattern="*.(?i)EV$")
  EVfileEK5.list<- list.files(file.path(CONV_EV_EK5), pattern="*.(?i)EV$")
  EVfile.list<-setdiff(list.files(file.path(CONV_EV), pattern="*.(?i)EV$"), list.dirs(file.path(CONV_EV),recursive = FALSE, full.names = FALSE)) #don't include directories

  # Loop through EV files
  nfiles=length(EVfile.list)  #number of EV files

  if (EK_flag==1){  #if using ek5 files at all
       EVlists<-list(EVfile.list,EVfileEK5.list)
       Transectlistdir<-list(CONV_EV,CONV_EV_EK5)
  }else{
       EVlists<-list(EVfile.list)
       Transectlistdir<-list(CONV_EV)
  }
  ## ------------- start loop
  for (j in 1:length(EVlists)){  #put back to j=1
       Thisfile.list<-EVlists[[j]]
       ThisTransectdir<-Transectlistdir[[j]]
       nfiles=length(Thisfile.list)  #number of EV files
       Thisvars=listvars[[j]]
       for (i in Thisfile.list[ni:nfiles]){
         ni=which(Thisfile.list==i)
         print(ni)
         # create COM connection between R and Echoview
         EVApp <- RDCOMClient::COMCreate("EchoviewCom.EvApplication")
         EVApp$Minimize()  #keep window in background

         # EV filenames to open
         EVfileNames <- file.path(ThisTransectdir, i)
         EvName <- strsplit(i, split = '*.EV')[[1]]

         # open EV file
         EVfile <- EVApp$OpenFile(EVfileNames)
         EVfileName <- file.path(ThisTransectdir, i)
         print(EVfileName)

         # Get transect number
         Temp<-strsplit(EvName,'x')
         #special case for 2011
         if (DirTableSurvey$Survey_num==201103){
              Tr<-as.numeric(gsub(".*?([0-9]+).*", "\\1", Temp))
              if (EvName=="x0.3hake69"){ Tr<-0.3}
              if (EvName=="x0.5hake69"){ Tr<-0.5}
              if (EvName=="x0.7hake69"){ Tr<-0.7}
         } else{
              Tr<-as.numeric(gsub(".*?([0-9]+).*", "\\1", Temp))
         }

         #Update export parameters specifically if the paramnamefile has an entry
         if (!is.na(paramnamefile)){
           Obj_ExpVar<-EVfile[['Properties']][['Export']][['Variables']]  #set export variables object
           par_Table<- readxl::read_excel(paramnamefile)  #read in directory table
           SetExpParamsFun(Obj_ExpVar, par_Table$ToExport[!is.na(par_Table$ToExport)],par_Table$NoExport[!is.na(par_Table$NoExport)])
           }

         #Set to export database or spreadsheet format
         Obj_ExpVarFor<-EVfile[['Properties']][['Export']]
         if(database==1){
           Obj_ExpVarFor[['Mode']]<-1  #database format
           }
         else if(database==0){
           Obj_ExpVarFor[['Mode']]<-2
           }  #spreadsheet format

         # Variables object
         Obj <- EVfile[["Variables"]]

         # loop through variables for integration
         for(v in 1:nrow(Thisvars)){
           #Obj <- EVfile[["Variables"]]  #reset this, gets unset in BiomassParameter setting
           var <- Thisvars$variables[v]
           if (is.null(Obj$FindByName(var))==TRUE){
                var<-paste0(var,' (1)')  #try adding (1) to the variable if it doesn't work the first time
           }
           varac <- Obj$FindByName(var)$AsVariableAcoustic()
           Obj_propC<-varac[['Properties']][['Calibration']]
           freqL<-Obj_propC$Get('Frequency',1)  #get the frequency from the calibration properties
           freq<-as.character(as.numeric(freqL))  #tidy up

           # Set analysis lines
           Obj_propA<-varac[['Properties']][['Analysis']]
           Obj_propA[['ExcludeAboveLine']]<-"14 m from surface"
           Obj_propA[['ExcludeBelowLine']]<-"1.0 m bottom offset"

           # Set analysis grid and exclude lines on Sv data
           Obj_propGrid <- varac[['Properties']][['Grid']]
           Obj_propGrid$SetDepthRangeGrid(1, gdepth)
           if (is.element(Tr,GPSdistX)){
                Obj_propGrid$SetTimeDistanceGrid(2, 0.5) #export as GPS distance
                #for EV files that are exporting with vessel log, change the export parameters to use Dist_S and Dist_E instead of vessel log start and end
                Obj<-Obj_ExpVar$Item("VL_start")
                Obj[["Enabled"]]<-FALSE
                Obj<-Obj_ExpVar$Item("VL_end")
                Obj[["Enabled"]]<-FALSE
                Obj<-Obj_ExpVar$Item("Dist_S")
                Obj[["Enabled"]]<-TRUE
                Obj<-Obj_ExpVar$Item("Dist_E")
                Obj[["Enabled"]]<-TRUE
           }else{
                Obj_propGrid$SetTimeDistanceGrid(3, 0.5)   #export as Vessel log distance
           }


           # Set threshold
           Obj_propD<-varac[['Properties']][['Data']]
           Obj_propD[['ApplyMinimumThreshold']]<-1
           Obj_propD[['LockSvMinimum']]<-0
           Obj_propD[['MinimumThreshold']]<--69

           # export by cells
           #exportcells <- file.path(CONV_export, paste(EvName, freq, "0.5nmi_10m_cells.csv", sep="_"))
           #use standard survey naming conventions
           V<-paste0('V',DirTableSurvey$Ship_num)  #vessel number;  %insert Canadian vessel as second
           S<-paste0('S',DirTableSurvey$Survey_num) #survey number  %also CAN
           X<-paste0('X',DirTableSurvey$Transducer_num) #(['X1-']);  #transducer number  %also CAN
           F<-c('F38') #frequency
           T<-paste0('T',Tr)  #transect number
           Z<-c('Z0-.csv') #%zone
           Outfname=paste(V,S,X,F,T,Z, sep="-"); #%name of output file with appropriate syntax for MACEBASE
           #name1<-paste(EvName, freq, "0.5nmi_10m_cells.csv", sep="_")
           exportcells <- file.path(CONV_export,Outfname)
           varac$ExportIntegrationByRegionsByCellsAll(exportcells)

           # Set analysis grid and exclude lines on Sv data back to original values
           Obj_propGrid<-varac[['Properties']][['Grid']]
           Obj_propGrid$SetDepthRangeGrid(1, 50)
           Obj_propGrid$SetTimeDistanceGrid(3, 0.5)
           }



         # save EV file
         #EVfile$Save()  Not saving the EV file

         #close EV file
         EVApp$CloseFile(EVfile)


         #quit echoview
         EVApp$Quit()


       ## ------------- end loop

       }
       ni<-1 #reset ni
  }
}

