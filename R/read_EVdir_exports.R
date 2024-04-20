#' read_EV_exports:  Read in a directory of .csv files exported from Echoview
#'
#' @description Read in a directory of .csv files (can be of several variables)
#' exported from Echoview.   The function preserves the cells by depth.
#' Uses packages readxl, readr, purrr, and dplyr.
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files. Uses
#'  Export_dir as the directory that the exported files are in.
#'
#' @param database is an optional argument for format of the .csv files to.  Default is database (1).  Spreadsheet =0
#' @param exportloc is an optional argument for using the Export_Dir or the Region_Export_dir column
#' entry in the DirNameFile spreadsheet.  1 is Export_Dir, and 0 is Region_Export_dir
#' @param sheet is an optional argument to denote which sheet of the DirNameFile
#' want to use. This defaults to the first sheet if not set.
#' @param hakeflag sets whether specific "hake"-type classes are kept.  hakeflag=0 keeps just age-0 hake,
#' hakeflag=1 keeps just age-1 hake, hakeflag=2 keeps just regular age2+ hake, hakeflag=3 keeps everything that could
#' possibly be hake.  Any other numbers (including -1) will keep everything regardless of whether it is hake.
#' The default is hakeflag=2, keeping just adult hake.
#'
#' @examples
#' temp<-read_EV_exports(SurveyName, DirNameFile,database=1,exportloc=1)
#'
#' @export

read_EVdir_exports<-function(SurveyName, DirNameFile,database=1,exportloc=1,sheet=NULL,hakeflag=2,...){


DirTable <- readxl::read_excel(DirNameFile, sheet)  #load in directories/paths/ltocations
DirTable_yr<- subset(DirTable, Survey ==SurveyName) #just year of interest
DirTable_yr[] <- lapply(DirTable_yr, function(x) if(is.factor(x)) factor(x) else x)  #to ensure dataframe actually removed other variables in subset

#setup folders to read in data
BaseYearPath<-as.character(DirTable_yr$Base_Path[1])
BaseProjPath<-as.character(DirTable_yr$Base_Path[1])

print(paste('Exported data directory is', DirTable_yr$Export_Dir)) #display the variable to be imported
if (exportloc==1){
        CONV_exportbase <- file.path(DirTable_yr$Export_Dir) #set to export directory
}
else if (exportloc==0){
        CONV_exportbase <- file.path(DirTable_yr$Region_Export_Dir) #set to region export directory
}
else {print("export location needs to be 0 or 1")}

setwd(CONV_exportbase)
# for spreadsheet format
if (!database==1){  #if database does not equal 1; that is, spreadsheet
NASC.list.to.combine <-list.files(file.path(CONV_exportbase), pattern = "*.csv") #be sure to verify correct number of files loaded
CONV_10m_combine <- NASC.list.to.combine %>% purrr::map_df(~read_csv_addflnm(.))
}

else if(database==1){#database format
     #load in analysis sheets
     NASC.list.to.combine.analysis <-list.files(file.path(CONV_exportbase), pattern = "*analysis).csv") #be sure to verify correct number of files loaded
     CONV_10m_combine_analysis <- NASC.list.to.combine.analysis %>% purrr::map_df(~read_csv_addflnm(.))

     #load in cell sheets
     NASC.list.to.combine.cells <-list.files(file.path(CONV_exportbase), pattern = "*cells).csv") #be sure to verify correct number of files loaded
     CONV_10m_combine_cells <- NASC.list.to.combine.cells %>% purrr::map_df(~read_csv_addflnm(.))
     #print(NASC.list.to.combine.cells)

     #load in interval sheets
     NASC.list.to.combine.intervals <-list.files(file.path(CONV_exportbase), pattern = "*intervals).csv") #be sure to verify correct number of files loaded
     CONV_10m_combine_intervals <- NASC.list.to.combine.intervals %>% purrr::map_df(~read_csv_addflnm(.))

     #load in layers sheets
     NASC.list.to.combine.layers <-list.files(file.path(CONV_exportbase), pattern = "*layers).csv") #be sure to verify correct number of files loaded
     CONV_10m_combine_layers <- NASC.list.to.combine.layers %>% purrr::map_df(~read_csv_addflnm(.))

     #do joins
     temp <- dplyr::inner_join(CONV_10m_combine_cells, CONV_10m_combine_intervals, by=c("Process_ID","Interval"))
     temp <- dplyr::inner_join(temp, CONV_10m_combine_layers, by=c("Process_ID","Layer"))
     CONV_10m_join<- dplyr::inner_join(temp, CONV_10m_combine_analysis, by="Process_ID")
     CONV_10m_join$Transect <- sapply(strsplit(CONV_10m_join$filename.x,"-", fixed = TRUE),`[`,5) #https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r
     CONV_10m_join$ID <- paste(CONV_10m_join$Transect,CONV_10m_join$Interval, sep="_")
     CONV_10m_join$Survey <- paste(SurveyName)
     CONV_10m_join$Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", CONV_10m_join$Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
     C_join=as.data.frame(CONV_10m_join)  #make into a more conventional dataframe

     ## filter data to just return some region_classes
     #hakeflag: -1, don't filter for hake, 0 age-0 hake, 1 = filter for age-1, 2 = filter for age-2+, 3=all hake
     if (hakeflag==-1){  #don't do anything
       } else if (hakeflag==0){
            C_join<-dplyr::filter(C_join,grepl("age-0 hake", Region_class, ignore.case=TRUE))
       } else if (hakeflag==1){
            C_join<-dplyr::filter(C_join,grepl("age-1 hake", Region_class, ignore.case=TRUE))
       } else if (hakeflag==2){
            C_join<-dplyr::filter(C_join,grepl("^hake", Region_class, ignore.case=TRUE))
       } else if (hakeflag==3){
            C_join<-dplyr::filter(C_join,grepl("hake", Region_class, ignore.case=TRUE)) #https://stackoverflow.com/questions/28860069/regular-expressions-regex-and-dplyrfilter
     }
     #everything else will keep everything.
     return (C_join) #return result
     }
}


