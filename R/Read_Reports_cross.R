#'Read_Reports_cross
#'
#' @description helper function to read in biomass at age reports from EchoPro. This version
#' is different from Read_Reports because it allows you to read in an age-2+ estimate from
#' an age-1+ estimation process
#'
#' @param repcases: a list of cases (will correspond to subfolder names) of reports to read in.
#' @param year: the year (used as a subfolder)
#' @param reportpath: the base path that the script will look for the (year then) case subfolders in
#' @param hakeflag: adult hake (hakeflag=2), not filtering for hake (hakeflag=-1), 
#' age-0 hake (hakeflag=0), age-1 hake only(hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4).  
#' set to 1 if want age1+ numbers, set to 0 (or other number) if want age2+.  Defaults to age2+
#' @examples
#'  \dontrun{
#' Blist<-Read_Reports(repcases, year, reportpath)
#'}
#' @export

Read_Reports_cross<-function(repcases, year, projpathbase,estimflag=4,hakeflag=2) {
     
     if (hakeflag==4){  #age1+
          Exind<-43  #look here when want age-1+ output
     }else{
          Exind<-44  #look here for age-2+ output
          Agepath<-'Age2+'
     }
     if (estimflag==4 ){  
          Agepath<-'Age1+'  #look here for age-1+ estimation method
     }else{
          Agepath<-'Age2+'  #look here for age-2+ estimation method
     }
     
     Biom2vec<-data.frame(matrix(NA, nrow = 1, ncol = length(repcases)))
     colnames(Biom2vec)<-repcases  #rename columns to match case names
     for (k in 1:length(repcases)){
          reportpath<-file.path(projpathbase,year,'Reports',repcases[[k]],Agepath)
          repname<-('kriged_len_age_biomass_table.xlsx')
          fulldata<-readxl::read_excel(file.path(reportpath,repname),sheet='Sheet3')
          Biom2vec[k]<-fulldata[Exind,2] #total age2+biomass
     }
     return(list(Biom2vec,fulldata))
}