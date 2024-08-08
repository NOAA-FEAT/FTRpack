#'Read_Spat_Unkriged_Reports_cross
#'
#' @description helper function to read in spatial biomass at age reports from EchoPro. This version
#' is different from Read_spat_unkriged_Reports because it allows you to read in an age-2+ estimate from
#' an age-1+ estimation process.  Will keep just the stratum name and total biomass (no at-age data)
#'
#' @param repcases: a list of cases (will correspond to subfolder names) of reports to read in.
#' @param year: the year (used as a subfolder)
#' @param projpathbase: the base path that the project is in
#' @param estimflag:  estimation method (age-2+ or age-1+ method).  Age-1+ method is 4,
#' age-2+ method is 2 (or any number other than 4)
#' @param hakeflag: adult hake (hakeflag=2), not filtering for hake (hakeflag=-1),
#' age-0 hake (hakeflag=0), age-1 hake only(hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4).
#' set to 1 if want age1+ numbers, set to 0 (or other number) if want age2+.  Defaults to age2+
#' @examples
#'  \dontrun{
#' Blist<-Read_Spat_Unkriged_Reports_cross(repcases, year, projpathbase,estimflag,hakeflag)
#'}
#' @export

Read_Spat_Unkriged_Reports_cross<-function(repcases, year, projpathbase,estimflag=4,hakeflag=2) {

     if (hakeflag==4){  #age1+
          Exind<-43  #look here when want age-1+ output
          Colind<-c(6:25)  #look here for age-1+
     }else{
          Exind<-44  #look here for age-2+ output
          Colind<-c(7:25)  #look here for age-2+
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
          #read in kriged biomass @ age file
          fbase<-c("EchoPro_un-kriged_aged_output")
          Fs<-list.files(file.path(reportpath), pattern = fbase)  #lists both the 0 and 1 - use the 0
          fpbase<-file.path(reportpath,Fs[1])
          SpBatAge<-readxl::read_excel(fpbase,sheet=1)
          SpBatAge_names<-SpBatAge[1,]
          SpBatAge=SpBatAge[-1,]
          colnames(SpBatAge)<-SpBatAge_names
          SpB<-SpBatAge[,c(1:5)] #keep metadata
          SpB$Agesum<-rowSums(SpBatAge[,Colind])
     }
     return(SpB)
}
