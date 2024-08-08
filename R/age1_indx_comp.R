#' age1_indx_comp.R
#'
#' @description Function to compare the age-1 abundance estimate from cases to that
#' from the original age-1 index.
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files. Uses
#'  Export_dir as the directory that the exported files are in.
#' @param imagedir  - the directory for the images files to be placed in
#' @param cases - is the set of cases to be compared
#' @param hakeflagin adult hake (hakeflag=2), not filtering for hake (hakeflag=-1), age-0 hake (hakeflag=0), age-1 hake only (hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4). Passing this although assuming this will be hakeflag=1
#' @param yearin is the year that is being investigated
#' @param age1indfname is the directory the age-1 index file is found in (file needs to
#' be named 'Age1_Full_Time_Series'

#' @examples
#'  \dontrun{
#' T<-age1_indx_comp(SurveyName, DirNameFile,imagedir,cases,hakeflagin, yearin, age1indfname)
#' }
#' @export
age1_indx_comp<-function(SurveyNamein,DirNameFilein,imagedirin, casesin, hakeflagin,yearin,age1indfnamein,...){
     #read in abundance comparisons for age-1
     repcases<-casesin[c(1,3,4)]
     year<-yearin
     hakeflagin<- 1 #age1 only
     A<-basic_abund_comp(SurveyNamein,DirNameFile,imagedirin, repcases,hakeflag=hakeflagin,yearin=year) #age-1  #compares the age-1 abundance between cases
     #read in age-1 index sheet
     Age1ind <- readxl::read_excel(age1indfnamein, 'Age1_Full_Time_Series')  #load in directories/paths/ltocations
     #find the index corresponding to the year
     Yearind<-which(Age1ind$Year==yearin)
     This_Age1ind<-Age1ind[Yearind,2]
     nA<-length(A[[1]])
     A[[1]][[nA+1]]<-This_Age1ind  #put Age1 index onto end
     B<-as.data.frame(A[[1]])
     nB<-length(B)
     fullcases<-c(repcases,"age-1 index")
     colnames(B)<-fullcases  #set column names of table
     Alist<-vector(length=length(fullcases))
     Per_diff<-vector(length=length(fullcases))
     for (k in (1:length(fullcases))){
            Per_diff[k]=round((B[k]-B[nB])*100/B[nB],1)  #round to 5 decimal places.  Compare to last case, assuming it is the age-1 index
     }

     Btable<-data.frame(year,rbind(B,Per_diff))
     #row.names(TotBiotable)<-c("Biomass","Percent difference")
     Btable<-cbind(c("Abundance","Percent difference"),Btable)
     colnames(Btable)<-c("value","year",fullcases)
     ftBtable<-flextable::flextable(Btable)
     saveRDS(ftBtable,file.path(imagedirin,paste0(year,"_age1_indx_comp_results_ft")))
     return(B)

}
