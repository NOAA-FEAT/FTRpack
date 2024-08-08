#'Read_Report_Summaries
#'
#' @description helper function to read in biomass at age reports from EchoPro from different cases
#' as well as with different methods (age1+ (full age) and age2+ methods).  Right now just tested for age-2+ results
#' for both methods
#'
#' @param repcases: a list of cases (will correspond to subfolder names) of reports to read in.
#' @param year: the year (used as a subfolder)
#' @param reportpath: the base path that the script will look for the (year then) case subfolders in
#' @param hakeflag: adult hake (hakeflag=2), not filtering for hake (hakeflag=-1),
#' age-0 hake (hakeflag=0), age-1 hake only(hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4).
#' set to 1 if want age1+ numbers, set to 0 (or other number) if want age2+.  Defaults to age2+
#' @examples
#'  \dontrun{
#' Blist<-Read_Report_Summaries(repcases, year, reportpath, hakeflag)
#'}
#' @export

Read_Report_Summaries<-function(repcases, year, reportpath, hakeflag=2) {
     mlist<-c(1,2) #different methods - 1 is age-1+ (full age) method, and 2 is age-2+ method
     # read in different methods
     for (m in mlist){
          ## test for which age group wanted
          if (hakeflag==2){
               Exind<-44  #assume want age-2+ number
          }else if (hakeflag==4){
               Exind<-43  #assume want age-1+ number
          }else {
               print("Error: age method not possible")
          }

          ## loop through methods
          #Read in kriged biomass
          if (m==1){ #age-1+ methods
               Agepath<-'Age1+'
               KBiom2vec1<-data.frame(matrix(NA, nrow = 1, ncol = length(repcases)))
               Kfulldata1list<-list()
               colnames(KBiom2vec1)<-repcases  #rename columns to match case names
               for (k in 1:length(repcases)){
                    reportpath<-file.path(projpathbase,year,'Reports',repcases[[k]],Agepath)
                    repname<-('kriged_len_age_biomass_table.xlsx')
                    Kfulldata1<-readxl::read_excel(file.path(reportpath,repname),sheet='Sheet3')
                    KBiom2vec1[k]<-Kfulldata1[Exind,2] #total age2+biomass
                    Kfulldata1list[[k]]<-Kfulldata1
               }
          }else if(m==2){ #age-1+ methods
               Agepath<-'Age2+'
               KBiom2vec2<-data.frame(matrix(NA, nrow = 1, ncol = length(repcases)))
               Kfulldata2list<-list()
               colnames(KBiom2vec2)<-repcases  #rename columns to match case names
               for (k in 1:length(repcases)){
                    reportpath<-file.path(projpathbase,year,'Reports',repcases[[k]],Agepath)
                    repname<-('kriged_len_age_biomass_table.xlsx')
                    Kfulldata2<-readxl::read_excel(file.path(reportpath,repname),sheet='Sheet3')
                    KBiom2vec2[k]<-Kfulldata2[Exind,2] #total age2+biomass
                    Kfulldata2list[[k]]<-Kfulldata2
               }
          }else{
               print("error in Read_Report_Summaries: kriged data")
          }

          ## read in unkriged bimoass
          if (m==1){ #age-1+ methods
               Agepath<-'Age1+'
               UKBiom2vec1<-data.frame(matrix(NA, nrow = 1, ncol = length(repcases)))
               UKfulldata1list<-list()
               colnames(UKBiom2vec1)<-repcases  #rename columns to match case names
               for (k in 1:length(repcases)){
                    reportpath<-file.path(projpathbase,year,'Reports',repcases[[k]],Agepath)
                    repname<-('un-kriged_len_age_biomass_table.xlsx')
                    UKfulldata1<-readxl::read_excel(file.path(reportpath,repname),sheet='Sheet3')
                    UKBiom2vec1[k]<-UKfulldata1[Exind,2] #total age2+biomass
                    UKfulldata1list[[k]]<-UKfulldata1
               }
          }else if(m==2){ #age-1+ methods
               Agepath<-'Age2+'
               UKBiom2vec2<-data.frame(matrix(NA, nrow = 1, ncol = length(repcases)))
               UKfulldata2list<-list()
               colnames(UKBiom2vec2)<-repcases  #rename columns to match case names
               for (k in 1:length(repcases)){
                    reportpath<-file.path(projpathbase,year,'Reports',repcases[[k]],Agepath)
                    repname<-('un-kriged_len_age_biomass_table.xlsx')
                    UKfulldata2<-readxl::read_excel(file.path(reportpath,repname),sheet='Sheet3')
                    UKBiom2vec2[k]<-UKfulldata2[Exind,2] #total age2+biomass
                    UKfulldata2list[[k]]<-UKfulldata2
               }
          }else{
               print("error in Read_Report_Summaries: kriged data")
          }

     }
     KFullTab<-rbind(KBiom2vec1,KBiom2vec2)
     UKFullTab<-rbind(UKBiom2vec1,UKBiom2vec2)
     rownames(KFullTab)<-c("Age1m_K","Age2m_K")
     rownames(UKFullTab)<-c("Age1m_UK","Age2m_UK")
     FullTab<-rbind(KFullTab,UKFullTab)
     return(list(KFullTab,UKFullTab,FullTab,Kfulldata1list,Kfulldata2list,UKfulldata1list,UKfulldata2list))

}
