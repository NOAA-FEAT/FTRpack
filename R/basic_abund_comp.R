#' basic_abund_comp.R
#'
#' @description Function to read in and compare abundance estimate from the various cases.
#' Similar to the biomass version, but doesn't do
#' the spatial comparison (no spatial abundance reports available).
#'
#'
#' @param SurveyName - name of survey
#' @param DirNameFile excel file - contains paths and locations of files. Uses
#'  Export_dir as the directory that the exported files are in.
#' @param imagedir  - the directory for the images files to be placed in
#' @param cases - is the set of cases to be compared
#' @param hakeflagin adult hake (hakeflag=2), not filtering for hake (hakeflag=-1), age-0 hake (hakeflag=0), age-1 hake only (hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4). Passing this although assuming this will be hakeflag=1
#' @param yearin is the year that is being investigated
#'
#' @examples
#'  \dontrun{
#' T<-basic_abund_comp(SurveyName,DirNameFile,imagedir,cases,hakeflagin,year)
#' }
#' @export


basic_abund_comp<-function(SurveyNamein,DirNameFilein,imagedirin, cases, hakeflagin,yearin,...){
     year<-yearin
     TotAbn<-vector(length=length(cases))
     KAoutlist<-vector(length=length(cases))
     Per_diff<-vector(length=length(cases))
     for (k in (1:length(cases))){
          KAoutlist[k]<-read_proc_reports_abund(SurveyNamein, DirNameFilein,cases[[k]],hakeflag=hakeflagin) #kriged biomass outsputs
          #KBout<-read_proc_reports(SurveyNamein, DirNameFilein,case1in,age1flag=age1flagin) #kriged biomass outsputs for case 1
          #KBout2<-read_proc_reports(SurveyNamein, DirNameFilein,case2in,age1flag=age1flagin)
          ## Make table of NASC and differences
          TotAbn[k]<-as.numeric(KAoutlist[[k]])
          Per_diff[k]=round((TotAbn[k]-TotAbn[1])*100/TotAbn[1],5)  #round to 5 decimal places.  Compare to first case, assuming it is "original"
     }
     #TotBiotable<-data.frame(year,TotBio,Per_diff)
     TotAbntable<-data.frame(year,rbind(TotAbn,Per_diff))
     #row.names(TotBiotable)<-c("Biomass","Percent difference")
     TotAbntable<-cbind(c("Abundance","Percent difference"),TotAbntable)
     colnames(TotAbntable)<-c("value","year",cases)
     ftTotAbn<-flextable::flextable(TotAbntable)
     #ftTotBio<-flextable::set_header_labels(ftTotBio,TotBio.1.=case1in,TotBio.2.=case2in, Per_diff="Percent Difference")
     #ftTotBio<-flextable::set_header_labels(ftTotBio,c(year,cases))
     saveRDS(ftTotAbn,file.path(imagedirin,paste0(yearin,"_export_AB_table", hakeflagin)))
     #
     # ## make a table of the difference in spatial unkriged
     # uKSpBoutlist<-list()
     # for (k in (1:length(cases))){
     #      uKSpBoutlist[[k]]<-read_spat_unkriged_reports(SurveyNamein, DirNameFilein,cases[[k]],hakeflag=hakeflagin) #kriged biomass outsputs
     #      uKSpBoutlist[[k]][]=lapply(uKSpBoutlist[[k]], as.numeric)
     #      #KBout<-read_proc_reports(SurveyNamein, DirNameFilein,case1in,age1flag=age1flagin) #kriged biomass outsputs for case 1
     #      #KBout2<-read_proc_reports(SurveyNamein, DirNameFilein,case2in,age1flag=age1flagin)
     #      ## Make table of NASC and differences
     #      #TotBio[k]<-as.numeric(KBoutlist[[k]])
     #      #Per_diff[k]=round((TotBio[k]-TotBio[1])*100/TotBio[1],5)  #round to 5 decimal places.  Compare to first case, assuming it is "original"
     # }
     # uKSpBoutlist<-lapply(uKSpBoutlist,round,digits=3) #need round early so that lat longs will match
     # uKSpBout<-uKSpBoutlist[[1]][,c(1:5)] #keep metadata, using that for first case
     # uKSpBout[]<-lapply(uKSpBout,as.numeric)
     # uKSpBout[,cases[[1]]]<-uKSpBoutlist[[1]][,c(6)]  #get biomass for first case
     # #Need to do some rounding before merging to lat longs match well enough
     # #uKSpBout<-round(uKSpBout,digits=6)
     # for (k in (2:length(cases))){
     #      uKSpBout<-merge(uKSpBout,uKSpBoutlist[[k]],by=c("Transect","Lat","Lon"),all=TRUE) #outer join
     #      #uKSpBout[is.na(uKSpBout)] <- 0 #change Nans to 0.
     #      colnames(uKSpBout)[colnames(uKSpBout)=='Agesum']<-cases[[k]]
     #      #uKSpBout[,cases[[k]]]<-uKSpBoutlist[[k]][,c(6)] #get biomass for other cases
     # }
     #
     # #Diff_uKSpBout<-uKSpBout[,c(1:5)] #keep metadata
     # Diff_uKSpBout<-uKSpBout #keep metatdata, strata and total weight for each  (keep all)
     # # Diff_uKSpBout[,cases[[1]]]<-NULL  #get rid of original column.  Need to keep it for later calculations
     # Diff_uKSpBout$wgt_total.x<-NULL #get rid of first weight total column
     # Diff_uKSpBout$wgt_total.y<-NULL #get rid of second weight total column
     # Diff_uKSpBoutGT0<-list()
     # p<-list()
     # for (k in (2:length(cases))){  #don't include difference of original from original in table
     #      Diff_uKSpBout[,cases[[k]]]<-uKSpBout[,cases[[k]]]
     #      Casepername<-paste0(cases[[k]],'%df')
     #      Diff_uKSpBout[,Casepername]<-100*(uKSpBout[,cases[[k]]]-uKSpBout[,cases[[1]]])/uKSpBout[,cases[[1]]]
     #      #p[[k]]<-filter(Diff_uKSpBout,.data[[cases[[k]]]]>0)%>% ggplot(aes(x=Lon,y=Lat, size=.data[[cases[[k]]]]))+
     #      p[[k]]<-filter(Diff_uKSpBout,abs(.data[[Casepername]])>3)%>% ggplot(aes(x=Lon,y=Lat, size=.data[[Casepername]]))+
     #           geom_point()
     #      #PLOT >3% difference
     #      Diff_uKSpBoutGT0[[k]]<-filter(Diff_uKSpBout,.data[[cases[[k]]]]!=0)
     # }
     #
     # for (k in 2:length(cases)){
     #      pfname<-file.path(imagedirin,paste0(yearin,"_BM_Difference_map", hakeflagin,'.png'))
     #      p[[k]]  #plot
     #      ggsave(pfname)
     # }
     #
     return(list(KAoutlist,TotAbntable))
}
