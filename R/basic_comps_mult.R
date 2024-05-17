#' basic_comps_mult.R
#'
#' @description Function to compare several dataframes of Echoview file exports.  
#' The output is a list along with plots.  The first item in the list are Boolean values that reflect
#' the answers to the following questions:
#' The second item in the list are the row and columns of differences between the 
#' two dataframes.
#'
#' @param datalist - list of dataframes
#' @param compcols - the names of columns to be compared for certain comparisons
#' @param imagedirin - the directory to save the images/tables in
#' @param cases - a list of text strings with the case names for each datafram 
#' @param hakeflag - adult hake (hakeflag=2), not filtering for hake (hakeflag=-1), 
#' age-0 hake (hakeflag=0), age-1 hake only (hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4).  
#' @examples
#'  \dontrun{
#' T<-basic_comps_mult(datalist,compcols,imagedirin,cases,,hakeflag[k])
#'}
#' @export

basic_comps_mult<-function(datalist,compcols,imagedirin,cases,hakeflag,...){
     #hakeflag is 1 for age-1 only, 2 for age-2+, (and 3 for age1+ only) unlike age-1 flag which is 1 for age-1, 0 for age-2, and -1 for age1 only
     #age1flag<-2-hakeflag
     if (hakeflag==1){
           #look here when age-1 only
          Agename<-'Age1'
     }else if (hakeflag==3){#look here for age-1+
          Agename<-'Age1+'  #which directory to look at
     }else {  #look here for age-2+
          Agename<-'Age2+'
     }
     nlist<-length(datalist)  #number of dataframes in the dataframe list
     nL<-10  #make this calculated?
     year<-substr(datalist[[1]]$Survey[1],1,4)
     L<-vector(length=nL)  #make vector to put test outputs into
     dataccat<-NULL
     for (k in 1:nlist){
          datac<-dplyr::select(datalist[[k]],dplyr::all_of(c(compcols,"Transect")))
          datac$dataset<-cases[[k]]
          dataccat<-rbind(dataccat,datac)
     }
     #Make a table to look at summed NASC by transect
     dataccat_SumbyT_c<-aggregate(.~Transect+dataset, dataccat, FUN=sum) 
     names(dataccat_SumbyT_c)[names(dataccat_SumbyT_c) == 'Group.1'] <- 'Transect'
     ###
     #make plots/displays
     ###
     
     # First make the images/year directory if it doesn't exist
     suppressWarnings(dir.create(file.path(imagedirin)))
     test<-janitor::compare_df_cols(datalist)  #use janitor package to show difference between columns
     print(test)
     values <- setNames(c('column_name',cases),colnames(test))
     ft<-flextable::flextable(test)
     ft<-flextable::set_header_labels(ft,values=values)
     saveRDS(ft,file.path(imagedirin,paste0(year,"_export_column_table")))
     
     # Make these plots regardless
     # Plot sum of transect NASC
     par(mfrow=c(1,1))
     for (k in 1:length(compcols)){
          # decided to switch to ggplot
          filename <- file.path(imagedirin,paste0(Agename," Overlain transect summed ", compcols[k]," for ", year , " survey.png"))
           p<-ggplot2::ggplot(data=dataccat_SumbyT_c,aes(x=Transect,y=!!sym(compcols[k])), group=dataset)+ #https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
                labs(title=compcols[k], x='Transect',y='Sum')+
                geom_point(aes(col=dataset))+
                theme_bw()
                print(p)
           ggplot2::ggsave(filename,dpi=500)  #could maybe use saveRDS instead if wanted to be loaded just into markdown later.
     }
     ## plot summed NASC
     par(mfrow=c(1,1))
     ss0<-aggregate(.~+dataset, dataccat, FUN=sum)  #isn't necessarily in original order
     ss0<-ss0[match(ss0$dataset,cases),]  #put into order of cases
     # temp<-dataccat %>%
     #      group_by(dataset) %>%
     #      summarise(PRC_NASC = sum(PRC_NASC)) %>%
     #      arrange(dataccat$dataset)
     ss<-ss0$PRC_NASC
     barplot(as.numeric(ss), main="Summed NASC")
     lbs=paste(c("difference:", round(diff(ss),2), ",proportion:",round(diff(ss)/ss[1],3)), collapse=" ")
     text(1,ss[1]/2,lbs)
     filename <- file.path(imagedirin,paste0(Agename," Summed NASC for ",  year, " survey.png"))
     dev.copy(png, filename, width = 1000)
     dev.off()
     
     Per_diff=round((ss-ss[1])*100/ss[1],5)  #round to 5 decimal places
     lbls2=c('year',cases)
     sstable<-data.frame(year,t(ss))
     colnames(sstable)<-c('year',cases)
     Per_difftable<-data.frame(year,t(Per_diff))
     colnames(Per_difftable)<-c('year',cases)
     ftNASC<-flextable::flextable(sstable)
     ftPer_diff<-flextable::flextable(Per_difftable)
     values <- setNames(lbls2,colnames(sstable))
     ftNASC<-flextable::set_header_labels(ftNASC,values=values)
     ftPer_diff<-flextable::set_header_labels(ftPer_diff,values=values)
     #ftNASC<-flextable::set_header_labels(ftNASC,case1_NASC=case1,case2_NASC=case2, Per_diff="Percent Difference")
     saveRDS(ftNASC,file.path(imagedirin,paste0(year,"_export_NASC_table_Age",hakeflag)))
     saveRDS(ftPer_diff,file.path(imagedirin,paste0(year,"_export_NASC_Per_diff_table_Age",hakeflag)))
     
     ## Make table of transects used for each case
     Tu<-list()
     NotTuX<-list()
     NotTuY<-list()
     for (k in 1:nlist){
          Tu[[k]]<-unique(datalist[[k]]$Transect)
     }
     for (k in 1:nlist){
          NotTuX[[k]]<-setdiff(Tu[[k]],Tu[[1]])  #assumes that we are comparing against the first case
          NotTuY[[k]]<-setdiff(Tu[[1]],Tu[[k]])
     }
     RC<-NA
     return(list(L,RC,sstable,NotTuX,NotTuY,Per_difftable))  
     #return(list(L,RC,sstable,notX,notY,RCind))
}
