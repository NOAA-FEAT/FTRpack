#' basic_comps.R
#'
#' @description Function to compare two dataframes of Echoview file exports.  
#' The output is a list along with plots.  The first item in the list are Boolean values that reflect
#' the answers to the following questions:
#' The second item in the list are the row and columns of differences between the 
#' two dataframes.
#'
#' @param X data frame
#' @param Y second data frame
#' @param compcols the names of columns to be compared for certain comparisons

#' @examples
#'  \dontrun{
#' T<-basiccomps(data,data2)
#'}
#' @export

basic_comps<-function(X,Y,compcols,imagedirin,case1in, case2in,...){
     nL<-10
     year<-substr(X$Survey[1],1,4)
     L<-vector(length=nL)  #make vector to put test outputs into
     #Test for same number of columns
     if ((ncol(X)!=ncol(Y))==TRUE){
          print('The number of columns in the two dataframes are not the same')
          L[1]<-FALSE
          SameNumCol<-FALSE
     } else{ 
          #Are data frames identical?
          L[1]<-identical(X,Y) #Are data frames identical?
          SameNumCol<-TRUE
     }
     Xc<-dplyr::select(X,dplyr::all_of(c(compcols,"Transect")))
     Xc$dataset<-case1in
     Yc<-dplyr::select(Y,dplyr::all_of(c(compcols, "Transect")))
     Yc$dataset<-case2in
     XYc<-rbind(Xc,Yc)
     #Are sums of compared columns identical?
     L[2]<-identical(colSums(Xc[,compcols]),colSums(Yc[,compcols]))
     #Are the (location and) values of the compared columns identical?
     L[3]<-identical(Xc,Yc)
     #Test for same number of row
     if ((nrow(Xc)!=nrow(Yc))==TRUE){
          print('The number of rows in the two dataframes are not the same')
          RCind<-NA
          RC<-FALSE
     } else{ 
          #Are data frames identical?
          RCind<-which(Xc != Yc, arr.ind=TRUE)  #has to have the same number of rows for this to work
          #Above isn't very useful.  Keep in case want to output later.
          #Set RC (which was above) to TRUE for having the same number of rows.
          RC<-TRUE
     }
     
     #Make a table to look at summed NASC by transect
     #data_SumbyT_c<-aggregate(X[compcols], list(data$Transect), FUN=sum) 
     #names(data_SumbyT_c)[names(data_SumbyT_c) == 'Group.1'] <- 'Transect'
     #data2_SumbyT_c<-aggregate(data2[compcols], list(data2$Transect), FUN=sum) 
     #names(data2_SumbyT_c)[names(data2_SumbyT_c) == 'Group.1'] <- 'Transect'
     XYc_SumbyT_c<-aggregate(.~Transect+dataset, XYc, FUN=sum) 
     names(XYc_SumbyT_c)[names(XYc_SumbyT_c) == 'Group.1'] <- 'Transect'
     ###
     #make plots/displays
     ###
     
     # First make the images/year directory if it doesn't exist
     suppressWarnings(dir.create(file.path(imagedirin)))
     # Display this if the numbers of columns are not the same
     if(SameNumCol==FALSE){
          test<-janitor::compare_df_cols(X,Y)  #use janitor package to show difference between columns
          print(test)
          ft<-flextable::flextable(test)
          ft<-flextable::set_header_labels(ft,X=Xc$dataset[1],Y=Yc$dataset[2])
          saveRDS(ft,file.path(imagedirin,paste0(year,"_export_column_table")))
     }
     
     #Make these plots of the numbers of rows are not the same
     if(RC==FALSE){
          VLdiffs1<-setdiff(X$VL_start, Y$VL_start)
          VLdiffs2<-setdiff(Y$VL_start, X$VL_start)
          # print(paste(c('The second dataframe does not have the vessel logs ',VLdiffs1),collapse =" "))
          # print(paste(c('The first dataframe does not have the vessel logs ',VLdiffs2),collapse =" "))
          }
     
     if(RC==TRUE){
          # Make these plots when the data frame sizes are the same
               #make plots of each of the compared columns in each dataset
          VLdiffs1<-NA
          VLdiffs2<-NA
          par(mfrow=c(length(compcols),1))
          for (k in 1:length(compcols)){
               plot(as.numeric(Xc[,k]),as.numeric(Yc[,k]),type='o',main=compcols[k],xlab='First data set',ylab='Second data set')
               filename <- file.path(imagedirin,paste0("Compared ", compcols[k]," for survey ",  X$Survey[1], ".png"))
               dev.copy(png, filename, width = 1000)
               dev.off()
          }
          
          #plot x axis as interval, y axis the data for each of the compared columns on top of each other 
          par(mfrow=c(1,1))
          for (k in 1:length(compcols)){
               plot(X$Interval,as.numeric(Xc[,k]),col='blue',main=compcols[k],xlab='Interval')
               par(new=TRUE)
               plot(Y$Interval,as.numeric(Yc[,k]),col='green',main=compcols[k],xlab='Interval')
               legend(x="topleft",legend =c('First dataset','Second dataset'),col=c("blue", "green"),pch=1, cex=0.8) 
               filename <- file.path(imagedirin,paste0("Overlain ", compcols[k]," for survey ",  X$Survey[1], ".png"))
               dev.copy(png, filename, width = 1000)
               dev.off()
          }
     }

     
     # Make these plots regardless
     # Plot sum of transect NASC
     par(mfrow=c(1,1))
     for (k in 1:length(compcols)){
          # decided to switch to ggplot
          filename <- file.path(imagedirin,paste0("Overlain transect summed ", compcols[k]," for ", year , " survey.png"))
           p<-ggplot2::ggplot(data=XYc_SumbyT_c,aes(x=Transect,y=!!sym(compcols[k])), group=dataset)+ #https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
                labs(title=compcols[k], x='Transect',y='Sum')+
                geom_point(aes(col=dataset))+
                theme_bw()
                print(p)
           ggplot2::ggsave(filename,dpi=500)  #could maybe use saveRDS instead if wanted to be loaded just into markdown later.
     }
     ## plot summed NASC
     par(mfrow=c(1,1))
     ss=c(sum(X$PRC_NASC),sum(Y$PRC_NASC))
     barplot(ss, main="Summed NASC")
     lbs=paste(c("difference:", round(diff(ss),2), ",proportion:",round(diff(ss)/ss[1],3)), collapse=" ")
     text(1,ss[1]/2,lbs)
     filename <- file.path(imagedirin,paste0("Summed NASC for ",  year, " survey.png"))
     dev.copy(png, filename, width = 1000)
     dev.off()
     
     ## Make table of NASC and differences
     case1_NASC<-ss[1]
     case2_NASC<-ss[2]
     Per_diff=round(diff(ss)*100/case1_NASC,5)  #round to 5 decimal places
     sstable<-data.frame(year,case1_NASC,case2_NASC,Per_diff)
     ftNASC<-flextable::flextable(sstable)
     ftNASC<-flextable::set_header_labels(ftNASC,case1_NASC=case1,case2_NASC=case2, Per_diff="Percent Difference")
     saveRDS(ftNASC,file.path(imagedirin,paste0(year,"_export_NASC_table")))
     
     ## Make table of transects used for each case
     XTu<-unique(X$Transect)
     YTu<-unique(Y$Transect)
     notX<-setdiff(XTu,YTu)
     notY<-setdiff(YTu,XTu)
     
     return(list(L,RC,sstable,notX,notY,RCind,VLdiffs1,VLdiffs2))  
     #return(list(L,RC,sstable,notX,notY,RCind))
}
