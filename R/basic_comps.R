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

basic_comps<-function(X,Y,compcols,...){
     nL<-10
     L<-vector(length=nL)
     #Are data frames identical?
     L[1]<-identical(X,Y) #Are data frames identical?
     Xc<-dplyr::select(X,dplyr::all_of(compcols))
     Yc<-dplyr::select(Y,dplyr::all_of(compcols))
     #Are sums of compared columns identical?
     L[2]<-identical(colSums(Xc),colSums(Yc))
     #Are the (location and) values of the compared columns identical?
     L[3]<-identical(Xc,Yc)
     RC=which(X != Y, arr.ind=TRUE)
     ###
     #make plots
     ###
     #make plots of each of the compared columns in each dataset
     par(mfrow=c(length(compcols),1))
     for (k in 1:length(compcols)){
          plot(as.numeric(Xc[,k]),as.numeric(Yc[,k]),type='o',main=compcols[k],xlab='First data set',ylab='Second data set')
     }
     
     #plot x axis as interval, y axis the data for each of the compared columns on top of each other 
     par(mfrow=c(1,1))
     for (k in 1:length(compcols)){
          plot(X$Interval,as.numeric(Xc[,k]),col='blue',main=compcols[k],xlab='Interval')
          par(new=TRUE)
          plot(Y$Interval,as.numeric(Yc[,k]),col='green',main=compcols[k],xlab='Interval')
          legend(x="topleft",legend =c('First dataset','Second dataset'),col=c("blue", "green"),pch=1, cex=0.8) 
     }
     
     return(list(L,RC))
     
     
}
