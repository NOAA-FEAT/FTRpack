#' getTnumv2.R
#'
#' @description Function to pull out the transect number form the name of an EV by using the 
#'  number after the 'x'
#'
#' @param A: string that is the name of the EV file
#' @examples
#'  \dontrun{
#' Tr<-getTnumv2("x040hake69_raw.EV")
#'}
#' @export

getTnumv2<-function(A){
     Transect <- sapply(strsplit(A,"/x", fixed = TRUE),`[`,2) #https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r
     Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
     return(Transect)
}
