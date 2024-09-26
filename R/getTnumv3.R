#' getTnumv3.R
#'
#' @description Function to pull out the transect number form the name of an EV by using the
#'  number after the last '/' in the filename.  Run if there is no "x" in the filename
#'
#' @param A: string that is the name of the EV file
#' @examples
#'  \dontrun{
#' Tr<-getTnumv3("C:\\rthomas\\Projects\\EV_compilation\\Local\\2003\\EV files\\age1\\original\\CAN/03gwf.ev")
#'}
#' @export

getTnumv3<-function(A){
     Transect <- sapply(strsplit(A,"/", fixed = TRUE),`[`,2) #https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r
     Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
     return(Transect)
}
