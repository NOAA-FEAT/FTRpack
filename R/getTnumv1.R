#' getTnumv2.R
#'
#' @description Function to pull out the transect number from the standard exported
#' csv convention of V###-S#####-X#-F38-T##-Z0- ( ).csv where the number after the V is the
#' vessel, after the S is the survey number, after the X is the transducer, after the F
#' is the frequency (generally 38 kHz), and after the T is the transect number
#'
#' @param A: string that is the name of the EV file
#' @examples
#'  \dontrun{
#' Tr<-getTnumv1("V499-S200952-X1-F38-T98-Z0- (analysis).csv")
#'}
#' @export

getTnumv1<-function(A){
     Transect <- sapply(strsplit(A,"-", fixed = TRUE),`[`,5) #https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r
     Transect<-as.numeric(gsub(".*?([0-9]+).*", "\\1", Transect))  #Make into the transect number itself (e.g. 1 instead of x1)
     return(Transect)
}
