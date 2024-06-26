% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Read_Reports_cross.R
\name{Read_Reports_cross}
\alias{Read_Reports_cross}
\title{Read_Reports_cross}
\usage{
Read_Reports_cross(repcases, year, projpathbase, estimflag = 4, hakeflag = 2)
}
\arguments{
\item{repcases:}{a list of cases (will correspond to subfolder names) of reports to read in.}

\item{year:}{the year (used as a subfolder)}

\item{reportpath:}{the base path that the script will look for the (year then) case subfolders in}

\item{hakeflag:}{adult hake (hakeflag=2), not filtering for hake (hakeflag=-1), 
age-0 hake (hakeflag=0), age-1 hake only(hakeflag=1), all hake (hakeflag=3), age-1+ (hakeflag=4).  
set to 1 if want age1+ numbers, set to 0 (or other number) if want age2+.  Defaults to age2+}
}
\description{
helper function to read in biomass at age reports from EchoPro. This version
is different from Read_Reports because it allows you to read in an age-2+ estimate from
an age-1+ estimation process
}
\examples{
 \dontrun{
Blist<-Read_Reports(repcases, year, reportpath)
}
}
