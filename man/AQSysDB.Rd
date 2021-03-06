% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AQSysDB.R
\name{AQSysDB}
\alias{AQSysDB}
\title{AQSysDB}
\usage{
AQSysDB(path)
}
\arguments{
\item{path}{String containing the full path to the XLS or XLSX file.}
}
\description{
Import DB data from an Excel Worksheet and process it through 
mathematical descriptors to output a highly structured variable comparable 
to a Database and which hold a list of references, chemicals and parameters
 for any implemented mathematical descriptors.
}
\examples{
\dontrun{
AQSysDB("C:/data.xlsx")
}
}
