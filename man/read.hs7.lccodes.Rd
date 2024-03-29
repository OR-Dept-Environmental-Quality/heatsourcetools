% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.hs7.lccodes.R
\name{read.hs7.lccodes}
\alias{read.hs7.lccodes}
\title{Read Heat Source 7 land cover codes.
Read inputs from the land cover code worksheet in a Heat Source version 7 model.
The Heat Source excel workbook needs to be saved as .xlsm. Workbooks in
.xls do not seem to work.}
\usage{
read.hs7.lccodes(output_dir, file_name, sheet_name = "Land Cover Codes")
}
\arguments{
\item{output_dir}{The path to directory where the Heat Source 7 .xlsm model
is located.}

\item{file_name}{The file name of the .xlsm Heat Source 7 model.
The ".xlsm" extension should be included in the file name.}

\item{sheet_name}{The name of the output worksheet to read. Default is
'Land Cover Codes'.}
}
\value{
dataframe
}
\description{
Read Heat Source 7 land cover codes.
Read inputs from the land cover code worksheet in a Heat Source version 7 model.
The Heat Source excel workbook needs to be saved as .xlsm. Workbooks in
.xls do not seem to work.
}
\keyword{7}
\keyword{Heat}
\keyword{Source}
\keyword{version}
