% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.hs8.outputs.R
\name{read.hs8.outputs}
\alias{read.hs8.outputs}
\title{Read Heat Source 8 outputs.}
\usage{
read.hs8.outputs(output_dir, file_name)
}
\arguments{
\item{output_dir}{The path to directory where the Heat Source 8 output files are located.}

\item{file_name}{The file name of the Heat Source 8 output. The file is a .txt text file.}
}
\value{
data frame
}
\description{
Read hourly output from Heat Source version 8.
}
\details{
Data is returned in wide format with the model km used as the column name.
An "X" is added as a prefix to every model km value to have syntactically
valid column names. The datetime is the first column and is formatted in
excel numeric date format.

This function reads the following output files:
\itemize{
\item Heat_SR1
\item Heat_SR2
\item Heat_SR3
\item Heat_SR4
\item Heat_SR5
\item Shade
\item VTS
\item Hyd_DA
\item Hyd_DM
\item Hyd_Flow
\item Hyd_Hyp
\item Hyd_Vel
\item Hyd_WT
\item Heat_SR6
\item Heat_SR7
\item Heat_Cond
\item Heat_Long
\item Heat_Conv
\item Heat_Evap
\item Rate_Evap
\item SolarBlock
\item Temp_H2O
\item Temp_Sed
\item Hyd_Disp
}
}
\keyword{8}
\keyword{Heat}
\keyword{Source}
\keyword{version}
