% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.hs.outputs.R
\name{read.hs.outputs}
\alias{read.hs.outputs}
\title{Read hourly Heat Source 6-8 outputs.}
\usage{
read.hs.outputs(
  output_dir,
  file_name,
  hs_ver = 9,
  sheet_name = NA_character_,
  constituent_name = NA_character_,
  sim_name = NA_character_
)
}
\arguments{
\item{output_dir}{The path to directory where the output Heat Source file is
located.}

\item{file_name}{The file name of the .xlsm for Heat Source version 6-7 or
the name of the .txt or .csv output file for Heat Source version 8-9.
For Heat Source 6-7, the ".xlsm" extension should be included in the
file name.}

\item{hs_ver}{The version of Heat Source. Input is a numeric value equal to
6, 7, 8, or 9. Default is 9.}

\item{sheet_name}{The name of the Heat Source 6-7 output worksheet to read.
String format. NA for Heat Source 8-9. Default is NA. The
\code{constituent_name} must be identified for Heat Source 6-7 models
when reading the 'Main Menu' or 'Output - Hydraulics' worksheets
See details for more information.}

\item{constituent_name}{The name of the output. String format. Required for
Heat Source 6 and 7 models when reading Hydraulic outputs
(see details). Default is NA. If NA, the function will attempt to
look up the constituent using \code{\link{output_lookup}}.}

\item{sim_name}{The name of the model scenario. String format. Default is NA.}
}
\value{
data frame
}
\description{
Wrapper function to read output from Heat Source version 6-9.
Heat Source 6-7 excel workbook needs to be saved as .xlsm. Workbooks in .xls
do not work. This function calls \code{\link{format_outputs}} so the returned
data frame has the following columns:
\itemize{
\item sim: Character value of the model scenario simulation name or other description.
\item constituent: Character value of the constituent (e.g.
'Temperature, water').
\item datetime: POSIXct datetime.
\item date: Character date in format "mm/dd/YYYY".
\item model_km: numeric stream kilometer extracted from the model.
\item sample: land cover sample. Only returned if reading 'SolarBlock' or 'Heat_SR3b'.
\item value: Constituent value.
}
}
\details{
If you just want the raw import use the specific 'read.hs...' function suited
to the model version, e.g. read.hs9.outputs.

This function converts heat source 6 long distance to stream km with the
most downstream end = km 0.0

For Heat Source 6-7 models it is necessary to identify the
\code{constituent_name} when reading hydraulic outputs
(e.g. \code{sheet_name = 'Output - Hydraulics'} or
\code{sheet_name = 'Main Menu'} for hs7 and hs6 models respectively). This is
necessary because these sheets have multiple outputs and the
constituent name helps the function identify which output to read. In this
situation use one of the following for \code{constituent_name}:
\itemize{
\item 'Flow' aka Flow Rate
\item 'Flow Velocity'
}
}
\seealso{
\code{\link{read.hs6.temp}}, \code{\link{read.hs6.shade}},
\code{\link{read.hs6.flow}}, \code{\link{read.hs6.velocity}},
\code{\link{read.hs7.outputs}}, \code{\link{read.hs7.shade}},
\code{\link{read.hs7.flow}}, \code{\link{read.hs7.velocity}},
\code{\link{read.hs8.outputs}},
\code{\link{read.hs9.outputs}}
}
\keyword{Heat}
\keyword{Source}
