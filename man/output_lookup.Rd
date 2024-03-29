% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_lookup.R
\name{output_lookup}
\alias{output_lookup}
\title{Return a vector of the constituent and units.}
\usage{
output_lookup(name = NA, hs_ver = 9)
}
\arguments{
\item{name}{The name of the .txt, or .csv output file for Heat Source
version 8-9; or excel sheet name for Heat Source 6-7. String format.}

\item{hs_ver}{The version of Heat Source. Input is a numeric value
equal to 6, 7, 8, or 9. Default is 9.}
}
\value{
vector of values with the first element being the output constituent and the second the units.
}
\description{
This function will return the name of the output constituent and units based
on the input file name or sheet. The input file name or sheet name should be
the standard name used in heat source. If there is not a match NA will be
returned.
}
