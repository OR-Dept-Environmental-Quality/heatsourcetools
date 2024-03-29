% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_7dadm.R
\name{calc_7dadm}
\alias{calc_7dadm}
\title{Calculate the 7DADM from hourly temperature data.}
\usage{
calc_7dadm(
  df,
  datetime_col = "datetime",
  sim_col = "sim",
  dis_col = "model_km",
  value_col = "value"
)
}
\arguments{
\item{df}{The input data frame. df must have the following columns:
\itemize{
\item datetime (with datetime as POSIXct),
\item sim (with simulation name in character format),
\item model_km (as numeric),
\item value (numeric hourly stream temperature)
}

\if{html}{\out{<div class="sourceCode">}}\preformatted{  These can be generated using the \code{\link{format_outputs}} function.
}\if{html}{\out{</div>}}}

\item{datetime_col}{datetime column name. Default is 'datetime'.}

\item{sim_col}{Simulation column name. Default is 'sim'.}

\item{dis_col}{Distance column name. Default is 'model_km'.}

\item{value_col}{Value column name. Default is 'value'.}
}
\value{
data frame with columns 'sim', 'datetime', 'date', value of \code{dis_col}, 'constituent', and 'value'.
}
\description{
Calculate the daily maximum and rolling seven day average of the daily maximums (7DADM) by date from hourly data.
The rolling average result is right aligned and indexed to the last day of the seven day period.
'Daily Maximum Temperature' and '7DADM Temperature' are values added to the
constituent column.
}
