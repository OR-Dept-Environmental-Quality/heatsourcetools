#' Read hourly Heat Source 7-8 outputs.
#'
#' Read hourly output from Heat Source version 7-9.
#' Heat Source 7 excel workbook needs to be saved as .xlsx. Workbooks in .xls do not work.
#'
#' @param output_dir The path to directory where the Heat Source 7 .xlsx model is located.
#' @param file_name The file name of the .xlsx Heat Source 7 model or
#' the name of the .txt or .csv output file for Heat Soruce version 8-9.
#' For Heat Soruce 7, the ".xlsx" extension should be included in the file name.
#' @param hs_ver The version of Heat Source. Input is a numeric value equal to 7, 8, or 9. Default is NULL.
#' @param sheet_name The name of the Heat Source 7 output worksheet to read. String format. NULL for Heat Soruce 8-9. Defualt is 9
#' @param constituent_name The name of the output. String format. Default is NULL
#' @param sim_name The name of the model scenerio. String format. Default is NULL
#' @keywords Heat Source version 7
#' @export
#' @return dataframe
#'

read.hs.outputs <- function(output_dir, file_name, hs_ver=9, sheet_name=NULL, constituent_name=NULL, statistic_name=NULL, sim_name=NULL) {
  # Reads any output from heat source version 7-9. Does some formatting, and returns the data
  # as a dataframe in long format.
  # Simulation name, constituent, statistic are strings
  # hours are added as ID variables.
  # sheet_name is only used for heat source 7

  library(reshape2)
  library(lubridate)

  # Assign the correct read function based on model version
  if (as.integer(hs_ver) == 7) {
    data.raw <- read.hs7.outputs(output_dir, file_name, sheet_name)
  }

  if (as.integer(hs_ver) == 8) {
    data.raw <- read.hs8.outputs(output_dir, file_name)
  }

  if (as.integer(hs_ver) == 9) {
    data.raw <- read.hs9.outputs(output_dir, file_name)
  }

  # read the data

  data.raw$constituent <- constituent_name
  data.raw$statistic <- statistic_name
  data.raw$sim <- sim_name

  # Convert data from wide to long
  data.l <- melt(data.raw, id.vars =c("Datetime","sim", "constituent","statistic"),variable.name=c("Stream_km"))
  colnames(data.l) <- c("Datetime","sim", "constituent","statistic","Stream_km","value")

  data.l$Stream_km <- as.numeric(gsub(pattern="X", replacement="",
                                      data.l$Stream_km, ignore.case = FALSE,fixed = FALSE))


  data.l$Datetime <-round_date(as.POSIXct((data.l$Datetime*60*60*24), origin="1899-12-30", tz="GMT"), unit = "minute")

  data.l$Date <- format(data.l$Datetime,"%m/%d/%Y")

  data.l$hour <-as.integer(format(data.l$Datetime, "%H"))

  return(data.l)

}
