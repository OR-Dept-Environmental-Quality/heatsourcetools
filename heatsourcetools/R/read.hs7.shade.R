#' Read Heat Source 7 effective shade outputs.
#'
#' Read xlsx effective shade output from Heat Source version 7.
#' Heat Source excel workbook needs to be saved as .xlsx. Workbooks in .xls do not seem to work.
#'
#' @param output_dir The path to directory where the Heat Source .xlsx or output files are located.
#' @param file_name The file name of the .xlsx heat source model.
#' The ".xlsx" extension shoudl be included in the file name.
#' @param sheet_name The name of the output worksheet to read.
#' @param ... Other arguments passed on to readxl::read_excel().
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe
#'

read.hs7.shade <- function(output_dir, file_name, sim_name, constituent_name="Effective Shade",
                           statistic_name="Percent", sheet_name="Chart-Shade") {
  # Function to read effective shade output from heat source 7. Returns the data
  # as a dataframe. Excel workbook needs to be saved as .xlsx. .xls do not seem to work.

  excel.data <- readxl::read_excel(path=paste0(output_dir,"/",file_name), sheet=sheet_name, skip=12, na = c("","N/A", " "),
                           col_names=c("Stream_km", "Datetime", "value"),
                           col_types =c("numeric","numeric","numeric"))

  excel.data$constituent <- constituent_name
  excel.data$statistic <- statistic_name
  excel.data$sim <- sim_name

  excel.data$Datetime <- as.numeric(excel.data$Datetime)

  excel.data$Datetime <-lubridate::round_date(as.POSIXct((excel.data$Datetime*60*60*24), origin="1899-12-30", tz="GMT"), unit = "minute")

  excel.data$Date <- format(excel.data$Datetime,"%m/%d/%Y")

  excel.data$hour <-as.integer(format(excel.data$Datetime, "%H"))

  return(excel.data)

}
