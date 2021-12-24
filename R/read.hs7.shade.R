#' Read Heat Source 7 effective shade outputs.
#'
#' Read xlsm effective shade output from Heat Source version 7.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do not seem to work.
#'
#' @param output_dir The path to the directory where the Heat Source .xlsm is located.
#' @param file_name The file name of the .xlsm heat source model.
#' The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is "Chart-Shade".
#' @param ... Other arguments passed on to readxl::read_excel().
#'
#' @keywords Heat Source version 7
#' @export
#' @return data frame
#'

read.hs7.shade <- function(output_dir, file_name, sheet_name = "Chart-Shade") {

  
  #sim_name <- NULL
  #constituent_name <- "Effective Shade"

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name), 
                                   sheet = sheet_name, skip = 12, 
                                   na = c("","N/A", " "),
                                   col_names = c("Stream_km", "Datetime", "value"),
                                   col_types = c("numeric","numeric","numeric"))

  excel.data$Datetime <- as.numeric(excel.data$Datetime)

  excel.data$Datetime <-lubridate::round_date(as.POSIXct((excel.data$Datetime * 60 * 60 * 24), 
                                                         origin = "1899-12-30", tz="GMT"), 
                                              unit = "minute")
  
  # Remove NA rows if there are spaces between each date
  excel.data <- excel.data[complete.cases(excel.data),]
  
  excel.data <- excel.data[, c("Datetime", "Stream_km", "value")]

  return(excel.data)

}
