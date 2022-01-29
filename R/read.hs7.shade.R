#' Read Heat Source 7 effective shade outputs.
#'
#' Read xlsm effective shade output from Heat Source version 7.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do
#' not seem to work.
#'
#' Data is returned in wide format with the model stream km used as the column
#' name for all values. An "X" is added as a prefix to every stream km value to
#' have syntactically valid column names. The datetime is the first column and
#' is formatted in excel numeric date format.
#'
#' This function is setup to read the "Chart-Shade" worksheet.
#'
#' @param output_dir The path to the directory where the Heat Source .xlsm is located.
#' @param file_name The file name of the .xlsm heat source model.
#' The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is "Chart-Shade".
#'
#' @keywords Heat Source version 7
#' @export
#' @return data frame
#'

read.hs7.shade <- function(output_dir, file_name, sheet_name = "Chart-Shade") {

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = sheet_name, skip = 12,
                                   na = c("","N/A", " "),
                                   col_names = c("model_km", "datetime", "value"),
                                   col_types = c("numeric","numeric","numeric"))

  excel.data$datetime <- as.numeric(excel.data$datetime)

  excel.data$datetime <- lubridate::round_date(as.POSIXct((excel.data$datetime * 60 * 60 * 24),
                                                          origin = "1899-12-30", tz = "UTC"),
                                               unit = "minute")

  # Remove NA rows if there are spaces between each date
  excel.data <- excel.data[complete.cases(excel.data),]

  excel.data <- excel.data[, c("datetime", "model_km", "value")]

  # convert to wide format
  excel.data <- tidyr::pivot_wider(excel.data, values_from = value,
                                   names_from = model_km, names_prefix = "X") %>%
    dplyr::select(datetime, dplyr::everything())


  return(excel.data)

}
