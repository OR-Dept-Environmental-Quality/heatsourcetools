#' Read Heat Source 7 outputs.
#'
#' Read hourly xlsm output from Heat Source version 7.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do not work.
#'
#' Data is returned in wide format with the stream km used as the column name for all values.
#' An "X" is added as a prefix to every stream km value to have syntactically valid
#' column names. The datetime is the first column and is formatted in excel
#' numeric date format.
#'
#' This function reads the following output sheets:
#'  \itemize{
#'  \item Output - Solar Potential
#'  \item Output - Solar Surface
#'  \item Output - Solar Received
#'  \item Output - Conduction
#'  \item Output - Longwave
#'  \item Output - Convection
#'  \item Output - Evaporation
#'  \item Output - Total Heat
#'  \item Output - Evaporation Rate
#'  \item Output - Temperature
#' }
#' To read heat source 7 effective shade outputs use \code{\link{read.hs7.shade}}.
#'
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'  The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read.
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe
#'

read.hs7.outputs <- function(output_dir, file_name, sheet_name) {

  #output_dir <- "/Users/rmichie/Desktop/heatsource/Jenny_Creek/hs7"
  #file_name <- "HS7.Jenny.Crk.CCCdx200.xlsm"
  #sheet_name <- "Output - Temperature"

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = sheet_name, skip = 14,
                                   na = c("","N/A", " "))

  stream_kms <- excel.data[[4]]
  hs7.data <- data.frame(t(excel.data[,c(5:ncol(excel.data))]))
  colnames(hs7.data) <- paste0("X", stream_kms)

  hs7.data$datetime <- as.numeric(rownames(hs7.data))
  rownames(hs7.data) <- NULL

  hs7.data <- dplyr::select(hs7.data, datetime, dplyr::everything())

  return(hs7.data)
}
