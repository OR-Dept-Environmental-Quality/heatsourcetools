#' Read Heat Source 7 outputs.
#'
#' Read hourly xlsm output from Heat Source version 7.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do not work.
#'
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'  The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read.
#' @param ... Other arguments passed on to readxl::read_excel().
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe
#'

read.hs7.outputs <- function(output_dir, file_name, sheet_name) {

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name), 
                                   sheet = sheet_name, skip=14, na = c("","N/A", " "))

  new_cols <- excel.data[[4]]
  hs7.data <- data.frame(t(excel.data[,c(5:ncol(excel.data))]))
  colnames(hs7.data) <- new_cols

  hs7.data$Datetime <- as.numeric(rownames(hs7.data))
  rownames(hs7.data) <-NULL

  return(hs7.data)
}
