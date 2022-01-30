#' Read Heat Source 7 land cover codes.

#' Read inputs from the land cover code worksheet in a Heat Source version 7 model. 
#' The Heat Source excel workbook needs to be saved as .xlsm. Workbooks in 
#' .xls do not seem to work.
#' 
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model
#'        is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'        The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is
#'        'Land Cover Codes'.
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe

read.hs7.lccodes <- function(output_dir, file_name, 
                             sheet_name = "Land Cover Codes") {
  
  lccode.data <- readxl::read_excel(path = file.path(output_dir, file_name), 
                                    sheet = sheet_name, 
                                    na = c("","N/A", " "),
                                    range=cell_cols("D:H"),
                                    col_names=c("landcover", "code", "height", "density", "overhang"),
                                    col_types =c("text", "text", "numeric", "numeric", "numeric"))
  
  lccode.data <- lccode.data[c(16:nrow(lccode.data)),c(1:5)]

  return(lccode.data)
  
}