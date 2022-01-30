#' Read Heat Source 8 land cover info.

#' Read landcover inputs from the TTools worksheet in a Heat Source version 8 
#' model. The Heat Source excel workbook needs to be saved as .xlsm. Workbooks 
#' in .xls do not seem to work.
#' 
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model
#'        is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'        The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is
#'        'TTools Data'.
#'
#' @keywords Heat Source version 8
#' @export
#' @return dataframe

read.hs8.landcover <- function(output_dir, file_name, 
                               sheet_name = "TTools Data") {

  lc.data <- readxl::read_excel(path=file.path(output_dir, file_name), 
                                sheet=sheet_name, na = c("","N/A", " "),
                                range=cell_cols("B:BL"),
                                col_names = FALSE)
  
  lc.data <- lc.data[c(4:nrow(lc.data)), c(1:35)]
  
  colnames(lc.data) <- c("model_km", "long", "lat",
                         "topo_w", "topo_s", "topo_e",
                         "LC_T0_S0", paste0("LC_T", rep(1:7, each = 4), "_S", 1:4))
  
  return(lc.data)
  
}