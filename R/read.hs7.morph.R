#' Read Heat Source 7 morphology.

#' Read inputs from the morphology worksheet in a Heat Source version 7 model. 
#' The Heat Source excel workbook needs to be saved as .xlsm. Workbooks in 
#' .xls do not seem to work.
#' 
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model
#'        is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'        The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is
#'        'Morphology Data'.
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe

read.hs7.morph <- function(output_dir, file_name, 
                           sheet_name = "Morphology Data") {

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name), 
                           sheet = sheet_name, na = c("","N/A", " "),
                           range = cell_cols("C:T"),
                           col_names = FALSE)
  
  excel.data <- excel.data[c(16:nrow(excel.data)),c(1:3,5:18)]
  
  colnames(excel.data) <- c("stream_node", "long_distance", "model_km", 
                            "gradient", "mannings_n","rosgen", "wd_ratio", 
                            "bankfull_width", "bottom_width", "max_depth",
                            "mean_depth","Xsec_Area","angle_z","x_factor",
                            "bed_conductivity","particle_size","embeddedness")
  
  return(excel.data)
  
}