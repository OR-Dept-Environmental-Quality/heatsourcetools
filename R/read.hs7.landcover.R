#' Read Heat Source 7 land cover info.

#' Read landcover inputs from the TTools worksheet in a Heat Source version 7 
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
#' @keywords Heat Source version 7
#' @export
#' @return dataframe

read.hs7.landcover <- function(output_dir, file_name, 
                               sheet_name = "TTools Data") {

  lc.data <- readxl::read_excel(path = file.path(output_dir, file_name), 
                                sheet=sheet_name, 
                                na = c("","N/A", " "),
                                range=cell_cols("C:FE"),
                                col_names = FALSE)
  
  # This keeps everything and only removes unused rows and cols
  #lc.data <- lc.data[c(16:nrow(lc.data)),c(1:124,126:133,135:142,144:145,147:148,150:156,158:159)]
  
  lc.data <- lc.data[c(16:nrow(lc.data)), c(1:40,144:145,147:148)]
  
  colnames(lc.data) <- c("stream_node", "long_distance", "model_km", "lat", 
                         "long", "elevation", "width", "aspect", 
                         "topo_w", "topo_s", "topo_e", "LC_T0_S0", 
                         paste0("LC_T", rep(1:7, each = 4), "_S", 1:4),
                         "height_l", "height_r", "density_l", "density_r")
  
  # only keep relevant rows for plotting, removes LC codes, elevations, etc
  #lc.data <- lc.data[c(16:nrow(lc.data)),c(1:11,144:145,147:148)]
  
  #colnames(lc.data) <- c("stream_node","long_distance", "model_km", "lat", "long", 
  #                       "elevation", "width", "aspect", "topo_w", "topo_s", "topo_e",
  #                       "height_l", "height_r", "density_l", "density_r")

  return(lc.data)
  
}