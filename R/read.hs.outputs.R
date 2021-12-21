#' Read hourly Heat Source 7-8 outputs.
#'
#' Read hourly output from Heat Source version 7-9.
#' Heat Source 7 excel workbook needs to be saved as .xlsm. Workbooks in .xls do not work.
#'
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model or
#' the name of the .txt or .csv output file for Heat Source version 8-9.
#' For Heat Source 7, the ".xlsm" extension should be included in the file name.
#' @param hs_ver The version of Heat Source. Input is a numeric value equal to 7, 8, or 9. Default is 9.
#' @param sheet_name The name of the Heat Source 7 output worksheet to read. String format. NULL for Heat Source 8-9. Default is NULL.
#' @param constituent_name The name of the output. String format. Default is NA. If NA, the function will attempt to look up the constituent using [lookup_output].
#' @param sim_name The name of the model scenario. String format. Default is NA.
#' @keywords Heat Source version 7
#' @export
#' @return data frame
#'

read.hs.outputs <- function(output_dir, file_name, hs_ver = 9, 
                            sheet_name = NULL, constituent_name = NA, 
                            sim_name = NA) {
  # Reads any output from heat source version 7-9. Does some formatting, and returns the data
  # as a dataframe in long format.
  # Simulation name, constituent, statistic are strings
  # hours are added as ID variables.
  # sheet_name is only used for heat source 7

  # Assign the correct read function based on model version
  if (as.integer(hs_ver) == 7) {
    
    if(grepl("shade", sheet_name, ignore.case = TRUE)) {
      # if shade we have to use a special function because the formatting of that sheet is different
      
      data.raw <- heatsourcetools::read.hs7.shade(output_dir, 
                                                  file_name, 
                                                  sheet_name = "Chart-Shade")
      
      data.raw$Date <- format(data.raw$Datetime,"%m/%d/%Y")
      
      data.raw$sim <- sim_name
      data.raw$constituent <- ifelse(is.na(constituent_name),"Effective Shade", constituent_name)
      
      data.raw <- data.raw[, c("sim", "constituent", "Datetime", "Date", "Stream_km", "value")]
      
      return(data.raw)
      
    } else {
      
      data.raw <- heatsourcetools::read.hs7.outputs(output_dir, file_name, sheet_name)
    }
  }

  if (as.integer(hs_ver) == 8) {
    data.raw <- heatsourcetools::read.hs8.outputs(output_dir, file_name)
  }

  if (as.integer(hs_ver) == 9) {
    data.raw <- heatsourcetools::read.hs9.outputs(output_dir, file_name)
  }
  
  data.final <- heatsourcetools::format.outputs(df = dat.raw,hs_ver=hs_ver, 
                                                sheet_name = sheet_name,
                                                constituent_name = constituent_name, 
                                                sim_name = sim_name) 

  return(data.final)

}
