#' Return a vector of the con.
#'
#' This function will return the name of the output constituent and units based on the input file name or sheet.
#'
#' @param file_name The name of the .txt or .csv output file for Heat Source version 8-9.
#' @param hs_ver The version of Heat Source. Input is a numeric value equal to 7, 8, or 9. Default is 9.
#' @param sheet_name The name of the Heat Source 7 output worksheet. String format. NULL for Heat Source 8-9
#' the name of the .txt or .csv output file for Heat Source version 8-9.
#' For Heat Source 7, the ".xlsm" extension should be included in the file name.
#' @export
#' @return vector of values with the first element being the output constituent and the second the units.

output_lookup <- function(file_name=NULL, hs_ver=9, sheet_name=NULL) {
  
  hs7_list <- list("Output - Solar Potential" = c("Solar Radiation Flux above Topographic Features", "watts/square meter"),
                   "Output - Solar Surface" = c("Solar Radiation Flux above the Stream", "watts/square meter"),
                   "Output - Solar Received" = c("Solar Radiation Flux Entering Stream", "watts/square meter"),
                   "Chart-Shade" = c("Effective Shade", "Percent"),
                   "Output - Conduction" = c("Streambed Conduction Flux", "watts/square meter"),
                   "Output - Longwave" = c("Longwave Flux", "watts/square meter"),
                   "Output - Convection" = c("Convection Flux", "watts/square meter"),
                   "Output - Evaporation" = c("Evaporation Flux", "watts/square meter"),
                   "Output - Total Heat" = c("Total Flux", "watts/square meter"),
                   "Output - Evaporation Rate" = c("Evaporation Rate", "mm/hour"),
                   "Output - Temperature" = c("Stream Temperature", "Celsius"))
  
  hs8_9_list <- list("Heat_SR1" = c("Solar Radiation Flux above Topographic Features", "watts/square meter"),
                     "Heat_SR2" = c("Solar Radiation Flux below Topographic Features", "watts/square meter"),
                     "Heat_SR3" = c("Solar Radiation Flux below Land Cover", "watts/square meter"),
                     "Heat_SR3b" = c("Solar Radiation Flux blocked by Land Cover", "watts/square meter"),
                     "Heat_SR4" = c("Solar Radiation Flux above the Stream", "watts/square meter"),
                     "Heat_SR5" = c("Solar Radiation Flux Entering Stream", "watts/square meter"),
                     "Shade" = c("Effective Shade", "Percent"),
                     "VTS" = c("View to Sky", "Fraction of the sky hemisphere obscured by land cover"),
                     "Hyd_DA" = c("Average Depth", "meters"),
                     "Hyd_DM" = c("Max Depth", "meters"),
                     "Hyd_Flow" = c("Flow Rate", "cms"),
                     "Hyd_Hyp" = c("Hyporheic Exchange", "cms"),
                     "Hyd_Vel" = c("Flow Velocity", "meters/second"),
                     "Hyd_WT" = c("Top Width", " meters"),
                     "Heat_SR6" = c("Solar Radiation Flux Received by Stream", "watts/square meter"),
                     "Heat_SR7" = c("Solar Radiation Flux Received by Substrate", "watts/square meter"),
                     "Heat_Cond" = c("Streambed Conduction Flux", "watts/square meter"),
                     "Heat_Long" = c("Longwave Flux", "watts/square meter"),
                     "Heat_Conv" = c("Convection Flux", "watts/square meter"),
                     "Heat_Evap" = c("Evaporation Flux", "watts/square meter"),
                     "Rate_Evap" = c("Evaporation Rate", "mm/hour"),
                     "Temp_H20" = c("Stream Temperature", "Celsius"),
                     "Temp_H2O" = c("Stream Temperature", "Celsius"),
                     "Temp_Sed" = c("Sediment Temperature", "Celsius"),
                     "Hyd_Disp" = c("Hydraulic Dispersion", "square meters/second"))
  
  if(hs_ver==7) {
    
    if(!sheet_name %in% names(hs7_list)) {
      stop(paste0("'",sheet_name, "' is not a valid Heat Source version 7 sheet name or a sheet that can be read by this function (e.g. 'Output - Hydraulics' or  'Output - Daily Heat Flux'"))
      
    } else {
      
      vals <- hs7_list[[sheet_name]]
      
      return(vals)
      
    }
    
  }
  

  if(hs_ver %in% c(8,9)) {
  
  # remove the extension in case it was added
  base_name <- gsub("\\..*","", file_name)
  
  if(!base_name %in% names(hs8_9_list)) {
    stop(paste0("'",file_name, "' is not a valid Heat Source version 8-9 output file name."))
  } else {
    
    vals <- hs8_9_list[[base_name]]
    
    return(vals)
    }
  }
}