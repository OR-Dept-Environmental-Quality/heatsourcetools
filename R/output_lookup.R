#' Return a vector of the constituent and units.
#'
#' This function will return the name of the output constituent and units based
#' on the input file name or sheet. The input file name or sheet name should be
#' the standard name used in heat source. If there is not a match NA will be
#' returned.
#'
#' @param name The name of the .txt, or .csv output file for Heat Source
#'        version 8-9; or excel sheet name for Heat Source 6-7. String format.
#' @param hs_ver The version of Heat Source. Input is a numeric value
#'        equal to 6, 7, 8, or 9. Default is 9.
#' @export
#' @return vector of values with the first element being the output constituent and the second the units.

output_lookup <- function(name=NA, hs_ver=9) {

  hs6_list <- list("Effective Shade Data" = c("Effective Shade", "Percent"),
                   "Long Temp Output" = c("Temperature, water", "deg C"))

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
                   "Output - Temperature" = c("Temperature, water", "deg C"))

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
                     "Hyd_Flow" = c("Flow Rate", "m3/sec"),
                     "Hyd_Hyp" = c("Hyporheic Exchange", "m3/sec"),
                     "Hyd_Vel" = c("Flow Velocity", "meters/second"),
                     "Hyd_WT" = c("Top Width", " meters"),
                     "Heat_SR6" = c("Solar Radiation Flux Received by Stream", "watts/square meter"),
                     "Heat_SR7" = c("Solar Radiation Flux Received by Substrate", "watts/square meter"),
                     "Heat_Cond" = c("Streambed Conduction Flux", "watts/square meter"),
                     "Heat_Long" = c("Longwave Flux", "watts/square meter"),
                     "Heat_Conv" = c("Convection Flux", "watts/square meter"),
                     "Heat_Evap" = c("Evaporation Flux", "watts/square meter"),
                     "Rate_Evap" = c("Evaporation Rate", "mm/hour"),
                     "Temp_H20" = c("Temperature, water", "deg C"),
                     "Temp_H2O" = c("Temperature, water", "deg C"),
                     "Temp_Sed" = c("Temperature, sediment", "deg C"),
                     "Hyd_Disp" = c("Hydraulic Dispersion", "square meters/second"))

  if (hs_ver == 6) {

    if (!name %in% names(hs6_list)) {
      warning(paste0("'",name, "' is not a valid Heat Source version 6 sheet name
                     or a sheet that can be read by this function. Returning NA"))

      return(c(NA_character_, NA_character_))

    } else {

      vals <- hs6_list[[name]]

      return(vals)

    }

  }



  if (hs_ver == 7) {

    if (!name %in% names(hs7_list)) {
      warning(paste0("'",name, "' is not a valid Heat Source version 7 sheet name
                  or a sheet that can be read by this function
                  (e.g. 'Output - Hydraulics' or  'Output - Daily Heat Flux'.
                  Returning NA"))

      return(c(NA_character_, NA_character_))

    } else {

      vals <- hs7_list[[name]]

      return(vals)

    }

  }


  if (hs_ver %in% c(8, 9)) {

  # remove the extension in case it was added
  base_name <- gsub("\\..*","", name)

  if (!base_name %in% names(hs8_9_list)) {
    warning(paste0("'", name, "' is not a valid Heat Source version 8-9 output
                   file name. Returning NA"))

    return(c(NA_character_, NA_character_))

  } else {

    vals <- hs8_9_list[[base_name]]

    return(vals)
    }
  }
}
