#' Calculate sediment hyporheic thickness
#'
#' Calculates sediment hyporheic thickness using heat source 7 channel 
#' morphology parameters. The calculation is the same as what is used in 
#' heat source 7. Heat source 7 bounds the thickness from 0.1 meters to 1 meter. 
#' This function is commonly  used to convert heat source 7 input parameters to 
#' heat source 8 or 9 sediment hyporheic thickness inputs.
#'
#' @param bed_particle_size Bed particle size, in millimeters.
#' @param min_thickness Minimum sediment hyporheic thickness, in meters. 
#' Default is 0.1 m, based on value used in Heat Source 7 code.
#' @param max_thickness Maximum sediment hyporheic thickness, in meters. 
#' Default is 1 m, based on value used in Heat Source 7 code.
#' @export
#' @return Sediment hyporheic thickness, in meters.
#' 
calc.hs7.sed.thickness <- function(bed_particle_size,
                                   min_thickness = 0.1,
                                   max_thickness = 1) {
  
  sed_hyporheic_thickness <- 10 * bed_particle_size / 1000
  
  sed_hyporheic_thickness <- pmin(sed_hyporheic_thickness, max_thickness)
  
  sed_hyporheic_thickness <- pmax(sed_hyporheic_thickness, min_thickness)
  
  return(sed_hyporheic_thickness)
}
