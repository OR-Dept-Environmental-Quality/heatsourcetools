#' Calculate sediment thermal diffusivity
#'
#' Calculates sediment thermal diffusivity using heat source 7 channel 
#' morphology parameters. The calculation is the same as what is used in 
#' heat source 7. This function is commonly used to convert 
#' heat source 7 input parameters to heat source 8 or 9 sediment thermal 
#' diffusivity inputs.
#'
#' @param bed_particle_size_mm Bed particle size, in millimeters.
#' @param embeddedness Embeddedness, as a decimal fraction from 0 to 1.
#' @param fine_particle_size_mm Fine sediment particle size, in millimeters. 
#' Default is 0.062 mm, based on value used in Heat Source 7 code.
#' @param sediment_thermal_diffusivity_m2_s Sediment thermal diffusivity, 
#' in square meters per second. Default is 0.0000045 m^2/s, based on value used 
#' in Heat Source 7 code.
#' @param water_thermal_diffusivity_m2_s Water thermal diffusivity, in square 
#' meters per second. Default is 0.00000014331 m^2/s, 
#' based on value used in Heat Source 7 code.
#' @export
#' @return Sediment thermal diffusivity, in square centimeters per second.

calc.hs7.sed.diffusivity <- function(bed_particle_size_mm,
                                 embeddedness,
                                 fine_particle_size_mm = 0.062,
                                 sediment_thermal_diffusivity_m2_s = 0.0000045,
                                 water_thermal_diffusivity_m2_s = 0.00000014331) {
  
  porosity <- 0.3683 * ((bed_particle_size_mm * (1 - embeddedness)) + 
                          (fine_particle_size_mm * embeddedness)) ^ (-0.0641)
  
  thermal_diffusivity_m2_s <- (sediment_thermal_diffusivity_m2_s * 
                                 (1 - porosity)) + 
    (water_thermal_diffusivity_m2_s * porosity)
  
  sed_thermal_diffusivity <- thermal_diffusivity_m2_s * 10000
  
  return(sed_thermal_diffusivity)
}
