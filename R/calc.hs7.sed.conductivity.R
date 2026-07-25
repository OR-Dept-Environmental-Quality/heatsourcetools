#' Calculate sediment thermal conductivity
#'
#' Calculates sediment thermal conductivity using heat source 7 channel 
#' morphology parameters. The calculation is the same as what is used in 
#' heat source 7. This function is commonly used to convert 
#' heat source 7 input parameters to heat source 8 or 9 sediment thermal 
#' conductivity inputs.
#'
#' @param bed_particle_size_mm Bed particle size, in millimeters.
#' @param embeddedness Embeddedness, as a decimal fraction from 0 to 1.
#' @param fine_particle_size_mm Fine sediment particle size, in millimeters. 
#' Default is 0.062 mm, based on value used in Heat Source 7 code.
#' @param sediment_density Sediment density, in kilograms per cubic meter. 
#' Default is 1600 kg/m^3, based on value used in Heat Source 7 code.
#' @param water_density Water density, in kilograms per cubic meter. 
#' Default is 1000 kg/m^3, based on value used in Heat Source 7 code.
#' @param sediment_heat_capacity Sediment specific heat capacity, in joules 
#' per kilogram per degree Celsius. Default is 2219 J/(kg deg C),
#'  based on value used in Heat Source 7 code.
#' @param water_heat_capacity Water specific heat capacity, in joules per 
#' kilogram per degree Celsius. Default is 4187 J/(kg deg C), based on value 
#' used in Heat Source 7 code.
#' @param sediment_thermal_diffusivity_m2_s Sediment thermal diffusivity, 
#' in square meters per second. Default is 0.0000045 m^2/s, based on value 
#' used in Heat Source 7 code.
#' @param water_thermal_diffusivity_m2_s Water thermal diffusivity, 
#' in square meters per second. Default is 0.00000014331 m^2/s, based on value 
#' used in Heat Source 7 code.
#' @export
#' @return Sediment thermal conductivity, in watts per meter per degree Celsius.
#' 

calc.hs7.sed.conductivity <- function(bed_particle_size_mm,
                                  embeddedness,
                                  fine_particle_size_mm = 0.062,
                                  sediment_density = 1600,
                                  water_density = 1000,
                                  sediment_heat_capacity = 2219,
                                  water_heat_capacity = 4187,
                                  sediment_thermal_diffusivity_m2_s = 0.0000045,
                                  water_thermal_diffusivity_m2_s = 0.00000014331) {
  
  porosity <- 0.3683 * ((bed_particle_size_mm * (1 - embeddedness)) + 
                          (fine_particle_size_mm * embeddedness)) ^ (-0.0641)

  density <- (sediment_density * (1 - porosity)) +(water_density * porosity)

  heat_capacity <- (sediment_heat_capacity * (1 - porosity)) + 
    (water_heat_capacity * porosity)

  thermal_diffusivity <- (sediment_thermal_diffusivity_m2_s * (1 - porosity)) + 
    (water_thermal_diffusivity_m2_s * porosity)

  sed_thermal_conductivity <- density * heat_capacity * thermal_diffusivity
  
  return(sed_thermal_conductivity)
}
