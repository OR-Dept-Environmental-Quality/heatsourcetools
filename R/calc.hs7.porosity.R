#' Calculate porosity
#'
#' Calculates porosity using heat source 7 channel morphology parameters. 
#' The porosity equation is adopted from Bedient and Huber 1992, as described 
#' in the heat source 7 user manual. This function is commonly used to convert 
#' heat source 7 input parameters to heat source 8 or 9 porosity inputs.
#'
#' @param bed_particle_size Bed particle size, in millimeters.
#' @param embeddedness Embeddedness, as a decimal fraction from 0 to 1.
#' @param fine_particle_size Fine sediment particle size, in millimeters. Default
#'  is 0.062 mm, based on value used in Heat Source 7 code.
#' @export
#' @return Porosity, as a decimal fraction from 0 to 1.
#' 
calc.hs7.porosity <- function(bed_particle_size, 
                              embeddedness, 
                              fine_particle_size = 0.062) {
  
  porosity <- 0.3683 * ((bed_particle_size * 
                           (1 - embeddedness)) + 
                          (fine_particle_size * embeddedness)) ^ (-0.0641)
  
  return(porosity)
}
