#' Matches observation location to the nearest model location
#'
#' Returns a vector of model kilometers (or long distances) that are closest to the
#' observation kilometer or distance. If an observation id is equally distant
#' between two output kilometers the first one in the vector is used.
#'
#' @param obskm Numeric vector of the observation location (e.g. stream kilometer)
#' @param simkm Numeric vector of the model output locations (e.g. stream kilometer)
#' @export
#' @return vector equal to the length of obskm
#'

obs2km <- function(obskm, simkm) {

  # Returns a dataframe matching the observation site to the closest model
  # simulation kilometer.
  # simkm is a vector of unique model simulation kilometers

  match_km <- sapply(obskm, function(o, s) {s[which.min(abs(s-o))]}, s = simkm)
  return(match_km)
}
