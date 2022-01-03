#' Matches observation location to the nearest model location
#'
#' Returns a vector of model kilometers (or long distances) that are closest to
#' the observation kilometer or distance. If an observation id is equally
#' distant between two output model kilometers the first one in the vector is
#' used.
#'
#' @param obskm Numeric vector of the observation location (e.g. stream kilometer)
#' @param modkm Numeric vector of the model output distance (e.g. model kilometer)
#' @export
#' @return vector of modkm values equal to the length of obskm
#'

obs2km <- function(obskm, modkm) {

  # Returns a data frame matching the observation site to the closest model
  # simulation kilometer.
  # simkm is a vector of unique model simulation kilometers

  match_km <- sapply(obskm, function(o, s) {s[which.min(abs(s - o))]}, s = modkm)
  return(match_km)
}
