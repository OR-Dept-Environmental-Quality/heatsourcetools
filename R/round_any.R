#' Round to multiple of any number
#'
#' Same function from plyr.
#'
#' @param x Numeric vector of the values to be rounded
#' @param accuracy number to round to
#' @param f Rounding function. One of: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @export
#' @examples
#' v <- c(1.22, 2.45, 4.12)
#' round_any(min(v, na.rm = TRUE), accuracy = 0.5 , f = floor)
#' round_any(max(v, na.rm = TRUE), accuracy = 0.5 , f = ceiling)


round_any <- function(x, accuracy, f = round) {
  
  f(x / accuracy) * accuracy
  }