#' Value matching based on closest value
#'
#' \code{match_near} is similar to the base function
#' \code{\link[base]{match}} except \code{match_near} returns a vector of
#' values or positions from \code{y} that are closest to the the vector of
#' values in \code{x}. If two values in vector\code{y} are equally close to the
#' value in \code{x} the first one is used.
#'
#' The closest match in \code{y} to a given value in \code{x} is determined
#' by the the value that has the minimum absolute difference between \code{x}
#' and \code{y}. e.g. \code{which.min(abs(x - y))}
#'
#' @param x Numeric vector of the values to be matched.
#' @param y Numeric vector of the values to be matched against
#' @param tolerance The maximum numeric difference between values in \code{x}
#'        and \code{y} to establish a match. If the difference exceeds the
#'        tolerance NA is returned for that match. The default tolerance is Inf
#'        which will return the closest value or position regardless of the
#'        absolute difference.
#' @param value If TRUE, return the value of \code{y} instead of the position.
#'        Default is TRUE.
#' @export
#' @return vector of values or positions from \code{y} that are closest to the
#' values in vector \code{x}.
#'
match_near <- function(x, y, tolerance = Inf, value = TRUE) {

  # test
  # x <- c(1, 5, 6, 22, 7)
  # y <- c(6, 7, 2, 10, 12, 2)
  # tolerance <- 3
  # value <- TRUE

  index_y <- sapply(X = x,
                    FUN = function(x, y, t) {ifelse(round(min(abs(x - y), na.rm = TRUE), 6) <= t,
                                                    which.min(abs(x - y)),
                                                    NA_real_)},
                    y = y, t = tolerance)

  if (value) {

    value_y <- y[index_y]
    return(value_y)

  } else {

    return(index_y)
  }
}
