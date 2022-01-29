#' Left join based on the closest value
#'
#' Add rows from y to x based on the closest numeric value in a single common
#' column. This function is used to join based on matches of values or datetimes
#' within a numeric tolerance.
#'
#' @param x Data frame of to index against
#' @param y Data frame that will be joined to \code{x} based on closest
#'          value in x.
#' @param by The column name to join by. There can only be one column.
#'        If x and y have different column names,
#'        use the format \code{by = c("a" = "b")}. Default is "datetime".
#' @param tolerance The maximum numeric difference between values in \code{x}
#'        and \code{y} to establish a match. If the difference exceeds the
#'        tolerance NA is returned for that row in x. The default tolerance is Inf
#'        which will return the closest value or position regardless of the
#'        absolute difference.
#' @export

near_join <- function(x, y, by = "datetime", tolerance) {

  if (is.character(names(by))) {
    xcol <- names(by)
    ycol <- by[[1]]
  } else {
    xcol <- by
    ycol <- by
  }

  index_y <- match_near(x = x[,xcol], y = y[,ycol],
                        tolerance = tolerance, value = FALSE)

  x_join <- rbind(x, y[index_y,])

  return(x_join)
}
