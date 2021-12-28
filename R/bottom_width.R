#' Compute Bottom Width
#'
#' Same procedure as used in Heat Source 7. If the width:depth ratio and
#' channel angle z values produces bottom width < 0 z is incrementally decreased
#' using the formula: z_new = z * 0.99 until bottom width >= 0.
#'
#' @param bfw Vector of bankfull width or active channel widths.
#' @param wd Vector of width to depth ratio (W/D) values.
#' @param z Vector of channel angle Z values.
#' @export
#' @return vector of bottom widths

bottom_width <- function(bfw, wd, z) {

  # test
  # bfw <- 8.0
  # wd <- 4.20
  # z <-  1.0

  width_loop <- function(bfw, wd, z) {

    # Average depth
    depth_avg <- bfw / wd

    # cross sectional area
    xarea = bfw * depth_avg

    # depth estimate
    depth_est <- depth_avg

    bfw_est <- bfw

    repeat {
      z_est <- z
      bottom_wid <- bfw - 2 * z_est * depth_est
      A_Est <- (bottom_wid  + z_est * depth_est) * depth_est
      delta <- (xarea - A_Est)

      if (delta < 0.001 & bottom_wid >= 0) {break}

      depth_est <- depth_est + 0.01

      if (bottom_wid < 0) {
        z <- z * 0.99
        z_final <- round(z, digits = 2)
        depth_est <- 0.01
      }

      if (A_Est > xarea) {break}
    }

    bottom_wid_final  <- round(bottom_wid, digits = 2)

    return(bottom_wid_final)

  }

bottomWidth <- mapply(bfw, FUN = width_loop, wd = wd, z = z)

return(bottomWidth)

}
