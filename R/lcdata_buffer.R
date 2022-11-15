#' Make a buffer width scenario.
#' 
#' This function assists with preparing heat source land cover data input to evaluate 
#' different buffer width scenarios.
#' 
#' The arguments \code{aspect}, \code{skygap}, \code{vegwd_L}, and \code{vegwd_R} can take a 
#' vector of multiple values. When this is the case all combinations of the 
#' supplied vectors are used to map the land cover codes. The returned data frame 
#' will have a row for each unique combination.
#' 
#' @param node_id Unique heat source node ID. Optional. Default is NA.
#' @param stream_id Unique stream identifier. Optional. Default is NA.
#' @param aspect The stream aspect in degrees. Can be a vector.
#' @param skygap The distance (meters) between the left and right bank vegetation buffers where there is open sky. 
#' This distance is about the same as the active channel width. Can take a vector of values as input.
#' @param vegwd_L The width of the vegetation buffer on the left bank (meters) looking downstream. Can take a vector of values as input.
#' @param vegwd_R The width of the vegetation buffer on the right bank (meters) looking downstream. Can take a vector of values as input.
#' @param lccode_L The numeric land cover code to use for samples that fall inside the vegetated buffer on the left bank. Do not use zero.
#' @param lccode_R The numeric land cover code to use for samples that fall inside the vegetated buffer on the right bank. Do not use zero.
#' @param lccode_noveg The numeric land cover code to use for samples that fall outside the vegetated buffer. Do not use zero.
#' @param lccode_stream The numeric land cover code to use for samples that fall on the stream. Do not use zero.
#' @param trans_dir A vector of integer values corresponding to the specific azimuth 
#'  directions of the sample transects. Default is Heat Source 9 directions = c(45, 90, 135, 180, 225, 270, 315, 360)
#' @param transsample_count Number of samples per transect. The number does not include the sample at the stream node.
#' @param transsample_distance The distance between transect samples (meters).
#' @export
#' @return dataframe


lcdata_buffer <- function(node_id = NA_real_, stream_id = NA_character_, 
                          aspect, skygap, vegwd_L, vegwd_R, 
                          lccode_L, lccode_R, lccode_noveg, lccode_stream,
                          trans_dir = c(45, 90, 135, 180, 225, 270, 315, 360), 
                          transsample_count, transsample_distance) {
  
  # test
  # stream_id <- "test"
  # node_id <- 55
  # aspect <- c(85)
  # skygap <- c(5)
  # vegwd_L <- c(10)
  # vegwd_R <- c(25)
  # lccode_L <- 11
  # lccode_R <- 12 
  # lccode_noveg <- 99
  # lccode_stream <- 301
  # trans_dir <- c(45, 90, 135, 180, 225, 270, 315, 360)
  # transsample_count <- 10
  # transsample_distance <- 2
  # test end
  
  # convert to numeric
  lccode_L <- as.numeric(lccode_L)
  lccode_R <- as.numeric(lccode_R)
  lccode_noveg <- as.numeric(lccode_noveg)
  lccode_stream <- as.numeric(lccode_stream)
  
  if (lccode_L == 0) {
    stop("lccode_L can not = 0")
  }
  
  if (lccode_R == 0) {
    stop("lccode_R can not = 0")
  }
  
  if (lccode_noveg == 0) {
    stop("lccode_noveg can not = 0")
  }
  
  if (lccode_stream == 0) {
    stop("lccode_stream can not = 0")
  }
  
  # We do the math relative to zero node location
  node_x <- 0
  node_y <- 0
  
  trans_count <- length(trans_dir)
  
  stmcombos <- expand.grid(aspect, skygap, vegwd_L, vegwd_R, stringsAsFactors = FALSE)
  colnames(stmcombos)[1] <- "aspect"
  colnames(stmcombos)[2] <- "skygap"
  colnames(stmcombos)[3] <- "vegwd_L"
  colnames(stmcombos)[4] <- "vegwd_R"
  
  # Build the x/y points for the vegetation samples
  transect <- seq(1, trans_count)
  sample <- seq(1, transsample_count)
  
  # make a matrix of transect directions numbers for each sample
  dummy1 <- expand.grid(sample, transect, stringsAsFactors = FALSE)
  colnames(dummy1)[1] <- "sample"
  colnames(dummy1)[2] <- "transect"
  dummy1$sample <- NULL
  
  # make a matrix of transect angles for each sample
  dummy2 <- expand.grid(sample, trans_dir, stringsAsFactors = FALSE)
  colnames(dummy2)[1] <- "sample"
  colnames(dummy2)[2] <- "trans_dir"

  # make a matrix of transect directions angles by sample
  node_samples <- cbind(dummy1, dummy2) %>%
    dplyr::mutate(lc_key = paste0("LC_T", transect, "_S", sample),
                  vegsam_x = sample * transsample_distance * sin(trans_dir * pi/180) + node_x,
                  vegsam_y = sample * transsample_distance * cos(trans_dir * pi/180) + node_y)
  
  rm(dummy1, dummy2)
  
  ## Construct Stream polygon
  rec <- expand.grid(seq(1, 4), stmcombos$aspect, stringsAsFactors = FALSE)
  colnames(rec)[1] <- "vertex"
  colnames(rec)[2] <- "aspect"
  
  line1 <- c(0, 180, 180, 0)
  line2 <- c(-90, -90, 90, 90)
  
  bufferL_line <- c(stmcombos$vegwd_L, stmcombos$vegwd_L, stmcombos$skygap, stmcombos$skygap)
  bufferR_line <- c(stmcombos$skygap, stmcombos$skygap, stmcombos$vegwd_R, stmcombos$vegwd_R)
  
  rec <- cbind(rec, line1, line2, bufferL_line, bufferR_line)
  rm(line1, line2, bufferL_line, bufferR_line)
    
  # calculate x/y of stream vertex points
  rec <- rec %>%
     dplyr::mutate(stream_x = ((max(transsample_count) * transsample_distance * sin((aspect - line1) * pi/180)) + node_x) + ((skygap/2) * sin((aspect + line2) * pi/180)),
                   stream_y = ((max(transsample_count) * transsample_distance * cos((aspect - line1) * pi/180)) + node_y) + ((skygap/2) * cos((aspect + line2) * pi/180)))

  # Construct R/L buffer
  rec <- rec %>%
    dplyr::mutate(bufferL_x = (bufferL_line * sin((aspect - 90) * pi/180)) + stream_x,
                  bufferL_y = (bufferL_line * cos((aspect - 90) * pi/180)) + stream_y,
                  bufferR_x = (bufferR_line * sin((aspect + 90) * pi/180)) + stream_x,
                  bufferR_y = (bufferR_line * cos((aspect + 90) * pi/180)) + stream_y)
  
  # determine if sample points are in inside L/R buffer and stream
  # 0, 2, 3 = outside buffer
  # 1 = inside buffer
  # 2 and 3 = on the line. For the stream polygon 2 and 3 = inside
  bufferL_in <- sp::point.in.polygon(point.x = node_samples$vegsam_x, 
                                     point.y = node_samples$vegsam_y,
                                     pol.x = rec$bufferL_x, 
                                     pol.y = rec$bufferL_y)
  
  bufferR_in <- sp::point.in.polygon(point.x = node_samples$vegsam_x, 
                                     point.y = node_samples$vegsam_y,
                                     pol.x = rec$bufferR_x, 
                                     pol.y = rec$bufferR_y)
  
  stream_in <- sp::point.in.polygon(point.x = node_samples$vegsam_x, 
                                    point.y = node_samples$vegsam_y,
                                    pol.x = rec$stream_x, 
                                    pol.y = rec$stream_y) 

  #1 = inside the veg buffer, replace w/ lccode
  lccode_L_in <- dplyr::if_else(bufferL_in == 1, lccode_L, 0)
  lccode_R_in <- dplyr::if_else(bufferR_in == 1, lccode_R, 0)
  lccode_S_in <- dplyr::if_else(stream_in %in% c(1,2,3), lccode_stream, 0)
  
  # Put the Left, Right, and stream together
  lccodes <- lccode_L_in + lccode_R_in + lccode_S_in
  
  # replace 0  with no veg code
  lccodes <- dplyr::if_else(lccodes == 0, lccode_noveg, lccodes)


  # Combine it all in the same dataframe
  aspstm <- seq(stmcombos$aspect, stmcombos$aspect, length.out = nrow(node_samples))
  skygap <- seq(stmcombos$skygap, stmcombos$skygap, length.out = nrow(node_samples))
  vegwd_L <- seq(stmcombos$vegwd_L, stmcombos$vegwd_L, length.out = nrow(node_samples))
  vegwd_R <- seq(stmcombos$vegwd_R, stmcombos$vegwd_R, length.out = nrow(node_samples))
  ttools <- cbind(node_samples, lccodes, aspstm, skygap, vegwd_L, vegwd_R)
  
  ttools <- node_samples %>%
    cbind(lccodes) %>%
    dplyr::mutate(stream_id = stream_id,
                  node_id = node_id,
                  aspect = aspect,
                  skygap = skygap, 
                  vegwd_L = vegwd_L,
                  vegwd_R = vegwd_R)
  
  # reorganize so it is in the same format as the heat source ttools lcdata input.
  ttools_lcdata <- ttools %>%
    dplyr::select(node_id, stream_id, aspect, skygap, vegwd_L, vegwd_R, lc_key, lccodes) %>%
    tidyr::pivot_wider(names_from = lc_key, values_from = lccodes)
  
  # check w/ plot
  # rec %>%
  #   dplyr::select(stream_x, stream_y, bufferL_x, bufferL_y, bufferR_x, bufferR_y) %>%
  #   tidyr::pivot_longer(cols = dplyr::everything(), names_to = c("poly", ".value"), values_to = ".value", names_sep = "_") %>%
  #   ggplot2::ggplot(ggplot2::aes(x = x, y = y, group = poly, fill = poly)) +
  #   ggplot2::geom_polygon() +
  #   ggplot2::geom_path(color = "white") +
  #   ggplot2::geom_point(data = node_samples, ggplot2::aes(x = vegsam_x, y = vegsam_y), inherit.aes = FALSE, size = 0.5) +
  #   ggplot2::coord_equal()

  return(ttools_lcdata)  
  
  
}


