#' Calculate the 7DADM from hourly data.
#'
#' Calculate the daily maximum and rolling seven day average of the daily maximums (7DADM) by date from hourly data.
#' The rolling average result is right aligned and indexed to the last day of the seven day period.
#' Constituent and statistic names are added as an ID variable.
#'
#' @param df The input dataframe. df must have the following columns names:
#'   Datetime (with datetime as POSIXlt),
#'   sim (with simulation name in character format),
#'   Stream_km (as numeric),
#'   value (numeric hourly stream temperature)
#' @param datetime_col Datetime column name. Default is 'Datetime'.
#' @param sim_col Simulation column name. Default is 'sim'.
#' @param stream_km_col Stream km column name. Default is 'Stream_km'.
#' @param value_col Value column name. Default is 'value'.
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe
#'

calc.7dadm <- function(df, datetime_col="Datetime", sim_col="sim", stream_km_col="Stream_km", value_col="value") {
  # Returns a dataframe of the daily maximum and 7DADM temperatures
  # by date in long format. Constituent and statistic names are added as an ID variable.

  df <- df %<% dplyr::rename_at()

  df <- df[,c(datetime_col, sim_col, stream_km_col, value_col)]
  colnames(df) <- c("Datetime","sim","Stream_km","value")

  # Add character Date
  df$Date <- format(df$Datetime, "%m/%d/%Y")

  data.max <- as.data.frame(aggregate(df$value,by=list(df$Date,df$Stream_km, df$sim),
                                      FUN=max, na.rm = TRUE))

  colnames(data.max) <- c("Date","Stream_km", "sim", "max")

  # sort by Stream_km and Date
  data.max <- data.max[with(data.max, order(sim,-Stream_km,Date)), ]

  # change infinite values to NA
  data.max$max <- ifelse(is.infinite(data.max$max), NA, data.max$max)

  # Calculate the 7 day max rolling average
  data.max$sdadm <- ave(data.max$max, data.max$Stream_km, FUN =
                          function(x) zoo:rollapply(zoo:zoo(x), 7, mean, fill = NA, align = "right"))

  # convert to long format
  data.l <- reshape2::melt(data.max, id.vars =c("Date","sim","Stream_km"))

  colnames(data.l) <- c("Date","sim","Stream_km","statistic","value")

  # make sure it is sorted
  data.l <- data.l[with(data.l, order(sim,statistic,-Stream_km,Date)), ]

  data.l$constituent <- unique(df$constituent)

  # rename the statistics
  data.l$statistic <- gsub(pattern="max", replacement="Daily Maximum Temperature",
                           data.l$statistic, ignore.case = FALSE, fixed = FALSE)

  data.l$statistic <- gsub(pattern="sdadm", replacement="7DADM Temperature",
                           data.l$statistic, ignore.case = FALSE,fixed = FALSE)



  return(data.l)
}
