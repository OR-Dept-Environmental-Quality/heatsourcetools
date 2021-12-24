#' Calculate the 7DADM from hourly temperature data.
#'
#' Calculate the daily maximum and rolling seven day average of the daily maximums (7DADM) by date from hourly data.
#' The rolling average result is right aligned and indexed to the last day of the seven day period.
#' 'Daily Maximum Temperature' and '7DADM Temperature' are values added to the
#' constituent column.
#'
#' @param df The input data frame. df must have the following columns names:
#'   Datetime (with datetime as POSIXlt),
#'   sim (with simulation name in character format),
#'   Stream_km (as numeric),
#'   value (numeric hourly stream temperature)
#'
#'   These can be generated using the [format.outputs] function.
#' @param datetime_col Datetime column name. Default is 'Datetime'.
#' @param sim_col Simulation column name. Default is 'sim'.
#' @param stream_km_col Stream km column name. Default is 'Stream_km'.
#' @param value_col Value column name. Default is 'value'.
#' @export
#' @return data frame with columns 'sim', Date', 'Stream_km', 'constituent', and 'value'.
#'

calc.7dadm <- function(df, datetime_col= "Datetime", sim_col="sim",
                       stream_km_col="Stream_km", value_col="value",
                       constituent="Temperature") {

  # Returns a data frame of the daily maximum and 7DADM temperatures
  # by date in long format. Constituent and statistic names are added as an ID variable.

  df <- df[,c(sim_col, datetime_col, stream_km_col, value_col)]
  colnames(df) <- c("sim", "Datetime", "Stream_km", "value")

  # Calc daily max, add character date, change infinite values to NA
  df.max <- df %>%
    dplyr::mutate(Date = format(Datetime, "%m/%d/%Y")) %>%
    dplyr::group_by(Date, Stream_km, sim) %>%
    dplyr::summarise(max = max(value, na.rm = TRUE)) %>%
    dplyr::arrange(sim, -Stream_km, Date) %>%
    dplyr::mutate(max = dplyr::na_if(max, Inf),
                  max = dplyr::na_if(max, -Inf)) %>%
    as.data.frame()

  # Calc 7-day mean of the daily maximums
  df.7dadm <- df.max %>%
    dplyr::group_by(Stream_km, sim) %>%
    dplyr::mutate(sdadm = rollmean(x = max, k = 7, fill = NA, align = "right")) %>%
  as.data.frame()

  df.long <- df.7dadm %>%
    tidyr::pivot_longer(cols = c("max", "sdadm"), names_to = "constituent", values_to = "value") %>%
    dplyr::arrange(sim, constituent, -Stream_km, Date)

  # rename the statistics
  df.long$constituent <- gsub(pattern = "max", replacement = "Daily Maximum Temperature",
                              df.long$constituent, ignore.case = FALSE, fixed = FALSE)

  df.long$constituent  <- gsub(pattern = "sdadm", replacement = "7DADM Temperature",
                               df.long$constituent , ignore.case = FALSE,fixed = FALSE)

  df.final <- df.long[, c("sim", "constituent", "Date", "Stream_km", "value")]

  return(df.final)
}
