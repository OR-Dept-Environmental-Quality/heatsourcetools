#' Calculate the 7DADM from hourly temperature data.
#'
#' Calculate the daily maximum and rolling seven day average of the daily maximums (7DADM) by date from hourly data.
#' The rolling average result is right aligned and indexed to the last day of the seven day period.
#' 'Daily Maximum Temperature' and '7DADM Temperature' are values added to the
#' constituent column.
#'
#' @param df The input data frame. df must have the following columns:
#'        \itemize{
#'           \item datetime (with datetime as POSIXct),
#'           \item sim (with simulation name in character format),
#'           \item model_km (as numeric),
#'           \item value (numeric hourly stream temperature)
#'           }
#'
#'       These can be generated using the \code{\link{format_outputs}} function.
#' @param datetime_col datetime column name. Default is 'datetime'.
#' @param sim_col Simulation column name. Default is 'sim'.
#' @param dis_col Distance column name. Default is 'model_km'.
#' @param value_col Value column name. Default is 'value'.
#' @export
#' @return data frame with columns 'sim', 'datetime', 'date', value of \code{dis_col}, 'constituent', and 'value'.
#'

calc_7dadm <- function(df, datetime_col= "datetime", sim_col="sim",
                       dis_col="model_km", value_col="value") {

  # Returns a data frame of the daily maximum and 7DADM temperatures
  # by date in long format. Constituent and statistic names are added as an ID variable.

  df <- df[,c(sim_col, datetime_col, dis_col, value_col)]
  colnames(df) <- c("sim", "datetime", "model_km", "value")

  # Calc daily max, add character date, change infinite values to NA
  df.max <- df %>%
    dplyr::mutate(date = format(datetime, "%m/%d/%Y")) %>%
    dplyr::group_by(date, model_km, sim) %>%
    dplyr::summarise(max = max(value, na.rm = TRUE)) %>%
    dplyr::arrange(sim, -model_km, date) %>%
    dplyr::mutate(datetime = lubridate::mdy_hm(paste0(date," 00:00")),
                  max = dplyr::na_if(max, Inf),
                  max = dplyr::na_if(max, -Inf)) %>%
    as.data.frame()

  # Calc 7-day mean of the daily maximums
  df.7dadm <- df.max %>%
    dplyr::group_by(model_km, sim) %>%
    dplyr::mutate(startdate7 = dplyr::lag(datetime, 6, order_by = datetime),
                  sdadm = dplyr::if_else(startdate7 == (datetime - lubridate::days(6)),
                                         zoo::rollmean(x = max, k = 7, fill = NA_real_, align = "right"),
                                         NA_real_)) %>%
  as.data.frame()
  
  # This is faster than the above code but assumes there is no break in the dates. Warning.
  # df.7dadm <- df.max %>%
  #   dplyr::group_by(model_km, sim) %>%
  #   dplyr::mutate(sdadm = zoo::rollmean(x = max, k = 7, fill = NA_real_, align = "right")) %>%
  #   as.data.frame()

  df.long <- df.7dadm %>%
    tidyr::pivot_longer(cols = c("max", "sdadm"), names_to = "constituent", values_to = "value") %>%
    dplyr::arrange(sim, constituent, -model_km, datetime)

  # rename the statistics
  df.long$constituent <- gsub(pattern = "max", replacement = "Daily Maximum Temperature",
                              df.long$constituent, ignore.case = FALSE, fixed = FALSE)

  df.long$constituent  <- gsub(pattern = "sdadm", replacement = "7DADM Temperature",
                               df.long$constituent , ignore.case = FALSE, fixed = FALSE)

  df.final <- df.long[, c("sim", "constituent", "datetime", "date", "model_km", "value")]

  names(df.final)[5] <- dis_col

  return(df.final)
}
