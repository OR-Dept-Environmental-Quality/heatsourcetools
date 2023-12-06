#' Calculate goodness of fit stats
#'
#' Returns a data frame of calculated goodness of fit stats for each
#' monitoring site and all sites combined. The function accepts multiple sites
#' and variable timeseries. \code{preds} and \code{obs} are matched based on the
#' Monitoring.Location.ID, constituent, and nearest datetime within a set number
#' of minutes defined using \code{tolerance}.If the difference in datetimes
#' exceeds the \code{tolerance} a match will not be made. Goodness of fit
#' statistics are calculated using the \code{\link[metrica]{metrica}} package.
#' Statistics calculated are Mean Bias Error (MBE), Mean Absolute Error (MAE), Root
#' Mean Square Error (RMSE), Nash-Sutcliffe Efficiency (NSE), and  Coefficient
#' of Determination (R2). Goodness of fit results are rounded to the hundredths. 
#' The count of \code{preds} and \code{obs} comparisons
#' at each monitoring location are also included (n).
#'
#' @param preds Data frame of model prediction results. The \code{preds} data
#'        frame must include the following columns:
#'  \itemize{
#'  \item Monitoring.Location.ID: Character value of the \code{obs} monitoring
#'        location ID
#'  \item constituent: Character value of the constituent (e.g. 'Temperature,
#'        water').
#'  \item datetime: POSIXct datetime.
#'  \item model_km: numeric model stream kilometer
#'  \item value: Constituent value.
#'  }
#' @param obs Data frame of field observation results. The \code{obs} data
#'        frame must include the following columns:
#'  \itemize{
#'  \item Monitoring.Location.ID: Character value of the \code{obs} monitoring
#'        location ID
#'  \item constituent: Character value of the constituent (e.g. 'Temperature,
#'        water').
#'  \item datetime: POSIXct datetime.
#'  \item model_km: numeric model stream kilometer
#'  \item value: Constituent value.
#'  }
#' @param tolerance The maximum number of minutes allowed between the datetimes
#'        in \code{preds} and \code{obs} to establish a match. If the difference
#'        exceeds the tolerance the prediction is not included in the goodness
#'        of fit stats. The tolerance cannot be greater than 1440 minutes
#'        (1 day). The default tolerance is 60 minutes.
#' @seealso \code{\link[metrica]{MBE}}, \code{\link[metrica]{MAE}},
#'          \code{\link[metrica]{RMSE}}, \code{\link[metrica]{NSE}},
#'          \code{\link{cor}}
#' @export
#' @return data frame
#'

calc_gof <- function(preds, obs, tolerance = 60) {

  # Testing
  # preds <- preds.temp
  # obs <- obs.temp
  
  if (tolerance > 1440) {
    warning("tolerance exceeds 1440 minutes. Reset to 1440.")
    tolerance <- 1440
  }

  t.min <- min(preds$datetime) - lubridate::minutes(tolerance)
  t.max <- max(preds$datetime) + lubridate::minutes(tolerance)

  obs2 <- obs %>%
    dplyr::select(Monitoring.Location.ID, constituent, model_km,
                  datetime.obs = datetime, Observations = value) %>%
    dplyr::mutate(datetime.hour = lubridate::round_date(datetime.obs, unit = "hour"),
                  year = lubridate::year(datetime.hour),
                  month = lubridate::month(datetime.hour),
                  day = lubridate::day(datetime.hour),
                  hour = lubridate::hour(datetime.hour)) %>%
    dplyr::filter(datetime.hour >= t.min & datetime.hour <= t.max) %>%
    dplyr::arrange(Monitoring.Location.ID, constituent, model_km, datetime.obs)

  preds2 <- preds %>%
    dplyr::select(Monitoring.Location.ID, constituent, model_km,
                  datetime.preds = datetime, Predictions = value) %>%
    dplyr::mutate(year = lubridate::year(datetime.preds),
                  month = lubridate::month(datetime.preds),
                  day = lubridate::day(datetime.preds),
                  hour = lubridate::hour(datetime.preds)) %>%
    dplyr::filter(model_km %in% unique(obs2$model_km)) %>%
    dplyr::arrange(Monitoring.Location.ID, constituent, model_km, datetime.preds)

  df <- preds2 %>%
    dplyr::inner_join(obs2, by = c("Monitoring.Location.ID", "constituent",
                                   "year", "month", "day", "hour", "model_km")) %>%
    dplyr::group_by(Monitoring.Location.ID, constituent, year, month, day, hour,
                    model_km) %>%
    dplyr::slice(which.min(abs(datetime.preds - datetime.obs))) %>%
    dplyr::mutate(diff.minutes = abs(as.numeric(difftime(datetime.preds,
                                                         datetime.obs,
                                                         units = "mins")))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(diff.minutes <= tolerance)

  if (length(unique(df$Monitoring.Location.ID)) > 1) {
    df <- df %>%
      dplyr::mutate(Monitoring.Location.ID = "_____ ALL") %>%
      bind_rows(df)
  }

  df.gof <- df %>%
    dplyr::group_by(Monitoring.Location.ID, constituent) %>%
    dplyr::summarise(round(metrica::MBE(pred = Predictions,
                                        obs = Observations,
                                        na.rm = TRUE, tidy = TRUE), digits = 2),
                     round(metrica::MAE(pred = Predictions,
                                        obs = Observations,
                                        na.rm = TRUE, tidy = TRUE), digits = 2),
                     round(metrica::RMSE(pred = Predictions,
                                         obs = Observations,
                                         na.rm = TRUE, tidy = TRUE), digits = 2),
                     round(metrica::NSE(pred = Predictions,
                                        obs = Observations,
                                        na.rm = TRUE, tidy = TRUE), digits = 2),
                     R2 = round((cor(x = Predictions, y = Observations,
                                     method = "pearson")^2), digits = 2),
                     n = dplyr::n()) %>%
    dplyr::arrange(constituent, Monitoring.Location.ID) %>%
    as.data.frame()

  return(df.gof)
}
