#' Format Heat Source output to a long format
#'
#' Format output from any of the read.hs functions so the returned data frame
#' is in long format with a consistent structure and column names.
#'
#' Output column names include:
#'  \itemize{
#'  \item sim: Character value of the model scenario simulation name or other description.
#'  \item constituent: Character value of the constituent (e.g. Temperature).
#'  \item datetime: POSIXct datetime.
#'  \item date: Character date in format "mm/dd/YYYY".
#'  \item stream_km: numeric stream kilometer extracted from the model.
#'  \item value: Constituent value.
#'  }
#'
#' Heat Source 6 long distances are converted to stream kilometer with the most
#' downstream long distance = stream kilometer 0.
#'
#' @param df A data frame output from any of the read.hs functions
#' @param hs_ver The version of Heat Source. Input is a numeric value
#'        equal to 6, 7, 8, or 9. Default is 9.
#' @param name The name of the .txt, or .csv output file for Heat Source
#'        version 8-9; or excel sheet name for Heat Source 6-7. String format.
#' @param constituent_name The name of the output. String format. Optional. Default is NA.
#'        If NA, the function will attempt to look up the constituent using \code{\link{output_lookup}}
#'        and the value in 'name'.
#' @param sim_name The name of the model scenario. String format. Optional. Default is NA.
#' @export
#' @return data frame

format_outputs <- function(df, hs_ver = 9, name = NA,
                           constituent_name = NA, sim_name = NA) {

  if (is.na(constituent_name)) {
    lu_names <- heatsourcetools::output_lookup(name = name,
                                               hs_ver = hs_ver)
    constituent_name <- lu_names[1]
  }

  df$constituent <- constituent_name
  df$sim <- sim_name

  # Convert data from wide to long (tidyr)
  df.long <- tidyr::pivot_longer(df, cols = starts_with(c("X", c(0:9))),
                                 names_to = "stream_km", values_to = "value")

  df.long$stream_km <- as.numeric(gsub(pattern = "X", replacement = "",
                                       df.long$stream_km,
                                       ignore.case = FALSE,
                                       fixed = FALSE))

  if (hs_ver == 6) {
    # convert long distance to stream km

    max_distance <- max(df.long$stream_km, na.rm = TRUE)

    df.long$stream_km <- (max_distance - df.long$stream_km) / 1000

  }


  df.long$datetime <- lubridate::round_date(as.POSIXct((df.long$datetime*60*60*24),
                                                       origin = "1899-12-30",
                                                       tz = "GMT"),
                                            unit = "minute")


  df.long$date <- format(df.long$datetime, "%m/%d/%Y")

  df.long <- df.long[, c("sim", "constituent", "datetime", "date", "stream_km", "value")]

  return(df.long)

}
