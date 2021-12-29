#' Read Heat Source 6 longitudinal temperature outputs.
#'
#' Read hourly xlsm temperature output from Heat Source version 6.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do not work.
#'
#' Data is returned in wide format with the long distance used as the column name for all values.
#' An "X" is added as a prefix to every long distance value to have syntactically valid
#' column names. The datetime is the first column and is formatted in excel
#' numeric date format.
#'
#' @param output_dir The path to directory where the Heat Source 6 .xlsm model is located.
#' @param file_name The file name of the .xlsm Heat Source 6 model.
#'  The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is "Long Temp Output".
#'
#' @keywords Heat Source version 6
#' @export
#' @return data frame
#'

read.hs6.temp <- function(output_dir, file_name,
                          sheet_name = "Long Temp Output") {

  #output_dir <- "/Users/rmichie/Desktop/heatsource/"
  #file_name <- "hs6_Johnson_Creek.xlsm"

  max_distance <- readxl::read_excel(path = file.path(output_dir, file_name),
                                      sheet = "Main Menu",
                                      range = "D4",
                                      col_types = c("numeric"),
                                      col_names = "long_distance",
                                      na = c("","N/A", " ")) %>%
    dplyr::mutate(long_distance = round(long_distance, -2)) %>%
    dplyr::pull(long_distance)

  # read the model date and convert to numeric value in excel format
  model_date <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = "Main Menu",
                                   range = "D6",
                                   col_types = c("date"),
                                   col_names = "model_date",
                                   na = c("","N/A", " ")) %>%
    dplyr::mutate(model_date = as.numeric(as.Date(model_date) -
                                            as.Date(0, origin = "1899-12-30",
                                                    tz = "UTC"))) %>%
    dplyr::pull(model_date)

  datetimes <- model_date + c(0:23/24)

  col_names <- c("long_distance", "sim_time", paste0("X",0:23))
  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name),
                                  sheet = sheet_name,
                                  range = cellranger::cell_limits(c(6, 2), c(NA, 27)),
                                  col_names = col_names,
                                  na = c("","N/A", " ")) %>%
    dplyr::select(-sim_time) %>%
    dplyr::filter(long_distance <= max_distance)

  # sort by long_distance ascending
  excel.data <- excel.data[order(excel.data$long_distance), ]
  long_distances <- excel.data$long_distance
  excel.data$long_distance <- NULL

  hs6.data <- data.frame(t(excel.data))
  colnames(hs6.data) <- paste0("X", long_distances)
  hs6.data$datetime  <- datetimes
  rownames(hs6.data) <- NULL

  hs6.data <- dplyr::select(hs6.data, datetime, dplyr::everything())

  return(hs6.data)
}
