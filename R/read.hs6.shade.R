#' Read Heat Source 6 effective shade outputs.
#'
#' Read xlsm effective shade output from Heat Source version 6.
#' Heat Source excel workbook needs to be saved as .xlsm. Workbooks in .xls do 
#' not seem to work.
#'
#' Data is returned in wide format with the long distance used as the column 
#' name for all values. An "X" is added as a prefix to every long distance value 
#' to have syntactically valid column names. The datetime is the first column 
#' and is formatted in excel numeric date format.
#'
#' This function is setup to read the "Effective Shade Data" worksheet.
#'
#' @param output_dir The path to the directory where the Heat Source 6 .xlsm 
#'        model is located.
#' @param file_name The file name of the .xlsm Heat Source 6 model.
#'  The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read.
#'
#' @keywords Heat Source version 6
#' @export
#' @return data frame
#'

read.hs6.shade <- function(output_dir, file_name, sheet_name = "Effective Shade Data") {

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

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = sheet_name,
                                   col_types = rep("numeric", 4),
                                   range = cellranger::cell_limits(c(6, 2), c(NA, 5)),
                                   col_names = c("long_distance", "SR1", "SR4", "value"),
                                   na = c("","N/A", " ")) %>%
    dplyr::select(-SR1, -SR4) %>%
    dplyr::filter(long_distance <= max_distance) %>%
    dplyr::mutate(value = round(value, 3))

  # sort by long_distance ascending
  excel.data  <- excel.data[order(excel.data$long_distance), ]

  excel.data$datetime <- model_date + (23/24)

  excel.data <- tidyr::pivot_wider(excel.data, values_from = value,
                                   names_from = long_distance, names_prefix = "X") %>%
    dplyr::select(datetime, dplyr::everything())

  return(excel.data)
}
