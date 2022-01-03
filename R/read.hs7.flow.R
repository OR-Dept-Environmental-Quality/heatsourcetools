#' Read Heat Source 7 flow rate outputs.
#'
#' Read flow rate outputs from the hydraulics output worksheet in a Heat Source
#' version 7 model. The Heat Source excel workbook needs to be saved as .xlsm.
#' Workbooks in .xls do not seem to work.
#'
#' Data is returned in wide format with the model stream km used as the column
#' name for all values. An "X" is added as a prefix to every stream km value to
#' have syntactically valid column names. The datetime is the first column and
#' is formatted in excel numeric date format. Heat Source 7 has a bug where the
#' dates in the hydraulics output worksheet are one day off. This function works
#' around this bug and returns the correct date for each result.
#'
#' This function is setup to read the "Output - Hydraulics" worksheet.
#'
#' @param output_dir The path to directory where the Heat Source 7 .xlsm model
#'        is located.
#' @param file_name The file name of the .xlsm Heat Source 7 model.
#'        The ".xlsm" extension should be included in the file name.
#' @param sheet_name The name of the output worksheet to read. Default is
#'        'Output - Hydraulics'.
#'
#' @keywords Heat Source version 7
#' @export
#' @return dataframe


read.hs7.flow <- function(output_dir, file_name, sheet_name = "Output - Hydraulics") {

  # read the model date and convert to numeric value in excel format
  model_date <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = "Output - Hydraulics",
                                   range = "I3",
                                   col_types = c("date"),
                                   col_names = "model_date",
                                   na = c("","N/A", " ")) %>%
    dplyr::mutate(model_date = as.numeric(as.Date(model_date) -
                                            as.Date(0, origin = "1899-12-30",
                                                    tz = "UTC"))) %>%
    dplyr::pull(model_date)

  # read the number of model days
  sim_days <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = "Output - Hydraulics",
                                   range = "I13",
                                   col_types = c("numeric"),
                                   col_names = "sim_days",
                                   na = c("","N/A", " ")) %>%
    dplyr::pull(sim_days)

  col_names <- c("model_km", seq(from = model_date, to = model_date + sim_days - 1))

  excel.data <- readxl::read_excel(path = file.path(output_dir, file_name),
                                   sheet = sheet_name,
                                   range = cellranger::cell_limits(c(17, 5), c(NA, sim_days + 5)),
                                   col_names = col_names,
                                   na = c("","N/A", " "))

  model_kms <- excel.data$model_km
  excel.data$model_km <- NULL
  hs7.data <- data.frame(t(excel.data))
  colnames(hs7.data) <- paste0("X", model_kms)

  hs7.data$datetime <- as.numeric(rownames(hs7.data))
  rownames(hs7.data) <- NULL

  hs7.data <- dplyr::select(hs7.data, datetime, dplyr::everything())

  return(hs7.data)

}
