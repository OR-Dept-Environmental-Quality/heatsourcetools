#' Read Heat Source 8 outputs.
#'
#' Read hourly output from Heat Source version 8.
#'
#' Data is returned in wide format with the model km used as the column name.
#' An "X" is added as a prefix to every model km value to have syntactically
#' valid column names. The datetime is the first column and is formatted in
#' excel numeric date format.
#'
#' This function reads the following output files:
#'  \itemize{
#'  \item Heat_SR1
#'  \item Heat_SR2
#'  \item Heat_SR3
#'  \item Heat_SR4
#'  \item Heat_SR5
#'  \item Shade
#'  \item VTS
#'  \item Hyd_DA
#'  \item Hyd_DM
#'  \item Hyd_Flow
#'  \item Hyd_Hyp
#'  \item Hyd_Vel
#'  \item Hyd_WT
#'  \item Heat_SR6
#'  \item Heat_SR7
#'  \item Heat_Cond
#'  \item Heat_Long
#'  \item Heat_Conv
#'  \item Heat_Evap
#'  \item Rate_Evap
#'  \item SolarBlock
#'  \item Temp_H2O
#'  \item Temp_Sed
#'  \item Hyd_Disp
#' }
#'
#' @param output_dir The path to directory where the Heat Source 8 output files are located.
#' @param file_name The file name of the Heat Source 8 output. The file is a .txt text file.
#'
#' @keywords Heat Source version 8
#' @export
#' @return data frame
#'

read.hs8.outputs <- function(output_dir, file_name) {

  # remove the extension in case it was added
  base_name <- gsub("\\..*","", file_name)

  hs8.data <- read.table(file.path(output_dir, paste0(base_name,".txt")),
                         sep = "", dec = ".", skip = 2, header = TRUE,
                         stringsAsFactors = FALSE, na.strings = "NA")

  # rename with lowercase
  hs8.data <- dplyr::rename(hs8.data, datetime = Datetime)

  return(hs8.data)

}
