#' Read Heat Source 9 outputs.
#'
#' Read output from Heat Source version 9.
#'
#' Data is returned in wide format with the stream km used as the column name for all values.
#' An "X" is added as a prefix to every stream km value to have syntactically valid
#' column names. The datetime is the first column and is formatted in excel
#' numeric date format.
#'
#' This function reads the following output files:
#'  \itemize{
#'  \item Heat_SR1
#'  \item Heat_SR2
#'  \item Heat_SR3
#'  \item Heat_SR3b
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
#'  \item Temp_H2O
#'  \item Temp_Sed
#'  \item Hyd_Disp
#' }
#'
#' @param output_dir The path to directory where the Heat Source 9 output files are located.
#' @param file_name The file name of the Heat Source 9 output. The file is a csv text file.
#'
#' @keywords Heat Source version 9
#' @export
#' @return dataframe
#'

read.hs9.outputs <- function(output_dir, file_name) {

  # remove the extension in case it was added
  base_name <- gsub("\\..*","", file_name)

  hs9.data <- read.table(file.path(output_dir, paste0(base_name,".csv")),
                         sep = ",", dec = ".", skip = 6, header = TRUE,
                         stringsAsFactors = FALSE, na.strings = "NA")
  # rename with lowercase
  hs9.data <- dplyr::rename(hs9.data, datetime = Datetime)

  return(hs9.data)
  }
