#' Read Heat Source 9 outputs.
#'
#' Read output from Heat Source version 9.
#'
#' @param output_dir The path to directory where the Heat Source 9 output files are located.
#' @param file_name The file name of the Heat Source 9 output. Thee file is a csv text file.
#'
#' @keywords Heat Source version 8
#' @export
#' @return dataframe
#'

read.hs9.outputs <- function(output_dir, file_name) {
  # Function to reads any hourly output from heat source 9. Returns the data
  # as a dataframe.

  # remove the extension in case it was added
  base_name <- gsub("\\..*","", file_name)

  hs9.data <- read.table(paste0(output_dir, base_name,".csv"),
                         sep=",",dec=".",skip=6,header=TRUE,
                         stringsAsFactors = FALSE, na.strings = "NA")
  return(hs9.data)
}
