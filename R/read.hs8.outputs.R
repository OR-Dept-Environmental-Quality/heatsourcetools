#' Read Heat Source 8 outputs.
#'
#' Read hourly output from Heat Source version 8.
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
  return(hs8.data)

}
