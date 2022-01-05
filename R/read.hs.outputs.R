#' Read hourly Heat Source 6-8 outputs.
#'
#' Wrapper function to read output from Heat Source version 6-9.
#' Heat Source 6-7 excel workbook needs to be saved as .xlsm. Workbooks in .xls
#' do not work. This function calls \code{\link{format_outputs}} so the returned
#' data frame has the following columns:
#'  \itemize{
#'  \item sim: Character value of the model scenario simulation name or other description.
#'  \item constituent: Character value of the constituent (e.g.
#'        'Temperature, water').
#'  \item datetime: POSIXct datetime.
#'  \item date: Character date in format "mm/dd/YYYY".
#'  \item model_km: numeric stream kilometer extracted from the model.
#'  \item value: Constituent value.
#'  }
#'
#' If you just want the raw import use the specific 'read.hs...' function suited
#' to the model version, e.g. read.hs9.outputs.
#'
#' This function converts heat source 6 long distance to stream km with the
#' most downstream end = km 0.0
#'
#' For Heat Source 6-7 models it is necessary to identify the
#' \code{constituent_name} when reading hydraulic outputs
#' (e.g. \code{sheet_name = 'Output - Hydraulics'} or
#' \code{sheet_name = 'Main Menu'} for hs7 and hs6 models respectively). This is
#' necessary because these sheets have multiple outputs and the
#' constituent name helps the function identify which output to read. In this
#' situation use one of the following for \code{constituent_name}:
#' \itemize{
#' \item 'Flow Rate'
#' \item 'Flow Velocity'
#' }
#'
#' @param output_dir The path to directory where the output Heat Source file is
#'        located.
#' @param file_name The file name of the .xlsm for Heat Source version 6-7 or
#'       the name of the .txt or .csv output file for Heat Source version 8-9.
#'       For Heat Source 6-7, the ".xlsm" extension should be included in the
#'       file name.
#' @param hs_ver The version of Heat Source. Input is a numeric value equal to
#'       6, 7, 8, or 9. Default is 9.
#' @param sheet_name The name of the Heat Source 6-7 output worksheet to read.
#'        String format. NA for Heat Source 8-9. Default is NA. The
#'        \code{constituent_name} must be identified for Heat Source 6-7 models
#'        when reading the 'Main Menu' or 'Output - Hydraulics' worksheets
#'        See details for more information.
#' @param constituent_name The name of the output. String format. Required for
#'        Heat Source 6 when reading from the (see details). Default is NA.
#'        If NA, the function will attempt to look up the constituent
#'        using \code{\link{output_lookup}}.
#' @param sim_name The name of the model scenario. String format. Default is NA.
#' @seealso \code{\link{read.hs6.temp}}, \code{\link{read.hs6.shade}},
#'          \code{\link{read.hs6.flow}}, \code{\link{read.hs6.velocity}},
#'          \code{\link{read.hs7.outputs}}, \code{\link{read.hs7.shade}},
#'          \code{\link{read.hs7.flow}}, \code{\link{read.hs7.velocity}},
#'          \code{\link{read.hs8.outputs}},
#'          \code{\link{read.hs9.outputs}}
#' @keywords Heat Source
#' @export
#' @return data frame
#'

read.hs.outputs <- function(output_dir, file_name, hs_ver = 9,
                            sheet_name = NA_character_,
                            constituent_name = NA_character_,
                            sim_name = NA_character_) {
  # Reads any output from heat source version 6-9. Does some formatting, and
  # returns the data as a data frame in long format.
  # Simulation name, constituent, statistic are strings
  # hours are added as ID variables.
  # sheet_name is only used for heat source 7

  # Assign the correct read function based on model version
  if (as.integer(hs_ver) == 6) {

    name <- sheet_name

    if (grepl("shade", sheet_name, ignore.case = TRUE)) {
      # if shade we have to use a special function because the formatting of
      # that sheet is different

      data.wide <- read.hs6.shade(output_dir = output_dir,
                                  file_name = file_name,
                                  sheet_name = sheet_name)
    }

    if (is.na(constituent_name) | !constituent_name %in% c("Flow Rate",
                                                           "Flow Velocity")) {

      data.wide <- read.hs6.temp(output_dir = output_dir,
                                 file_name = file_name,
                                 sheet_name = sheet_name)
    } else {

      if (constituent_name == "Flow Rate") {

        data.wide <- read.hs6.flow(output_dir = output_dir,
                                   file_name = file_name,
                                   sheet_name = sheet_name)
      }

      if (constituent_name == "Flow Velocity") {

        data.wide <- read.hs6.velocity(output_dir = output_dir,
                                       file_name = file_name,
                                       sheet_name = sheet_name)
      }
    }
  }

  # Assign the correct read function based on model version
  if (as.integer(hs_ver) == 7) {

    name <- sheet_name

    if (grepl("shade", sheet_name, ignore.case = TRUE)) {
      # if shade we have to use a special function because the formatting of
      # that sheet is different

      data.wide <- read.hs7.shade(output_dir = output_dir,
                                  file_name = file_name,
                                  sheet_name = sheet_name)
    }

    if (is.na(constituent_name) | constituent_name %in% c("Flow Rate",
                                                          "Flow Velocity")) {

      data.wide <- read.hs7.outputs(output_dir = output_dir,
                                    file_name = file_name,
                                    sheet_name = sheet_name)
    } else {

      if (constituent_name == "Flow Rate") {

        data.wide <- read.hs7.flow(output_dir = output_dir,
                                   file_name = file_name,
                                   sheet_name = sheet_name)
      }

      if (constituent_name == "Flow Velocity") {

        data.wide <- read.hs7.velocity(output_dir = output_dir,
                                       file_name = file_name,
                                       sheet_name = sheet_name)
      }
    }
  }

  if (as.integer(hs_ver) == 8) {
    data.wide <- read.hs8.outputs(output_dir = output_dir,
                                  file_name = file_name)
    name <- file_name
  }

  if (as.integer(hs_ver) == 9) {
    data.wide <- read.hs9.outputs(output_dir = output_dir,
                                  file_name = file_name)
    name <- file_name
  }

  data.final <- format_outputs(df = data.wide, hs_ver = hs_ver,
                               name = name,
                               constituent_name = constituent_name,
                               sim_name = sim_name)

  return(data.final)

}
