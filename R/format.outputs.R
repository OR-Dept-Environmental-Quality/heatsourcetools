#' Format raw Heat Source output to a consistent format
#'
#' Format output from any of the read.hs functions so the dataframe has a consistent structure and column names.
#' 
#' @param df The path to directory where the Heat Source 7 .xlsm model is located.
#' @param hs_ver The version of Heat Source. Input is a numeric value equal to 7, 8, or 9. Default is 9.
#' @param sheet_name The name of the Heat Source 7 output worksheet to read. String format. NULL for Heat Source 8-9. Default is NULL.
#' @param constituent_name The name of the output. String format. Optional. Default is NA. If NA, the function will attempt to look up the constituent using [lookup_output].
#' @param sim_name The name of the model scenario. String format. Optional. Default is NA.
#' @keywords
#' @export
#' @return dataframe

format.outputs <- function(df,  hs_ver = 9, sheet_name = NULL, 
                           constituent_name = NA, sim_name = NA) {
  
  if(hs_ver==7 & grepl("shade", sheet_name, ignore.case = TRUE)) {
    # if shade the data already comes in long format
    
    df$Date <- format(data.raw$Datetime,"%m/%d/%Y")
    
    df$sim <- sim_name
    df$constituent <- ifelse(is.na(constituent_name),"Effective Shade", constituent_name)
    
    df.final <- df[, c("sim", "constituent", "Datetime", "Date", "Stream_km", "value")]
    
    return(df.final)
  }
  
  
  if(is.na(constituent_name)) {
    lu_names <- heatsourcetools::output_lookup(file_name = file_name, 
                                               hs_ver = hs_ver, 
                                               sheet_name = sheet_name)
    constituent_name <- lu_names[1]
  }

  df$constituent <- constituent_name
  df$sim <- sim_name
  
  # Convert data from wide to long (tidyr)
  data.l <- tidyr::pivot_longer(df, cols = starts_with(c("X", c(0:9))), 
                                names_to = "Stream_km", values_to = "value")
  
  data.l$Stream_km <- as.numeric(gsub(pattern="X", replacement="",
                                      data.l$Stream_km, 
                                      ignore.case = FALSE, 
                                      fixed = FALSE))
  
  
  data.l$Datetime <-lubridate::round_date(as.POSIXct((data.l$Datetime*60*60*24), 
                                                     origin="1899-12-30", tz="GMT"), 
                                          unit = "minute")
  
  data.l$Date <- format(data.l$Datetime,"%m/%d/%Y")
  
  data.l <- data.l[, c("sim", "constituent", "Datetime", "Date", "Stream_km", "value")]
  
  return(data.l)
  
}
