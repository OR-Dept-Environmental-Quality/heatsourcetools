#' Read field observation data
#'
#' Read a .xlsx workbook formatted with field observation data. The workbook is 
#' setup using a similar format as DEQ's AWQMS data submission templates.
#' 
#' The workbook must contain two worksheets named:
#' \itemize{
#'  \item Monitoring_Locations
#'  \item Results
#'  }
#' 
#' The worksheets must contain the columns identified in the lists below.
#' Additional columns can be included but will not be used by the function.
#' 
#' Required 'Monitoring_Locations' worksheet columns
#' \itemize{
#'  \item Monitoring.Location.ID: Character value of the monitoring location ID.
#'  \item Latitude: Numeric Latitude of the monitoring location in decimal degrees.
#'  \item Longitude: Numeric Longitude of the monitoring location in decimal degrees.
#'  \item GNIS_Name: Character name of the waterbody.
#'  \item model_km: Numeric model stream kilometer corresponding to the location of the monitoring site
#'  }
#' 
#'Required 'Results' worksheet columns:
#'  \itemize{
#'  \item Monitoring.Location.ID: Character value of the monitoring location ID.
#'  \item Activity.Start.Date: Date of sample. Formatted as an excel date in the format 'YYYY/MM/DD'.
#'  \item Activity.Start.Time: Time of sample. Formatted as an excel date in the format 'HH24:MM'.
#'  \item Activity.Start.End.Time Zone: Character timezone (e.g. PDT or PST).
#'  \item Characteristic.Name: Characteristic name used in AWQMS. (e.g. 'Temperature, water', 'Flow') 
#'  \item Result.Value: Measured characteristic result value.
#'  \item Result.Unit: Units for measured characteristic result. Same as what is used in AWQMS. (e.g. 'deg C', 'm3/sec', 'm') 
#'  }
#'
#' @param obs_dir The path to directory where observation data .xlsx file is located.
#' @param file_name The name of the observation data .xlsx file.
#' @export

read.obs <- function(obs_dir, file_name) {
  # Read observation data
  
  
  mlocs <- readxl::read_excel(path = file.path(obs_dir, file_name),
                             sheet = "Monitoring_Locations",
                             na = c("","N/A", " "))
  
  results <- readxl::read_excel(path = file.path(obs_dir, file_name),
                                sheet = "Results",
                                na = c("","N/A", " "))
  
 mloc_cols <- c("Monitoring.Location.ID", "Monitoring.Location.Name", "Latitude", "Longitude", "GNIS_Name", "model_km")
 
 result_cols <- c("Monitoring.Location.ID", "Activity.Start.Date", "Activity.Start.Time", "Activity.Start.End.Time Zone", 
                  "Characteristic.Name",	"Result.Value",	"Result.Unit")
 
  if(nrow(mlocs)==0) {
    stop("Monitoring_Locations worksheet has no rows.")
    
  }
  
  if(nrow(results)==0) {
    stop("Results worksheet has no rows.")
    
  }
  
  # column name check
  if (any(!mloc_cols %in% names(mlocs))) {
    stop(paste("Monitoring_Locations worksheet has missing columns: ", 
               paste(mloc_cols[!mloc_cols %in% names(mlocs)], collapse = ", ")))
  }
  
  if (any(!result_cols %in% names(results))) {
    stop(paste("Results worksheet has missing columns: ", 
               paste(result_cols[!result_cols %in% names(results)], collapse = ", ")))
  }
  
  df <- dplyr::left_join(results, mlocs, by="Monitoring.Location.ID")
  
  df_final <- df %>%
    dplyr::mutate(date = format(Activity.Start.Date, "%m/%d/%Y"),
                  time_char = format(Activity.Start.Time, "%H:%M:%S"),
                  datetime_char = paste(date, time_char),
                  datetime = lubridate::mdy_hms(datetime_char, tz="UTC")) %>%
    dplyr::select(Monitoring.Location.ID, Monitoring.Location.Name, Latitude, Longitude, 
                  GNIS_Name, model_km, datetime, date, Characteristic.Name, 
                  Result.Value, Result.Unit)
  
  return(df_final)
  
}