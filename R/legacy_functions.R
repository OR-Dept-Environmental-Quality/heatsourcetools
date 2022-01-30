#-------------------------------------------------------------------------------
# Legacy functions to read and format heat source inputs and outputs. Functions 
# here are not being maintained and some are not setup as proper package 
# functions.
#-------------------------------------------------------------------------------


#' Matches observation location to the nearest model location
#'
#' Returns a vector of model kilometers (or long distances) that are closest to
#' the observation kilometer or distance. If an observation id is equally
#' distant between two output model kilometers the first one in the vector is
#' used.
#'
#' @param obskm Numeric vector of the observation location (e.g. stream kilometer)
#' @param modkm Numeric vector of the model output distance (e.g. model kilometer)
#' @export
#' @return vector of modkm values equal to the length of obskm
#'
obs2km <- function(obskm, modkm) {
  
  warning("obs2km is deprecated. Use the function match_near instead. 
          E.g. match_near(obskm, modkm)")
  
  match_km <- sapply(obskm, function(o, s) {s[which.min(abs(s - o))]}, s = modkm)
  return(match_km)
}


read.flux.outputs <-function(output_dir, sim_name, hs_ver=8) {
  # Reads all the hourly flux output from heat source, does some formatting, calculates total flux by
  # summing all the fluxes, and returns the data as a dataframe in long format.
  # Simulation name and flux constituents are added as an ID variable.

  library(reshape2)
  library(lubridate)

  # Assign the correct read function based on model version
  if (as.integer(hs_ver) == 7) {
    stop("Reading flux outputs for heat source version 7 has not been implemented yet. Sorry.")
  }

  if (as.integer(hs_ver) == 8) {
    read.hs.outputs = read.hs8.outputs
    long <- "Heat_TR"
  }

  if (as.integer(hs_ver) == 9) {
    read.hs.outputs = read.hs9.outputs
    long <- "Heat_Long"
  }

  # read the data
  flux.cond <- read.hs.outputs(output_dir,"Heat_Cond")
  flux.conv <- read.hs.outputs(output_dir,"Heat_Conv")
  flux.evap <- read.hs.outputs(output_dir,"Heat_Evap")
  flux.long <- read.hs.outputs(output_dir,long)
  flux.sr4  <- read.hs.outputs(output_dir,"Heat_SR4")

  flux.cond$constituent <- "Bed Conduction"
  flux.conv$constituent <- "Air Convection"
  flux.evap$constituent <- "Evaporation"
  flux.long$constituent <- "Longwave"
  flux.sr4$constituent <- "Solar Radiation"

  flux.raw <- rbind(flux.cond,flux.conv,flux.evap,flux.long,flux.sr4)

  flux.raw$sim <- sim_name

  # Convert data from wide to long
  flux.l <- melt(flux.raw, id.vars =c("Datetime","sim", "constituent"),variable.name=c("stream_km"))
  colnames(flux.l) <- c("Datetime","sim", "constituent","Stream_km","value")

  flux.l$Stream_km <- as.numeric(gsub(pattern="X", replacement="",
                                      flux.l$Stream_km, ignore.case = FALSE,fixed = FALSE))

  # Convert back to wide to sum total flux
  flux.w  <- reshape(flux.l, timevar="constituent", idvar = c("Stream_km","sim", "Datetime"),
                     direction="wide")

  colnames(flux.w) <- c("Datetime","sim", "Stream_km",
                        "Bed Conduction","Air Convection",
                        "Evaporation","Longwave","Solar Radiation")

  flux.w$Total <- rowSums(flux.w[,4:8])

  # now back to long format
  flux.l <- melt(flux.w, id.vars =c("Datetime","sim", "Stream_km"))
  colnames(flux.l) <- c("Datetime", "sim", "Stream_km","constituent","value")

  flux.l$Datetime <-round_date(as.POSIXct(( flux.l$Datetime*60*60*24), origin="1899-12-30", tz="GMT"), unit = "minute")
  flux.l$Date <- format(flux.l$Datetime,"%m/%d/%Y")
  flux.l$hour <-as.integer(format(flux.l$Datetime, "%H"))

  return(flux.l)

}

read.solar.flux.outputs <-function(output_dir, sim_name, hs_ver=8) {
  # Reads all the hourly solar flux outputs from heat source, does some formatting, and returns the data as
  # a dataframe in long format.
  # Simulation name and flux constituents are added as an ID variable.

  library(reshape2)
  library(lubridate)

  # Assign the correct read function based on model version and
  # read the data

  if (as.integer(hs_ver) == 7) {
    stop("Reading solar flux outputs for heat source version 7 has not been implemented yet. Sorry.")
  }


  if (as.integer(hs_ver) == 8) {
    read.hs.outputs = read.hs8.outputs

    # read the data
    flux.sr1 <- read.hs.outputs(output_dir,"Heat_SR1")
    flux.sr4 <- read.hs.outputs(output_dir,"Heat_SR4")
    flux.sr6 <- read.hs.outputs(output_dir,"Heat_SR6")

    flux.sr1$constituent <- "SR1: Solar Radiation above Topo"
    flux.sr4$constituent <- "SR4: Solar Radiation above Stream"
    flux.sr6$constituent <- "SR6: Solar Radiation received by Stream"

    flux.raw <- rbind(flux.sr1, flux.sr4, flux.sr6)

  }

  if (as.integer(hs_ver) == 9) {
    read.hs.outputs = read.hs9.outputs

    # read the data
    flux.sr1 <- read.hs.outputs(output_dir,"Heat_SR1")
    flux.sr4 <- read.hs.outputs(output_dir,"Heat_SR4")
    flux.sr6 <- read.hs.outputs(output_dir,"Heat_SR6")

    flux.sr1$constituent <- "SR1: Solar Radiation above Topo"
    flux.sr4$constituent <- "SR4: Solar Radiation above Stream"
    flux.sr6$constituent <- "SR6: Solar Radiation received by Stream"

    # These are availibele in hs9 but not in hs8 so they are left out
    #flux.sr2 <- read.hs.outputs(output_dir,"Heat_SR2")
    #flux.sr3 <- read.hs.outputs(output_dir,"Heat_SR3")
    #flux.sr5 <- read.hs.outputs(output_dir,"Heat_SR5")

    #flux.sr2$constituent <- "SR2: Solar Radiation below Topo"
    #flux.sr3$constituent <- "SR3: Solar Radiation below Land Cover"
    #flux.sr5$constituent <- "SR5: Solar Radiation entering Stream"

    flux.raw <- rbind(flux.sr1, flux.sr4, flux.sr6)
    #flux.raw <- rbind(flux.sr1, flux.sr2, flux.sr3, flux.sr4, flux.sr5, flux.sr6)

  }

  flux.raw$sim <- sim_name

  # Convert data from wide to long
  flux.l <- melt(flux.raw, id.vars =c("Datetime","sim", "constituent"),variable.name=c("Stream_km"))
  colnames(flux.l) <- c("Datetime","sim", "constituent","Stream_km","value")

  flux.l$Stream_km <- as.numeric(gsub(pattern="X", replacement="",
                                      flux.l$Stream_km, ignore.case = FALSE,fixed = FALSE))

  # Convert back to wide to sum total flux
  #flux.w  <- reshape(flux.l, timevar="constituent", idvar = c("Stream_km","sim", "Datetime"),
  #                   direction="wide")

  #colnames(flux.w) <- c("Datetime","sim", "Stream_km",
  #                      "Solar Radiation above Topo","Solar Radiation above Topo",
  #                      "Solar Radiation received by Stream")

  # now back to long format
  #flux.l <- melt(flux.w, id.vars =c("Datetime","sim", "Stream_km"))
  #colnames(flux.l) <- c("Datetime", "sim", "Stream_km","constituent","value")

  flux.l$Datetime <-round_date(as.POSIXct((flux.l$Datetime*60*60*24), origin="1899-12-30", tz="GMT"), unit = "minute")
  flux.l$Date <- format(flux.l$Datetime,"%m/%d/%Y")
  flux.l$hour <-as.integer(format(flux.l$Datetime, "%H"))

  return(flux.l)

}

calc.7dadm2 <- function(output_dir, sim_name, hs_ver=9, file_name="Temp_H2O", sheet_name=NULL) {
  # Same function as calc.7DADM but instad of passing a dataframe as the input this function
  # reads the the model hourly temperature output using read.hs.outputs().
  # Function does some formatting, and returns a dataframe of the hourly, daily maximum, and 7DADM temperatures
  # by date in long format. Simulation name and statistics are added as an ID variable.

  library(zoo)
  library(reshape2)

  data.raw <- read.hs.outputs(output_dir=output_dir,
                              file_name=file_name,
                              constituent_name="Temperature",
                              statistic_name = "hourly",
                              sim_name=sim_name,
                              hs_ver=hs_ver,
                              sheet_name=sheet_name)

  # Add character Date
  data.raw$Date <- format(data.raw$Datetime, "%m/%d/%Y")

  data.max <- as.data.frame(aggregate(data.raw$value,by=list(data.raw$Date,data.raw$Stream_km),
                                      FUN=max, na.rm = TRUE))

  colnames(data.max) <- c("Date","Stream_km", "max")

  data.max$sim <- sim_name

  # sort by Stream_km and Date
  data.max <- data.max[with(data.max, order(-Stream_km,Date)), ]

  # Calculate the 7 day max rolling average
  data.max$sdadm <- ave(data.max$max, data.max$Stream_km, FUN =
                          function(x) rollapply(zoo(x), 7, mean, fill = NA, align = "right"))

  # convert to long format
  data.l <- melt(data.max, id.vars =c("Date","sim","Stream_km"))

  colnames(data.l) <- c("Date","sim","Stream_km","statistic","value")

  data.l$constituent <- constituent_name

  # rename the stats
  data.l$statistic <- gsub(pattern="max", replacement="Daily Maximum Temperature",
                             data.l$statistic, ignore.case = FALSE, fixed = FALSE)

  data.l$statistic <- gsub(pattern="sdadm", replacement="7DADM Temperature",
                             data.l$statistic, ignore.case = FALSE,fixed = FALSE)

  return(data.l)
}

calc.summary <- function(df) {
  # Function to calculate maximum, median, and minimum values for all dates by sim, constituent, statistic, and stream_km,
  # df input must be a dataframe in long format with the following column names:
  # Date (as character),
  # Stream_km (as numeric),
  # sim (as string),
  # constituent (as string),
  # statistic (as string)
  # value (as numeric)
  # Returns a dataframe

  data.summary <- as.data.frame(as.list(aggregate(df$value, by=c(list(df$Stream_km),
                                                                 list(df$sim),
                                                                 list(df$constituent),
                                                                 list(df$statistic)),
                                                  FUN = function(x) c(mn = min(x,na.rm=TRUE),
                                                                      med = median(x,na.rm=TRUE),
                                                                      mx = max(x,na.rm=TRUE)))))
  # fix colnames
  colnames(data.summary) <- c("Stream_km", "sim", "constituent", "statistic", "min", "median","max")

  # undo factors
  data.summary$sim <- as.character(data.summary$sim)
  data.summary$constituent <- as.character(data.summary$constituent)
  data.summary$statistic <- as.character(data.summary$statistic)

  return(data.summary)

}

