


format.outputs <- function(df.wide) {

  df.wide$constituent <- constituent_name
  df.wide$statistic <- statistic_name
  df.wide$sim <- sim_name

  # Convert data from wide to long
  df.long <- melt(df.wide, id.vars =c("Datetime","sim", "constituent","statistic"),variable.name=c("stream_km"))
  colnames(df.long) <- c("Datetime","sim", "constituent","statistic","stream_km","value")

  df.long$stream_km <- as.numeric(gsub(pattern="X", replacement="",
                                      df.long$stream_km, ignore.case = FALSE,fixed = FALSE))


  df.long$Datetime <-round_date(as.POSIXct((df.long$Datetime*60*60*24), origin="1899-12-30", tz="GMT"), unit = "minute")

  df.long$date <- format(df.long$Datetime,"%m/%d/%Y")

  df.long$hour <-as.integer(format(df.long$Datetime, "%H"))
}
