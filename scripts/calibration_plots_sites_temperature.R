#-------------------------------------------------------------------------------
# Import observed temperature data and heat source hourly temperature output.
# Calculate goodness of fit summary statistics, save as xlsx.
# Plot observed and model predictions (hourly and daily maximum) at each site
#-------------------------------------------------------------------------------

library(heatsourcetools)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)

# Plot size for word (in)
h <- 3.5
w <- 6.75

# Plot size for ppt (in)
#h <- 5
#w <- 10

# File name for output plot
out_name <- "Jenny_CCC_vs_Obs"

# The directory to save the plot output
out_dir <- "//path/to/plot/directory"

sim_name <- "Predictions"
sim_dir <- "//path/to/sim/model/directory"
sim_file <- "HS7.Jenny.Crk.CCC.xlsm"

obs_name <- "Observations"
obs_dir <- "//path/to/obs/directory"
obs_file <- "Jenny_Creek_observed_data.xlsx"

# Stations to compare with model predictions
obs_mlocs <- c("BXON", "BXOS", "LWRX")

# This function might need to be modified depending on the model version.
df.preds <- read.hs.outputs(output_dir = sim_dir, 
                            file_name = sim_file,
                            hs_ver = 7, 
                            sheet_name = "Output - Temperature",
                            sim_name = sim_name)

# Read obs
# Select only calibration temperature sites
# Match the obs km to the preds km
df.obs <- read.obs(obs_dir = obs_dir, file_name = obs_file) %>%
  filter(Monitoring.Location.ID %in% obs_mlocs &
           Characteristic.Name %in% c("Temperature, water")) %>%
  mutate(Result.Value = case_when(Result.Unit == "deg F" ~ (Result.Value * 9/5) + 32,
                                  TRUE ~ Result.Value),
         sim = obs_name,
         model_km = match_near(x = model_km, y = unique(df.preds$model_km),
                               tolerance = 0.1)) %>%
  rename(constituent = Characteristic.Name,
         value = Result.Value) %>%
  select(-Result.Unit)

# summarize the temperature obs monitoring sites for joining
obs.km.lookup <- df.obs %>%
  select(Monitoring.Location.ID, Monitoring.Location.Name, GNIS_Name, model_km) %>%
  distinct()

obs <- calc_7dadm(df.obs) %>%
  left_join(obs.km.lookup, by = c("model_km")) %>%
  bind_rows(df.obs)

preds <- calc_7dadm(df.preds) %>%
  bind_rows(df.preds) %>%
  inner_join(obs.km.lookup, by = c("model_km")) %>%
  arrange(constituent, -model_km, datetime)

df.gof <- calc_gof(preds = preds, obs = obs, tolerance = 30)

write_xlsx(x = df.gof,
           path = file.path(out_dir, paste0(out_name,"_GOF.xlsx")))

df <- preds %>%
  filter(model_km %in% unique(obs$model_km)) %>%
  rbind(obs)


ymin <- 0
ymax <- heatsourcetools::round_any(max(preds$value, na.rm = TRUE), 5, ceiling)

xmin <- floor_date(min(preds$datetime), unit = "day")
xmax <- ceiling_date(max(preds$datetime), unit = "day")

for (i in 1:length(obs.km.lookup$model_km)) {

  # The actual model stream kilometer
  skm <- obs.km.lookup$model_km[i]
  mlocID <- obs.km.lookup$Monitoring.Location.ID[i]
  p.title <- paste0(obs.km.lookup$Monitoring.Location.ID[i], ": ",
                   obs.km.lookup$Monitoring.Location.Name[i])

  # Hourly plot model predicted vs observed temperatures
  p1 <- df %>%
    filter(model_km == skm & constituent == "Temperature, water") %>%
    ggplot(aes(x = datetime, y = value, 
               size = sim,
               linetype = sim, 
               color = sim,
               shape = sim)) +
    geom_line() + geom_point() + 
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c(NA, "solid")) +
    scale_shape_manual(values = c(16, NA)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m-%d-%Y",
                     limits = c(xmin, xmax)) +
    labs(title = p.title,
         subtitle = paste0("Model Kilometer ", skm)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "lightgrey"),
          plot.title = element_text(size = 12, hjust = 0)) +
    xlab("Date") +
    ylab("Hourly Temperature (deg-C)")

  ggsave(filename = file.path(out_dir, paste0(out_name, "_hourly_km_",skm,"_",mlocID,".png")),
         plot = p1,
         height = h,
         width = w,
         units = "in")

  # Daily Maximum Temperature
  p2 <- df %>%
    filter(model_km == skm & constituent == "Daily Maximum Temperature") %>%
    ggplot(aes(x = datetime, y = value, 
               color = sim,
               size = sim,
               linetype = sim,
               shape = sim)) +
    geom_line() + geom_point() +
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c(NA, "solid")) +
    scale_shape_manual(values = c(16, NA)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    xlab("Date") +
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m-%d-%Y",
                     limits = c(xmin, xmax)) +
    ylab("Daily Maximum Temperature (deg-C)") +
    labs(title = p.title,
         subtitle = paste0("Model Kilometer ", skm)) +
    #theme(text = element_text(size = 10)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "lightgrey"),
          plot.title = element_text(size = 12, hjust = 0))

  ggsave(filename = file.path(out_dir,paste0(out_name,"_DailyMax_km_",skm,"_",mlocID,".png")),
         plot = p2,
         height = h,
         width = w,
         units = "in")
}
