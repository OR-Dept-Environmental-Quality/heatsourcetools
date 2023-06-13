#-------------------------------------------------------------------------------
# Import observed TIR temperature data and heat source hourly temperature output.
# Calculate goodness of fit summary statistics, save as xlsx.
# Plot observed and model predictions longitudinally
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
out_name <- "Jenny_CCC_vs_TIR"

# The directory to save the plot output
out_dir <- "//path/to/plot/directory"

sim_name <- "Predictions"
sim_dir <- "//path/to/sim/model/directory"
sim_file <- "HS7.Jenny.Crk.CCC.xlsm"

obs_name <- "Observations"
obs_dir <- "//path/to/obs/directory"
obs_file <- "Jenny_Creek_observed_data.xlsx"

# Stations to compare with model predictions
obs_mlocs <- c("TIR")

# This function might need to be modified depending on the model version.
df.preds <- read.hs.outputs(output_dir = sim_dir, 
                            file_name = sim_file,
                            hs_ver = 7, 
                            sheet_name = "Output - Temperature",
                            sim_name = sim_name)

# Read observation data
# Select only calibration temperature sites
# Match the obs km to the preds km
df.obs <- read.obs(obs_dir = obs_dir, file_name = obs_file)

obs <- df.obs %>%
  filter(Monitoring.Location.ID %in% obs_mlocs &
           Characteristic.Name %in% c("Temperature, water")) %>%
  mutate(Result.Value = case_when(Result.Unit == "deg F" ~ (Result.Value * 9/5) + 32,
                                  TRUE ~ Result.Value),
         sim = obs_name,
         model_km = match_near(x = model_km, 
                               y = unique(df.preds$model_km),
                               tolerance = 0.1)) %>%
  rename(constituent = Characteristic.Name,
         value = Result.Value) %>%
  select(-Result.Unit)

# summarize the temperature obs monitoring sites for joining
obs.km.lookup <- obs %>%
  select(Monitoring.Location.ID, Monitoring.Location.Name, GNIS_Name, model_km) %>%
  distinct()

preds <- df.preds %>%
  left_join(obs.km.lookup, by = c("model_km"))

df.gof <- calc_gof(preds = preds, obs = obs, tolerance = 30)

write_xlsx(x = df.gof,
           path = file.path(out_dir, paste0(out_name,"_GOF.xlsx")))

#-- Plot longitudinal values --------------------------------------------------

df <- preds %>%
  filter(model_km %in% unique(obs$model_km)) %>%
  filter(!is.na(match_near(x = datetime, y = unique(obs$datetime), tolerance = minutes(30)))) %>%
  rbind(obs)

# set y axis plot limits
ymin <- 0
ymax <- heatsourcetools::round_any(max(df$value, na.rm = TRUE), 5, ceiling)

p.tir <- ggplot(data = df, aes(y = value, x = model_km, 
                               color = sim,
                               size = sim,
                               linetype = sim,
                               shape = sim)) +
  geom_line() + geom_point() +
  scale_color_manual(values = c("black", "red")) +
  scale_size_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c(NA, "solid")) +
  scale_shape_manual(values = c(16, NA)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.title = element_text(size = 12, hjust = 0.5),
        text = element_text(size = 10, family = "sans")) +
  xlab("Model Stream Kilometer") +
  ylab("Temperature (deg-C)") +
  scale_y_continuous(limits = c(ymin, ymax)) +
  scale_x_reverse(limits = c(NA, 0))

p.tir

ggsave(file = file.path(out_dir, paste0(out_name, ".png")),
       plot = p.tir,
       height = h,
       width = w,
       units = "in")