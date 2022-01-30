#-------------------------------------------------------------------------------
# Import observed temperature data and heat source hourly temperature output.
# Plot observed and model prediction (hourly and 7DADM), generate
# goodness of fit summary statistics
#-------------------------------------------------------------------------------

library(heatsourcetools)
library(dplyr)
library(lubridate)
library(ggplot2)

out_name <- "Jenny_01_CCC"
out_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek"

# Name that goes on plot
sim_name <- "Predictions"
obs_name <- "Observations"

sim_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7"
obs_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek"

sim_file <- "HS7.Jenny.Crk.CCCdx200.xlsm"
obs_file <- "Jenny_Creek_observed_data.xlsx"

obs_mlocs <- c("BXON", "BXOS", "LWRX")

df.preds <- read.hs.outputs(output_dir = sim_dir, file_name = sim_file,
                            hs_ver = 7, sheet_name = "Output - Temperature",
                            sim_name = sim_name)

# Read obs
# Select only calibration temperature sites
# Match the obs kn to the preds km
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

df <- preds %>%
  filter(model_km %in% unique(obs$model_km)) %>%
  rbind(obs)


# set y axis plot limits
round_any = function(x, accuracy, f = round) {f(x / accuracy) * accuracy}

ymin <- round_any(min(df$value, na.rm = TRUE), 5, floor)
ymax <- round_any(max(df$value, na.rm = TRUE), 5, ceiling)

xmin <- round_date(min(preds$datetime), unit = "day")
xmax <- round_date(max(preds$datetime), unit = "day")

i <- 1

for (i in 1:length(obs.km.lookup$model_km)) {

  # The actual model stream kilometer
  skm <- obs.km.lookup$model_km[i]
  mlocID <- obs.km.lookup$Monitoring.Location.ID[i]
  p.title <- paste0(obs.km.lookup$Monitoring.Location.ID[i], ": ",
                   obs.km.lookup$Monitoring.Location.Name[i])

  # Hourly plot model predicted vs observed temperatures
  p1 <- df %>%
    filter(model_km == skm & constituent == "Temperature, water") %>%
    ggplot(aes(x = datetime, y = value, size = sim,
               linetype = sim, colour = sim)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c("solid","solid")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    xlab("Date") +
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m-%d-%Y",
                     limits = c(xmin, xmax)) +
    ylab("Hourly Temperature (deg-C)") +
    labs(title = p.title,
         subtitle = paste0("Model Kilometer ", skm)) +
    theme(text = element_text(size = 10)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())

  ggsave(filename = file.path(out_dir, paste0(out_name, "_hourly_km_",skm,"_",mlocID,".png")),
         plot = p1,
         height = 3.5,
         width = 6.75,
         units = "in")

  # Daily Maximum Temperature
  p2 <- df %>%
    filter(model_km == skm & constituent == "Daily Maximum Temperature") %>%
    ggplot(aes(x = datetime, y = value, colour = sim, size = sim, linetype = sim)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c("solid","solid")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    xlab("Date") +
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m-%d-%Y",
                     limits = c(xmin, xmax)) +
    ylab("Daily Maximum Temperature (deg-C)") +
    labs(title = p.title,
         subtitle = paste0("Model Kilometer ", skm)) +
    theme(text = element_text(size = 10)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())

  ggsave(filename = file.path(out_dir,paste0(out_name,"_7DADM_km_",skm,"_",mlocID,".png")),
         plot = p2,
         height = 3.5,
         width = 6.75,
         units = "in")
}
