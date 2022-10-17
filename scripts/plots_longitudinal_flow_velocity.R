#-------------------------------------------------------------------------------
# Import heat source model flow velocity. Plot the data on a
# longitudinal plot
#-------------------------------------------------------------------------------

library(heatsourcetools)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


# Plot size for word (in)
h <- 3.5
w <- 6.75

# Plot size for ppt (in)
#h <- 5
#w <- 10

# Name to use on the output plot
out_name <- "Jenny_01_CCC"

# The directory to save the plot output
out_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/"

# directory to the model files
sim_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7"
obs_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek"

sim_file <- "HS7.Jenny.Crk.CCCdx200.xlsm"
obs_file <- "Jenny_Creek_observed_data.xlsx"

# The flow monitoring location IDs from the obs_file to compare to the model flows
obs_mlocs <- c("BXON", "BXOS", "JNYM", "LWRX")


df.preds <- read.hs.outputs(output_dir = sim_dir, file_name = sim_file,
                            hs_ver = 7, sheet_name = "Output - Hydraulics",
                            constituent_name = "Flow Velocity",
                            sim_name = "Predictions")

# Need to fill NAs. Sometimes most downstream km doesn't have a flow.
df.preds <- df.preds %>%
  arrange(constituent, datetime, -model_km) %>%
  group_by(constituent, date) %>%
  fill(value, .direction = "down") %>%
  ungroup()

# Calculate the daily mean flow rate, convert to cfs
df.preds <- df.preds %>%
  group_by(sim, constituent, datetime, date, model_km) %>%
  ungroup()

df.obs <- read.obs(obs_dir = obs_dir, file_name = obs_file) %>%
  filter(Monitoring.Location.ID %in% obs_mlocs &
           Characteristic.Name %in% c("Flow Velocity")) %>%
  mutate(model_km = match_near(x = model_km,
                               y = unique(df.preds$model_km),
                               tolerance = 0.1)) %>%
  rename(constituent = Characteristic.Name,
         obs = Result.Value) %>%
  select(-Result.Unit)

# Only use observed data for the same time period as the model
df.obs <- df.obs %>%
  filter(datetime >= min(df.preds$datetime) & datetime <= max(df.preds$datetime))

plot.dates <- unique(df.obs$date)

# filter predictions to just dates in obs
df.preds <- df.preds %>%
  filter(date  %in% plot.dates)

df <- df.preds %>%
  left_join(df.obs) %>%
  mutate(date = factor(date))

p.flow <- ggplot(data = df, aes(x = model_km)) +
  geom_line(aes(y = preds, color = "Model")) +
  geom_point(aes(y = obs, color = "Observations"), size = 2.0) +
  guides(color = guide_legend(override.aes = list(shape = c(16, NA),
                                                  linetype = c(0, 1))))  +
  scale_colour_manual(values = c("Observations" = "Black", "Model" = "red")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlab("Model Stream Kilometer") +
  ylab("Flow Rate (cfs)") +
  facet_wrap(~date, nrow = length(plot.dates), strip.position = "top")

ggsave(file = file.path(out_dir, paste0(out_name, "_flow.png")),
       plot = p.flow,
       height = h,
       width = w,
       units = "in")
