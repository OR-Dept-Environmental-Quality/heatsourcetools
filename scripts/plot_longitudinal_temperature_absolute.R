#---------------------------------------------------------------------------------------
# Import heat source hourly temperature outputs from multiple simulations,
# calculate 7DADM, and plot temperatures longitudinally
#---------------------------------------------------------------------------------------

library(heatsourcetools)
library(dplyr)
library(tidyr)
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
out_name <- "Thomas_All_Scenarios"

# The directory to save the plot output
out_dir <- "//path/to/plot/directory"

# Name that goes on plot and order of legend
sim1_name <- "Current Condition"
sim2_name <- "Restored Vegetation"
sim3_name <- "Natural Flow"
sim4_name <- "Restored Vegetation - Tribs"
sim5_name <- "Background"
sim6_name <- "Biologically Based Criteria"

sim_colors <- c("Current Condition" = "blue", 
                "Restored Vegetation" = "dark green", 
                "Natural Flow" = "red",
                "Restored Vegetation - Tribs" = "orange",
                "Background" = "violet",
                "Biologically Based Criteria" = "black")

sim_linetype <- c("Current Condition" = "solid", 
                  "Restored Vegetation" = "solid",
                  "Natural Flow" = "solid",
                  "Restored Vegetation - Tribs" = "solid",
                  "Background" = "solid",
                  "Biologically Based Criteria" = "dashed")


sim1_dir <- "//path/to/sim1/model/directory"
sim2_dir <- "//path/to/sim2/model/directory"
sim3_dir <- "//path/to/sim3/model/directory"
sim4_dir <- "//path/to/sim4/model/directory"
sim5_dir <- "//path/to/sim5/model/directory"

sim1_file <- "Thomas.HS6.CCC.FINAL.xls"
sim2_file <- "Thomas.HS6.VEG.FINAL.xls"
sim3_file <- "Thomas.HS6.VEG.FLOW.FINAL.xls"
sim4_file <- "Thomas.HS6.VEG.TRIBS.FINAL.xls"
sim5_file <- "Thomas.HS6.BACKGROUND.FINAL.xls"


# Either "7DADM Temperature" or "Daily Maximum Temperature"
plot_stat <- "Daily Maximum Temperature"

plot_sims <- c(sim1_name, sim2_name, sim3_name, sim4_name, sim5_name, sim6_name)

#--  Read temps and calc 7dadm ------------------------------------

df.sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim1_file,
                           hs_ver = 6, sheet_name = "Long Temp Output",
                           sim_name = sim1_name)

df.sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim2_file,
                           hs_ver = 6, sheet_name = "Long Temp Output",
                           sim_name = sim2_name)

df.sim3 <- read.hs.outputs(output_dir = sim3_dir, file_name = sim3_file,
                           hs_ver = 6, sheet_name = "Long Temp Output",
                           sim_name = sim3_name)

df.sim4 <- read.hs.outputs(output_dir = sim4_dir, file_name = sim4_file,
                           hs_ver = 6, sheet_name = "Long Temp Output",
                           sim_name = sim4_name)

df.sim5 <- read.hs.outputs(output_dir = sim5_dir, file_name = sim5_file,
                           hs_ver = 6, sheet_name = "Long Temp Output",
                           sim_name = sim5_name)

data1 <- calc_7dadm(df.sim1)
data2 <- calc_7dadm(df.sim2)
data3 <- calc_7dadm(df.sim3)
data4 <- calc_7dadm(df.sim4)
data5 <- calc_7dadm(df.sim5)

# This builds a dataframe of when and where the BBNC apply. 
# Need to be modified as necessary for the specific model location and temperature WQS.
data6 <- data1 %>%
  mutate(value = case_when(model_km < 30.3271 ~ 18,
                           TRUE ~ 16),
         sim = sim6_name)

df <- rbind(data1, data2, data3, data4, data5, data6) %>%
  filter(constituent == plot_stat) %>%
  pivot_wider(names_from = "sim", values_from = "value") %>%
  drop_na(!!sim1_name, !!sim2_name, !!sim3_name, !!sim4_name, !!sim5_name, BBNC) %>%
  rename(sim1 = !!sim1_name, sim2 = !!sim2_name, sim3 = !!sim3_name, 
         sim4 = !!sim4_name, sim5 = !!sim5_name) %>%
  mutate(Change = if_else(sim2 >= BBNC, sim2 - sim1, NA_real_)) %>%
  pivot_longer(cols = all_of(c("sim1", "sim2", "sim3", "sim4", "sim5", "BBNC", "Change")), 
               names_to = "sim", values_to = "value") %>%
  mutate(sim = case_when(sim == "sim1" ~ sim1_name,
                         sim == "sim2" ~ sim2_name,
                         sim == "sim3" ~ sim3_name,
                         sim == "sim4" ~ sim4_name,
                         sim == "sim5" ~ sim5_name,
                         sim == "BBNC" ~ sim6_name,
                         TRUE ~ sim))

df <- rbind(data1, data2, data3, data4, data5, data6) %>%
  filter(constituent == plot_stat) %>%
  pivot_wider(names_from = "sim", values_from = "value") %>%
  drop_na(!!sim1_name, !!sim2_name, !!sim3_name, !!sim4_name, !!sim5_name, !!sim6_name) %>%
  pivot_longer(cols = all_of(plot_sims), 
               names_to = "sim", values_to = "value")

# set y axis plot limits
ymin <- 0
ymax <- heatsourcetools::round_any(max(df$value, na.rm = TRUE), 5, ceiling)

p.T <- df %>%
  filter(sim %in% plot_sims) %>%
  mutate(sim = factor(sim, levels = plot_sims)) %>%
  ggplot(aes(x = model_km, y = value, color = sim, linetype = sim)) +
  geom_line() +
  scale_color_manual(values = sim_colors) +
  scale_linetype_manual(values = sim_linetype) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.title = element_text(size = 12, hjust = 0.5),
        text = element_text(size = 10, family = "sans")) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(ymin, ymax), breaks = scales::pretty_breaks(n = 5)) +
  xlab("Model Stream Kilometer") +
  ylab("Daily Maximum Temperature (deg-C)")

p.T

ggsave(file = paste0(file.path(out_dir,out_name),"_Absolute_Temps.png"),
       plot = p.dT,
       height = h,
       width = w,
       units = "in")


