#---------------------------------------------------------------------------------------
# Import heat source 8 hourly temperature outputs from multiple simulations,
# calculate 7DADM, and plot the maximum and median 7DADM temperatures longitudinally
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

out_name <- "Smith_Creek_Scenarios"
out_dir <- "//path/to/model/directory/Results/All_Scenarios/"

# Name that goes on plot
sim1_name <- "Current Condition"
sim2_name <- "Restored Vegetation"
sim3_name <- "No Point Sources"
sim4_name <- "Wasteload Allocations"
sim5_name <- "Tributary Temperatures"
sim6_name <- "Natural Flow"
sim7_name <- "Background"
sim8_name <- "Biologically Based Criteria"

sim_hs_ver <- 8
sim_sheet <- NA_character_
sim_filename <- "Temp_H2O"

sim1_dir <- "//path/to/model/directory/1_CCC"
sim2_dir <- "//path/to/model/directory/3_VEG"
sim3_dir <- "//path/to/model/directory/5_NoPointSources"
sim4_dir <- "//path/to/model/directory/6_WLA"
sim5_dir <- "//path/to/model/directory/8_TRIBS"
sim6_dir <- "//path/to/model/directory/7_FLOW_CU00_NaturalFlow"
sim7_dir <- "//path/to/model/directory/9_BACKGROUND"


# Either "7DADM Temperature" or "Daily Maximum Temperature"
plot_stat <- "7DADM Temperature"

# The sims you want to include on the plot
plot_sims <- c(sim1_name, sim2_name, sim3_name, sim4_name, sim5_name, sim6_name, sim7_name, sim8_name)

legend_order <- c(sim1_name, sim2_name, sim3_name, sim4_name, sim5_name, sim6_name, sim7_name, sim8_name)

# the order of these values is the same as the plot_sim order
sim_colors <- setNames(c("blue", 
                         "dark green", 
                         "red",
                         "orange",
                         "violet",
                         "green",
                         "yellow",
                         "black"), 
                       plot_sims)

sim_linetype = setNames(c("solid", 
                          "solid",
                          "solid",
                          "solid",
                          "solid",
                          "solid",
                          "solid",
                          "dashed"), 
                        plot_sims)


sim_linesize <- setNames(c(1, 1, 1, 1, 1, 1, 1, 1), plot_sims)

#-  Import models --------------------------------------------------------------

df.sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim3 <- read.hs.outputs(output_dir = sim3_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim4 <- read.hs.outputs(output_dir = sim4_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim5 <- read.hs.outputs(output_dir = sim5_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim6 <- read.hs.outputs(output_dir = sim6_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)

df.sim7 <- read.hs.outputs(output_dir = sim7_dir, file_name = sim_filename,
                           hs_ver = sim_hs_ver, sheet_name = sim_sheet, 
                           sim_name = sim1_name)


#--  Read temps and calc 7dadm, summary stats ------------------------------------

data1 <- calc_7dadm(df.sim1)
data2 <- calc_7dadm(df.sim2)
data3 <- calc_7dadm(df.sim3)
data4 <- calc_7dadm(df.sim4)
data5 <- calc_7dadm(df.sim5)
data6 <- calc_7dadm(df.sim6)
data7 <- calc_7dadm(df.sim7)

# This builds a dataframe of when and where the BBNC apply. 
# Needs to be modified as necessary for the specific model location and temperature WQS.
data8 <- data3 %>%
  mutate(value = 18,
         sim = sim8_name)

df <- rbind(data1, data2, data3, data4, data5, data6, data7, data8) %>%
  filter(constituent == plot_stat) %>%
  drop_na(value)

# Calc max, median, and min
df.summary <- df %>%
  dplyr::group_by(constituent, model_km, sim) %>%
  dplyr::summarize(median = quantile(value, probs = c(.50), na.rm = TRUE),
                   min = min(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   range = max - min)


# Location of Maximum Temperature
df.pomi <- df %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(abs(value))) %>%
  dplyr::mutate(location = "POMI") %>%
  as.data.frame()

# Impact at most downstream node (outlet)
df.pomi <- df %>%
  dplyr::filter(model_km == min(model_km)) %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(abs(value))) %>%
  dplyr::mutate(location = "outlet") %>%
  as.data.frame() %>%
  rbind(df.pomi)

df.pomi

write_xlsx(x = df.pomi,
           path = file.path(out_dir, paste0(out_name,"_Summary.xlsx")))

#-- Longitudinal plot of maximum temperatures

p.max <- df.summary %>%
  filter(sim %in% plot_sims) %>%
  mutate(sim = factor(sim, levels = legend_order)) %>%
  ggplot(aes(x = model_km, y = max, color = sim, linetype = sim, size = sim)) +
  geom_line() +
  scale_color_manual(values = sim_colors) +
  scale_linetype_manual(values = sim_linetype) +
  scale_size_manual(values = sim_linesize) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_reverse(name = "Model Stream Kilometer") +
  scale_y_continuous(name = "Maximum 7DADM Temperature (deg-C)",
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,NA))

p.max


p.median <- df.summary %>%
  filter(sim %in% plot_sims) %>%
  mutate(sim = factor(sim, levels = legend_order)) %>%
  ggplot(aes(x = model_km, y = median, color = sim, linetype = sim, size = sim)) +
  geom_line() +
  scale_color_manual(values = sim_colors) +
  scale_linetype_manual(values = sim_linetype) +
  scale_size_manual(values = sim_linesize) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_reverse(name = "Model Stream Kilometer") +
  scale_y_continuous(name = "Median 7DADM Temperature (deg-C)",
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,NA))

p.median

ggsave(file = paste0(file.path(out_dir, out_name),"_Max_7DADM_Temps.png"),
       plot = p.max,
       height = h,
       width = w,
       units = "in")

ggsave(file = paste0(file.path(out_dir, out_name),"_Median_7DADM_Temps.png"),
       plot = p.medan,
       height = h,
       width = w,
       units = "in")