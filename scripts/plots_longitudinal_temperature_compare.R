#---------------------------------------------------------------------------------------
# Import heat source hourly temperature outputs from two simulations,
# Calculate 7DADM, calculate the change (sim2 - sim1), and plot the maximum change in 7DADM
# when sim2 is >= the temperature WQS.
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
out_name <- "Jenny_VEG_vs_CCC"

HUA_allocation <- 0.10

# The directory to save the plot output
out_dir <- "//path/to/plot/directory"

sim1_name <- "Restored Vegetation"
sim1_dir <- "//path/to/sim1/model/directory"
sim1_file <- "HS7.Jenny.Crk.VEG.xlsm"
sim1_hs_ver <- 7
sim1_sheet <- "Output - Temperature"

sim2_name <- "Current Condition"
sim2_dir <- "//path/to/sim2/model/directory"
sim2_file <- "HS7.Jenny.Crk.CCC.xlsm"
sim2_hs_ver <- 7
sim2_sheet <- "Output - Temperature"

# Used for Oregon temperature WQS/HUA attainment assessment. 
# Need to filter out days that attain criteria (sim2 <= BBNC)
sim3_name <- "BBNC"

# Either "7DADM Temperature" or "Daily Maximum Temperature"
plot_stat <- "7DADM Temperature"

plot.sims <- c(sim1_name, sim2_name)

# These functions might need to be modified depending on the model version.
df.sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim1_file,
                            hs_ver = sim1_hs_ver, sheet_name = sim1_sheet,
                            sim_name = sim1_name)

df.sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim2_file,
                           hs_ver = sim2_hs_ver, sheet_name = sim2_sheet,
                           sim_name = sim2_name)


#--  Read temps and calc 7dadm ------------------------------------
data1 <- calc_7dadm(df.sim1)
data2 <- calc_7dadm(df.sim2)

# This builds a dataframe of when and where the BBNC apply. 
# Need to modify as necessary for the specific model location and temperature WQS.
data3 <- data1 %>%
  mutate(value = case_when(model_km <= 42.25 ~ 18.0,
                           model_km > 42.25 & datetime >= mdy_hms("8/15/2016 00:00:00") ~ 13.0,
                           TRUE ~ 16.0),
         sim = sim3_name)

df <- rbind(data1, data2, data3) %>%
  pivot_wider(names_from = "sim", values_from = "value") %>%
  drop_na(!!sim1_name, !!sim2_name, !!sim3_name) %>%
  rename(sim1 = !!sim1_name, sim2 = !!sim2_name) %>%
  mutate(Change = if_else(sim2 >= BBNC, sim2 - sim1, NA_real_)) %>%
  pivot_longer(cols = all_of(c("sim1", "sim2", "BBNC", "Change")), 
               names_to = "sim", values_to = "value") %>%
  mutate(sim = case_when(sim == "sim1" ~ sim1_name,
                         sim == "sim2" ~ sim2_name,
                         TRUE ~ sim))

rm(data1, data2)

#--  Calc Summary Stats ------------------------------------

# Calc max, median, and min
df.summary <- df %>%
  group_by(constituent, model_km, sim) %>%
  summarise(p95 = quantile(value, probs = c(.95), na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# Point of Maximum Impact
df.pomi <- df %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(location = "POMI") %>%
  dplyr::filter(sim == "Change") %>%
  as.data.frame()

# Impact at most downstream node (outlet)
df.pomi <- df %>%
  dplyr::filter(model_km == min(model_km)) %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(location = "outlet") %>%
  as.data.frame() %>%
  rbind(df.pomi)

df.pomi

write_xlsx(x = df.pomi,
           path = file.path(out_dir, paste0(out_name,"_POMI.xlsx")))

#-- Plot longitudinal summary for each simulation and difference between them

# set y axis plot limits
y <- filter(df.summary, sim == "Change" & constituent == plot_stat)
ymin <- heatsourcetools::round_any(min(y$min, na.rm = TRUE), accuracy = 0.5 , f = floor)
ymax <- heatsourcetools::round_any(max(y$max, na.rm = TRUE), accuracy = 0.5 , f = ceiling)
rm(y)

# longitudinal plot of dT summary
p.dT <- df.summary %>%
  filter(sim == "Change" & constituent == plot_stat) %>%
  ggplot(aes(x = model_km)) +
  geom_line(aes(y = max, linetype = "Maximum change")) +
  geom_line(aes(y = HUA_allocation, linetype = "HUA Allocation")) +
  scale_linetype_manual(values = c("Maximum change" = "solid", 
                                   "HUA Allocation" = "dashed")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.title = element_text(size = 12, hjust = 0.5),
        text = element_text(size = 10, family = "sans")) +
  xlab("Model Stream Kilometer") +
  ylab("Change 7DADM (deg-C)") +
  ylim(ymin, ymax) +
  scale_x_reverse(limits = c(NA, 0))

p.dT

ggsave(file = file.path(out_dir, paste0(out_name,"_dT.png")),
       plot = p.dT,
       height = h,
       width = w,
       units = "in")

