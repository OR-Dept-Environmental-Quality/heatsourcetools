#---------------------------------------------------------------------------------------
# Import heat source hourly temperature outputs from two simulations,
# calculate 7DADM, calculate the change, and plot longitudinal data
#---------------------------------------------------------------------------------------

library(heatsourcetools)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(writexl)

bbnc <- 20

out_name <- "Jenny_01_CCC"
out_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek"

# Name that goes on plot
sim1_name <- "Jenny Creek hs7"
sim2_name <- "Jenny Creek hs9"

sim1_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7"
sim2_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs9/outputs"

sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"
sim2_file <- "Temp_H2O.csv"

# Either "7DADM Temperature" or "Daily Maximum Temperature"
plot_stat <- "Daily Maximum Temperature"

plot.sims <- c(sim1_name, sim2_name)

df.sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim1_file,
                            hs_ver = 7, sheet_name = "Output - Temperature",
                            sim_name = "sim1")

df.sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim2_file,
                           hs_ver = 9, sheet_name = NA,
                           sim_name = "sim2")


#--  Read temps and calc 7dadm ------------------------------------

data1 <- calc_7dadm(df.sim1) #%>% rbind(df.sim1)
data2 <- calc_7dadm(df.sim2) #%>% rbind(df.sim2)

df <- rbind(data1, data2) %>%
  pivot_wider(names_from = "sim", values_from = "value") %>%
  drop_na(sim1, sim2) %>%
  mutate(Change = sim2 - sim1) %>%
  pivot_longer(cols = all_of(c("sim1", "sim2", "Change")), names_to = "sim", values_to = "value")

rm(data1, data2)

#--  Calc Summary Stats ------------------------------------

# Calc max, median, and min
df.summary <- df %>%
  group_by(constituent, datetime, date, model_km, sim) %>%
  summarise(min = min(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            range = max - min)

# Point of Maximum Impact
df.pomi <- df %>%
  dplyr::mutate(value = round(value, 2)) %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(abs(value))) %>%
  dplyr::mutate(metric = "POMI") %>%
  as.data.frame()

# Impact at most downstream node (outlet)
df.pomi <- df %>%
  dplyr::filter(model_km == min(model_km)) %>%
  dplyr::mutate(value = round(value, 2)) %>%
  dplyr::group_by(sim, constituent) %>%
  dplyr::slice(which.max(abs(value))) %>%
  dplyr::mutate(metric = "outlet") %>%
  as.data.frame() %>%
  rbind(df.pomi)

df.pomi

write_xlsx(x = df.pomi,
           path = file.path(out_dir, paste0(out_name,"_POMI.xlsx")))

#-- Plot longitudinal summary for each simulation and difference between them

# set y axis plot limits
round_any = function(x, accuracy, f = round) {f(x / accuracy) * accuracy}

ymin <- df.summary %>%
  filter(sim == "Change" & constituent == plot_stat) %>%
  slice(which.min(min)) %>%
  pull(min) %>%
  round_any(accuracy = 5, f = floor)

ymax <- df.summary %>%
  filter(sim %in% c("sim1", "sim2") & constituent == plot_stat) %>%
  slice(which.max(max)) %>%
  pull(max) %>%
  round_any(accuracy = 5, f = ceiling)

# longitudinal plot of dT summary

p.dT <- df.summary %>%
  filter(sim == "Change" & constituent == plot_stat) %>%
  ggplot(aes(x = model_km)) +
  geom_ribbon(aes(ymax = max, ymin = min, fill = "Range"), alpha = 0.6) +
  geom_line(aes(y = median, linetype = "Median Change")) +
  geom_line(aes(y = 0.3, linetype = "HUA")) +
  scale_linetype_manual(values = c("Median Change" = "solid",
                                   "HUA allocation" = "dashed")) +
  scale_fill_manual(values = "skyblue") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  xlab("Model Stream Kilometer") +
  ylab("Change 7DADM (deg-C)") +
  ylim(0, ymax)
  #facet_wrap(~sim+constituent, nrow = 2)
p.dT

ggsave(file = file.path(out_dir, paste0(out_name,"_dT_7DADM.png")),
       plot = p.dT,
       height = 3,
       width = 6.75,
       units = "in")

# longitudinal plot of each simulation
p.sims <-  df.summary %>%
  filter(sim %in% c("sim1", "sim2") & constituent == plot_stat) %>%
  ggplot(aes(x = model_km)) +
  #geom_ribbon(aes(ymax = max, ymin = min, color = sim, fill = sim), alpha = 0.6) +
  #geom_ribbon(aes(ymax = max, ymin = min, color  =sim, fill = "Min & Max Range"), alpha = 0.6) +
  geom_line(aes(y = median, linetype = "Median", color = sim)) +
  geom_line(aes(y = bbnc, linetype = "Applicable Criteria")) +
  scale_linetype_manual(values = c("Median" = "solid", "Applicable Criteria" = "dashed")) +
  #scale_fill_manual(values = c("grey65","grey15")) +
  scale_color_manual(values = c("grey65","grey15")) +
  #scale_fill_manual(values = c("#FFC20A","#0C7BDC")) +
  #scale_color_manual(values = c("#FFC20A","#0C7BDC")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  xlab("Model Stream Kilometer") +
  ylab("7DADM Temperature(C)") +
  ylim(0, ymax)

p.sims

ggsave(file=paste0(out_dir,name,"_SIM_SUMM_7DADM.png"),
       plot=p.sim.summ ,
       height=5,
       width=6.75,
       units="in")

