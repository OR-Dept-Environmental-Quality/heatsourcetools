
#---------------------------------------------------------------------------------------
# Import heat source effective shade output from current conditions, 
# restored conditions, and topographic only (no veg) model scenarios. Plot the 
# absolute results and calculate summary stats and output as an xlsx.
#---------------------------------------------------------------------------------------

library(heatsourcetools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

# Plot size for word (in)
h <- 3.5
w <- 6.75

# Plot size for ppt (in)
#h <- 5
#w <- 10

plot.date <- c("07/05/2001")
name <- "Jenny Creek"

sim1_name <- "Current Condition"
sim1_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/1_CCC/"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"

sim2_name <- "Restored Vegetation"
sim2_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/2_VEG/"
sim2_file <- "HS7.Jenny.Crk.VEG.xlsm"

sim3_name <- "Topographic"
sim3_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/0_TOPO/"
sim3_file <- "HS7.Jenny.Crk.TOPO.xlsm"

sheet_name = "Chart-Shade"
constituent_name <- "Effective Shade"

out_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek"


#--  Read data ------------------------------------

# Current
sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim1_file,
                        hs_ver = 7, sheet_name = sheet_name,
                        constituent_name = "Effective Shade",
                        sim_name = sim1_name)

# Restored
sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim2_file,
                        hs_ver = 7, sheet_name = sheet_name,
                        constituent_name = "Effective Shade",
                        sim_name = sim2_name)

# Topo
sim3 <- read.hs.outputs(output_dir = sim3_dir, file_name = sim3_file,
                        hs_ver = 7, sheet_name = sheet_name,
                        constituent_name = "Effective Shade",
                        sim_name = sim3_name)

# Combine, sort and make true percent
df.all <- rbind(sim1, sim2, sim3) %>%
  arrange(sim, -model_km, datetime) %>%
  mutate(value = value * 100)

df.change <- df.all %>%
  filter(date %in% plot.date) %>%
  spread(key = sim, value = value) %>%
  mutate(topo_range = get(sim2_name) - get(sim3_name),
         shade_gap = get(sim2_name) - get(sim1_name))

df.cfd <- df.all %>%
  filter(sim %in% c(sim1_name, sim2_name))

df.mean <- df.change %>%
  select(date, model_km, c(sim1_name), c(sim2_name), shade_gap, c(sim3_name)) %>%
  mutate(km_length=max(model_km)-min(model_km)) %>%
  group_by(date, km_length) %>%
  summarise_at(c(sim1_name, sim2_name, "shade_gap", sim3_name) , mean, na.rm=TRUE)

# longitudinal plot of ES for Heat Source
p.es <- df.all %>%
  filter(date %in% plot.date) %>%
  ggplot(aes(x = model_km)) +
  geom_line(aes(y = value, color = sim), size = 1) +
  geom_ribbon(data = df.change, aes(ymax = get(sim3_name), ymin = get(sim2_name), fill = "Disturbance Range"), alpha = 0.6) + 
  scale_color_manual(values = c("blue", "dark green", "black"),
                        labels = c(sim1_name, sim2_name, sim3_name)) +
  scale_fill_manual(values = "grey") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Model Kilometer") +
  ylab("Effective Shade %") + 
  ylim(0, 100) +
  facet_wrap(~date)
p.es

ggsave(file = file.path(out_dir, paste0(name, "_Effective_Shade_", gsub("/","_", plot.date),".png")),
       plot = p.es,
       height = h,
       width = w,
       units = "in")

# Output mean to xlsx
write_xlsx(df.mean, file = file.path(out_dir, paste0(name,"_Effective_Shade_Mean_",gsub("/","_", plot.date),".xlsx")))



