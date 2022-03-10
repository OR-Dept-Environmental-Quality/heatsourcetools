
#---------------------------------------------------------------------------------------
# Import heat source effective shade output from two scenarios. Plot the 
# absolute results, the difference (sim2 - sim1), and calculate summary stats and output as an xlsx.
#---------------------------------------------------------------------------------------

library(heatsourcetools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)

# Plot size for word (in)
h <- 3.5
w <- 6.75

# Plot size for ppt (in)
#h <- 5
#w <- 10

plot.date <- c("07/05/2001")
out_name <- "CCC-RC"
name <- "Jenny Creek"

# The directory to save plot outputs and the summary xlsx.
out_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7"

sim1_name <- "Current Condition"
sim1_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/1_CCC"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"

sim2_name <- "Restored Vegetation"
sim2_dir <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7/2_VEG"
sim2_file <- "HS7.Jenny.Crk.VEG.xlsm"

sheet_name = "Chart-Shade"
constituent_name <- "Effective Shade"


#--  Read data ------------------------------------

sim1 <- read.hs.outputs(output_dir = sim1_dir, file_name = sim1_file,
                        hs_ver = 7, sheet_name = sheet_name,
                        constituent_name = constituent_name,
                        sim_name = sim1_name)


sim2 <- read.hs.outputs(output_dir = sim2_dir, file_name = sim2_file,
                        hs_ver = 7, sheet_name = sheet_name,
                        constituent_name = constituent_name,
                        sim_name = sim2_name)

# Combine, sort and make true percent
df.all <- rbind(sim1, sim2) %>%
  arrange(sim, -model_km, datetime) %>%
  mutate(value = value * 100)

df.diff <- df.all %>%
  filter(date %in% plot.date) %>%
  pivot_wider(names_from = sim, values_from = value) %>%
  mutate(shade_difference = get(sim2_name) - get(sim1_name),
         facet = plot.date)

df.mean <- df.diff %>%
  select(date, model_km, all_of(sim1_name), all_of(sim2_name), shade_difference) %>%
  mutate(km_length=max(model_km)-min(model_km)) %>%
  group_by(date, km_length) %>%
  summarise_at(c(sim1_name, sim2_name, "shade_difference") , mean, na.rm=TRUE)

df.plot <- df.diff %>%
  #mutate(shade_difference2 = shade_difference) %>%
  pivot_longer(cols = all_of(c(sim1_name, sim2_name, "shade_difference")), 
               names_to = "sim", values_to = "value") %>%
  mutate(facet = case_when(sim == "shade_difference" ~ "Difference", 
                           TRUE ~ plot.date))

ymax_diff <- heatsourcetools::round_any(x = max(df.diff$shade_difference, na.rm = TRUE), 
                                        accuracy = 5, f = ceiling)

# longitudinal plot of absolute effective shade
p.es1 <- df.all %>%
  filter(date %in% plot.date) %>%
  ggplot(aes(x = model_km)) +
  geom_line(aes(y = value, color = sim), size = 1) +
  geom_ribbon(data = df.diff, aes(ymax = get(sim2_name), ymin = get(sim1_name), fill = "Difference"), alpha = 0.6) + 
  scale_color_manual(values = c("blue", "dark green"),
                     labels = c(sim1_name, sim2_name)) +
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
p.es1

p.es2 <- df.diff %>%
  filter(date %in% plot.date) %>%
  ggplot(aes(x = model_km, y = shade_difference)) +
  geom_line(color = "black", size = 1) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Model Kilometer") +
  ylab("Effective Shade Difference") + 
  ylim(0, ymax_diff)
p.es2


# longitudinal plot of effective shade difference
p.es3 <- df.plot %>%
  filter(date %in% plot.date) %>%
  ggplot(aes(x = model_km)) +
  geom_line(aes(y = value, color = sim), size = 1) +
  geom_ribbon(data = df.diff, aes(ymax = get(sim2_name), ymin = get(sim1_name), fill = "Shade Deficit"), alpha = 0.6) + 
  scale_color_manual(values = c("blue", "dark green", "black"),
                     labels = c(sim1_name, sim2_name, "Shade Difference")) +
  scale_fill_manual(values = "grey") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Model Kilometer") +
  ylab("Effective Shade") + 
  ylim(0, NA) +
  facet_wrap(~facet, nrow = 2, ncol = 1, scales = "free_y")
p.es3

ggsave(file = file.path(out_dir, paste0(name, "_Effective_Shade_", out_name, "_", gsub("/","_", plot.date),".png")),
       plot = p.es1,
       height = h,
       width = w,
       units = "in")

ggsave(file = file.path(out_dir, paste0(name, "_Effective_Shade_", out_name,"_diff_", gsub("/","_", plot.date),".png")),
       plot = p.es2,
       height = h,
       width = w,
       units = "in")


ggsave(file = file.path(out_dir, paste0(name, "_Effective_Shade_", out_name,"_facet_", gsub("/","_", plot.date),".png")),
       plot = p.es3,
       height = h,
       width = w,
       units = "in")


# Output mean to xlsx
write_xlsx(df.mean, path = file.path(out_dir, paste0(name,"_Effective_Shade_Mean_", out_name, "_", gsub("/","_", plot.date),".xlsx")))



