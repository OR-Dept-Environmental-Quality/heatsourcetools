
#---------------------------------------------------------------------------------------
# Import heat source effective shade output from current condtions, restored condtions,
# and topo. Plot the absolute results and change from current to restored.
#---------------------------------------------------------------------------------------

#library(reshape2)
library(tidyr )
library(dplyr)
library(ggplot2)
library(readxl)
#library(zoo)
library(TMDLMrkdwn)

plot.date <- c("07/05/2001")
name <- "Jenny Creek"

sim1_name <- "Current Condition"
sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/1_CCC/"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"

sim2_name <- "Restored Vegetation"
sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/2_VEG/"
sim2_file <- "HS7.Jenny.Crk.VEG.xlsm"

sim3_name <- "Topographic"
sim3_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/0_TOPO/"
sim3_file <- "HS7.Jenny.Crk.TOPO.xlsm"

sheet_name = "Chart-Shade"
constituent_name <- "Effective Shade"
statistic_name <- "Percent"

fun_dir <- "E:/GitHub/Rscripts/heatsource"
out_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/R/"

source(paste0(fun_dir,"/","R_functions_hs_read_and_format.R"))

#setwd(out_dir)


#--  Read data ------------------------------------

# Current
sim1 <- read.hs7.shade(output_dir=sim1_dir, file_name=sim1_file, 
                        constituent_name="Effective Shade",
                        statistic_name="Percent",
                        sim_name=sim1_name,
                        sheet_name=sheet_name, skip_rows=12)

# Restored
sim2 <- read.hs7.shade(output_dir=sim2_dir, file_name=sim2_file,
                        constituent_name="Effective Shade",
                        statistic_name="Percent",
                        sim_name=sim2_name,
                        sheet_name=sheet_name, skip_rows=12)

# Topo
sim3 <- read.hs7.shade(output_dir=sim3_dir, file_name=sim3_file,
                        constituent_name="Effective Shade",
                        statistic_name="Percent",
                        sim_name=sim3_name,
                        sheet_name=sheet_name, skip_rows=12)

sim1$value <- sim1$value*100
sim2$value <- sim2$value*100
sim3$value <- sim3$value*100

# sort by Stream_km and Date
sim1 <- sim1[with(sim1, order(-Stream_km,Datetime)), ]
sim2 <- sim2[with(sim2, order(-Stream_km,Datetime)), ]
sim3 <- sim3[with(sim3, order(-Stream_km,Datetime)), ]

df.all <- rbind(sim1,sim2,sim3)

df.change <- df.all %>%
  filter(Date %in% plot.date) %>%
  spread(key=sim,value=value) %>%
  mutate(topo_range=get(sim2_name)-get(sim3_name),
         shade_gap=get(sim2_name)-get(sim1_name))

df.cfd <- df.all %>%
  filter(sim %in% c(sim1_name, sim2_name))

df.mean <- df.change %>%
  dplyr::select(Date, Stream_km, c(sim1_name), c(sim2_name), shade_gap, c(sim3_name)) %>%
  dplyr::mutate(km_length=max(Stream_km)-min(Stream_km)) %>%
  dplyr::group_by(Date, km_length) %>%
  dplyr::summarise_at(c(sim1_name, sim2_name, "shade_gap", sim3_name) , mean, na.rm=TRUE) %>%
  dplyr::mutate_at(vars(sim1_name, sim2_name, "shade_gap", sim3_name), round, 2)

# longitudinal plot of ES for Heat Source
p.es <- df.all %>%
  filter(Date %in% plot.date) %>%
  ggplot(aes(x=Stream_km)) +
  geom_line(aes(y=value, color=sim),size=1) +
  geom_ribbon(data=df.change, aes(ymax = get(sim3_name), ymin = get(sim2_name), fill="Disturbance Range"), alpha = 0.6) + 
  #guides(color=guide_legend(override.aes=list(linetype=c(0,1))))  +
  scale_color_manual(values=c("blue","dark green","black"),
                        labels=c(sim1_name, sim2_name, sim3_name)) +
  scale_fill_manual(values="grey") +
  scale_x_reverse() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Distance from OR/CA Stateline (Kilometers)") +
  ylab("Effective Shade %") + 
  ylim(0, 100) +
  facet_wrap(~Date)
p.es

p.cfd <- ggplot(data = df.all) +
  stat_ecdf(ggplot2::aes(x = value, color =sim, linetype = sim ),
            geom = "line",
            size = 1,
            na.rm = TRUE) +
  scale_color_manual(values=c("blue","dark green","black"),
                     labels=c(sim1_name, sim2_name, sim3_name)) +
  scale_linetype_manual(values=rep(c(1, 2, 3))) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_line(colour = "lightgrey"),
        plot.title = element_text(size=12, hjust = 0)) +
  scale_y_continuous(breaks=seq(0,1,0.2),
                     limits = c(0,1),
                     expand = c(0,0)) +
  labs(x = "Effective Shade (Percent)",
       y = "Quantile")

p.cfd <- ggplot(data = df.change) +
  stat_ecdf(ggplot2::aes(x = shade_gap),
            geom = "line",
            size = 1,
            na.rm = TRUE) +
  scale_color_manual(values=c("black"),
                     labels=c("Effective Shade Gap")) +
  scale_linetype_manual(values=rep(c(1))) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_line(colour = "lightgrey"),
        plot.title = element_text(size=12, hjust = 0)) +
  scale_y_continuous(breaks=seq(0,1,0.2),
                     limits = c(0,1),
                     expand = c(0,0)) +
  labs(x = "Effective Shade Gap (Percent)",
       y = "Quantile")
p.cfd 

ggsave(file=paste0(out_dir,name,"_Effective_Shade_",gsub("/","_", plot.date),".png"),
       plot=p.es,
       height=4,
       width=6.75,
       units="in")

# Output mean csv table
write.csv(df.mean, file=paste(out_dir,name,"_Effective_Shade_Mean_",gsub("/","_", plot.date),".csv",sep=""),row.names = FALSE)


