
library(tidyr)
library(ggplot2)

# plot height and width
h <- 4.25
w <- 6.25

fun_dir <- "E:/GitHub/Rscripts/heatsource"

name <- "Jenny Creek"
bbnc <- 20.3

sim1_name <- "1_CCC"
sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/1_CCC/"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"
sim1_sheet = "Output - Temperature"

hs_ver <- 7

out_dir <-"T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/R/"

obs_file <- "Miller.Observed.Data.ext.csv"
obs_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/"


fun_dir <- "E:/GitHub/Rscripts/heatsource"
source(paste0(fun_dir,"/","R_functions_hs_read_and_format.R"))

setwd(out_dir)

#--  Read temps ------------------------------------

sim1 <- read.hs.outputs(output_dir=sim1_dir, 
                        file_name=sim1_file, 
                        constituent_name = "Temperature (deg-C)",
                        statistic_name="Hourly",
                        sim_name=sim1_name,
                        hs_ver=hs_ver,
                        sheet_name=sheet1_name)


# Read observation data
obs.raw <- read.obs(obs_dir=obs_dir, file_name=obs_file)

obs.tir <- obs.raw %>%
  dplyr::filter(constituentCode == "STEMPG") %>%
  dplyr::select(-constituentCode,-siteName,-notes) %>%
  dplyr::mutate(sim="TIR",
                hour=as.integer(format(Datetime, "%H"))) %>%
  dplyr::select(colnames(sim1))

df  <- sim1 %>%
  dplyr::filter(hour==unique(obs.tir$hour) & Date==unique(obs.tir$Date)) %>%
  rbind(obs.tir) %>%
  dplyr::filter(Stream_km >= 5.15)

p.tir <- ggplot(data=df, aes(y=value, x=Stream_km, color=sim, size=sim)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("Current Condition"="blue","TIR"="black"),
                     labels=c("Current Condition"="Model Results","TIR"="TIR")) +
  scale_size_manual(values=c("Current Condition"=1,"TIR"=1.5),
                    labels=c("Current Condition"="Model Results","TIR"="TIR")) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle(paste0(name, " July 17, 2001 14:00 - 14:17 PDT")) +
  xlim(0,NA) +
  xlab("Distance from Mouth (Kilometers)") +
  ylab("Temperature (deg-C)")
p.tir

ggsave(file=paste0(out_dir,name,"_TIR_CCC.png"),
       plot=p.tir,
       height=h,
       width=w,
       units="in")

