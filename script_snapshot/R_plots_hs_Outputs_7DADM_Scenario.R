

#---------------------------------------------------------------------------------------
# Import heat source hourly temperature outputs from multiple simulations, 
# calculate min, median, and maximum 7DADM, plot logitudinally.
#---------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(viridis)

fun_dir <- "E:/GitHub/Rscripts/heatsource"

#plot size for word
h<-4
w<-6.75

#plot size for ppt
#h<-5
#w<-10


name <- "Jenny Creek"
bbnc <- 20

sim0_name <- "0 TOPO"
sim0_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/0_TOPO/"
sim0_file <- "HS7.Jenny.Crk.TOPO.xlsm"
sim0_sheet = "Output - Temperature"

sim1_name <- "1 CCC"
sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/1_CCC/"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"
sim1_sheet = "Output - Temperature"

sim2_name <- "2 VEG"
sim2_file <- "HS7.Jenny.Crk.VEG.xlsm"
sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/2_VEG/"
sim2_sheet = "Output - Temperature"

sim31_name <- "3.1 FLOW"
sim31_file <- "HS7.Jenny.Crk.FLOW.xlsm"
sim31_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/3_1_FLOW/"
sim31_sheet = "Output - Temperature"

sim32_name <- "3.2 PACFLOW"
sim32_file <- "HS7.Jenny.Crk.PACFLOW.xlsm"
sim32_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/3_2_PACFLOW/"
sim32_sheet = "Output - Temperature"

sim4_name <- "4 TRIBS"
sim4_file <- "HS7.Jenny.Crk.TRIBS_FLOW_TEMP.xlsm"
sim4_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/4_TRIBS_FLOW_TEMP/"
sim4_sheet = "Output - Temperature"

sim5_name <- "5 MORPH"
sim5_file <- "HS7.Jenny.Crk.MORPH.xlsm"
sim5_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/5_MORPH/"
sim5_sheet = "Output - Temperature"

sim6_name <- "6 VEGFLOW"
sim6_file <- "HS7.Jenny.Crk.NTP.xlsm"
sim6_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/6_VEG_FLOW_NTP/"
sim6_sheet = "Output - Temperature"

sim7_name <- "7 VEGFLOW TRIBS"
sim7_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.xlsm"
sim7_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/7_VEG_FLOW_TRIBS/"
sim7_sheet = "Output - Temperature"

sim8_name <- "8 RC"
sim8_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.MORPH.xlsm"
sim8_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/8_VEG_FLOW_TRIBS_MORPH/"
sim8_sheet = "Output - Temperature"

skip_rows <- 16
hs_ver <- 7
constituent_name <- "Temperature"
statistic_name <- "Hourly"

out_dir <-"T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/R/"

source(paste0(fun_dir,"/","R_functions_hs_read_and_format.R"))

setwd(out_dir)
options(scipen = 999)

#--  Read temps and calc 7dadm ------------------------------------

data0 <- calc.7dadm2(sim0_dir, sim0_name, hs_ver=hs_ver, file_name = sim0_file, skip_rows=skip_rows, sheet_name = sim0_sheet)
data1 <- calc.7dadm2(sim1_dir, sim1_name, hs_ver=hs_ver, file_name = sim1_file, skip_rows=skip_rows, sheet_name = sim1_sheet)
data2 <- calc.7dadm2(sim2_dir, sim2_name, hs_ver=hs_ver, file_name = sim2_file, skip_rows=skip_rows, sheet_name = sim2_sheet)
data31 <- calc.7dadm2(sim31_dir, sim31_name, hs_ver=hs_ver, file_name = sim31_file, skip_rows=skip_rows, sheet_name = sim31_sheet)
data32 <- calc.7dadm2(sim32_dir, sim32_name, hs_ver=hs_ver, file_name = sim32_file, skip_rows=skip_rows, sheet_name = sim32_sheet)
data4 <- calc.7dadm2(sim4_dir, sim4_name, hs_ver=hs_ver, file_name = sim4_file, skip_rows=skip_rows, sheet_name = sim4_sheet)
data5 <- calc.7dadm2(sim5_dir, sim5_name, hs_ver=hs_ver, file_name = sim5_file, skip_rows=skip_rows, sheet_name = sim5_sheet)
data6 <- calc.7dadm2(sim6_dir, sim6_name, hs_ver=hs_ver, file_name = sim6_file, skip_rows=skip_rows, sheet_name = sim6_sheet)
data7 <- calc.7dadm2(sim7_dir, sim7_name, hs_ver=hs_ver, file_name = sim7_file, skip_rows=skip_rows, sheet_name = sim7_sheet)
data8 <- calc.7dadm2(sim8_dir, sim8_name, hs_ver=hs_ver, file_name = sim8_file, skip_rows=skip_rows, sheet_name = sim8_sheet)

#-- combine data ------------------------------------

df <- rbind(data0,data1,data2,data31,data32,data4,data5,data6,data7,data8)

# convert the character date back to POSIXct
df$Datetime <- as.POSIXct(df$Date,format="%m/%d/%Y") #6/17/2003

rm(data0,data1,data2,data31,data32,data4,data5,data6,data7,data8)

#--  Calc Summary Stats ------------------------------------

# calc max, mean, min for all dates by sim, constituent, and stream_km
df.summary <- calc.summary(df=df)

# pomi and mouth
pomi.df <- df %>%
  dplyr::group_by(sim, constituent, statistic) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(metric="POMI") %>%
  as.data.frame()

pomi.df <- df %>%
  dplyr::filter(Stream_km==min(Stream_km)) %>%
  dplyr::group_by(sim, constituent, statistic) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(metric="Stateline") %>%
  as.data.frame() %>%
  rbind(pomi.df) %>%
  dplyr::mutate(value=round(value, 2))

pomi.df

options(scipen = 0)
#-- Plot longitudinal summary for Loading and temp reduction ---------------

# Color Blind palette
cbbPalette <- c(rgb(0, 0, 0, maxColorValue=255),
                rgb(0, 73, 73, maxColorValue=255),
                rgb(0, 146, 146, maxColorValue=255),
                rgb(255, 109, 182, maxColorValue=255),
                rgb(255, 182, 119, maxColorValue=255),
                rgb(73, 0, 146, maxColorValue=255),
                rgb(0, 109, 219, maxColorValue=255),
                rgb(182, 109, 255, maxColorValue=255),
                rgb(109, 182, 255, maxColorValue=255),
                rgb(182, 219, 255, maxColorValue=255),
                rgb(146, 0, 0, maxColorValue=255),
                rgb(146, 73, 0, maxColorValue=255))
                
p.df <- df.summary %>%
  filter(statistic %in% c("7DADM Temperature"))

p.1 <- ggplot(data=p.df, aes(x=Stream_km, y=max, color=sim)) +
  geom_line(size=1.5,  alpha = .8) +
  geom_hline(yintercept=20.3, size=1, linetype="dashed") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_line(colour = "lightgrey"),
        plot.title = element_text(size=12, hjust = 0.5)) +
  scale_colour_manual(values=cbbPalette, guide = guide_legend()) +
  #scale_color_viridis(discrete=TRUE, option="cividis") +
  guides(colour=guide_legend(nrow=2)) +
  ylab("Max 7DADM Temperature (deg-C)") +
  xlab("Distance from CA/OR Stateline (kilometers)") +
  ylim(10,NA)
  
p.1 

ggsave(file=paste0(out_dir,name,"_SCENARIOS_7DADM.png"),
       plot=p.1,
       height=h,
       width=w,
       units="in")
options(scipen = 999)

write.table(pomi.df,file=paste0(out_dir,name,"_SCENARIOS_7DADM.csv"), row.names = FALSE, sep=",")



