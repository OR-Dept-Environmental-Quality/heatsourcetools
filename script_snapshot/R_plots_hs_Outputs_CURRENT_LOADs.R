

#---------------------------------------------------------------------------------------
# Import heat source hourly temperature outputs from two simulations, 
# calculate 7DADM, calculae the difference, and plot longitudinal data
#---------------------------------------------------------------------------------------


library(ggplot2)
library(dplyr)

fun_dir <- "E:/GitHub/Rscripts/heatsource"

out_dir <-"T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/R/"

name <- "Jenny Creek"
bbnc <- 20

plot.label.a <- "a. Change 7DADM Temperature (deg-C)"
plot.label.b <- "b. Thermal Loading (kilocalories/day)"

sim1_name <- "1_CCC"
sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/1_CCC/"
sim1_file <- "HS7.Jenny.Crk.CCC.xlsm"
sim1_sheet = "Output - Temperature"

#sim1_name <- "3_2_PACFLOW"
#sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/3_2_PACFLOW/"
#sim1_file <- "HS7.Jenny.Crk.PACFLOW.xlsm"
#sim1_sheet = "Output - Temperature"

#sim2_name <- "2_VEG"
#sim2_file <- "HS7.Jenny.Crk.VEG.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/2_VEG/"
#sim2_sheet = "Output - Temperature"

#sim2_name <- "3_1_FLOW"
#sim2_file <- "HS7.Jenny.Crk.FLOW.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/3_1_FLOW/"
#sim2_sheet = "Output - Temperature"

#sim2_name <- "4_TRIBS_FLOW_TEMP"
#sim2_file <- "HS7.Jenny.Crk.TRIBS_FLOW_TEMP.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/4_TRIBS_FLOW_TEMP/"
#sim2_sheet = "Output - Temperature"

#sim2_name <- "5_MORPH"
#sim2_file <- "HS7.Jenny.Crk.MORPH.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/5_MORPH/"
#sim2_sheet = "Output - Temperature"

#sim2_name <- "7_VEG_FLOW_TRIBS"
#sim2_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/7_VEG_FLOW_TRIBS/"
#sim2_sheet = "Output - Temperature"

#sim2_name <- "8_VEG_FLOW_TRIBS_MORPH"
#sim2_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.MORPH.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/8_VEG_FLOW_TRIBS_MORPH/"
#sim2_sheet = "Output - Temperature"

# Total Excess Load
#sim2_name <- "0_TOTAL_EXCESS_LOAD"
#sim2_file <- "HS7.Jenny.Crk.BBNC_HUA.xlsx"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/9_BBNC_HUA_ONLY/"
#sim2_sheet = "Output - Temperature"

#plot.label.a <- "a. Excess 7DADM Temperature (deg-C)"
#plot.label.b <- "b. Excess Loading (kilocalories/day)"

# For calculating Background Excess Load [BBNC - anthro]

#sim1_name <- "8_VEG_FLOW_TRIBS_MORPH"
#sim1_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.MORPH.xlsm"
#sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/8_VEG_FLOW_TRIBS_MORPH/"
#sim1_sheet = "Output - Temperature"

#sim2_name <- "0_BACKGROUND"
#sim2_file <- "HS7.Jenny.Crk.BBNC_HUA.xlsx"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/9_BBNC_HUA_ONLY/"
#sim2_sheet = "Output - Temperature"

#-- HUA Attainment Scenarios 0.1 C increase in tribs

#sim1_name <- "10_HUA_ALLOCATION"
#sim1_file <- "HS7.Jenny.Crk.HUA_ALLOCATION4.xlsm"
#sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/10_HUA_ALLOCATION/"
#sim1_sheet = "Output - Temperature"

#sim2_name <- "8_VEG_FLOW_TRIBS_MORPH"
#sim2_file <- "HS7.Jenny.Crk.VEG.FLOW.TRIBS.MORPH.xlsm"
#sim2_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Jenny_Creek/8_VEG_FLOW_TRIBS_MORPH/"
#sim2_sheet = "Output - Temperature"


# Flow

flow1_dir <- sim1_dir
flow1_file <- sim1_file
flow1_sheet <- "Output - Flow Only"

flow2_dir <- sim2_dir
flow2_file <- sim2_file
flow2_sheet <- "Output - Flow Only"

skip_rows <- 16
hs_ver <- 7
constituent_name <- "Temperature"
statistic_name <- "Hourly"

plot.sims <- c(sim1_name,sim2_name)

source(paste0(fun_dir,"/","R_functions_hs_read_and_format.R"))

setwd(out_dir)
options(scipen = 999)

#--  Read temps and calc 7dadm ------------------------------------

data1 <- calc.7dadm2(sim1_dir, sim1_name, hs_ver=hs_ver, file_name = sim1_file, skip_rows=skip_rows, sheet_name = sim1_sheet)
data2 <- calc.7dadm2(sim2_dir, sim2_name, hs_ver=hs_ver, file_name = sim2_file, skip_rows=skip_rows, sheet_name = sim2_sheet)


flow.sim1 <- read.hs.outputs(output_dir=flow1_dir, 
                                file_name= flow1_file, 
                                constituent_name = "flow",
                                statistic_name = "hourly",
                                sim_name=sim1_name,
                                hs_ver=hs_ver,
                                sheet_name=flow1_sheet, skip_rows=skip_rows)

flow.sim2 <- read.hs.outputs(output_dir=flow2_dir, 
                                file_name= flow2_file, 
                                constituent_name = "flow",
                                statistic_name = "hourly",
                                sim_name=sim2_name,
                                hs_ver=hs_ver,
                                sheet_name=flow2_sheet, skip_rows=skip_rows)

flow.sim1 <- dplyr::rename(flow.sim1, flow.sim1=value)
flow.sim2 <- dplyr::rename(flow.sim2, flow.sim2=value)

#-- Change in 7DADM Temperatures ----------------------------

dt.df <- merge(data1, data2, by=c("Date", "Stream_km", "constituent", "statistic"), all=TRUE)

dt.df <- dt.df[dt.df$statistic =="7DADM Temperature",]

# Calculate the warming above criteria
#dt.df$value <- ifelse(dt.df$value.y > bbnc, dt.df$value.x - dt.df$value.y, 0)
dt.df$value <- dt.df$value.x - dt.df$value.y

dt.df <- dt.df[,c("Date","sim.y","Stream_km","constituent","statistic", "value")]

dt.df <- dt.df %>%
  dplyr::rename(sim=sim.y) %>%
  dplyr::mutate(sim=paste0("[",sim1_name," - ",sim2_name,"]"),
                statistic="Change 7DADM Temperature")

#--  Loading due to sources/changes made in sim 2 ------------------------------------

load.df <- dt.df[,c("Date","sim","Stream_km","constituent","statistic", "value")]

load.df <- merge(load.df,flow.sim1[,c("Date", "Stream_km", "flow.sim1")], by=c("Date", "Stream_km"))

load.df$constituent <- "Thermal Loading"

# loading, convert cms to cfs before calc
load.df$value <- (load.df$flow.sim1 * 35.3147 * load.df$value * 2446622)

# no negative excess loads (meets criteria)
#load.df$value <- ifelse(load.df$value <0, 0,load.df$value)

load.df$flow.sim1<- NULL

#-- combine data ------------------------------------

data0 <- rbind(data1,data2,dt.df, load.df)

# convert the character date back to POSIXct
data0$Datetime <- as.POSIXct(data0$Date,format="%m/%d/%Y") #6/17/2003

rm(data1,data2,dt.df, load.df)

#--  Calc Summary Stats ------------------------------------

# calc max, mean, min for all dates by sim, constituent, and stream_km
data0.summary <- calc.summary(df=data0)

# can't have negative loading values (does not exceed criterion)
data0.summary$min <- ifelse(data0.summary$min < 0, 0, data0.summary$min)
data0.summary$median <- ifelse(data0.summary$median < 0, 0, data0.summary$median)
data0.summary$max <- ifelse(data0.summary$max < 0, 0, data0.summary$max)

# pomi and mouth
pomi.df <- data0 %>%
  dplyr::group_by(sim, constituent, statistic) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(metric="POMI") %>%
  as.data.frame()

pomi.df <- data0 %>%
  dplyr::filter(Stream_km==min(Stream_km)) %>%
  dplyr::group_by(sim, constituent, statistic) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::mutate(metric="mouth") %>%
  as.data.frame() %>%
  rbind(pomi.df)

pomi.df

options(scipen = 0)
#-- Plot longitudinal summary for Loading and temp reduction ---------------

p.df <- data0.summary[(data0.summary$statistic %in% c("Change 7DADM Temperature") & data0.summary$sim==paste0("[",sim1_name," - ",sim2_name,"]")),]

p.df$facet.label <- ifelse(p.df$constituent == "Thermal Loading" , plot.label.b, plot.label.a)

p.tl <- ggplot(data=p.df, aes(x=Stream_km)) +
  geom_ribbon(aes(ymax = max, ymin = min, fill="Min and Max Range"), alpha = 0.6) + 
  geom_line(aes(y=median, linetype="Median")) +
  scale_linetype_manual(values=c("Median"="solid")) +
  scale_fill_manual(values="darkgrey") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.title.y =element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_line(colour = "lightgrey"),
        plot.title = element_text(size=12, hjust = 0.5)) +
  #scale_color_colorblind() +
  xlim(0,NA) +
  xlab("Distance from OR/CA Stateline (Kilometers)") +
  facet_wrap(~facet.label,nrow=2, scales = "free_y", strip.position="top")
  
p.tl 

ggsave(file=paste0(out_dir,name,"_LOADING_",sim2_name,".png"),
       plot=p.tl,
       height=6,
       width=10,
       units="in")
options(scipen = 999)

write.table(pomi.df,file=paste0(out_dir,name,"_LOADING_",sim2_name,".csv"), row.names = FALSE, sep=",")



