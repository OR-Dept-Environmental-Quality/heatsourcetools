#---------------------------------------------------------------------------------------
# Plot heat source 7 land cover model input parameters including:

# Channel Width (m)
# Topographic Angles
# Mean Landcover height (m)
# Mean Landcover density (%)
#---------------------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

# plot height and width
h <- 4.25
w <- 6.25

name <- "Miller Creek"

sim1_name <- "Current Condition"
sim1_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Miller_Creek/1_CCC/"
sim1_file <- "HS7.miller.creek.solar.extent.CCC.xlsm"

fun_dir <- "E:/GitHub/heatsource-9/post_processing"
out_dir <- "T:/TMDL_ER/Klamath_and_Lost_Rivers/Heatsource/Miller_Creek/R/"

source(paste0(fun_dir,"/","R_functions_hs_read_and_format.R"))


#--  Read data ------------------------------------

lcdata.raw <- read.hs7.landcover(output_dir=sim1_dir, file_name=sim1_file, sim_name=sim1_name)

morph.raw <- read.hs7.morphology(output_dir=sim1_dir, file_name=sim1_file, sim_name=sim1_name)


#---------------------------------------------------------------------------------------
# Channel Width

morph.width <- morph.raw %>%
  dplyr::select(Stream_km, Bankfull_Width, Bottom_Width, AngleZ) %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value))

p.wid <- morph.width %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5),
        axis.title.y=element_blank()) +
  ggtitle(name) +
  xlab("Distance from Mouth (Kilometers)") +
  ylim(0,NA) +
  facet_wrap(~legend, ncol = 1, scales = "free_y",
             labeller = as_labeller(c("Bankfull_Width"="Bankfull Width (m)", "Bottom_Width"="Bottom Width (m)", "AngleZ"="Channel Angle z (m/m)")))
p.wid

ggsave(file=paste0(out_dir,name,"_Morh_ChanWidth_",sim1_name,".png"),
       plot=p.wid,
       height=h+1,
       width=w,
       units="in")

#---------------------------------------------------------------------------------------
# Topo

lcdata.topo <- lcdata.raw %>%
  dplyr::select(Stream_km, topo_w, topo_s, topo_e) %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value))

p.topo <- lcdata.topo %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Distance from Mouth (Kilometers)") +
  ylab("Topographic Shade Angle (degrees)") + 
  facet_wrap(~legend, ncol = 1, labeller = as_labeller(c("topo_w"="West", "topo_s"="South", "topo_e"="East")))
p.topo

ggsave(file=paste0(out_dir,name,"_Topo_",sim1_name,".png"),
       plot=p.topo,
       height=h,
       width=w,
       units="in")

#---------------------------------------------------------------------------------------
# Height

lcdata.ht <- lcdata.raw %>%
  dplyr::select(Stream_km,height_l, height_r) %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value))
    
  # longitudinal plot of ES for Heat Source
p.ht <- lcdata.ht %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle(name) +
  xlab("Distance from Mouth (Kilometers)") +
  ylab("Mean Land Cover Height (m)") + 
  ylim(0,NA) + 
  facet_wrap(~legend, ncol = 1, labeller = as_labeller(c("height_l"="Left Bank", "height_r"="Right Bank")))
p.ht

ggsave(file=paste0(out_dir,name,"_LC_Height_",sim1_name,".png"),
       plot=p.ht,
       height=h,
       width=w,
       units="in")

#---------------------------------------------------------------------------------------
# Canopy

lcdata.ca <- lcdata.raw %>%
  dplyr::select(Stream_km,density_l, density_r) %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value))

p.ca <- lcdata.ca %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  scale_y_continuous(limits=c(0,1), labels = scales::percent_format(accuracy = 1)) +
  ggtitle(name) +
  xlab("Distance from Mouth (Kilometers)") +
  ylab("Mean Land Cover Density (%)") +
  facet_wrap(~legend, ncol = 1, labeller = as_labeller(c("density_l"="Left Bank", "density_r"="Right Bank")))
p.ca

ggsave(file=paste0(out_dir,name,"_LC_Density_",sim1_name,".png"),
       plot=p.ca,
       height=h,
       width=w,
       units="in")

#---------------------------------------------------------------------------------------
# Elevation and Gradient

lcdata.z <- lcdata.raw %>%
  dplyr::select(Stream_km, Elevation)

morph.z <- morph.raw %>%
  dplyr::select(Stream_km, Gradient) %>%
  dplyr::left_join(lcdata.z, by="Stream_km") %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value))

# longitudinal plot of ES for Heat Source
p.z <- morph.z %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5),
        axis.title.y=element_blank()) +
  ggtitle(name) +
  xlab("Distance from Mouth (Kilometers)") +
  facet_wrap(~legend, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c("Elevation"="Stream Elevation (m)", "Gradient"="Gradient (m/m)")))
p.z

ggsave(file=paste0(out_dir,name,"_Morph_Elevation_Gradient_",sim1_name,".png"),
       plot=p.z,
       height=h,
       width=w,
       units="in")

#---------------------------------------------------------------------------------------
# Mannings n


morph.mann <- morph.raw %>%
  dplyr::select(Stream_km, Mannings_n) %>%
  tidyr::gather(key="legend",value="value", -Stream_km) %>%
  dplyr::mutate(Stream_km=as.numeric(Stream_km),
                value=as.numeric(value)) %>%
  dplyr::filter(Stream_km >= 5.15)

# longitudinal plot of ES for Heat Source
p.mann <- morph.mann  %>%
  ggplot(aes(x=Stream_km, y=value)) +
  geom_line(size=1) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        strip.background =element_rect(fill="white", colour = "black"),
        panel.grid.major =element_blank(),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle(name) +
  xlim(0,NA) +
  ylab("Mannings n") +
  xlab("Distance from Mouth (Kilometers)")
p.mann

ggsave(file=paste0(out_dir,name,"_Morph_Mannings_n_",sim1_name,".png"),
       plot=p.mann,
       height=3.25,
       width=w,
       units="in")
