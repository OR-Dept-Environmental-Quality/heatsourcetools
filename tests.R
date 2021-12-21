

library(heatsourcetools)
library(dplyr)
library(ggplot2)

dir_hs9 <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs9/outputs"
dir_hs8 <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs8"
dir_hs7 <- "C:/workspace/GitHub/heatsource-9/tests/Jenny_Creek/hs7"

df9.temp <- read.hs.outputs(output_dir =dir_hs9, file_name="Temp_H2O", hs_ver = 9)
df8.temp <- read.hs.outputs(output_dir =dir_hs8, file_name="Temp_H2O", hs_ver = 8)
df7.temp <- read.hs.outputs(output_dir =dir_hs7, file_name="HS7.Jenny.Crk.CCCdx200.xlsm", hs_ver = 7, sheet_name = "Output - Temperature")

df7.temp <- read.hs7.outputs(output_dir =dir_hs7, file_name="HS7.Jenny.Crk.CCCdx200.xlsm",sheet_name = "Output - Temperature")

df9.shade <- read.hs.outputs(output_dir =dir_hs9, file_name="Shade", hs_ver = 9)
df8.shade <- read.hs.outputs(output_dir =dir_hs8, file_name="shade", hs_ver = 8)
df7.shade <- read.hs.outputs(output_dir =dir_hs7, file_name="HS7.Jenny.Crk.CCCdx200.xlsm", hs_ver = 7, sheet_name = "Chart-Shade")

df7.shade <- read.hs7.shade(output_dir =dir_hs7, file_name="HS7.Jenny.Crk.CCCdx200.xlsm", sheet_name = "Chart-Shade")

df9$sim <- "hs9b26"
df8$sim <- "hs808"


df.temp <- df8.temp %>%
  dplyr::left_join(y=df9.temp, by=c("Datetime", "Stream_km")) %>%
  dplyr::mutate(diff = value.y - value.x,
                date=format(Datetime, format="%m-%d-%Y")) %>%
  dplyr::group_by(date, Stream_km) %>%
  dplyr::summarise(max_diff=diff)

df.shade <- df8.shade %>%
  dplyr::left_join(y=df9.shade, by=c("Datetime", "Stream_km")) %>%
  dplyr::mutate(diff = value.y - value.x,
                date=format(Datetime, format="%m-%d-%Y")) %>%
  dplyr::group_by(date, Stream_km) %>%
  dplyr::summarise(max_diff=diff)

p1 <- ggplot(df.temp, aes(x=Stream_km, y=max_diff, color=date)) +
  geom_point()
p1

p2 <- ggplot(df.shade, aes(x=Stream_km, y=max_diff, color=date)) +
  geom_point()
p2