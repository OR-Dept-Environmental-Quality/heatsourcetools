# heatsourcetools
 R package to assist post processing of heat source models

## Install

```R
devtools::install_github("DEQrmichie/heatsourcetools", host = "https://api.github.com", 
                         dependencies = TRUE, force = TRUE, upgrade = "never")
```
## Example

```R
library(heatsourcetools)
library(dplyr)

dir_hs9 <- "C:/workspace/GitHub/tests/Jenny_Creek/hs9/outputs"
dir_hs8 <- "C:/workspace/GitHub/tests/Jenny_Creek/hs8"
dir_hs7 <- "C:/workspace/GitHub/tests/Jenny_Creek/hs7"
dir_hs6 <- "C:/workspace/GitHub/tests/Jenny_Creek/hs6"

# Read the temperature output from various models. Data is returned in long format
df.hs9temp <- read.hs.outputs(output_dir = dir_hs9, file_name = "Temp_H2O", hs_ver = 9)
df.hs8temp <- read.hs.outputs(output_dir = dir_hs8, file_name = "Temp_H2O", hs_ver = 8)
df.hs7temp <- read.hs.outputs(output_dir = dir_hs7, file_name = "HS7.Jenny.Crk.xlsm", 
                              hs_ver = 7, sheet_name = "Output - Temperature")
df.hs6temp <- read.hs.outputs(output_dir = dir_hs6, file_name = "HS6.Jenny.Crk.xlsm",
                              hs_ver = 6, sheet_name = "Long Temp Output")

# Calc 7DADM

df.hs8.7dadm <- calc_7dadm(df = df.hs8temp)
```