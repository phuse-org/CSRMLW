# Intent: This R script will read in the results prepared in SAS

# 00: Lib Prep
library(tidyverse)
library(vcdExtra)

# 01: Read in SAS results
sas_3ha <- haven::read_sas(here::here('cmh/results/SAS','result_part1.sas7bdat'))
sas_mh  <- haven::read_sas(here::here('cmh/results/SAS','result_part2.sas7bdat'))

# 02: Load R Results
load(file=here::here('cmh/results/R/r_3ha.Rdata'))
load(file=here::here('cmh/results/R/r_mhor.Rdata'))
