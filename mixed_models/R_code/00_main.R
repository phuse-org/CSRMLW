####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Main run of MMRM models in R for PHUSE WG
#-------------------------------------------------------------------------------

#' Change this if you want to save the R image and corresponding .csv files
save_output <- TRUE
path_nm <- "/XXX/XXX"

#' Where to save the results?
dir.create(file.path(path_nm, "data/R"), showWarnings = TRUE)

#' Read in some packages & source utility functions
library(readr)
library(tidyverse)
library(nlme)
library(lme4)
library(emmeans)
library(purrr)
source(paste0(path_nm, "/99_utility_functions.R"))

#' Read in example data
adlbh <- haven::read_xpt(paste0(path_nm, "/adlbh.xpt"))

#' Run data wrangle prep - note steps are specific to the adlbh.xpt file
source(paste0(path_nm, "/01_data_prep.R"))

#' Source the \code{nlme::gls} specification file
source(paste0(path_nm, "/02_gls_spec.R"))

#' Run the prepared data through the \code{nlme::gls} run file
source(paste0(path_nm, "/03_gls_run.R"))

#' Run the prepared data through the \code{lme4::lmer} run file
source(paste0(path_nm, "/04_lmer_run.R"))

gls_out <- wrangle_output(gls_fits)
lmer_out <- wrangle_output(lmer_fits)

#' Save tables
if(save_output){
  gls_out %>%
    names(.) %>%
    walk(~ write_csv(gls_out[[.]], paste0(path_nm, "/data/R/gls_", ., ".csv")))
}

if(save_output){
  lmer_out %>%
    names(.) %>%
    walk(~ write_csv(lmer_out[[.]], paste0(path_nm,"/data/R/lmer_", ., ".csv")))
}
