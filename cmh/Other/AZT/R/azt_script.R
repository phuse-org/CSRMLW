library(tidyverse)
library(magrittr)
library(epiDisplay)
library(vcdExtra)

# data
azt <- tribble(~Race, ~AZT,  ~Symptoms,  ~Freq,
               'white',  'azt',    'yes', 14, 
               'white',  'azt',    'no',  93,
               'white',  'no-azt', 'yes', 32, 
               'white',  'no-azt', 'no',  81,
               'black',  'azt',    'yes', 11, 
               'black',  'azt',    'no',  52,
               'black',  'no-azt', 'yes', 12, 
               'black',  'no-azt', 'no',  43 
               )

# vcdExtra
vcdExtra_results <- CMHtest(Freq~AZT+Symptoms|Race, data=azt, overall=TRUE, details=TRUE )$ALL$table %>%
  as.data.frame() %>%
  rownames_to_column(., "Hypothesis")

# epiDisplay
azt %>%
  uncount(weights = Freq) %$%
  capture.output(mhor(AZT,Symptoms,Race, graph = FALSE)) %>%
  as.data.frame() %>%
  rename(., col1 = .) %>%
  filter(str_detect(col1 , "M-H|Homogeneity test")) -> epiDisplay_results

# base
azt %>%
  uncount(weights = Freq) %$%
  mantelhaen.test(x = AZT, y = Symptoms, z = Race) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(., col1 = .) %>%
  tibble::rownames_to_column(var = "statistic") -> base_results

# save
# save.image(file = "C:/cmh/azt/r/results.Rdata")

# load
load('C:/cmh/azt/r/results.Rdata')