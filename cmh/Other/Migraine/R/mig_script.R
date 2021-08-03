library(tidyverse)
library(magrittr)
library(epiDisplay)
library(vcdExtra)

# data
migraine <- tribble(~Gender, ~Treatment,  ~Response,  ~Freq,
               'female', 'Active',  'Better', 16,
               'female', 'Active',  'Same',   11,
               'female', 'Placebo', 'Better',  5,
               'female', 'Placebo', 'Same',   20,
               'male',   'Active',  'Better', 12,
               'male',   'Active',  'Same',   16,
               'male',   'Placebo', 'Better',  7,
               'male',   'Placebo', 'Same',   19
)


# vcdExtra
mig_vcdExtra_results <- CMHtest(Freq~Treatment+Response|Gender, data=migraine, overall=TRUE, details=TRUE )$ALL$table %>%
  as.data.frame() %>%
  rownames_to_column(., "Hypothesis")

# epiDisplay
migraine %>%
  uncount(weights = Freq) %$%
  capture.output(mhor(Treatment,Response,Gender, graph = FALSE)) %>%
  as.data.frame() %>%
  rename(., col1 = .) %>%
  filter(str_detect(col1 , "M-H|Homogeneity test")) -> mig_epiDisplay_results

# base
migraine %>%
  uncount(weights = Freq) %$%
  mantelhaen.test(x = Treatment, y = Response, z = Gender) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(., col1 = .) %>%
  tibble::rownames_to_column(var = "statistic") -> mig_base_results

migraine %>%
  uncount(weights = Freq) %$%
  mantelhaen.test(x = Treatment, y = Response, z = Gender, correct = FALSE) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(., col1 = .) %>%
  tibble::rownames_to_column(var = "statistic") -> mig_base_results_uncorrected

# save
save.image(here::here('cmh/Other/migraine/R', 'mig_results.Rdata'))

# load
load(here::here('cmh/Other/migraine/R', 'mig_results.Rdata'))
