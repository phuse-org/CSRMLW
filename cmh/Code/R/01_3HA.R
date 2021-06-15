# Intent: This R script will use the vcdExtra package (CRAN version) to run and extract:
# 1) Chi-Square test statistic, 2) degrees of freedom, 3) p-value
# for each of the 3 alternative hypothesis for the CMH test.

# 00: Lib Prep
library(tidyverse)
library(here)
library(vcdExtra)
library(xlsx)

# 01: Data Prep

# This has been downloaded and saved, no need to do this again
# adcibc <- haven::read_xpt("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adcibc.xpt") %>%
#   filter(EFFFL == 'Y' & ITTFL == 'Y', AVISITN == 8 & ANL01FL=='Y')
#
# save(adcibc, file = here::here("cmh/Data/R", "adcibc_filtered.Rdata"))


# load saved data
load(file = here::here('cmh/Data/R','adcibc_filtered.Rdata'))

# change data to character
adcibc <- adcibc %>%
  mutate(across(where(is.numeric), as.character))

# 02: Schema Prep
# Schema 1
s1data <- adcibc %>%
  filter(AGEGR1 != "<65" & TRTP != "Xanomeline High Dose") %>%
  count(TRTP, SEX, AGEGR1) %>%
  dplyr::select(Freq = n, k = AGEGR1, x = TRTP, y = SEX) %>%
  mutate(Schema = 1)

# Schema 2
s2data <- adcibc %>%
  filter(RACE != "AMERICAN INDIAN OR ALASKA NATIVE" & TRTP != "Xanomeline High Dose") %>%
  count(TRTP, SEX, RACE) %>%
  dplyr::select(Freq = n, x = TRTP, y = SEX, k = RACE) %>%
  mutate(Schema = 2)

# Schema 3
s3data <- adcibc %>%
  count(TRTP, SEX, RACE) %>%
  dplyr::select(Freq = n, x = TRTP, y = SEX, k = RACE) %>%
  mutate(Schema = 3)

# Schema 4
s4data <- adcibc %>%
  filter(TRTP != "Xanomeline High Dose") %>%
  count(TRTP, SEX, SITEID) %>%
  dplyr::select(Freq = n, x = TRTP, y = SEX, k = SITEID) %>%
  mutate(Schema = 4)

# Schema 5
s5data <- adcibc %>%
  count(TRTP, SEX, SITEID) %>%
  dplyr::select(Freq = n, x = TRTP, y = SEX, k = SITEID) %>%
  mutate(Schema = 5)

# Schema 6
s6data <- adcibc %>%
  filter(TRTP != "Xanomeline High Dose") %>%
  count(TRTP, SEX, AVAL) %>%
  dplyr::select(Freq = n, x = TRTP, y = SEX, k = AVAL) %>%
  mutate(Schema = 6)

# Schema 7
s7data <- adcibc %>%
  count(TRTP, RACE, AVAL) %>%
  dplyr::select(Freq = n, x = TRTP, k = RACE, y = AVAL) %>%
  mutate(Schema = 7)

# Schema 8
s8data <- adcibc %>%
  count(TRTP, AGEGR1, AVAL) %>%
  dplyr::select(Freq = n, x = TRTP, k = AGEGR1, y = AVAL) %>%
  mutate(Schema = 8)

# Schema 9
s9data <- adcibc %>%
  count(TRTP, SITEID, AVAL) %>%
  dplyr::select(Freq = n, x = TRTP, k = SITEID, y = AVAL) %>%
  mutate(Schema = 9)


# Schema 10
s10data <- adcibc %>%
  count(TRTP, AVAL, AGEGR1) %>%
  dplyr::select(Freq = n, x = AVAL, y = AGEGR1, k = TRTP) %>%
  mutate(Schema = 10)


# 03: Analysis Prep
# Write a function to be applied over each schema and return results
# If analysis bombs (due to an error), indicate this, for potential follow-up
grab_cmh <- function(data) {
  tryCatch({
    CMHtest(Freq~x+y|k, data=data, overall=TRUE, details=TRUE )$ALL$table %>%
      as.data.frame() %>%
      rownames_to_column(., "Hypothesis")},
    error = function(e){return('Analysis Did Not Run')})
}


# Collect all schema data sets
all_data<- mget(ls(pattern = "data")) %>%
  reduce(bind_rows)

# Run analysis en masse
all_results <- all_data %>%
  group_by(Schema) %>%
  do(results = grab_cmh(.))

# 04: Clean Up
cleaned_results <- all_results %>%
  unnest(results) %>%
  mutate(Hypothesis = case_when(Hypothesis == "general" ~ "General Association",
                                Hypothesis == "rmeans" ~ "Row Mean Scores Differ",
                                Hypothesis == "cor" ~ "Non-zero Correlation",
                                Hypothesis == "cmeans" ~ "C",
                                !is.na(results) ~ "Analysis Did Not Run")) %>%
  # Not provided in SAS
  filter(Hypothesis != "C") %>%
  mutate_all(as.character)

# Write results
xlsx::write.xlsx(cleaned_results,
                 file = here::here("cmh/Results/R","R_3HA_vcdExtra_forked.xlsx"),
                 sheetName = "R_Results_1")

# Write results - for processing down the road
r_3ha <- cleaned_results
save(r_3ha, file = here::here('cmh/results/R/r_3ha.Rdata'))

