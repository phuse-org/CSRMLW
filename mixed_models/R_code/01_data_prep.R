####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Data prep for the single case study used in this project
#-------------------------------------------------------------------------------

#' Very light touch data wrangling pipeline to filter, select, reorder and
#' remove duplicates

data_prep <-
  adlbh %>%
  filter(grepl("WEEK", VISIT)) %>%
  select(SUBJID,
         TRTA,
         VISIT,
         VISITNUM,
         PARAMCD,
         PARAM,
         AVAL,
         BASE,
         CHG) %>%
  mutate(TRTA = ordered(TRTA,
                        c("Placebo",
                          "Xanomeline Low Dose",
                          "Xanomeline High Dose")),
         VISIT = ordered(VISIT,
                         paste0("WEEK ", c(2,4,6,8,12,16,20,24,26))),
         SUBJID = as.numeric(SUBJID)) %>%
  distinct(SUBJID,
           TRTA,
           PARAM,
           VISIT, .keep_all = TRUE) %>%
  filter(str_starts(PARAMCD, "_", negate = TRUE)) %>%
  nest(data = -c(PARAMCD))
