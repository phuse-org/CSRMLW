####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Wrangle the SAS and R results together. Assumes the available
#              files (.csv's) have been processed already
#-------------------------------------------------------------------------------

library(readr)
library(tidyverse)

#' Specify a named list of file paths - R
r_file_list <-
  list(gls_contrasts = "./data/R/gls_contrasts.csv",
       gls_lsmeans = "./data/R/gls_lsmeans.csv",
       lmer_contrasts = "./data/R/lmer_contrasts.csv",
       lmer_lsmeans = "./data/R/lmer_lsmeans.csv")

#' Specify a named list of file paths - SAS
sas_file_list <-
  set_names(paste0("./data/SAS/", list.files("./data/SAS/")),
            nm = c("betwithin_lsmeans",
                   "contain_lsmeans",
                   "kr_lsmeans",
                   "satterthwaite_lsmeans",
                   "betwithin_contrasts",
                   "contain_contrasts",
                   "kr_contrasts",
                   "satterthwaite_contrasts")) %>% as.list()

#' Read in the pre-run R results and assign to the global environment
walk(names(r_file_list),
     ~ assign(.,
              read_csv(r_file_list[[.]]),
              envir = globalenv()))

#' Pivot the R lsmeans output to join to SAS for both the \code{gls} and the
#' \code{lmer} fits
r_lsmeans <-
  map_dfr(
    list(gls_lsmeans, lmer_lsmeans),
    ~.x %>%
      select(VISIT, TRTA, PARAMCD, emmean, SE, corr, type) %>%
      pivot_longer(c(emmean, SE)) %>%
      mutate(
        corr =
          case_when(corr == "corCompSymm" ~ "Heterogeneous Compound Symmetry",
                    corr == "corCAR1" ~ "Heterogeneous First Order Autoregressive",
                    corr == "corSymm" & type == "satterthwaite" ~ "Unstructured (satterthwaite)",
                    corr == "Unstructured" & type == "kr" ~ "Unstructured (KR)",
                    TRUE ~ corr)
      ) %>%
      rename(VALUE = TRTA))

r_contrasts <-
  map_dfr(
    list(gls_contrasts, lmer_contrasts),
    ~.x %>%
      mutate(ci_width = abs(upper.CL - lower.CL)) %>%
      select(VISIT, contrast, PARAMCD, estimate, SE, ci_width, corr, type) %>%
      pivot_longer(c(estimate, SE, ci_width)) %>%
      mutate(
        corr =
          case_when(corr == "corCompSymm" ~ "Heterogeneous Compound Symmetry",
                    corr == "corCAR1" ~ "Heterogeneous First Order Autoregressive",
                    corr == "corSymm" & type == "satterthwaite" ~ "Unstructured (satterthwaite)",
                    corr == "Unstructured" & type == "kr" ~ "Unstructured (KR)",
                    TRUE ~ corr)
      ) %>%
      rename(VALUE = contrast))

#' Read in the pre-run SAS results and row-bind them together [lsmeans]
sas_lsmeans <-
  map_dfr(sas_file_list[grepl("lsmeans", names(sas_file_list))],
          ~readr::read_csv(.x), .id = "type") %>%
  rename(emmean = lsmeans,
         SE = lsmeans_stderr,
         VALUE = TRTA) %>%
  mutate(
    type = str_remove(type, "_lsmeans"),
    VISIT = replace_na(VISIT, "Overall"),
    corr =
      case_when(covariatestructure == "Unstructured" & type == "satterthwaite" ~ "Unstructured (satterthwaite)",
                covariatestructure == "Unstructured" & type == "kr" ~ "Unstructured (KR)",
                TRUE ~ covariatestructure)
  ) %>%
  select(type, VISIT, VALUE, PARAMCD, PARAM, emmean, SE, corr) %>%
  pivot_longer(c(emmean, SE))

#' Read in the pre-run SAS results and row-bind them together [contrasts]
sas_contrasts <-
  map_dfr(sas_file_list[grepl("contrasts", names(sas_file_list))],
          ~readr::read_csv(.x), .id = "type") %>%
  mutate(
    type = str_remove(type, "_contrasts"),
    VALUE = paste0(TRTA, " - ", reftrta),
    ci_width = abs(upperci95 - lowerci95),
    VISIT = replace_na(VISIT, "Overall"),
    corr =
      case_when(covariatestructure == "Unstructured" & type == "satterthwaite" ~ "Unstructured (satterthwaite)",
                covariatestructure == "Unstructured" & type == "kr" ~ "Unstructured (KR)",
                TRUE ~ covariatestructure)
  ) %>%
  rename(estimate = lsmeans_difference) %>%
  select(VISIT, VALUE, PARAM, PARAMCD, estimate, ci_width, corr, type) %>%
  pivot_longer(c(estimate, ci_width))

#' Right join the above R output to the wrangled SAS output
join_lsmeans <-
  inner_join(sas_lsmeans, r_lsmeans,
             by = c("VISIT", "VALUE", "PARAMCD", "corr", "name", "type"),
             suffix = c("_r", "_sas")) %>%
  mutate(diff = value_r - value_sas,
         avg = (value_r + value_sas)/2) %>%
  drop_na(corr)

join_contrasts <-
  inner_join(sas_contrasts, r_contrasts,
             by = c("VISIT", "VALUE", "PARAMCD", "corr", "name", "type"),
             suffix = c("_r", "_sas")) %>%
  mutate(diff = value_r - value_sas,
         avg = (value_r + value_sas)/2) %>%
  drop_na(corr)

# END
