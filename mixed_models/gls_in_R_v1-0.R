####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Compound/Study/Reporting Effort: General Utility
# Developer: Doug Thompson
# Current Version Number: 1
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Description: Exploring GLS model fits as part of a PHUSE WG
#-------------------------------------------------------------------------------

#' Read in some packages
library(readr)
library(tidyverse)
library(nlme)
library(emmeans)

library(purrr)

#-------------------------------------------------------------------------------
#' Read in example data
#' Source the adlbh.xpt data from 
#'    https://github.com/phuse-org/TestDataFactory/blob/main/Updated/TDF_ADaM/adlbh.xpt

#-------------------------------------------------------------------------------
#' Specify a nested tibble of generalised least square models to be bound
#'    to the modeling pipeline - issue with emmeans and data construction
#'    with homoscedastic weights, for now we do not process these fits
model_list <-
  crossing(weight = c("hetroscedastic"),
           corr = c("corCompSymm",
                    "corCAR1",
                    "corSymm")) %>%
  mutate(args =
           list(
             # Hetroscedastic variance
             list(correlation = "corCAR1(form= ~ 1 | SUBJID)",
                  weights = "varIdent(form = ~1|VISIT)"),
             list(correlation = "corCompSymm(form= ~ 1 | SUBJID)",
                  weights = "varIdent(form = ~1|VISIT)"),
             list(correlation = "corSymm(form = ~1|SUBJID)",
                  weights = "varIdent(form = ~1|VISIT)"))
  ) %>%
  nest(model_def = everything())

#-------------------------------------------------------------------------------
#' Modeling pipeline
#'    (1) very light touch data wrangling to filter, select, reorder and remove
#'    duplicates; (2) run the \code{gls()} function against each data split
#'    and each model type; and (3) obtain the expected marginal means (lsmeans)
#'    and corresponding pairwise differences for exporting. Note that a
#'    \code{tryCatch} is used in multiple steps so as to ensure that models
#'    which fail to converge for _any_ reason will return a NULL. Further
#'    inspection as to why will be required afterwards.

full_fit <-
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
  nest(data = -c(PARAMCD)) %>%
  bind_cols(model_list) %>%
  unnest(model_def) %>%
  mutate(
    # Fit the model
    model = map2(.x = data,
                 .y = args,
                 ~tryCatch(
                   gls(data = .x,
                       model = CHG ~ TRTA * VISIT + VISIT + BASE,
                       correlation = eval(parse(text = .y[["correlation"]])),
                       weights = eval(parse(text = .y[["weights"]])),
                       control = glsControl(opt = "optim"),
                       method = "REML",
                       na.action = "na.omit"),
                   error = function(x) return(NULL)
                 )),
    # Generate a reference grid
    grid = map2(.x = model,
                .y = data,
                ~tryCatch(
                  ref_grid(.x,
                           data = .y,
                           mode = "df.error"),
                  error = function(x) return(NULL)
                )),
    # Extract all emmeans
    ls_means_all = map(.x = grid,
                       ~tryCatch(
                         emmeans(.,
                                 specs = ~ TRTA * VISIT,
                                 mode = "df.error"),
                         error = function(x) return(NULL)
                       )),
    # Extract the emmeans averaged over visit
    ls_means_trt = map(.x = grid,
                       ~tryCatch(
                         emmeans(.,
                                 specs = ~ TRTA,
                                 mode = "df.error"),
                         error = function(x) return(NULL)
                       )),
    # Extract the differences in pairwise treatment emmeans at each visit
    diffs = map(.x = ls_means_all,
                ~tryCatch(
                  emmeans(., ~ TRTA|VISIT) %>%
                    pairs() %>%
                    confint(),
                  error = function(x) return(NULL)
                ))
  )

save.image("results.RData")

#-------------------------------------------------------------------------------
#' Extract tables
full_fit_tab <-
  full_fit %>%
  pivot_longer(-c(PARAMCD, data, model, args, grid, weight, corr),
               names_to = "table_name",
               values_to = "table_obj") %>%
  mutate(table_values = map(table_obj, ~as_tibble(.))) %>%
  pivot_wider(c(PARAMCD, corr, weight),
              names_from = table_name,
              values_from = table_values)

#' emmeans table
lsmeans_dat <-
  full_fit_tab %>%
  mutate(lsmean_final = map2(ls_means_all, ls_means_trt,
                             ~ tryCatch(
                               bind_rows(.x, .y),
                               error = function(x) tibble(NA, .rows = 1)
                             )
  )) %>%
  unnest(cols = lsmean_final) %>%
  mutate(VISIT = if_else(is.na(VISIT), "Overall", as.character(VISIT)),
         VISIT = ordered(VISIT,
                         c("Overall",
                           paste0("WEEK ", c(2,4,6,8,12,16,20,24,26)))),
         PARAMCD = factor(PARAMCD, unique(full_fit$PARAMCD))) %>%
  complete(PARAMCD) %>%
  arrange(as.character(PARAMCD), VISIT, TRTA) %>%
  select(-ls_means_all, -ls_means_trt, -diffs)

write.csv(lsmeans_dat, "emmeans.csv", row.names = FALSE)

#' Pairwise differences table
diff_dat <-
  full_fit_tab %>%
  select(PARAMCD, weight, corr, diffs) %>%
  mutate(PARAMCD = factor(PARAMCD, unique(full_fit$PARAMCD))) %>%
  complete(PARAMCD) %>%
  arrange(as.character(PARAMCD)) %>%
  unnest(cols = c(diffs))

write.csv(diff_dat, "pair_diff.csv", row.names = FALSE)

#' END
#-------------------------------------------------------------------------------
