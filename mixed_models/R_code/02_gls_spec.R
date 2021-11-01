####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Specification set up for a range of nlme::gls fits
#-------------------------------------------------------------------------------

#' Specify a nested tibble of generalised least square models to be bound to
#' the modeling pipeline - issue with emmeans and data construction with
#' homoscedastic weights, for now we do not process these fits
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
