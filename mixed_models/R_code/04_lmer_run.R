####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Fit lme4::lmer() models
#-------------------------------------------------------------------------------

#' Modelling pipeline
#' (1) run the \code{lmer()} function against each data split and each model
#' type; and (2) obtain the expected marginal means (lsmeans) and corresponding
#' pairwise differences for exporting. Note that a \code{tryCatch} is used in
#' multiple steps so as to ensure that models which fail to converge for _any_
#' reason will return a NULL. Further inspection as to why will be required
#' afterwards.

lmer_fits <-
  data_prep %>%
  mutate(weight = NA_character_,
         corr = "Unstructured",
         args = NA_character_,
         type = "kr",
         model = map(.x = data,
                     ~ tryCatch(
                       lmer(
                         CHG ~ TRTA * VISIT + VISIT + BASE + (0 + VISIT|SUBJID),
                         data = .x,
                         control = lmerControl(check.nobs.vs.nRE = "ignore"),
                         na.action = na.omit
                       ),
                       error = function(x) return(NULL)
                     )),
         grid = map2(.x = model,
                     .y = data,
                     ~ tryCatch(
                       ref_grid(.x, data = .y),
                       error = function(x) return(NULL)
                     )),
         ls_means_all = map(.x = grid,
                            ~tryCatch(
                              emmeans(.,
                                      specs = ~ TRTA * VISIT,
                                      lmer.df = "kenward-roger"),
                              error = function(x) return(NULL)
                            )),
         # Extract the emmeans averaged over visit
         ls_means_trt = map(.x = grid,
                            ~tryCatch(
                              emmeans(.,
                                      specs = ~ TRTA,
                                      lmer.df = "kenward-roger"),
                              error = function(x) return(NULL)
                            )),
         # Extract the differences in pairwise treatment emmeans at each visit
         diffs = map(.x = ls_means_all,
                     ~tryCatch(
                       emmeans(., ~ TRTA|VISIT,
                               lmer.df = "kenward-roger") %>%
                         pairs(reverse = TRUE) %>%
                         confint(),
                       error = function(x) return(NULL)
                     )))

if(save_output) {save.image("lmer_results.RData")}
