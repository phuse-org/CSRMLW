####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Fit nlme::gls() models
#-------------------------------------------------------------------------------

#' Modelling pipeline
#' (1) run the \code{gls()} function against each data split and each model
#' type; and (2) obtain the expected marginal means (lsmeans) and corresponding
#' pairwise differences for exporting. Note that a \code{tryCatch} is used in
#' multiple steps so as to ensure that models which fail to converge for _any_
#' reason will return a NULL. Further inspection as to why will be required
#' afterwards.

gls_fits <-
  data_prep %>%
  bind_cols(model_list) %>%
  unnest(model_def) %>%
  mutate(
    type = "satterthwaite",
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
                    pairs(reverse = TRUE) %>%
                    confint(),
                  error = function(x) return(NULL)
                ))
  )

if(save_output) {save.image("gls_results.RData")}
