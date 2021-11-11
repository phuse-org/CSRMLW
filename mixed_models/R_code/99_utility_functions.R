####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Utility functions
#-------------------------------------------------------------------------------

#' @title Single use data wrangle function
#'
#' @description Prepare R out put once user has sourced the _run.R scripts.
#'
#' @param .model a nested tibble of model fit results from _run.R scripts
#'
#'@details Nothing more than a useful function to re-factor data-steps that
#'   we need to run a number of times (e.g., on \code{nlme::gls()} and on
#'   \code{lme4::lmer()} models)

wrangle_output <- function(.model){

  full_fit_tab <-
    .model %>%
    pivot_longer(-c(PARAMCD, data, model, args, grid, weight, corr, type),
                 names_to = "table_name",
                 values_to = "table_obj") %>%
    mutate(table_values = map(table_obj, ~as_tibble(.))) %>%
    pivot_wider(c(PARAMCD, corr, weight, type),
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
           PARAMCD = factor(PARAMCD, unique(.model$PARAMCD))) %>%
    complete(PARAMCD) %>%
    arrange(as.character(PARAMCD), VISIT, TRTA) %>%
    select(-ls_means_all, -ls_means_trt, -diffs)

  #' Pairwise differences table
  diff_dat <-
    full_fit_tab %>%
    select(PARAMCD, weight, corr, diffs, type) %>%
    mutate(PARAMCD = factor(PARAMCD, unique(.model$PARAMCD))) %>%
    complete(PARAMCD) %>%
    arrange(as.character(PARAMCD)) %>%
    unnest(cols = c(diffs))

  list(lsmeans = lsmeans_dat, contrasts = diff_dat)
}


#' @title Single use plotting function
#'
#' @description Filters the full join of SAS and R output and generates some
#'    plots, namely Bland-Altman style and simple scatter
#'
#' @param .data a tibble as per full join (e.g., \code{join_lsmeans})
#' @param .type_plot character specification of either a 'bland-altman'
#'    (default) or a 'scatter'
#' @param .l_cols list of column names to filter on in .data. Default is NULL,
#'    i.e., no filter.
#' @param .l_vals corresponding list of values to filter by in .data and
#'    .l_cols. Default is NULL, i.e., no filter.
#'
#'@details Nothing more than a useful function to plot some of our data

plot_fn <- function(.data,
                    .type_plot = "bland-altman",
                    .l_cols = NULL,
                    .l_vals = NULL){

  .filter_on <- !(is.null(.l_cols)|is.null(.l_vals))

  if(.filter_on)
    .filter <- purrr::map2(.l_cols, .l_vals,
                           ~quo((!!(as.name(.x))) %in% !!.y))

  if(.filter_on)
    .distinct <- purrr::map(.l_cols,
                            ~quo((!!(as.name(.x)))))

  .dat <-
    .data %>%
    { if(.filter_on) filter(., !!!.filter) else . } %>%
    drop_na()

  .lab <-
    .dat %>%
    { if(.filter_on)
      distinct(., !!!.distinct) %>%
        pivot_longer(everything()) %>%
        rowid_to_column("id") %>%
        unite(lab, -id, sep = " = ") %>%
        pull(lab) %>%
        paste0(., collapse = ", ")
      else
        NULL
    }

  theme_spec <-
    ggthemes::theme_clean()+
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "left",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))

  if(.type_plot == "bland-altman"){

    rslt <-
      ggplot(data = .dat,
             aes(x = avg, y = diff, col = VALUE, pch = corr))+
      facet_wrap(.~PARAM, scales = "free",
                 labeller = label_wrap_gen(width = 25))+
      geom_point()+
      guides(col = FALSE, pch = FALSE)+
      geom_hline(yintercept = 0, lty = 2)+
      labs(x = "Average (R versus SAS)",
           y = "Difference (R minus SAS)")+
      switch(!is.null(.lab), labs(caption = .lab))+
      theme_spec

  } else if(.type_plot == "scatter"){

    rslt <-
      ggplot(data = .dat,
             aes(x = value_sas, y = value_r, col = VALUE, pch = corr))+
      facet_wrap(.~PARAM, scales = "free",
                 labeller = label_wrap_gen(width = 25))+
      geom_point()+
      guides(col = FALSE, pch = FALSE)+
      geom_abline(intercept = 0, slope = 1, lty = 2)+
      labs(x = "SAS result",
           y = "R result")+
      switch(!is.null(.lab), labs(caption = .lab))+
      theme_spec

  } else {
    stop("type_plot argument can only be 'bland-altman' or 'scatter'",
         call. = FALSE)
  }
  return(rslt)
}

