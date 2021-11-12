####------------------------------ Header ----------------------------------####
# Program Name: Mixed Models in R
# Date: 12-Jul-2021
# OS / R Version: R version 4.0.2 (2020-06-22)
# Date: 22-Oct-2021
# Description: Generate some summary plots
#-------------------------------------------------------------------------------

#' Create a file path to store the plots
dir.create(file.path(path_nm, "plots"), showWarnings = TRUE)

#' Identify a subset to run through the \code{plot_fn}
filter_by_these_1 <-
  join_lsmeans %>%
  drop_na(corr) %>%
  distinct(corr) %>%
  unlist(use.names = FALSE) %>%
  setNames(., .)

#' LS Means Bland-Altman [emmean]
lsmeans_bland_alt_emmean <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "bland-altman",
               list("corr", "name"), list(.x, "emmean")))

#' LS Means Bland-Altman [SE]
lsmeans_bland_alt_se <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "bland-altman",
               list("corr", "name"), list(.x, "SE")))

#' LS Means Scatter [emmean]
lsmeans_scatter_emmean <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "scatter",
               list("corr", "name"), list(.x, "emmean")))

#' LS Means Scatter [SE]
lsmeans_scatter_se <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "scatter",
               list("corr", "name"), list(.x, "SE")))

#' Contrast Bland-Altman [estimate]
contrasts_bland_alt_estimate <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "bland-altman",
               list("corr", "name"), list(.x, "estimate")))

#' Contrast Bland-Altman [CI width]
contrasts_bland_alt_ci <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "bland-altman",
               list("corr", "name"), list(.x, "ci_width")))

#' Contrast Scatter [estimate]
contrasts_scatter_estimate <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "scatter",
               list("corr", "name"), list(.x, "estimate")))

#' Contrast Scatter [CI width]
contrasts_scatter_ci <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "scatter",
               list("corr", "name"), list(.x, "ci_width")))

plot_list <-
  tibble::lst(lsmeans_bland_alt_emmean,
              lsmeans_bland_alt_se,
              lsmeans_scatter_emmean,
              lsmeans_scatter_se,
              contrasts_bland_alt_estimate,
              contrasts_bland_alt_ci,
              contrasts_scatter_estimate,
              contrasts_scatter_ci)

#' Save R image
saveRDS(plot_list, file = file.path(path_nm, "plots", "plots.rds"))

#' Save to .pdf
for(i in names(plot_list)){
  for(j in names(plot_list[[i]])){
    pdf(file.path(path_nm,  "plots", paste(i, paste0(j, ".pdf"), sep = "_")),
        width = 8, height = 6)
    print(plot_list[[i]][[j]])
    dev.off()
  }
}
