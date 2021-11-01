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

#' LS Means Bland-Altman
lsmeans_bland_alt <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "bland-altman",
               list("corr"), list(.x)))

#' LS Means Scatter
lsmeans_scatter <-
  filter_by_these_1 %>%
  map(~plot_fn(join_lsmeans,
               .type_plot = "scatter",
               list("corr"), list(.x)))

#' Contrast Bland-Altman
contrasts_bland_alt <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "bland-altman",
               list("corr"), list(.x)))

#' Contrast Scatter
contrasts_scatter <-
  filter_by_these_1 %>%
  map(~plot_fn(join_contrasts,
               .type_plot = "scatter",
               list("corr"), list(.x)))

plot_list <-
  tibble::lst(lsmeans_bland_alt,
              lsmeans_scatter,
              contrasts_bland_alt,
              contrasts_scatter)

#' Save to .pdf
for(i in names(plot_list)){
  for(j in names(plot_list[[i]])){
    pdf(file.path(path_nm,  "plots", paste(i, paste0(j, ".pdf"), sep = "_")),
        width = 8, height = 6)
    print(plot_list[[i]][[j]])
    dev.off()
  }
}
