
# SAS Example 50.3 --------------------------------------------------------
# Unbalanced Anova for Two Way Design with Interaction

# https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_glm_examples03.htm

# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, broom, emmeans)

df_sas <-
  list(
    tibble(drug = 1, disease = 1, y = c(42, 44, 36, 13, 19, 22)),
    tibble(drug = 1, disease = 2, y = c(33, NA, 26, NA, 33, 21)),
    tibble(drug = 1, disease = 3, y = c(31, -3, NA, 25, 25, 24)),
    tibble(drug = 2, disease = 1, y = c(28, NA, 23, 34, 42, 13)),
    tibble(drug = 2, disease = 2, y = c(NA, 34, 33, 31, NA, 36)),
    tibble(drug = 2, disease = 3, y = c( 3, 26, 28, 32,  4, 16)),
    tibble(drug = 3, disease = 1, y = c(NA, NA,  1, 29, NA, 19)),
    tibble(drug = 3, disease = 2, y = c(NA, 11,  9,  7,  1, -6)),
    tibble(drug = 3, disease = 3, y = c(21,  1, NA,  9,  3, NA)),
    tibble(drug = 4, disease = 1, y = c(24, NA,  9, 22, -2, 15)),
    tibble(drug = 4, disease = 2, y = c(27, 12, 12, -5, 16, 15)),
    tibble(drug = 4, disease = 3, y = c(22,  7, 25,  5, 12, NA))) %>%
  bind_rows() %>%
  mutate(across(c(drug, disease), factor)) %>%
  print()

df_sas %>%
  summarize(
    n_drug     = n_distinct(drug),
    n_disease  = n_distinct(disease),
    n_obs      = n(),
    y_mean     = mean(y, na.rm = TRUE)
  )

lm_model <- lm(y ~ drug:disease, df_sas)
lm_glance <- lm_model %>% glance() %>% print()
lm_tidy   <- lm_model %>% tidy()   %>% print()


# Matches SAS Anova Table
lm_table <-
  lm_model %>%
  anova() %>%
  tidy() %>%
  add_row(term = "Total", df = sum(.$df), sumsq = sum(.$sumsq)) %>%
  print()


# Matches SAS Type 1/2/3 Tables, R has no Type 4 Equivalent
library(rstatix)
my_formula <- as.formula(y ~ drug + disease + drug*disease)
df_sas %>% anova_test(my_formula, type = 1, detailed = TRUE) %>% get_anova_table()
df_sas %>% anova_test(my_formula, type = 2, detailed = TRUE) %>% get_anova_table()
df_sas %>% anova_test(my_formula, type = 3, detailed = TRUE) %>% get_anova_table()


# Least Square Means, matches SAS Tables
lm_model %>% emmeans::lsmeans("drug") %>% emmeans::pwpm()
lm_model %>% emmeans::lsmeans("drug") %>% plot(comparisons = TRUE)
