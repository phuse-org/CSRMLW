library(tidyverse)
library(gt)

interim_raw <- xlsx::read.xlsx(here::here('cmh/Docu/interim_update','cmhsummary.xlsx'), sheetIndex = 1) %>%
  janitor::clean_names()

# remove the stats package entries so things are comparable
interim_raw1 <- interim_raw %>%
  filter(r_package != 'stats')

# table 1
# we want to present the info side by side, r vs sas, for the same statistic
part1 <- interim_raw1 %>%
  filter(type %in% c('General Association','Correlation', 'Row Means')) %>%
  dplyr::select(schema, type, statistic, r_result) %>%
  pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "r_result", names_prefix = "r_")  %>%
  bind_cols(interim_raw1 %>%
              filter(type %in% c('General Association','Correlation', 'Row Means')) %>%
              dplyr::select(schema, type, statistic, sas_result) %>%
              pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "sas_result", names_prefix = "sas_") %>%
              dplyr::select(-c(type, schema))
  ) %>%
  janitor::clean_names()

unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}

part1$schema <- unfill_vec(part1$schema)

t1 <- part1 %>%
  janitor::clean_names() %>%
  gt::gt() %>%
  fmt_missing(columns = 1,
              missing_text = " "
  ) %>%
  tab_spanner(
    label = md("**Chi-Square**"),
    columns = c('r_chi_square', 'sas_chi_square')
  ) %>%
  tab_spanner(
    label = md("**df**"),
    columns = c('r_df', 'sas_df')
  ) %>%
  tab_spanner(
    label =  md("**P-Value**"),
    columns = c('r_p_value','sas_p_value')
  ) %>%
  cols_label(
    schema =  md("**Schema**"),
    type =  md("**Test**"),
    r_chi_square = "R",
    r_df = "R",
    r_p_value = "R",
    sas_chi_square = "SAS",
    sas_df = "SAS",
    sas_p_value = "SAS"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = 2
  ) %>%
  cols_width(
    1 ~ px(75),
    2 ~ px(180),
    starts_with('r') ~ px(150),
    starts_with('sas') ~ px(150)
  ) %>%
  tab_header(title = md("**Cochran-Mantel-Haenszel Test Statistics**")) %>%
  opt_align_table_header(align = "left")




# table 2
# we want to present the info side by side, r vs sas, for the same statistic
part2<- interim_raw1 %>%
  filter(type == "M-H ") %>%
  dplyr::select(schema, type, statistic, r_result) %>%
  pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "r_result", names_prefix = "r_")  %>%
  bind_cols(interim_raw1 %>%
              filter(type == "M-H ") %>%
              dplyr::select(schema, type, statistic, sas_result) %>%
              pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "sas_result", names_prefix = "sas_") %>%
              dplyr::select(-c(type, schema))
  ) %>%
  janitor::clean_names()

part2$schema <- unfill_vec(part2$schema)

t2 <- part2 %>%
  janitor::clean_names() %>%
  gt::gt() %>%
  fmt_missing(columns = 1,
              missing_text = " "
  ) %>%
  tab_spanner(
    label = md("**OR**"),
    columns = c('r_or', 'sas_or')
  ) %>%
  tab_spanner(
    label = md("**LCL**"),
    columns = c('r_lcl', 'sas_lcl')
  ) %>%
  tab_spanner(
    label = md("**UCL**"),
    columns = c('r_ucl', 'sas_ucl')
  ) %>%
  tab_spanner(
    label = md("**P-Value**"),
    columns = c('r_p_value', 'sas_p_value')
  ) %>%
  cols_label(
    schema =  md("**Schema**"),
    type =  md("**Test**"),
    r_or = "R",
    r_ucl = "R",
    r_lcl = "R",
    r_p_value = "R",
    sas_or = "SAS",
    sas_lcl = "SAS",
    sas_ucl = "SAS",
    sas_p_value = "SAS"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = 2
  ) %>%
  cols_width(
    1 ~ px(75),
    2 ~ px(50),
    starts_with('r') ~ px(130),
    starts_with('sas') ~ px(130)
  ) %>%
  tab_header(title = md("**Cochran-Mantel-Haenszel Odds Ratios**")) %>%
  opt_align_table_header(align = "left")

# table 3
# we want to present the info side by side, r vs sas, for the same statistic
part3 <- interim_raw1 %>%
  filter(type == "Homogeneity Test") %>%
  dplyr::select(schema, type, statistic, r_result) %>%
  pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "r_result", names_prefix = "r_")  %>%
  bind_cols(interim_raw1 %>%
              filter(type == "Homogeneity Test") %>%
              dplyr::select(schema, type, statistic, sas_result) %>%
              pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "sas_result", names_prefix = "sas_") %>%
              dplyr::select(-c(type, schema))
  ) %>%
  janitor::clean_names()

part3$schema <- unfill_vec(part3$schema)

t3 <- part3 %>%
  janitor::clean_names() %>%
  gt::gt() %>%
  fmt_missing(columns = 1,
              missing_text = " "
  ) %>%
  tab_spanner(
    label = md("**Chi-Square**"),
    columns = c('r_chi_square', 'sas_chi_square')
  ) %>%
  tab_spanner(
    label = md("**df**"),
    columns = c('r_df', 'sas_df')
  ) %>%
  tab_spanner(
    label =  md("**P-Value**"),
    columns = c('r_p_value','sas_p_value')
  ) %>%
  cols_label(
    schema =  md("**Schema**"),
    type =  md("**Test**"),
    r_chi_square = "R",
    r_df = "R",
    r_p_value = "R",
    sas_chi_square = "SAS",
    sas_df = "SAS",
    sas_p_value = "SAS"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = 2
  ) %>%
  cols_width(
    1 ~ px(75),
    2 ~ px(180),
    starts_with('r') ~ px(150),
    starts_with('sas') ~ px(150)
  ) %>%
  tab_header(title = md("**Breslow-Day Homogeneity Test of Odds Ratio**")) %>%
  opt_align_table_header(align = "left")

# view
t1
t2
t3
