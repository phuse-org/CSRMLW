library(tidyverse)
library(gt)
library(janitor)


interim_raw <- read_csv(here::here('cmh/Docu/interim_update','cmhsummary.csv')) %>%
  janitor::clean_names()


# remove the stats package entries so things are comparable
interim_raw1 <- interim_raw %>%
  filter(r_package != 'stats') %>%
  arrange(schema) %>%
  mutate(r_result = ifelse(r_result == "ERROR CODE 4", "Error", r_result))

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

part1a <- part1 %>% filter(schema %in% c(3,9))
part1b <- part1 %>% anti_join(part1a, by = "schema")


unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}

part1a$schema <- unfill_vec(part1a$schema)
part1b$schema <- unfill_vec(part1b$schema)

t1a <- part1a %>%
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
    columns = 1
  ) %>%
  cols_width(
    1 ~ px(75),
    2 ~ px(180),
    starts_with('r') ~ px(150),
    starts_with('sas') ~ px(150)
  ) %>%
  tab_header(title = md("**Cochran-Mantel-Haenszel Test Statistics**")) %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan")
    ),
    locations = cells_body(
      columns = 1:8,
      rows = c(3,5)
    )
  )


t1b <- part1b %>%
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
    columns = 1
  ) %>%
  cols_width(
    1 ~ px(75),
    2 ~ px(180),
    starts_with('r') ~ px(150),
    starts_with('sas') ~ px(150)
  ) %>%
  tab_header(title = md("**Cochran-Mantel-Haenszel Test Statistics**")) %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan")
    ),
    locations = cells_body(
      columns = 1:8,
      rows = c(3,5,7)
    )
  )

# table 2
# we want to present the info side by side, r vs sas, for the same statistic
part2<- interim_raw1 %>%
  dplyr::filter(type == "M-H") %>%
  dplyr::select(schema, type, statistic, r_result) %>%
  pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "r_result", names_prefix = "r_")  %>%
  bind_cols(interim_raw %>%
              filter(type == "M-H") %>%
              dplyr::select(schema, type, statistic, sas_result) %>%
              pivot_wider(id_cols = c(schema, type), names_from = "statistic", values_from = "sas_result", names_prefix = "sas_") %>%
              dplyr::select(-c(type, schema))
  ) %>%
  janitor::clean_names() %>%
  # right now our only schema applicable is schema #1 because it's 2x2x2 - only keep this
  filter(schema == 1)  %>%
  dplyr::select(-c(type))

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
    columns = 1
  ) %>%
  cols_width(
    1 ~ px(75),
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
  janitor::clean_names() %>%
  # right now our only schema applicable is schema #1 because it's 2x2x2 - only keep this
  filter(schema == 1) %>%
  dplyr::select(-c(type))

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
    columns = 1
  ) %>%
  cols_width(
    1 ~ px(75),
    starts_with('r') ~ px(150),
    starts_with('sas') ~ px(150)
  ) %>%
  tab_header(title = md("**Breslow-Day Homogeneity Test of Odds Ratio**")) %>%
  opt_align_table_header(align = "left")



# view
t1a
t1b
t2
t3

# save using here - more consistent
saveRDS(t1a, here::here('cmh/Docu/interim_update','table1a.RDS'))
saveRDS(t1b, here::here('cmh/Docu/interim_update','table1b.RDS'))
saveRDS(t2, here::here('cmh/Docu/interim_update','table2.RDS'))
saveRDS(t3, here::here('cmh/Docu/interim_update','table3.RDS'))


