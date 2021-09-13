
# Matching SAS and R ------------------------------------------------------
# Randomized Complete Blocks with Means Comparisons and Contrasts

# Source:
# https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_glm_examples01.htm


# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, broom, emmeans)

# Data --------------------------------------------------------------------

plants <- tribble(
  ~type, ~block, ~StemLength,
  "Clarion", 1,  32.7,
  "Clarion", 2,  32.3,
  "Clarion", 3,  31.5,
  "Clinton", 1,  32.1,
  "Clinton", 2,  29.7,
  "Clinton", 3,  29.1,
  "Knox",    1,  35.7,
  "Knox",    2,  35.9,
  "Knox",    3,  33.1,
  "O'Neill", 1,  36.0,
  "O'Neill", 2,  34.2,
  "O'Neill", 3,  31.2,
  "Compost", 1,  31.8,
  "Compost", 2,  28.0,
  "Compost", 3,  29.2,
  "Wabash",  1,  38.2,
  "Wabash",  2,  37.8,
  "Wabash",  3,  31.9,
  "Webster", 1,  32.5,
  "Webster", 2,  31.1,
  "Webster", 3,  29.7) %>%
  arrange(type, block) %>%
  mutate(across(c(type, block), factor))

summary(plants)

plants %>%
  summarize(
    n_block = n_distinct(block),
    n_type  = n_distinct(type),
    n_obs   = n()
  )


# Analysis ----------------------------------------------------------------
# Test for a significant difference in PULSE between values of SEX by
# running a one way ANOVA using the model PULSE = SEX
# Tidy the results using the tidy() function


# Matches Type 1/3 SS table in SAS
aov1      <- aov(StemLength ~ type + block, data = plants) %>% print()
aov1_tidy <- aov1 %>% tidy()    %>% print()
aov1_sumr <- aov1 %>% summary() %>% print()
aov1_glnc <- aov1 %>% glance()  %>% print()          # Matches R-Sq table in SAS




lm1      <- lm(StemLength ~ type + block, data = plants) %>% print()
lm1_tidy <- lm1 %>% tidy()    %>% print()
lm1_sumr <- lm1 %>% summary() %>% print()
lm1_augm <- lm1 %>% augment() %>% print()
lm1_glnc <- lm1 %>% glance()  %>% print()            # Matches F table in SAS


# glm1      <- glm(StemLength ~ type + block, data = plants) %>% print()
# glm1_tidy <- glm1 %>% tidy()    %>% print()
# glm1_sumr <- glm1 %>% summary() %>% print()
# glm1_augm <- glm1 %>% augment() %>% print()
# glm1_glnc <- glm1 %>% glance()  %>% print()


# Contrasts ---------------------------------------------------------------

lm1_tidy

# Model terms:
# Default comparisons against Clarion and Block 1


# Intercept        : Stem Length for Block 1 and Type Clarion
# Type  - Clinton  : Difference in Stem Length between Clinton and Clarion w/ B1
# Type  - Compost  : Difference in Stem Length between Compost and Clarion w/ B1
# Type  - Knox     : Difference in Stem Length between Knox    and Clarion w/ B1
# Type  - O'Neill  : Difference in Stem Length between O'Neill and Clarion w/ B1
# Type  - Wabash   : Difference in Stem Length between Wabash  and Clarion w/ B1
# Type  - Webster  : Difference in Stem Length between Webster and Clarion w/ B1
# Block - 2        : Difference in Stem Length between Block 2 and Block 1 w/ Clarion
# Block - 3        : Difference in Stem Length between Block 3 and Block 1 w/ Clarion

emm1 <- emmeans::emmeans(lm1, ~ type)

# SAS Contrasts
# contrast 'Compost vs. others'  Type   -1   -1    6  -1   -1   -1   -1;
# contrast 'River soils vs. non' Type   -1   -1    0  -1   -1    5   -1,
#                                Type   -1    4    0  -1   -1    0   -1;
# contrast 'Glacial vs. drift'   Type   -1    0    0   1    1    0   -1;
# contrast 'Clarion vs. Webster' Type   -1    0    0   0    0    0    1;
# contrast "Knox vs. O'Neill"    Type    0    0    0   1   -1    0    0;


contrast_1 <- c(-1, -1,  6, -1, -1, -1, -1)
contrast_2 <- c(-1, -1,  0, -1, -1,  5, -1)             # SAS quadratic contrast
contrast_3 <- c(-1,  4,  0, -1, -1,  0, -1)             # SAS quadratic contrast
contrast_4 <- c(-1,  0,  0,  1,  1,  0, -1)
contrast_5 <- c(-1,  0,  0,  0,  0,  0,  1)
contrast_6 <- c( 0,  0,  0,  1, -1,  0,  0)

lm1 %>%
  emmeans(~type) %>%
  contrast(
    method = list(
      "Compost vs Others"  = contrast_1,                # Match
      "River Soils vs Non" = contrast_2,                # No Match
      "Glacial vs Drift"   = contrast_4,                # Match
      "Clarion vs Webster" = contrast_5,                # Match
      "Knox vs O'Neill"    = contrast_6)) %>%           # Match
  tidy() %>%
  mutate(f_value = statistic^2) %>%
  DT::datatable() %>%
  DT::formatRound(c("estimate", "std.error", "statistic", "p.value", "f_value"),
                  digits = 4)



# Brian's Work ------------------------------------------------------------
# plants2 <- plants
#
# type_contrasts = contrasts(plants2$type)
# type_contrasts = type_contrasts[,1:2]
# type_contrasts[,1] = c(-1, -1, -1, -1, 6, -1, -1)
# type_contrasts[,2] = c(0, 0, 1, -1, 0, 0, 0)
# contrasts(plants2$type) = type_contrasts
#
# # Using lm() function
# lm2 <- lm(StemLength ~ type + block, data = plants2) %>%
#   print()
#
# tidy_lm2    <- tidy(lm2) %>% print()
# summary_lm2 <- summary(lm2) %>% print()
# augment_lm2 <- augment(lm2) %>% print()
# glance_lm2  <- glance(lm2)   %>% print()
