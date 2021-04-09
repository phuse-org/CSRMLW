

library(tidyverse)
library(broom)

# changed block to be a character variable so the aov()
# function treated it as a class variable.
plants <- tribble(
~type, ~block, ~StemLength,  
"Clarion", "1",  32.7,
"Clarion", "2",  32.3,
"Clarion", "3",  31.5,
"Clinton", "1",  32.1,
"Clinton", "2",  29.7,
"Clinton", "3",  29.1,
"Knox",    "1",  35.7,
"Knox",    "2",  35.9,
"Knox",    "3",  33.1,
"O'Neill", "1",  36.0,
"O'Neill", "2",  34.2,
"O'Neill", "3",  31.2,
"Compost", "1",  31.8,
"Compost", "2",  28.0,
"Compost", "3",  29.2,
"Wabash",  "1",  38.2,
"Wabash",  "2",  37.8,
"Wabash",  "3",  31.9,
"Webster", "1",  32.5,
"Webster", "2",  31.1,
"Webster", "3",  29.7
)

summary(plants)

# Test for a significant difference in PULSE between values of SEX by 
# running a one way ANOVA using the model PULSE = SEX. Tidy the results 
# using the tidy() function.
glm1 <- aov(StemLength ~ type + block, data = plants) %>%
  print()

tidy_glm1 <- 
  tidy(glm1) %>% 
  print()

summary_glm1 <- 
  summary(glm1)  %>% 
  print()

augment_glm1 <- 
  augment(glm1) %>% 
  print()

glance_glm1 <- 
  glance(glm1)  %>% 
  print()

plot(glm1)



