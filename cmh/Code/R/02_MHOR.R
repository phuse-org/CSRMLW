# Intent: This R script will use the epiDisplay package run and extract:
# 1) M-H OR, 95% CI, and P-Value,
# 2) M-H Chi Square, df, P-value,
# 3) Homogeneity test, Chi Square, df, P-value

# It relies on the intermediate datasets created in 01_3HA.R (s1data, s2data, ... ,s9data)

# 00: Lib Prep
library(magrittr)
library(epiDisplay)

# 01: Generic function to extract MHOR
get_or <- function(data) {
  data %>%
    uncount(weights = Freq) %$%
    capture.output(mhor(x,y,k, graph = FALSE)) %>%
    as.data.frame() %>%
    rename(., col1 = .) %>%
    filter(str_detect(col1 , "M-H|Homogeneity test"))
}

# 02: Run things through a loop - 10 schemas in total
for(i in 1:10) {
  tryCatch({
  assign(paste0("mhor", i),
         get(paste0("s",i,"data")) %>%
           get_or() %>%
           mutate(Schema = i),
         )
  }, error=function(e){return('Analysis Did Not Run')})
}

# 03: Collect Results
all_or_results <- mget(ls(pattern = "mhor")) %>%
  reduce(bind_rows)

# Write results - for processing down the road
r_mhor <- all_or_results
save(r_mhor, file = here::here('cmh/results/R/r_mhor.Rdata'))



