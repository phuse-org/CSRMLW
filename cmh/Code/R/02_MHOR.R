# Intent: This R script will use the epiDisplay package run and extract:
# 1) M-H OR, 95% CI, and P-Value,
# 2) M-H Chi Square, df, P-value,
# 3) Homogeneity test, Chi Square, df, P-value

# It relies on the intermediate datasets created in 01_3HA.R (s1data, s2data, ... ,s9data)

# 00: Lib Prep
library(magrittr)
library(epiDisplay)

# 01: Generic function to extract MHOR
# the ordering inside of mhor needs to be verified to line up with schemas !!!
get_or <- function(data) {
  data %>%
    uncount(weights = Freq) %$%
    capture.output(mhor(x,y,k, graph = FALSE)) %>%
    as.data.frame() %>%
    rename(., col1 = .) %>%
    filter(str_detect(col1 , "M-H|Homogeneity test"))
}

# 02: Run things through a loop - 9 schemas in total
for(i in 1:9) {
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

# schema 1,2,4,6 only ran
# why not 8?
# why did 4 run?

# Need to verify the coding of x, y and k inside mhor()


