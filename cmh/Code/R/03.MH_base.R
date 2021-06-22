### base R mantelhaen package

# Schema 1
s1data_mh <- adcibc %>%
  filter(AGEGR1 != "<65" & TRTP != "Xanomeline High Dose") %>%
  dplyr::select(k = AGEGR1, x = TRTP, y = SEX) %>%
  mutate(Schema = 1, )

# Schema 2
s2data_mh <- adcibc %>%
  filter(RACE != "AMERICAN INDIAN OR ALASKA NATIVE" & TRTP != "Xanomeline High Dose") %>%
  dplyr::select(x = TRTP, y = SEX, k = RACE) %>%
  mutate(Schema = 2)

# Schema 3
s3data_mh <- adcibc %>%
  dplyr::select(x = TRTP, y = SEX, k = RACE) %>%
  mutate(Schema = 3)

# Schema 4
s4data_mh <- adcibc %>%
  filter(TRTP != "Xanomeline High Dose") %>%
  dplyr::select(x = TRTP, y = SEX, k = SITEID) %>%
  mutate(Schema = 4)

# Schema 5
s5data_mh <- adcibc %>%
  dplyr::select(x = TRTP, y = SEX, k = SITEID) %>%
  mutate(Schema = 5)

# Schema 6
s6data_mh <- adcibc %>%
  filter(TRTP != "Xanomeline High Dose") %>%
  dplyr::select(x = TRTP, k = SEX, y = AVAL) %>%
  mutate(Schema = 6)

# Schema 7
s7data_mh <- adcibc %>%
  dplyr::select(x = TRTP, k = RACE, y = AVAL) %>%
  mutate(Schema = 7)

# Schema 8
s8data_mh <- adcibc %>%
  dplyr::select(x = TRTP, k = AGEGR1, y = AVAL) %>%
  mutate(Schema = 8)

# Schema 9
s9data_mh <- adcibc %>%
  dplyr::select(x = TRTP, k = SITEID, y = AVAL) %>%
  mutate(Schema = 9)

# Schema 10

s10data_mh <- adcibc %>%
  dplyr::select(x = AVAL, y = AGEGR1, k = TRTP) %>%
  mutate(Schema = 10)


### run

results <- as.data.frame(matrix(ncol = 4, nrow = 10))
colnames(results) <- c("Schema", "Chi-Square", "p-value", "Common odds-ratio")

run_mantelhaen <- function(data){

  model <- mantelhaen.test(x = data$x, y = data$y, z = data$k)

  results <- c(unique(data$Schema), model$statistic, model$p.value, model$estimate)

  if (length(results) < 4) {
    results <- c(results,NA)
  }
  return(results)
}

results[1,] <- run_mantelhaen(s1data_mh)
results[2,] <- run_mantelhaen(s2data_mh)
results[3,] <- run_mantelhaen(s3data_mh) # error sample size in each stratum must be > 1
results[4,] <- run_mantelhaen(s4data_mh) # error sample size in each stratum must be > 1
results[5,] <- run_mantelhaen(s5data_mh) # error sample size in each stratum must be > 1
results[6,] <- run_mantelhaen(s6data_mh)
results[7,] <- run_mantelhaen(s7data_mh) # error sample size in each stratum must be > 1
results[8,] <- run_mantelhaen(s8data_mh)
results[9,] <- run_mantelhaen(s9data_mh) # error sample size in each stratum must be > 1
results[10,] <- run_mantelhaen(s10data_mh)

# Write results - for processing down the road
r_base <- results
save(r_base, file = here::here('cmh/results/R/r_base.Rdata'))


