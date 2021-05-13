# Issue with CMHtest

# load saved data
load(file = here::here('cmh/Data/R','adcibc_filtered.Rdata'))

# change data to character
adcibc <- adcibc %>%
  mutate(across(where(is.numeric), as.character))

# create schema 10 data
s10data <- adcibc %>%
  count(TRTP, AVAL, AGEGR1) %>%
  dplyr::select(Freq = n, x = AVAL, y = AGEGR1, k = TRTP) %>%
  mutate(Schema = 10)


# running CMH Test

# try 1 - no specification of types
CMHtest(Freq~x+y|k, data=s10data, overall=TRUE, details=TRUE)$ALL$table

# try 2 - specify types = ALL
# different DF, and thus P-values for 'cor' and 'general'
CMHtest(Freq~x+y|k, data=s10data, overall=TRUE, details=TRUE, types="ALL")$ALL$table

# try 3 - specify individually
# results match try 1
CMHtest(Freq~x+y|k, data=s10data, overall=TRUE, details=TRUE, types=c("cor","general"))$ALL$table

# try 4 - specify all individually
# there is mismatch in degrees of freedom e.g. rmeans now has 8 df...
CMHtest(Freq~x+y|k, data=s10data, overall=TRUE, details=TRUE, types=c("cor","general","rmeans"))$ALL$table

# try 5 - only rmeans
CMHtest(Freq~x+y|k, data=s10data, overall=TRUE, details=TRUE, types=c("rmeans"))$ALL$table

# I dont think this function is consistent and is bugged?...


