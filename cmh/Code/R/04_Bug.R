# SAS Example:
# Levels k = 2, x = 3, y = 2
# We chose a non 2x2xk to illustrate this
# a 2x2x2 would essentially mask the mixing up of results.

# PROC FREQ data = all;
#   weight count;
#   tables k * x * y/chisq cmh expected nocol nopct;
# RUN;

# Cochran-Mantel-Haenszel Statistics (Based on Table Scores)
# Statistic    Alternative Hypothesis    DF       Value      Prob
# ---------------------------------------------------------------
#   1        Nonzero Correlation        1      6.4586    0.0110
#   2        Row Mean Scores Differ     2     26.0278    <.0001
#   3        General Association        2     26.0278    <.0001



# Recreate in R
test_eg <- data.frame(x = c('low','low','low','low','med','med','med','med', 'high','high','high','high'),
                      k = c('yes','yes','no','no', 'yes','yes','no','no','yes','yes','no','no'),
                      y = c('yes','no', 'yes','no','yes','no','yes','no','yes','no','yes','no'),
                      Freq = c(11,43,42,169,14,104,20,132,8,196,2,59))

library(vcdExtra)

# test 1
CMHtest(Freq~x+y|k, data=test_eg, overall=TRUE, details=TRUE)$ALL$table
# Chisq    Df Prob
# cor     6.458575 1  0.01104181
# rmeans  26.02779 2  2.229135e-06
# cmeans  6.458575 1  0.01104181
# general 26.02779 2  2.229135e-06

# Results match

# test 2 - ask for all explicitly
CMHtest(Freq~x+y|k, data=test_eg, overall=TRUE, details=TRUE, type = "ALL")$ALL$table

# Chisq    Df Prob
# general 26.02779 1  3.365374e-07
# rmeans  26.02779 2  2.229135e-06
# cmeans  6.458575 1  0.01104181
# cor     6.458575 2  0.03958569

# general df incorrect
# cor df incorrect
# rmeans ok
# cmeans ok


# test 3 - ask for what exactly SAS produces
CMHtest(Freq~x+y|k, data=test_eg, overall=TRUE, details=TRUE, types=c("cor","general","rmeans"))$ALL$table

# Chisq    Df Prob
# cor     6.458575 1  0.01104181
# general 26.02779 2  2.229135e-06
# rmeans  26.02779 2  2.229135e-06

# seems to match again


# test 5- ask for what exactly SAS produces, but specify in different order
CMHtest(Freq~x+y|k, data=test_eg, overall=TRUE, details=TRUE, types=c("rmeans","general","cor"))$ALL$table


# Chisq    Df Prob
# rmeans  26.02779 1  3.365374e-07
# general 26.02779 2  2.229135e-06
# cor     6.458575 2  0.03958569

# ah ha
# test 5 - ask for a single test
CMHtest(Freq~x+y|k, data=test_eg, overall=TRUE, details=TRUE, type=c("rmeans"))$ALL$table

# Chisq    Df Prob
# rmeans 26.02779 NA NA

# no p-value
