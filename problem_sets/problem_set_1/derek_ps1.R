library(tidyverse)
library(lmtest)
library(sandwich)


DIR <- file.path("C:/users/derew/Documents/Github/ResEcon703/problem_sets/problem_set_1")

com_binary <- read.csv(file.path(DIR, "commute_binary.csv"))
com_multi <- read.csv(file.path(DIR, "commute_multinomial.csv"))

# Binary Choice Linear Probability Model

# Y_{n} = \beta_{0} + \beta_{1}C_{nc} + \beta_{2}T_{nc} + \beta_{3}T_{nb} + \epsilon_{n}

# Y_{n} is a binary indicator if student n drives
# C_{nc} is the cost to student n of driving
# T_{nc} is the time for student n to drive
# T_{nb} is the time for student n to take the bus
# \beta coefficients are to be estimated.



# (i)
names(com_binary)
 
com_binary <- com_binary %>%
                mutate(mode_binary = ifelse(mode == "car", 1, 0)) %>%
                relocate(mode_binary, .after = mode)


binary_est <- lm(com_binary$mode_binary ~ com_binary$cost.car + com_binary$time.car + com_binary$time.bus)

coeftest(binary_est)

# these parameters are the marginal **effects** (probabilities?) of a change in explanatory variable

vcovHC(binary_est)
