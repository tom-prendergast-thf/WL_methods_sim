install.packages('tidyverse')
install.packages('stats')
install.packages('fixest')
install.packages('simstudy')
install.packages('faux')

# Example

library(tidyverse)
library(stats)
library(fixest)
library(simstudy)
library(faux)

# SIMULATE WAITING LIST DATA


# CREATE SIMULATED DATA
# Create set of time-invariant variables: Patient IDs, age, deprivation, clock starts and stops, propensity for healthcare use 

set.seed(1814)

patients <- 1:1000

age_probs <- c(
  rep(0.1, 14*1000), rep(0.2, 14*1000), rep(0.3, 31*1000), rep(0.1, 14*1000)
) 

ages <- sample(rep(seq(18, 90), each = 1000), 1000, prob=age_probs)

deprivation <- sample(rep(seq(1, 5), each = 1000), 1000)

clock_starts <- sample(rep(seq(as.Date('01-04-2022', "%d-%m-%Y"), as.Date("31-03-2023", "%d-%m-%Y"), by="day"), 1000), 1000)

healthcare_use_propensity <- rnorm(1000, mean = 0, sd = 1) # Create a random propensity for healthcare use for each patient so that their use has some consistency over time

# Create wait times, sampling from a distribution between 15 days and 645 days. Some correlation is built in 
# with deprivation (probability distribution skewed more towards longer wait times for those in bottom two quintiles)
# Clock stops then created based on wait times 

wait_time_probs_1 <- c(
  rep(0.3, 126*1000), rep(0.3, 126*1000), rep(0.2, 127*1000), rep(0.15, 126*1000), rep(0.05, 126*1000)
)

wait_time_probs_2 <- c(
  rep(0.2, 126*1000), rep(0.2, 126*1000), rep(0.3, 127*1000), rep(0.2, 126*1000), rep(0.1, 126*1000)
)

time_invariant_df <- data.frame(patients, ages, clock_starts, deprivation, healthcare_use_propensity) %>%
  mutate(wait_times = case_when(deprivation %in% c(3,4,5) ~ sample(rep(seq(15, 645), each = 1000), 1000, prob=wait_time_probs_1),
                                deprivation %in% c(1,2) ~ sample(rep(seq(15, 645), each = 1000), 1000, prob=wait_time_probs_2)))

time_invariant_df$clock_stops <- time_invariant_df$clock_starts + time_invariant_df$wait_times



# Create set of time-variant variables - dates, monthly care use
date1 <- "01-01-2022"
date2 <- "31-12-2023"

full_time_df <- cbind(time_invariant_df, rep(row.names(time_invariant_df), each = 730)) %>%
  select(1:7) %>%
  mutate(treated = case_when(wait_times <= 126 ~ 0,
                             TRUE ~ 1)) %>%
  mutate(days = rep(seq(as.Date(date1, "%d-%m-%Y"), as.Date(date2, "%d-%m-%Y"), by = "day"), each = 1000))


healthcare_use_probs <- pnorm(rnorm_pre(data.frame(full_time_df$ages, full_time_df$treated, full_time_df$healthcare_use_propensity), r = c(0.3, 0.4, 0.6), empirical = TRUE))

use_gam <- qgamma(healthcare_use_probs, shape = 4, rate = 1)

full_time_df$healthcare_use <- round(use_gam, 0)