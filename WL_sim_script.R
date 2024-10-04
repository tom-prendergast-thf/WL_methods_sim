## This script loads in required packages 


# Check for and load required packages 

packages <- c('tidyverse', 'stats', 'simstudy', 'fixest', 'faux')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

# SIMULATE WAITING LIST DATA

# Create set of time-invariant variables: Patient IDs, age, deprivation, clock starts and stops, propensity for healthcare use 

set.seed(1814)

patients <- 1:1000

age_probs <- c(
  rep(0.1, 14*1000), rep(0.2, 14*1000), rep(0.3, 31*1000), rep(0.1, 14*1000)
) 

ages <- sample(rep(seq(18, 90), each = 1000), 1000, prob=age_probs)

deprivation <- sample(rep(seq(1, 5), each = 1000), 1000)

clock_starts <- sample(rep(seq(as.Date('01-01-2022', "%d-%m-%Y"), as.Date("30-11-2023", "%d-%m-%Y"), by="day"), 1000), 1000)

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
date2 <- "30-04-2024"

full_time_df <- cbind(time_invariant_df, rep(row.names(time_invariant_df), each = 851)) %>%
  select(1:7) %>%
  mutate(days = rep(seq(as.Date(date1, "%d-%m-%Y"), as.Date(date2, "%d-%m-%Y"), by = "day"), each = 1000)) %>% 
  mutate(days_since_clock_start = days - clock_starts) %>%
  mutate(treated = case_when(wait_times > 126 & days_since_clock_start < wait_times ~ 1, 
                             TRUE ~ 0))

# Create vector of healthcare use probabilities, correlated with age, treatment status, and individual propensity for healthcare use
healthcare_use_probs <- pnorm(rnorm_pre(data.frame(full_time_df$ages, full_time_df$treated, full_time_df$healthcare_use_propensity), r = c(0.2, 0.2, 0.5), empirical = TRUE))

use_gam <- qgamma(healthcare_use_probs, shape = 4, rate = 1)

full_time_df$healthcare_use <- round(use_gam, 0)

