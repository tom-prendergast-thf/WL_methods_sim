install.packages('tidyverse')
install.packages('did')
install.packages('fixest')
install.packages('fect')

# Example

library(did)
library(tidyverse)
library(stats)
library(fixest)

# SIMULATE WAITING LIST DATA

# Key idea to work with: People waiting 18 weeks or less are the comparator group

# Variables: week (but what exactly does this represent?), patient_ID, care_use, wait_time_group, treated, currently_long_waiting
# GROUPS:
# 0 (waited 18 weeks or less for treatment)
# 5 - 11 months
# 12 - 18 months
# 19 - 25 months
# 26 - 30 months

set.seed(1814)

months <- rep(1:36, each = 1000)

patients <- rep(1:1000, 36)

x1 <- rep(rpois(1000, lambda = 1), 36)

care_use <- x1 * rpois(36000, lambda = 1)

WL_data_initial <- data.frame(months, patients, care_use)

care_use_summed <- WL_data_initial %>%
  group_by(patients) %>%
  summarise(total_care_use = sum(care_use)) 

quants <- quantile(care_use_summed$total_care_use)

care_use_summed <- care_use_summed %>%
    mutate(group_number = case_when(
      (total_care_use < as.numeric(quants[[2]])) ~ sample(0:4, size = 1, rep = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)),

      (total_care_use >= as.numeric(quants[[2]])) & total_care_use < quants[3]  ~ sample(0:4, size = 1, rep = TRUE, prob = c(0.4, 0.25, 0.2, 0.1, 0.05)),

      (total_care_use >= as.numeric(quants[[3]])) & total_care_use < quants[4]  ~ sample(0:4, size = 1, rep = TRUE, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)),

      (total_care_use >= as.numeric(quants[[4]]))  ~ sample(0:4, size = 1, rep = TRUE, prob = c(0.1, 0.15, 0.25, 0.3, 0.2)),
        TRUE ~ 0
      ))

WL_data_groups <- WL_data_initial %>%
  left_join(., care_use_summed, by = 'patients') %>%
  mutate(wait_time_group = case_when(
    group_number == 0 ~ 0,
    group_number == 1 ~ 5,
    group_number == 2 ~ 12,
    group_number == 3 ~ 19,
    group_number == 4 ~ 26
  )) %>%
  select(-total_care_use) %>%
  mutate(treat_post_5_months = case_when((group_number != 0 & months > 5) ~ 1,
                                         TRUE ~ 0
  )) %>%
  mutate(treated = case_when(
    group_number == 0 ~ 0,
    group_number == 1 & months >= 5 ~ 1,
    group_number == 2 & months >= 12 ~ 1,
    group_number == 3 & months >= 19 ~ 1,
    group_number == 4 & months >= 26 ~ 1,
    TRUE ~ 0
  ))

WL_filtered <- WL_data_groups %>%
  filter(wait_time_group == 12 | wait_time_group == 0) %>%
  mutate(long_wait_time = case_when(wait_time_group == 12 ~ 5,
                                    TRUE ~ 0
  )) %>%
  mutate(time_to_treat = case_when(group_number != 0 ~ months - 5,
                                   TRUE ~ 0))
  
  
## Test using Callaway & Sant'Anna did package (for staggered did, calculates average treatment-time effect)

out <- att_gt(yname = "care_use",
              gname = "long_wait_time",
              idname = "patients",
              tname = "months",
              xformla = ~1,
              data = WL_filtered,
              est_method = "reg"
)

summary(out)

ggdid(out)


# Test using fixest package

fe <- feols(care_use ~ i(time_to_treat, treat_post_5_months, ref = -1),
     fixef = c('patients', 'months'),
      data = WL_filtered)

summary(fe)

iplot(fe)



