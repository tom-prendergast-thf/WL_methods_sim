
## Filter for waits below 12 weeks

waiting_list_df <- full_time_df %>%
  filter(wait_times > 84)

## Assign to groups

waiting_list_df <- waiting_list_df %>%
  mutate(intervention_point = case_when(wait_times <= 126 ~ NA,
                                             TRUE ~ round_date(clock_starts + 126))) %>%
  mutate(group = case_when(wait_times <= 126 ~ '<= 18 weeks',
                           wait_times > 126 & wait_times <= 252 ~ '19-36 weeks',
                           wait_times > 252 & wait_times <= 378 ~ '37-54 weeks',
                           wait_times > 378 & wait_times <= 504 ~ '55-72 weeks',
                           wait_times > 504  ~ '> 72 weeks')) %>%
  mutate(after_clock_start = case_when(days < clock_starts ~ 0,
                                       TRUE ~ 1)) %>%
  filter(after_clock_start == 1) %>%
  mutate(days_since_clock_start = days - clock_starts) %>%  # For use in the creation of time-centred variable below
  mutate(six_week_period = case_when(days_since_clock_start <= 42 ~ -2, # Creates a time-centred variable of six-week time periods - doesn't go perfectly into our timeframe
                                     days_since_clock_start > 42 & days_since_clock_start <= 84 ~ -1, 
                                     days_since_clock_start > 84 & days_since_clock_start <= 126 ~ 0, # 18 weeks
                                     days_since_clock_start > 126 & days_since_clock_start <= 168 ~ 1,
                                     days_since_clock_start > 168 & days_since_clock_start <= 210 ~ 2,
                                     days_since_clock_start > 210 & days_since_clock_start <= 252 ~ 3,
                                     days_since_clock_start > 252 & days_since_clock_start <= 294 ~ 4,
                                     days_since_clock_start > 294 & days_since_clock_start <= 336 ~ 5,
                                     days_since_clock_start > 336 & days_since_clock_start <= 378 ~ 6,
                                     days_since_clock_start > 378 & days_since_clock_start <= 420 ~ 7,
                                     days_since_clock_start > 420 & days_since_clock_start <= 462 ~ 8,
                                     days_since_clock_start > 462 & days_since_clock_start <= 504 ~ 9,
                                     days_since_clock_start > 504 & days_since_clock_start <= 546 ~ 10,
                                     days_since_clock_start > 546 & days_since_clock_start <= 588 ~ 11,
                                     days_since_clock_start > 588 & days_since_clock_start <= 630 ~ 12,
                                     days_since_clock_start > 630 & days_since_clock_start <= 672 ~ 13,
                                     days_since_clock_start > 672 & days_since_clock_start <= 714 ~ 14,
                                     days_since_clock_start > 714 ~ 15
  ))



waiting_grouped_by_time_period <- waiting_list_df %>%
  group_by(patients, six_week_period, group, intervention_point, ages, deprivation) %>%
  summarise(total_hc_use = sum(healthcare_use)) %>%
  group_by(patients) %>%
  mutate(total_time_covered = sum(six_week_period)) %>%
  ungroup()

# Test using fixest package

df <- waiting_grouped_by_time_period

control_group <- '<= 18 weeks'

treatment_group <- '19-36 weeks'

last_compare_period <- 8

total_time_covered

for (i in -2:(last_compare_period-1)) {
  x <- i + 1
  times <- i+x
}

df_filtered <- df %>%
  filter((group == control_group |group == treatment_group) & six_week_period <= last_compare_period) %>%
  mutate(treated = case_when(group == control_group ~ 0,
                             group == treatment_group ~ 1))

df_grouped <- df_filtered %>%
  group_by(group, six_week_period) %>%
  summarise(hc_use = sum(total_hc_use))

ggplot(data = df_grouped, aes(x = six_week_period, y = hc_use, color = group))+
  geom_line()+
  theme_minimal()

fe <- feols(total_hc_use ~ i(six_week_period, treated, ref = -1) 
            | patients + six_week_period,
            data = df_filtered)

summary(fe)

iplot(fe)
