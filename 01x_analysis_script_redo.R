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
  filter(after_clock_start == 1) %>%                     # Filter so only entries after clock start are included
  mutate(days_since_clock_start = days - clock_starts)  # For use in the creation of time variable below
 



# Create function which prepares data for comparisons. This can be used to create 
# usable dataframes for each group comparison

prep_data_function <- function(df, #the dataframe we're transforming
         comparator_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
         control_group,   # A string describing the control group, typically '<= 18 weeks' in our case
         timeframe_1_bound,  # A number delineating the end of our waiting period in days, e.g. if comparing 36 week waiters this would be 252
         timeframe_2_bound){ # A number delineating the end of our follow-up period in days, e.g. if comparing 36 week waiters this would be 336 (36 weeks + 12 weeks)

  df_filtered <- df %>%
    filter(group == control_group | group == comparator_group) %>%
    filter(days_since_clock_start <= timeframe_2_bound) %>%
    mutate(time_period = case_when(days_since_clock_start <= 126 ~ 0,
                                    days_since_clock_start > 126 & days_since_clock_start <= timeframe_1_bound ~ 1,
                                    days_since_clock_start > timeframe_1_bound ~ 2
    ))
    
  
  df_grouped_by_time_period <- df_filtered %>%
    group_by(patients) %>%
    mutate(max_time_covered = max(days_since_clock_start)) %>%
    ungroup() %>%
    group_by(patients, time_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>%
    filter(max_time_covered == timeframe_2_bound) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
           group == comparator_group ~ 1))

return(df_grouped_by_time_period)

}

df_36_weeks <- prep_data_function(df = waiting_list_df, 
                           comparator_group = '19-36 weeks', 
                           control_group = '<= 18 weeks',
                           timeframe_1_bound = 252,  
                           timeframe_2_bound = 336)

df_54_weeks <- prep_data_function(df = waiting_list_df, 
                                  comparator_group = '37-54 weeks', 
                                  control_group = '<= 18 weeks',
                                  timeframe_1_bound = 378,  
                                  timeframe_2_bound = 462)

df_36_weeks %>%
  group_by(group, time_period) %>%
  summarise(total_hc_use = sum(total_hc_use)) %>%
ggplot(data = ., aes(x = time_period, y = total_hc_use, color = group)) +
  geom_line() +
  theme_minimal()

# Test using fixest package

fe_ols <- feols(total_hc_use ~ i(time_period, treated, ref = 0) |
                  patients + time_period,
             data = df_36_weeks)

summary(fe_ols)

iplot(fe_ols)
