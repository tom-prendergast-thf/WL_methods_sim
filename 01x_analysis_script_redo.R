## Filter for waits below 12 weeks

waiting_list_df <- full_time_df %>%
  filter(wait_times > 84)

## Assign to groups

waiting_list_df <- waiting_list_df %>%
  mutate(intervention_point = case_when(wait_times <= 126 ~ NA,
                                        TRUE ~ round_date(clock_starts + 126))) %>%
  mutate(group = case_when(wait_times > 84 & wait_times <= 126 ~ '<= 18 weeks', # Create six-week bins
                           wait_times > 126 & wait_times <= 168 ~ '19-24 weeks',
                           wait_times > 168 & wait_times <= 210 ~ '25-30 weeks',
                           wait_times > 210 & wait_times <= 252 ~ '31-36 weeks',
                           wait_times > 252 & wait_times <= 294 ~ '37-42 weeks',
                           wait_times > 294 & wait_times <= 336 ~ '43-48 weeks',
                           wait_times > 336 & wait_times <= 378 ~ '49-54 weeks',
                           wait_times > 378 & wait_times <= 420 ~ '55-60 weeks',
                           wait_times > 420 & wait_times <= 462 ~ '61-66 weeks',
                           wait_times > 462 & wait_times <= 504 ~ '67-72 weeks',
                           wait_times > 504  ~ '> 72 weeks'
  )) %>%
  mutate(after_clock_start = case_when(days < clock_starts ~ 0,
                                       TRUE ~ 1)) %>%
  filter(after_clock_start == 1) %>%                     # Filter so only entries after clock start are included
  mutate(days_since_clock_start = days - clock_starts)  # For use in the creation of time variable below
 



# Create function which prepares data for comparisons. This can be used to create 
# usable dataframes for each group comparison

prep_data_function <- function(df, #the dataframe we're transforming
         intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
         control_group,   # A string describing the control group, typically '<= 18 weeks' in our case
         timeframe_1_bound,  # A number delineating the lower bound of our waiting period bin in days, 
                             # e.g. if comparing 31-36 week waiters this would be 210
         timeframe_2_bound){ # A number delineating the end of our follow-up period in days, 
                             # e.g. if comparing 31-36 week waiters this would be 336 (36 weeks + 12 weeks)

  df_filtered <- df %>%     # Filters for our control and intervention group of interest, then creates time periods
    filter(group == control_group | group == intervention_group) %>%
    filter(days_since_clock_start <= timeframe_2_bound) %>%
    mutate(time_period = case_when(days_since_clock_start <= 126 ~ 0,
                                    days_since_clock_start > 126 & days_since_clock_start <= timeframe_1_bound ~ 1,
                                    days_since_clock_start > timeframe_1_bound ~ 2
    ))
    
  
  df_grouped_by_time_period <- df_filtered %>%  # Aggregates each patient's HC use by time period, 
                                                # then removes any who do not have data for the whole period
    group_by(patients) %>%
    mutate(max_time_covered = max(days_since_clock_start)) %>%
    ungroup() %>%
    group_by(patients, time_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>%
    filter(max_time_covered == timeframe_2_bound) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
           group == intervention_group ~ 1))

return(df_grouped_by_time_period)

}

# Function applied to produce data for comparing the 25-30 week waiters group
df_30_weeks <- prep_data_function(df = waiting_list_df, 
                           intervention_group = '25-30 weeks', 
                           control_group = '<= 18 weeks',
                           timeframe_1_bound = 168,       # Lower bound of the bin in days  
                           timeframe_2_bound = 294)       # Upper bound of the bin plus 12 weeks in days 

# Function applied to produce data for comparing the 31-36 week waiters group
df_36_weeks <- prep_data_function(df = waiting_list_df, 
                                  intervention_group = '31-36 weeks', 
                                  control_group = '<= 18 weeks',
                                  timeframe_1_bound = 210,     # Lower bound of the bin in days 
                                  timeframe_2_bound = 336)     # Upper bound of the bin plus 12 weeks in days


# Run regression for 25-30 week waiters and consider coefficients

fe_ols_30weeks <- feols(total_hc_use ~ i(time_period, treated, ref = 0) |  # Interaction between time and treatment
                  patients + time_period,  # Fixed effects
             data = df_30_weeks)

summary(fe_ols_30weeks)

# Plot ATT coefficients over time for 25-30 week waiters
iplot(fe_ols_30weeks)


# Run regression for 31-36 week waiters and consider coefficients 
fe_ols_36weeks <- feols(total_hc_use ~ i(time_period, treated, ref = 0) |
                          patients + time_period,
                        data = df_36_weeks)

# Plot ATT coefficients over time for 31-36 week waiters
summary(fe_ols_36weeks)

iplot(fe_ols_36weeks)
