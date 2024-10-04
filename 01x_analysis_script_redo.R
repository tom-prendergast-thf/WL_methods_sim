## WAITING LIST DATA SIMULATION - PROCESSING AND REGRESSIONS
# In this script, we process the simulated waiting list data for use in difference-in-difference comparisons. 
# We then run an example difference in difference, with a placebo test before to check for the plausibility of our assumptions. 

source('WL_sim_script.R')

## Filter for waits below 12 weeks

waiting_list_df <- full_time_df %>%
  filter(wait_times > 84)

waiting_list_df$patients <- as.factor(waiting_list_df$patients)

## Assign to groups

waiting_list_df <- waiting_list_df %>%
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
  filter(after_clock_start == 1)                     # Filter so only entries after clock start are included
  
 

## FUNCTIONS

# Create functions which prepare data for comparisons and checks. These can be used to create 
# usable dataframes for each group comparison

# This function creates two comparison groups and filters the data . This dataset can then be used to graphically 
# examine the data day-by-day healthcare use of each group if desired, a crude but quick check to gauge the 
# plausibility of parallel trends

filter_data_function <- function(df, #the dataframe we're transforming
         intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
         control_group,   # A string describing the control group, typically '<= 18 weeks' in our case
         time_procedure_received,  # A number delineating the upper bound of our waiting period bin in days, 
                             # e.g. if comparing 31-36 week waiters this would be 252
         length_of_recovery # The length of our desired follow-up period for exclusion in days - by default 28
         ){
  
  # Define start and end of reference period based on parameters above
  start_of_reference_period <- time_procedure_received + length_of_recovery
  end_of_reference_period <- start_of_reference_period + 168
  
  df_filtered <- df %>%     # Filters for our control and intervention group of interest, then creates time periods
    filter(group == control_group | group == intervention_group) %>%
    filter(days_since_clock_start <= end_of_reference_period) %>%
    mutate(time_period = case_when(days_since_clock_start < time_procedure_received ~ 1,
                                    days_since_clock_start >= time_procedure_received & days_since_clock_start < start_of_reference_period ~ 100,
                                    days_since_clock_start >= start_of_reference_period ~ 0
    ))  %>% 
    group_by(patients) %>%
    mutate(max_time_covered = max(days_since_clock_start)) %>%
    ungroup() %>%
    filter(max_time_covered == end_of_reference_period) %>%
  
    return(df_filtered)
}


# This function creates grouped data which can be used to run our regressions

group_data_function <- function(df_filtered, #the filtered dataframe we're transforming
          intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
          control_group,   # A string describing the control group, typically '<= 18 weeks' in our case
          time_procedure_received,  # A number delineating the upper bound of our waiting period bin in days, 
          # e.g. if comparing 31-36 week waiters this would be 252
          length_of_recovery # The length of our desired follow-up period for exclusion in days - by default 28
){
  
  # Define start and end of reference period based on parameters above
  start_of_reference_period <- time_procedure_received + length_of_recovery
  end_of_reference_period <- start_of_reference_period + 168
  
  df_grouped_by_time_period <- df_filtered %>%  # Aggregates each patient's HC use by time period, 
                                                # then removes the recovery period
    
    group_by(patients, time_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
           group == intervention_group ~ 1)) %>%
    filter(time_period != 100)

return(df_grouped_by_time_period)

}



## This function can be used to perform placebo tests on the reference period. 

do_placebo_test <- function(filtered_df,  # Dataframe output from the filtering function above
                            intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
                            control_group,   # A string describing the control group, typically '<= 18 weeks' in our case# A dataframe output by the filter_data_function above
                            time_procedure_received){   # Time in which procedure was received in days, indicating what group we're dealing with 
  
  more_filtered <- filtered_df %>%
    filter(time_period == 0) %>%
    mutate(placebo_period = case_when(days_since_clock_start <= time_procedure_received + 112 ~ 0,
                                       days_since_clock_start > time_procedure_received + 112 ~ 1))
  
  
  df_grouped_by_time_period <- more_filtered %>%  # Aggregates each patient's HC use by time period
                                                
    
    group_by(patients, placebo_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
                               group == intervention_group ~ 1))
  
  plot <- df_grouped_by_time_period %>%
    group_by(group, placebo_period) %>%
    summarise(hc_use = sum(total_hc_use)) %>%
  ggplot(., aes(x = placebo_period, y = hc_use, colour = group)) +
    geom_line() +
    theme_minimal()
  
  placebo_test <- feols(total_hc_use ~ i(placebo_period, treated, ref = 0) # Interaction between time and treatment
                          |  patients + placebo_period,  # Fixed effects
                          data = df_grouped_by_time_period)
  
  summary(placebo_test)
  
  return(list(plot, summary(placebo_test)))
}



## DATA PREPARATION AND CHECKS

# Function applied to produce data for comparing the 25-30 week waiters group
df_30_weeks_filtered <- filter_data_function(df = waiting_list_df, 
                           intervention_group = '25-30 weeks', 
                           control_group = '<= 18 weeks',
                           time_procedure_received = 210,       # Upper bound of the bin in days  
                           length_of_recovery = 28)       # Default four week recovery period 

df_30_weeks_grouped <- group_data_function(df = df_30_weeks_filtered, 
                                             intervention_group = '25-30 weeks', 
                                             control_group = '<= 18 weeks',
                                             time_procedure_received = 210,       # Upper bound of the bin in days  
                                             length_of_recovery = 28)       # Default four week recovery period



# Do placebo test to check for plausibility of parallel trends

do_placebo_test(filtered_df = df_30_weeks_filtered, 
                intervention_group = '25-30 weeks', 
                control_group = '<= 18 weeks',
                time_procedure_received = 210)


### DIFFERENCE-IN-DIFFERENCE

# Run regression for 25-30 week waiters and examine coefficients

fe_ols_30weeks <- feols(total_hc_use ~ i(time_period, treated, ref = 0) # Interaction between time and treatment
                       |  patients + time_period,  # Fixed effects
             data = df_30_weeks_grouped)

summary(fe_ols_30weeks)

iplot(fe_ols_30weeks)
