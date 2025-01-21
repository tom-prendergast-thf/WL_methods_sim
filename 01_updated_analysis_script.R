## WAITING LIST DATA SIMULATION - PROCESSING AND ANALYSIS
# In this script, we process the simulated waiting list data for use in difference-in-difference comparisons. 
# We then run an example difference in difference, with a placebo test before to check for the plausibility of our assumptions. 

source('WL_sim_script.R')

## Filter for waits below 12 weeks and assign to groups

waiting_list_df <- full_time_df %>%
  filter(wait_times > 84) %>%
  mutate(group = case_when(wait_times > 84 & wait_times <= 126 ~ '<= 18 weeks', # Create six-week bins
                           wait_times > 126 & wait_times <= 168 ~ '19-24 weeks',
                           wait_times > 168 & wait_times <= 210 ~ '25-30 weeks',
                           wait_times > 210 & wait_times <= 252 ~ '31-36 weeks',
                           wait_times > 252 & wait_times <= 294 ~ '37-42 weeks',
                           wait_times > 294 & wait_times <= 336 ~ '43-48 weeks',
                           wait_times > 336 & wait_times <= 378 ~ '49-54 weeks',
                           wait_times > 378 & wait_times <= 420 ~ '55+ weeks'
  )) %>%
  mutate(after_clock_start = case_when(days < clock_starts ~ 0,
                                       TRUE ~ 1)) %>%
  filter(after_clock_start == 1) %>%                    # Filter so only entries after clock start are included
  mutate(washout_period_end_date = clock_stops + 28) %>%                # Add markers for when the recovery and reference periods should begin    
  mutate(follow_up_end_date = washout_period_end_date + wait_times) %>%
  mutate(washout_period_end_days = wait_times + 28) %>%
  mutate(follow_up_end_days = washout_period_end_days + wait_times)
 
waiting_list_df$patients <- as.factor(waiting_list_df$patients)

###################################################################################################################################
###################################################################################################################################

## FUNCTIONS

# Create functions which prepare data for comparisons and checks. These can be used to create 
# usable dataframes for each group comparison and the various checks that can be performed in advance.

# This function creates two comparison groups and filters the data accordingly. This dataset can then be used as 
# an input for the placebo test function, or passed on to the grouping function to prepare for diff-in-diff 

filter_data_function <- function(df, #the dataframe we're transforming
         intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
         control_group   # A string describing the control group, typically '<= 18 weeks' in our case
         ){
  
  df_filtered <- df %>%     # Filters for our control and intervention group of interest, then creates time periods
    filter(group == control_group | group == intervention_group) %>%
    filter(days_since_clock_start <= follow_up_end_days) %>%
    mutate(time_period = case_when(days_since_clock_start < wait_times ~ 1,
                                    days_since_clock_start >= wait_times & days_since_clock_start < washout_period_end_days ~ 100,
                                    days_since_clock_start >= washout_period_end_days ~ 0
    ))  %>% 
    group_by(patients) %>%
    mutate(max_time_covered = max(days_since_clock_start)) %>%
    ungroup() %>%
    filter(max_time_covered == follow_up_end_days)
    
    return(df_filtered)
}


# This function creates grouped data which can be used to run our regressions

group_data_function <- function(df_filtered, # the filtered dataframe we're transforming
          intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
          control_group   # A string describing the control group, typically '<= 18 weeks' in our case
){
  
  df_grouped_by_time_period <- df_filtered %>%  # Aggregates each patient's HC use by time period, 
                                                # then removes the recovery period and adds average weekly use over each time period 
    
    group_by(patients, time_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
           group == intervention_group ~ 1)) %>%
    filter(time_period != 100) %>%
   mutate(avg_weekly_use = total_hc_use/as.numeric(max_time_covered)*7)

return(df_grouped_by_time_period)

}



## This function can be used to perform placebo tests on the reference period. 

do_placebo_test <- function(filtered_df,  # Dataframe output from the filtering function above
                            intervention_group,  # A string describing the group we are interested in, eg. '19-36 weeks'
                            control_group   # A string describing the control group, typically '<= 18 weeks' in our case# A dataframe output by the filter_data_function above
                            ){    
  
  more_filtered <- filtered_df %>%
    filter(time_period == 0) %>%
    mutate(placebo_period = case_when(days_since_clock_start <= wait_times + (wait_times/2) ~ 0,
                                       days_since_clock_start > wait_times + (wait_times/2) ~ 1))
  
  
  df_grouped_by_time_period <- more_filtered %>%  # Aggregates each patient's HC use by time period
                                                
    
    group_by(patients, placebo_period, group, ages, deprivation, max_time_covered) %>%
    summarise(total_hc_use = sum(healthcare_use)) %>% 
    mutate(treated = case_when(group == control_group ~ 0,
                               group == intervention_group ~ 1))
  
  placebo_test <- feols(total_hc_use ~ i(placebo_period, treated, ref = 0) # Interaction between time and treatment
                          |  patients + placebo_period,  # Fixed effects
                          data = df_grouped_by_time_period)
  
  summary(placebo_test)
  
  return(list(summary(placebo_test)))
}


# Functions for difference in difference pt 1: Comparison of group totals 

totals_table_function <- function(df,
                                 control_group,
                                 intervention_group){

df_group <- df %>%
  group_by(time_period, group) %>%
  summarise(hc_use = sum(total_hc_use), days_covered = sum(as.numeric(max_time_covered)), no_patients = n_distinct(patients))

table_fill <- data.frame(matrix(ncol = 3, nrow = 4))

colnames(table_fill) <- c('metrics', control_group, intervention_group)

table_fill$metrics <- c('Healthcare use reference period', 'Healthcare use intervention period', 'Excess use', 'person-weeks')

table_fill[2] <- c(df_group$hc_use[df_group$time_period == 0 & df_group$group == control_group],
                              df_group$hc_use[df_group$time_period == 1 & df_group$group == control_group],
                              NA,
                              df_group$days_covered[df_group$time_period == 0 & df_group$group == control_group]/7)

table_fill[3] <- c(df_group$hc_use[df_group$time_period == 0 & df_group$group == intervention_group],
                              df_group$hc_use[df_group$time_period == 1 & df_group$group == intervention_group],
                              ((df_group$hc_use[df_group$time_period == 1 & df_group$group == intervention_group]-df_group$hc_use[df_group$time_period == 0 & df_group$group == intervention_group]) -
                              (df_group$hc_use[df_group$time_period == 1 & df_group$group == control_group]-df_group$hc_use[df_group$time_period == 0 & df_group$group == control_group])),
                              df_group$days_covered[df_group$time_period == 0 & df_group$group == intervention_group]/7)

return(table_fill)

}
  

# Pt. 2 Run fixed effects regression on average weekly use

run_fixed_effects <- function(df){   # The name of the 
  # OUTCOME VARIABLE CHANGED HERE
  fe_ols <- feols(total_hc_use ~ i(time_period, treated, ref = 0) # Interaction between time and treatment
                          |  patients + time_period,  # Fixed effects
                          data = df)
  
  return(summary(fe_ols))
  
}

#############################################################################################################################
#############################################################################################################################

### EXAMPLE WORKFLOW

# In the example below, we apply the functions created above to run for the 25-30 week group

# Function applied to produce data for comparing the 25-30 week waiters group
df_30_weeks_filtered <- filter_data_function(df = waiting_list_df, 
                                             intervention_group = '25-30 weeks', 
                                             control_group = '<= 18 weeks')        

df_30_weeks_grouped <- group_data_function(df = df_30_weeks_filtered, 
                                           intervention_group = '25-30 weeks', 
                                           control_group = '<= 18 weeks')     
  

# Do placebo test to further check for plausibility of parallel trends. Results indicate that no statistically significant difference in trend 
# is observed in the placebo period, lending credence to parallel trends assumption

do_placebo_test(filtered_df = df_30_weeks_filtered, 
                intervention_group = '25-30 weeks', 
                control_group = '<= 18 weeks')

# Group total comparisons

totals_30weeks <- totals_table_function(df = df_30_weeks_grouped,
                                        control_group = '<= 18 weeks',
                                        intervention_group = '25-30 weeks')

# Fixed effects

run_fixed_effects(df_30_weeks_grouped)




######################################################################################
## The above workflow, if desired, can also be combined into a single function to be more easily applied to many groups

total_did_workflow <- function(initial_df,
                               compared_group){

df_filtered <- filter_data_function(df = initial_df, 
                                             intervention_group = compared_group, 
                                             control_group = '<= 18 weeks')        

df_grouped <- group_data_function(df = df_filtered, 
                                           intervention_group = compared_group, 
                                           control_group = '<= 18 weeks')     


# Do placebo test to further check for plausibility of parallel trends. Results indicate that no statistically significant difference in trend 
# is observed in the placebo period, lending credence to parallel trends assumption

placebo <- do_placebo_test(filtered_df = df_filtered, 
                intervention_group = compared_group, 
                control_group = '<= 18 weeks')

# Group total comparisons

totals_weeks <- totals_table_function(df = df_grouped,
                                        control_group = '<= 18 weeks',
                                        intervention_group = compared_group)

# Fixed effects

fe <- run_fixed_effects(df_grouped)

return(list(placebo, totals_weeks, fe))

}

# Example in action
total_did_workflow(initial_df = waiting_list_df, compared_group = '31-36 weeks')
