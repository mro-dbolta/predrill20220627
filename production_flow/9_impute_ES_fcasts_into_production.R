
impute_ES_fcasts_into_production = function(previous_integrated_prod_data_location,
                                            ES_declines_data_location,
                                            dca_output_target_directory,
                                            run_if = TRUE){
  
  if (run_if) {
    
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("general_purpose_support_functions/arps_equations.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    source("general_purpose_support_functions/install_load_libraries.R", local = TRUE)
    source("anonymous_support_functions/_add_fcasts_to_prod.make_40_year_template_df.R", local = TRUE)
    
    
    install_load_libraries("lubridate")
    
    PERIOD_LENGTH = 30 # DAYS
    
    previous_integrated_prod_data = load_data_that_has_api(target_path = previous_integrated_prod_data_location)
    ES_declines_data_in = load_data_that_has_api(target_path = ES_declines_data_location)
    
    previous_integrated_prod_data =
      previous_integrated_prod_data %>% 
      mutate(PROD_DATE = ymd(PROD_DATE))
    
    ##############################################
    
    APIs_getting_updated = unique(ES_declines_data_in$API)
    
    old_prod_data_to_leave_alone =
      previous_integrated_prod_data %>% 
      filter(!(API %in% APIs_getting_updated))
    
    old_prod_data_to_update =
      previous_integrated_prod_data %>% 
      filter(API %in% APIs_getting_updated)
    
    prod_template = make_40_year_template_df(APIs_getting_updated)
    
    ##############################################
    
    prod_data_to_update_with_40_yr_template =
      prod_template %>% 
      ### add available arps and ES data
      left_join(old_prod_data_to_update %>% 
                  select(-DAYS_OF_ACTUAL_PRODUCTION),
                by = c("API", "CUM_PRODUCING_DAYS")) %>%
      left_join(old_prod_data_to_update %>% 
                  select(API, DAYS_OF_ACTUAL_PRODUCTION) %>% 
                  distinct(),
                by = "API") %>% 
      ### add arps fit parameters
      left_join(ES_declines_data_in %>% 
                  select(-Di_Ae_tries,
                         -MEAN_ABS_ERROR_LOG10_VOLUME),
                by = "API") %>% 
      ### add arps volumes
      mutate(ARPS_VOLUME = arps_cum(days = CUM_PRODUCING_DAYS, 
                                    q_i = IP_tries, 
                                    di_nom_daily = Di_daily_nom_tries, 
                                    b_factor = b_tries) -
               arps_cum(days = (CUM_PRODUCING_DAYS - PERIOD_LENGTH), 
                        q_i = IP_tries, 
                        di_nom_daily = Di_daily_nom_tries, 
                        b_factor = b_tries)) %>% 
      ### impute Arp volume
      mutate(VOLUME = ifelse(is.na(VOLUME), ARPS_VOLUME, VOLUME)) %>% 
      select(-contains("_tries"),
             -ARPS_VOLUME)
    
    ### fill PROD_DATE
    prod_data_to_update_with_40_yrs =
      prod_data_to_update_with_40_yr_template %>% 
      ### get last date and days_on for each well
      left_join(prod_data_to_update_with_40_yr_template %>% 
                  filter(!is.na(PROD_DATE)) %>% 
                  group_by(API) %>%   
                  filter(CUM_PRODUCING_DAYS == max(CUM_PRODUCING_DAYS)) %>% 
                  ungroup() %>% 
                  select(API, PROD_DATE, CUM_PRODUCING_DAYS) %>% 
                  rename(LATEST_PROD_DATE = PROD_DATE,
                         LATEST_CUM_PRODUCING_DAYS = CUM_PRODUCING_DAYS),
                by = "API") %>% 
      mutate(PROD_DATE = if_else(is.na(PROD_DATE),
                                 LATEST_PROD_DATE + days(CUM_PRODUCING_DAYS) - days(LATEST_CUM_PRODUCING_DAYS),
                                 PROD_DATE)) %>%
      select(-contains("LATEST"))
    
    ##############################################
    
    final_data_all_at_40_yrs =
      old_prod_data_to_leave_alone %>% 
      bind_rows(prod_data_to_update_with_40_yrs) %>% 
      ### get cum volumes
      arrange(API, CUM_PRODUCING_DAYS) %>% 
      group_by(API) %>% 
      mutate(CUM_VOLUME = cumsum(VOLUME)) %>% 
      ungroup()
    
    
    
    final_data_all_at_40_yrs %>% 
    fwrite(paste0(
      add_path_slash_if_needed(dca_output_target_directory),
      "integrated_prod_data_ES_to_40_yrs.csv"            
      ))
    
    ############################################ 5 years pivot
    
    final_data_all_at_40_yrs_5_YEARS_PIVOT =
      final_data_all_at_40_yrs %>% 
      select(API, CUM_PRODUCING_DAYS, CUM_VOLUME) %>% 
      filter((CUM_PRODUCING_DAYS <= (5 * 360)) |
               (CUM_PRODUCING_DAYS == (40 * 360))) %>% 
      pivot_wider(values_from = CUM_VOLUME,
                  names_from = CUM_PRODUCING_DAYS,
                  names_prefix = "LIQUID_")
    
    final_data_all_at_40_yrs_5_YEARS_PIVOT %>% 
      fwrite(paste0(
        add_path_slash_if_needed(dca_output_target_directory),
        "final_data_all_at_40_yrs_5_YEARS_PIVOT.csv"            
      ))
    
    ########################################### 5 yrs plus 10 yr, 20 yr
    final_data_all_at_40_yrs_5_10_20_YEARS_PIVOT =
      final_data_all_at_40_yrs %>% 
      select(API, CUM_PRODUCING_DAYS, CUM_VOLUME) %>% 
      filter((CUM_PRODUCING_DAYS <= (5 * 360)) |
               (CUM_PRODUCING_DAYS == (10 * 360)) |
               (CUM_PRODUCING_DAYS == (20 * 360)) |
               (CUM_PRODUCING_DAYS == (40 * 360))) %>% 
      pivot_wider(values_from = CUM_VOLUME,
                  names_from = CUM_PRODUCING_DAYS,
                  names_prefix = "LIQUID_")
    
    final_data_all_at_40_yrs_5_10_20_YEARS_PIVOT %>% 
      fwrite(paste0(
        add_path_slash_if_needed(dca_output_target_directory),
        "final_data_all_at_40_yrs_5_10_20_YEARS_PIVOT.csv"            
      ))
  }
}