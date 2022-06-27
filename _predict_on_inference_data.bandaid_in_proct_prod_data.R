
bandaid_in_proct_prod_data =
  function(
    data_with_predictions,
    procount_monthly_data_locn
  ){
    ### split predictions data, only wells apparently with 0 days will get bandaids
    data_with_predictions_0_days =
      data_with_predictions %>% 
      filter(DAYS_ACTUAL_HISTORY < 1)
    
    data_with_predictions_positive_days =
      data_with_predictions %>% 
      filter(DAYS_ACTUAL_HISTORY > 0)
    
    procount_monthly_data =
      load_data_that_has_api(procount_monthly_data_locn) %>% 
      select(-PROD_DATE, -VOLUME, -SOURCE, -VERSION_DATE) %>% 
      filter(PRODUCT == "L") %>% 
      select(-PRODUCT) %>% 
      ### keep only needed wells
      filter(API %in% data_with_predictions_0_days$API)
    
    procount_updated_actual_days_on = 
      procount_monthly_data %>% 
      select(API, CUM_PRODUCING_DAYS) %>% 
      group_by(API) %>% 
      summarise(DAYS_ACTUAL_HISTORY_2 = max(CUM_PRODUCING_DAYS),
                .groups = "drop")
    
    
    ### join in updated days_actual_history
    data_with_predictions_0_days = 
      data_with_predictions_0_days %>% 
      left_join(procount_updated_actual_days_on, by = "API") %>% 
      ### update original column with new days_online
      mutate(DAYS_ACTUAL_HISTORY = ifelse(is.na(DAYS_ACTUAL_HISTORY_2),
                                          DAYS_ACTUAL_HISTORY,
                                          DAYS_ACTUAL_HISTORY_2)) %>% 
      select(-DAYS_ACTUAL_HISTORY_2)
    
    ### pivot procount data wide
    procount_monthly_data_wide =
      procount_monthly_data %>% 
      pivot_wider(values_from = CUM_VOLUME,
                  names_from = CUM_PRODUCING_DAYS,
                  names_prefix = "PROCT_")
    
    ### join procount production to predictions
    data_with_predictions_0_days =
      data_with_predictions_0_days %>%
      left_join(procount_monthly_data_wide, by = "API")
    
    
    proct_columns_time_steps =
      data_with_predictions_0_days %>% 
      select(starts_with("PROCT_")) %>% 
      ### extract proct column names
      names() %>% 
      ### keep only numerical portion of column name
      str_sub(start = 7L)
    
    ### overwrite values with procoutn data
    rows_to_update_with_proct = which(!is.na(data_with_predictions_0_days[,"PROCT_30"]))
    
    for (row_i in rows_to_update_with_proct) {
      for (proct_time_step in proct_columns_time_steps) {
        proct_volume_at_row_and_time =
          data_with_predictions_0_days[row_i, paste0("PROCT_", proct_time_step)]
        
        if (!is.na(proct_volume_at_row_and_time)) {
          data_with_predictions_0_days[row_i, paste0("LIQUID_", proct_time_step)] =
            proct_volume_at_row_and_time
        }
      }
    }
    data_with_predictions_0_days =
      data_with_predictions_0_days %>% 
      select(-starts_with("PROCT_"))
    
    data_with_predictions =
      data_with_predictions_0_days %>% 
      bind_rows(data_with_predictions_positive_days)
    
    return(data_with_predictions)
  }