
complete_setup_prod_data_for_oil_fits =
  function(df, MINIMUM_NUMBER_RECORDS_TO_FIT){
    ### setup for oil fits
    procount_prod_data_2a_LIQUID = setup_for_oil_fits(df, MINIMUM_NUMBER_RECORDS_TO_FIT)
    
    ### get avg rate over last n records (not days) per well  
    last_30_days_avg_rate = 
      get_avg_rate_over_last_n_records(procount_prod_data_2a_LIQUID)
    
    ### left join this column back into oil production data
    init_nrows = nrow(procount_prod_data_2a_LIQUID)
    
    procount_prod_data_2a_LIQUID =
      procount_prod_data_2a_LIQUID %>% 
      left_join(last_30_days_avg_rate, by = "API")
    
    ### Error check
    if (nrow(procount_prod_data_2a_LIQUID) != init_nrows) stop("Left joining last_30_days_avg_rate changed row count")
    rm(init_nrows)
    rm(last_30_days_avg_rate)
    
    ### get start dates of oil fit data
    oil_fit_start_dates_df = get_start_dates_of_oil_fit_data(procount_prod_data_2a_LIQUID)
    
    init_nrows = nrow(procount_prod_data_2a_LIQUID)
    procount_prod_data_2a_LIQUID = 
      procount_prod_data_2a_LIQUID %>% 
      left_join(oil_fit_start_dates_df, by = "API")
    if (nrow(procount_prod_data_2a_LIQUID) != init_nrows) stop("Left joining oil_fit_start_dates_df changed row count")
    rm(init_nrows)
    rm(oil_fit_start_dates_df)
    
    return(procount_prod_data_2a_LIQUID)
  }