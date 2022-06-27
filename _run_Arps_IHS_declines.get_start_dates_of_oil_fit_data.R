
get_start_dates_of_oil_fit_data =
  function(df){
    df %>% 
      filter(OIL_CUM_PRODUCING_DAYS == DAYS_PER_TIMESTEP) %>% 
      mutate(PROD_DATE_OIL_DAY_1 = PROD_DATE) %>% 
      select(API, PROD_DATE_OIL_DAY_1) %>% 
      distinct()
  }