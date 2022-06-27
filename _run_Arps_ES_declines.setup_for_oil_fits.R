
setup_for_oil_fits =
  function(df, MINIMUM_NUMBER_RECORDS_TO_FIT){
    df %>% 
      ### remove wells without at least n records in historic fit time window
      mutate(counter = 1.0) %>% 
      group_by(API) %>% 
      mutate(NUMBER_RECORDS_IN_HISTORY = sum(counter)) %>% 
      ungroup() %>% 
      filter(NUMBER_RECORDS_IN_HISTORY >= MINIMUM_NUMBER_RECORDS_TO_FIT) %>% 
      select(-counter, -NUMBER_RECORDS_IN_HISTORY) %>% 
      ### sort the df
      arrange(API, CUM_PRODUCING_DAYS) %>% 
      ### get max volume
      group_by(API) %>% 
      mutate(MAX_VOLUME = max(VOLUME)) %>% 
      ungroup() %>% 
      ### find day of max volume
      mutate(MAX_VOLUME_INSTANTANEOUS_FLAG = if_else(VOLUME == MAX_VOLUME, TRUE, FALSE)) %>% 
      ### find all days at or after max rate
      group_by(API) %>% 
      mutate(MAX_VOLUME_CUMSUM_FLAG = cumsum(MAX_VOLUME_INSTANTANEOUS_FLAG)) %>% 
      ungroup() %>% 
      ### filter to days at or after max rate
      filter(MAX_VOLUME_CUMSUM_FLAG > 0.0) %>% 
      select(-MAX_VOLUME_INSTANTANEOUS_FLAG, -MAX_VOLUME_CUMSUM_FLAG) %>% 
      ### reset days_on to start at day of may rate
      group_by(API) %>% 
      mutate(MIN_CUM_PRODUCING_DAYS = min(CUM_PRODUCING_DAYS)) %>% 
      ungroup() %>% 
      mutate(OIL_CUM_PRODUCING_DAYS = CUM_PRODUCING_DAYS - MIN_CUM_PRODUCING_DAYS + DAYS_PER_TIMESTEP) %>% 
      select(-CUM_PRODUCING_DAYS, -MIN_CUM_PRODUCING_DAYS) 
  }