
get_avg_rate_over_last_n_records = 
  function(df){
    df %>% 
      mutate(counter = 1.0) %>% 
      arrange(desc(OIL_CUM_PRODUCING_DAYS)) %>% 
      group_by(API) %>% 
      mutate(records_counting_backwards = cumsum(counter)) %>% 
      ungroup() %>% 
      filter(records_counting_backwards <= number_of_records_at_end_to_anchor_on) %>% 
      select(-counter, -records_counting_backwards) %>% 
      group_by(API) %>% 
      mutate(LAST_30_DAYS_MEAN_VOLUME = mean(VOLUME)) %>% 
      ungroup() %>% 
      select(API, LAST_30_DAYS_MEAN_VOLUME) %>% 
      distinct()
  }