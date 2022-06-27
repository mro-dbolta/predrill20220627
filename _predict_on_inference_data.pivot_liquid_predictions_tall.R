
pivot_liquid_predictions_tall =
  function(
    df,
    starter_string = ""
  ){
    full_starter_string = paste0(starter_string,
                                 "LIQUID_")
    column_name = str_sub(full_starter_string, end = -2L)
    
    df %>% 
      select(API, SENSITIVITY_CASE, starts_with(full_starter_string)) %>% 
      ### truncate column names to just their times (the portion of string at last "_")
      rename_with(~ str_sub(.x, start = str_locate_start_of_last_instance(.x, "_")), contains(full_starter_string)) %>% 
      pivot_longer(cols = starts_with("_"), names_to = "DAYS", values_to = column_name) %>% 
      mutate(DAYS = as.numeric(str_sub(DAYS, start = 2L)))
  }