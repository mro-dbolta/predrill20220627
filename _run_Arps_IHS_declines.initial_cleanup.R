

initial_cleanup =
  function(df){
    df %>% 
      ### remove water
      filter(PRODUCT != "W") %>% 
      ### only keep records with volume
      filter(!is.na(VOLUME)) %>% 
      filter(VOLUME > 0.0) %>% 
      ### keep only api's with length > 5
      mutate(api_length = str_length(API)) %>% 
      filter(api_length > 5L) %>% 
      select(-api_length) %>% 
      ### calculate api10
      mutate(API10 = str_sub(API, end = 10L)) %>% 
      convert_PROD_DATE_string_to_date()
  }