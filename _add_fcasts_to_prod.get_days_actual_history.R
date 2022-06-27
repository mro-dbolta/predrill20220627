
get_days_actual_history = 
  function(
    fn_oil_production_1
    ){
  result =
    fn_oil_production_1 %>% 
    select(-VOLUME) %>% 
    group_by(API) %>% 
    filter(CUM_PRODUCING_DAYS == max(CUM_PRODUCING_DAYS)) %>% 
    ungroup()
  
  return(result)
}