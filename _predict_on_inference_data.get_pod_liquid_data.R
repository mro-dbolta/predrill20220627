
get_pod_liquid_data =
  function(
    pod_data_locn
  ){
    "
    This reuses code from 5_impute_fcasts_into_production
    
    Get all pod forecasts, for both pod and pdp wells
    
    Returns df of columns API, DAYS, LIQUID_ES_POD
    "
    
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    
    pod_data_in = load_data_that_has_api(target_path = pod_data_locn)
    
    oil_pod_1 =
      pod_data_in %>% 
      filter(ASSET_NAME == "EAGLEFORD",
             PRODUCT == "L") %>% 
      select(-PRODUCT, -ASSET_NAME, -VOLUME, -SOURCE, -PROD_DATE) %>% 
      filter(CUM_PRODUCING_DAYS <= 12 * 40 * 30) %>% 
      rename(DAYS = CUM_PRODUCING_DAYS,
             LIQUID_ES_POD = CUM_VOLUME)
    
    return(oil_pod_1)
    
  }