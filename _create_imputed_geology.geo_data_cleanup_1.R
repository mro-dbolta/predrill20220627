### _create_imputed_geology.geo_data_cleanup_1.R
geo_data_cleanup_1 = 
  function(fn_data_1, 
           fn_geo_feats_remove, 
           fn_mgeo_header_columns_not_needed){
    data_1b_EF_TX = fn_data_1 %>% 
      filter(GEO_PROD_ZONE_NAME == "EAGLE FORD") %>% 
      filter(!is.na(LOC_MP_LATITUDE), 
             !is.na(LOC_MP_LONGITUDE))
    
    data_1b_EF_TX = 
      data_1b_EF_TX %>% 
      select(-one_of(fn_geo_feats_remove)) %>% 
      select(-one_of(fn_mgeo_header_columns_not_needed))
    
    return(data_1b_EF_TX)
  }