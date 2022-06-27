
### _create_imputed_geology.impute_primary_geo_columns.R

impute_primary_geo_columns = 
  function(fn_data_2_geo, fn_primary_geo_variables){
  
  formula_call = paste0(fn_primary_geo_variables[1], " ~ LOC_MP_LATITUDE + LOC_MP_LONGITUDE")
  
  ranger_model = ranger(as.formula(formula_call),
                        data = fn_data_2_geo %>% 
                          select(one_of(fn_primary_geo_variables[1], "LOC_MP_LATITUDE", "LOC_MP_LONGITUDE")) %>% 
                          na.omit(),
                        seed = 42)
  
  fn_data_2_geo = 
    fn_data_2_geo %>% 
    mutate(PREDS = predict(ranger_model, fn_data_2_geo)$predictions)
  
  fn_data_2_geo[, fn_primary_geo_variables[1]] =
    ifelse(is.na(fn_data_2_geo[, fn_primary_geo_variables[1]]),
           fn_data_2_geo$PREDS,
           fn_data_2_geo[, fn_primary_geo_variables[1]])
  
  fn_data_2_geo = 
    fn_data_2_geo %>%
    select(-PREDS)
  
  for (i in 2:length(fn_primary_geo_variables)) {
    
    formula_call = paste0(fn_primary_geo_variables[i], 
                          " ~ LOC_MP_LATITUDE + LOC_MP_LONGITUDE + ",
                          paste0(fn_primary_geo_variables[1:(i - 1)], collapse = " + ")
    )
    
    ranger_model = ranger(as.formula(formula_call),
                          data = fn_data_2_geo %>% 
                            select(one_of(fn_primary_geo_variables[1:i], "LOC_MP_LATITUDE", "LOC_MP_LONGITUDE")) %>% 
                            na.omit(),
                          seed = 42)
    
    fn_data_2_geo = 
      fn_data_2_geo %>% 
      mutate(PREDS = predict(ranger_model, fn_data_2_geo)$predictions)
    
    fn_data_2_geo[, fn_primary_geo_variables[i]] =
      ifelse(is.na(fn_data_2_geo[, fn_primary_geo_variables[i]]),
             fn_data_2_geo$PREDS,
             fn_data_2_geo[, fn_primary_geo_variables[i]])
    
    fn_data_2_geo = 
      fn_data_2_geo %>%
      select(-PREDS)
    
  }
  
  return(fn_data_2_geo)
  
}