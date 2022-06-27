
### _create_imputed_geology.impute_secondary_geo_columns.R

impute_secondary_geo_columns = 
  function(fn_data_2_geo, 
           fn_primary_geo_variables,
           fn_secondary_geo_variables){
    
    for (i in 1:length(fn_secondary_geo_variables)) {
      
      formula_call = paste0(fn_secondary_geo_variables[i], 
                            " ~ ",
                            paste0(fn_primary_geo_variables, collapse = " + ")
      )
      
      ranger_model = ranger(as.formula(formula_call),
                            data = fn_data_2_geo %>% 
                              select(one_of(fn_primary_geo_variables, 
                                            fn_secondary_geo_variables[i])) %>% 
                              na.omit(),
                            seed = 42)
      
      fn_data_2_geo = 
        fn_data_2_geo %>% 
        mutate(PREDS = predict(ranger_model, fn_data_2_geo)$predictions)
      
      fn_data_2_geo[, fn_secondary_geo_variables[i]] =
        ifelse(is.na(fn_data_2_geo[, fn_secondary_geo_variables[i]]),
               fn_data_2_geo$PREDS,
               fn_data_2_geo[, fn_secondary_geo_variables[i]])
      
      fn_data_2_geo = 
        fn_data_2_geo %>%
        select(-PREDS)
      
    }
    
    return(fn_data_2_geo)
    
  }