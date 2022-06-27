
bandaid_quantile_predictions_with_factor =
  function(
    quantile_prediction_csv_locn,
    quantile_stretch_factor = 1.0,
    run_if = FALSE
  ){
    if (run_if) {
      "
      When viewing raw quantile results, it may be necessary to stretch them wider/pinch narrower,
      this is possibly more needed when sampling with replacement is used in random forest.
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      
      quantile_prediction =
        load_data_that_has_api(quantile_prediction_csv_locn)
      
      ### Error check
      quantile_prediction_column_name_check =
        quantile_prediction %>% 
        select(-API, -SENSITIVITY_CASE) %>% 
        select(-starts_with("P10_PREDICTIONS_"),
               -starts_with("P50_PREDICTIONS_"),
               -starts_with("P90_PREDICTIONS_")) %>% 
        names()
      if (length(quantile_prediction_column_name_check) > 0) {
        stop(paste0("Unexpected add'l columns in quantile prediction df: ", quantile_prediction_column_name_check))
        }
      
      target_variables = 
        quantile_prediction %>% 
        select(-API, -SENSITIVITY_CASE) %>% 
        names()
      
      target_variables_no_prefixes =
      str_sub(target_variables,
              start = 17L) %>% 
        unique()
      
      ### iterate through each p10/p50/p90 triplet and adjust per factor
      for (target_var in target_variables_no_prefixes) {
        p10_var = paste0("P10_PREDICTIONS_", target_var)
        p50_var = paste0("P50_PREDICTIONS_", target_var)
        p90_var = paste0("P90_PREDICTIONS_", target_var)
        
        quantile_prediction[,p10_var] = 
          quantile_prediction[,p50_var] - 
          quantile_stretch_factor * abs(quantile_prediction[,p50_var] - quantile_prediction[,p10_var])
        
        quantile_prediction[,p90_var] = 
          quantile_prediction[,p50_var] + 
          quantile_stretch_factor * abs(quantile_prediction[,p90_var] - quantile_prediction[,p50_var])
        
        ### fix if stretch made predictions negative
        quantile_prediction[,p10_var] = ifelse(quantile_prediction[,p10_var] < 0, 0, quantile_prediction[,p10_var])
      }
      
      
      quantile_prediction %>% 
        fwrite(quantile_prediction_csv_locn)
      
      return(paste0("P10 and P90 predictions updated for stretch factor ",
                    quantile_prediction_csv_locn))
      
    }
  }