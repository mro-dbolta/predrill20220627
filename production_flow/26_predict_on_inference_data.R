predict_on_inference_data =
  function(
    inference_data_locns,
    output_and_rfsrc_model_locn,
    get_shaps = FALSE,
    run_if = FALSE
  ){
    if (run_if) {
      "
      Run rfsrc predictions given rfsrc model and inference data.
      The inference data location csv's are full data tables with a SENSITIVITY_CASE column added.
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/get_shaps_values.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.get_target_vars_from_model_call.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.rfsrc_replace_imputed_values_with_NAs.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.rfsrc_fix_NAs_so_predict_will_calc.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "data.table",
                                       "rlang", "randomForestSRC"))
      
      output_locn = 
        output_and_rfsrc_model_locn = add_path_slash_if_needed(output_and_rfsrc_model_locn)
      rfsrc_model_locn = paste0(output_and_rfsrc_model_locn,
                                "final_model.rds")
      rfsrc_model = readRDS(rfsrc_model_locn)
      
      model_call_target_vars = get_target_vars_from_model_call(rfsrc_model)
      
      PREDICTION_column_names = paste0("PREDICTIONS_", model_call_target_vars)
      
      ### bind_rows all inference data
      data_for_predictions =
        map(inference_data_locns,
            function(inference_data_locn){
              ### load inference data
              inference_data =
                load_data_that_has_api(inference_data_locn)
              
              return(inference_data)
              
            }) %>%
        bind_rows()
      
      ### rfsrc only handles factors
      ### set character input columns as factors with correct levels 
      ### based on globally available values from flat file
      best_features =
        fread(paste0(output_and_rfsrc_model_locn,
                     "best_features.csv"))
      best_features = best_features$BEST_FEATURES
      
      columns_to_set_as_factors =
        data_for_predictions %>% 
        select(one_of(best_features)) %>% 
        select_if(is.character) %>% 
        names()
      
      data_for_predictions =
        data_for_predictions %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      data_with_predictions = data_for_predictions
      data_for_predictions = rfsrc_fix_NAs_so_predict_will_calc(data_for_predictions)
      
      rfsrc_prediction_no_impute = predict(rfsrc_model, data_for_predictions)
      rfsrc_prediction_with_impute = predict(rfsrc_model, data_for_predictions, na.action = "na.impute")
      
      length_PREDICTION_column_names = length(PREDICTION_column_names)
      ### get vector of NAs for bad predictions
      ### and index of short prediction vector to use when good
      if (length_PREDICTION_column_names == 1) { 
        NAs_index_indicator_vector = 
          rfsrc_replace_imputed_values_with_NAs(
            short_vector = rfsrc_prediction_no_impute$predicted,
            long_vector = rfsrc_prediction_with_impute$predicted
          )
      } else {
        NAs_index_indicator_vector = 
          rfsrc_replace_imputed_values_with_NAs(
            short_vector = rfsrc_prediction_no_impute$regrOutput[[model_call_target_vars[1]]]$predicted,
            long_vector = rfsrc_prediction_with_impute$regrOutput[[model_call_target_vars[1]]]$predicted
          )
      }
      
      for (target_var_i in 1:length_PREDICTION_column_names) {
        clean_vector_of_predictions =
          ### for each row of inference data
          map_dbl(1:length(NAs_index_indicator_vector),
                  function(i){
                    if (is.na(NAs_index_indicator_vector[i])) { 
                      ### if bad, return NA
                      na_dbl
                    } else { 
                      ### replace with good prediction
                      if (length_PREDICTION_column_names == 1){
                        rfsrc_prediction_no_impute$predicted[NAs_index_indicator_vector[i]]
                      } else {
                        rfsrc_prediction_no_impute$regrOutput[[model_call_target_vars[target_var_i]]]$predicted[NAs_index_indicator_vector[i]]
                      }
                    }
                  })
        
        data_with_predictions =
          data_with_predictions %>% 
          mutate(!! PREDICTION_column_names[target_var_i] := 
                   clean_vector_of_predictions)
        
      }
      
      data_with_predictions %>% 
        fwrite(paste0(output_locn,
                      "data_with_predictions_1.csv"))
      
      if (get_shaps) {
        
        shap_caret_dummy = readRDS(paste0(output_and_rfsrc_model_locn,
                                          "shap_caret_dummy.rds"))
        
        shap_tree_unified_to_model = readRDS(paste0(output_and_rfsrc_model_locn,
                                                    "shap_tree_unified_to_model.rds"))
        
        data_for_shaps_with_header = 
          data_for_predictions %>% 
          select(API, SENSITIVITY_CASE, one_of(best_features)) %>% 
          na.omit()
        
        data_for_shaps =
          data_for_shaps_with_header %>% 
          select(-API, -SENSITIVITY_CASE)
        
        
        data_for_shaps_one_hot = 
          data.frame(predict(caret_dummy_for_shap, newdata = data_for_shaps))
        
        
        shap_inference_set =
          get_shaps_values(number_shaps_to_calc = nrow(data_for_shaps_one_hot),
                           train_data_one_hot = data_for_shaps_one_hot,
                           train_data_one_hot_with_header_columns = data_for_shaps_with_header,
                           header_columns = c("API", "SENSITIVITY_CASE"),
                           shap_tree_unified_to_model = shap_tree_unified_to_model,
                           input_vars_for_shap_model = best_features)
        
        shap_inference_set %>% 
          fwrite(paste0(output_and_rfsrc_model_locn,
                        "shap_inference_set.csv"))
        
        print("Shaps on inference data successful.")
        
      }
      
      ### return for good status exit
      return("Base predictions (not quantile preds) successful.")
      
    }
  }