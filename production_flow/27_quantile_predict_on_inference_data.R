
quantile_predict_on_inference_data =
  function(
    inference_data_locns,
    quantile_models_locn = "../10_modeling/run_1/quantile_regression_models/",
    number_iters_to_run = 10L,
    output_locn,
    fast_mode = FALSE, ### use MG's +/- 30% in a pinch
    fast_mode_base_preds_locn, ### in fast mode, specify base predictions file
    run_if = FALSE
  ){
    if (run_if) {
      "
      similar to the base production approach 
      but iterated several times to produce quantiles
      
      Includes a 'fast mode' which takes the base model predictions and swags up/down by 30%
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.get_target_vars_from_model_call.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.rfsrc_replace_imputed_values_with_NAs.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.rfsrc_fix_NAs_so_predict_will_calc.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.summarise_at_support_functions.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "data.table",
                                       "rlang"))
      
      output_locn = add_path_slash_if_needed(output_locn)
      
      if (fast_mode) {
        
        base_prediction_data =
          load_data_that_has_api(fast_mode_base_preds_locn) %>% 
          select(API, SENSITIVITY_CASE, starts_with("PREDICTIONS_")) %>% 
          mutate(across(starts_with("PREDICTIONS_"), ~(.x * 0.7), .names = "P10_{.col}")) %>% 
          mutate(across(starts_with("PREDICTIONS_"), ~(.x * 1.0), .names = "P50_{.col}")) %>% 
          mutate(across(starts_with("PREDICTIONS_"), ~(.x * 1.3), .names = "P90_{.col}")) %>% 
          select(-starts_with("PREDICTIONS_"))
        
        base_prediction_data %>%
          head(1) %>% 
          fwrite(paste0(output_locn,
                        "quantile_predictions_1_tall.csv"))
        
        base_prediction_data %>% 
          fwrite(paste0(output_locn,
                        "quantile_predictions_2_final.csv"))
        
        
      } else {
        
        
        ### Error check
        if (number_iters_to_run < 2) stop("Specified number_iters_to_run too low")
        
        quantile_models_locn = add_path_slash_if_needed(quantile_models_locn)
        
        
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
          fread(paste0(output_locn,
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
        
        
        
        data_with_predictions_tall =
          data_with_predictions_iter = 
          data_for_predictions %>% 
          select(API, SENSITIVITY_CASE)
        
        ### fix data sent to model for prediction
        data_for_predictions = rfsrc_fix_NAs_so_predict_will_calc(data_for_predictions)
        
        ### initial model read to get target variable names, NA_vector
        tree_i = 1
        
        rfsrc_model = readRDS(paste0(quantile_models_locn,
                                     "quantile_model_", tree_i, ".rds"))
        model_call_target_vars = get_target_vars_from_model_call(rfsrc_model)
        
        PREDICTION_column_names = paste0("PREDICTIONS_", model_call_target_vars)
        
        rfsrc_prediction_no_impute = predict(rfsrc_model, data_for_predictions)
        rfsrc_prediction_with_impute = predict(rfsrc_model, data_for_predictions, na.action = "na.impute")
        
        length_PREDICTION_column_names = length(PREDICTION_column_names)
        ### get vector of NAs for bad predictions
        ### and index of short prediction vector to use when good
        if (length_PREDICTION_column_names == 1){
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
        
        data_with_predictions_tall =
          data_with_predictions_tall %>% 
          mutate(tree_i = tree_i)
        
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
          
          data_with_predictions_tall =
            data_with_predictions_tall %>% 
            mutate(!! PREDICTION_column_names[target_var_i] := 
                     clean_vector_of_predictions)
        }
        
        ### remaining trees in run
        for (tree_i in 2:number_iters_to_run) {
          rfsrc_model = readRDS(paste0(quantile_models_locn,
                                       "quantile_model_", tree_i, ".rds"))
          
          rfsrc_prediction_no_impute = predict(rfsrc_model, data_for_predictions)
          
          data_with_predictions_iter =
            data_with_predictions_iter %>% 
            select(API, SENSITIVITY_CASE) %>% 
            mutate(tree_i = tree_i)
          
          for (target_var_i in 1:length(PREDICTION_column_names)) {
            clean_vector_of_predictions =
              ### for each row of inference data
              map_dbl(1:length(NAs_index_indicator_vector),
                      function(i){
                        if (is.na(NAs_index_indicator_vector[i])) { 
                          ### if bad, return NA
                          na_dbl
                        } else { 
                          ### replace with good prediction
                          rfsrc_prediction_no_impute$regrOutput[[model_call_target_vars[target_var_i]]]$predicted[NAs_index_indicator_vector[i]]
                        }
                      })
            
            data_with_predictions_iter =
              data_with_predictions_iter %>% 
              mutate(!! PREDICTION_column_names[target_var_i] := 
                       clean_vector_of_predictions)
          }
          
          data_with_predictions_tall =
            data_with_predictions_tall %>% 
            bind_rows(data_with_predictions_iter)
        }
        
        data_with_predictions_tall %>% 
          fwrite(paste0(output_locn,
                        "quantile_predictions_1_tall.csv"))
        
        ### get quantiles
        data_with_predictions_P10 =
          data_with_predictions_tall %>% 
          group_by(API, SENSITIVITY_CASE) %>%
          summarise(across(starts_with("PREDICTIONS_"), tmp_quant_10, .names = "P10_{.col}"),
                    .groups = "drop") 
        
        data_with_predictions_P50 =
          data_with_predictions_tall %>% 
          group_by(API, SENSITIVITY_CASE) %>%
          summarise(across(starts_with("PREDICTIONS_"), tmp_quant_50, .names = "P50_{.col}"),
                    .groups = "drop") 
        
        data_with_predictions_P90 =
          data_with_predictions_tall %>% 
          group_by(API, SENSITIVITY_CASE) %>%
          summarise(across(starts_with("PREDICTIONS_"), tmp_quant_90, .names = "P90_{.col}"),
                    .groups = "drop") 
          
        
        quantile_predictions_2_final =
          data_with_predictions_P10 %>% 
          left_join(data_with_predictions_P50,
                    by = c("API", "SENSITIVITY_CASE")) %>% 
          left_join(data_with_predictions_P90,
                    by = c("API", "SENSITIVITY_CASE"))
        
        ### Error check after left_join
        if (nrow(quantile_predictions_2_final) != nrow(data_with_predictions_P10)) {
          stop("Number of rows changed during join of P10/P50/P90 files.")
        } 
        
        quantile_predictions_2_final %>% 
          fwrite(paste0(output_locn,
                        "quantile_predictions_2_final.csv"))
        
        return("Quantile predictions successful.")
      }   
    }
  }


