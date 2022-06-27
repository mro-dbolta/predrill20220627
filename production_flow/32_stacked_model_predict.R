

stacked_model_predict =
  function(
    model_folder_locns,
    input_variables,
    stack_model_folder_locn = "stacked_model/",
    run_if = FALSE
  ){
    if (run_if) {
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("anonymous_support_functions/_train_stack_model.extract_model_names.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table", "randomForestSRC",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "lubridate"))
      
      stack_model_folder_locn = add_path_slash_if_needed(input_path = stack_model_folder_locn)
      
      model_names =
        extract_model_names(model_folder_locns)
      
      final_stacked_model = 
        readRDS(paste0("../10_modeling/stacked_model/final_stacked_model.rds"))
      
      ### load inference data sets
      initial_inference_preds_list =
        map(1:length(model_folder_locns),
            function(i){
              inference_preds = fread(paste0("../10_modeling/",
                                             model_folder_locns[i],
                                             "/data_with_predictions_2b_fewer_columns.csv")) %>% 
                data.frame(stringsAsFactors = FALSE) %>% 
                mutate(API = as.character(API)) %>% 
                select(API, SENSITIVITY_CASE,
                       one_of(paste0("PREDICTIONS_",input_variables)))
              
              inference_preds %>% 
                filter(API != "a") %>% 
                rename_with(~ paste0(.x, "_", model_names[i]), .cols = starts_with("PREDICTIONS_LIQUID_")) %>% 
                rename_with(~ str_sub(.x, start = 13L), .cols = starts_with("PREDICTIONS_LIQUID_"))
            })
      
      inference_preds_df =
        map_dfr(1:length(initial_inference_preds_list),
                function(i){
                  initial_inference_preds_list[[i]] %>% 
                    select(API, SENSITIVITY_CASE)
                }) %>% 
        distinct()
      
      # join production back onto the APIs
      for (i in 1:length(model_folder_locns)) {
        inference_preds_df =
          inference_preds_df %>% 
          left_join(initial_inference_preds_list[[i]],
                    by = c("API", "SENSITIVITY_CASE"))
      }
      
      inference_preds_df =
        inference_preds_df %>% 
        na.omit()
      
      ### RUN predictions
      rfsrc_prediction_no_impute = predict(final_stacked_model, inference_preds_df)
      
      
      for (target_var_i in 1:length(input_variables)) {
        inference_preds_df =
          inference_preds_df %>% 
          mutate(!! input_variables[target_var_i] := 
                   rfsrc_prediction_no_impute$regrOutput[[input_variables[target_var_i]]]$predicted)
        
      }
      
      ### save predictions
      initial_preds_from_stack =
        inference_preds_df %>% 
        select(-contains("_run_")) %>% 
        pivot_longer(cols = starts_with("LIQUID_"), names_to = "DAYS", values_to = "PREDICTIONS_LIQUID") %>% 
        mutate(DAYS = str_sub(DAYS, start = 8L),
               DAYS = as.numeric(DAYS))
      
      initial_preds_from_stack %>% 
        fwrite(paste0("../10_modeling/", stack_model_folder_locn, "initial_preds_from_stack.csv"))
      
      print("Stacked model predictions successful.")
    }
  }