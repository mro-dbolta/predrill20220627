
create_final_GCM =
  function(
    sample_inference_data_locn = "../11_inference_data_sets/BASE_PLAN_sensitivity.csv",
    rfsrc_model_locn,
    old_GCM_locn = "../9_GCM/GCM_2_with_depln.csv",
    output_locn,
    parent_data_config_locn = "config_files/final_GCM_parent_assumptions.xlsx",
    run_if = FALSE
  ){
    if (run_if) {
      "
      Take sample inference data
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/read_xlsx.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.get_target_vars_from_model_call.R", local = TRUE)
      source("anonymous_support_functions/_build_final_GCM.get_input_vars_from_rfsrc_call.R", local = TRUE)
      
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
      
      sample_inference_data = load_data_that_has_api(sample_inference_data_locn) %>% 
        mutate(DT_FIRST_PRODUCTION = ymd(DT_FIRST_PRODUCTION))
      
      output_locn = add_path_slash_if_needed(output_locn)
      
      parent_data_assumption = read_xlsx(xlsx_file = parent_data_config_locn) %>% 
        mutate(DT_FIRST_PRODUCTION = ymd(DT_FIRST_PRODUCTION))
      
      old_GCM = fread(old_GCM_locn) %>% 
        data.frame(stringsAsFactors = FALSE)
      
      ### assign prefix for old GCM predictions
      data_for_predictions = 
        old_GCM %>% 
        rename_with(.fn = function(x){ paste0("GCM_", x) },
                    .cols = starts_with("LIQUID_"))
      
      
      parent_data_without_GCM_prepopulated_columns =
        suppressWarnings(
          parent_data_assumption %>% 
            select(-one_of(names(data_for_predictions)))
        )
      
      data_for_predictions =
        data_for_predictions %>% 
        bind_cols(parent_data_without_GCM_prepopulated_columns)
      
      ### fix column types
      data_for_predictions_with_inference = 
        sample_inference_data %>% 
        bind_rows(data_for_predictions)
      
      data_for_predictions = 
        data_for_predictions_with_inference[-(1:nrow(sample_inference_data)), names(data_for_predictions)]
      
      ### bandaid TVD_SS
      ### this is not because missing in ES, cause it needs to be based off of a GEO map
      data_for_predictions =
        data_for_predictions %>%
        mutate(DP_TVD_SS = MGEO_EGFDL_TVDSS + 22592.1)
      
      
      
      
      ### Error check for NAs
      if (nrow(data_for_predictions) != nrow(na.omit(data_for_predictions))) stop("NAs present in GCM inference data")
      
      ####################
      ### get variables from rfsrc model as check
      rfsrc_model = readRDS(rfsrc_model_locn)
      
      model_call_input_variable_names = get_input_vars_from_rfsrc_call(rfsrc_model)
      model_call_target_vars = get_target_vars_from_model_call(rfsrc_model)
      
      model_variable_names = c(model_call_input_variable_names, 
                               model_call_target_vars)
      
      model_variable_names_not_in_data =
        model_variable_names[-which(model_variable_names %in% names(data_for_predictions))]
      
      ### Error check compare available and needed columns
      if (length(model_variable_names_not_in_data) > 0) {
        stop(paste0(model_variable_names_not_in_data, collapse = ","),
             " not in data but in model")
      }
      
      ### rfsrc only handles factors
      ### set character input columns as factors with correct levels 
      ### based on globally available values from flat file
      columns_to_set_as_factors =
        data_for_predictions %>% 
        select(one_of(model_variable_names)) %>% 
        select_if(is.character) %>% 
        names()
      
      data_for_predictions =
        data_for_predictions %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      # return(list(old_GCM = old_GCM,
      #             data_for_predictions = data_for_predictions,
      #             sample_inference_data = sample_inference_data,
      #             model_variable_names = model_variable_names))
      
      ### predict
      # rfsrc_prediction_no_impute = predict(rfsrc_model, sample_inference_data[1:10,])
      # return(rfsrc_prediction_no_impute)
      
      rfsrc_prediction_no_impute = predict(rfsrc_model, data_for_predictions #, do.trace = TRUE
                                           )
      
      #return(rfsrc_prediction_no_impute)
      
      PREDICTION_column_names = paste0("NEW_GCM_", model_call_target_vars)
      
      data_with_predictions = data_for_predictions
      
      length_PREDICTION_column_names = length(PREDICTION_column_names)
      for (target_var_i in 1:length_PREDICTION_column_names) {
        if (length_PREDICTION_column_names == 1) {
          data_with_predictions =
            data_with_predictions %>% 
            mutate(!! PREDICTION_column_names[target_var_i] := 
                     rfsrc_prediction_no_impute$predicted
            )  
        } else {
          data_with_predictions =
            data_with_predictions %>% 
            mutate(!! PREDICTION_column_names[target_var_i] := 
                     rfsrc_prediction_no_impute$regrOutput[[model_call_target_vars[target_var_i]]]$predicted
            )
        }
      }
      
      data_with_predictions %>% 
        fwrite(paste0(output_locn, "new_GCM.csv"))
      
      return("New GCM succesffully built.")
      
    }
  }