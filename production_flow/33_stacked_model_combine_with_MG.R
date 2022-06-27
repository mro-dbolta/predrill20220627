
combine_stack_and_MG_predictions =
  function(
    model_folder_locns,
    stack_model_folder_locn = "stacked_model/",
    MG_weight_of_whole = 0.3,
    Mariano_data_locn = "../../../../Viewers/Data/PLAN_Output_for_maps_EAGLEFORD.csv",
    run_if = FALSE
  ) {
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
                                       "lubridate",
                                       "openxlsx"))
      
      stack_model_folder_locn = add_path_slash_if_needed(input_path = stack_model_folder_locn)
      
      model_names =
        extract_model_names(model_folder_locns)
      
      ### get intital stack results
      stacked_predictions =
        fread(paste0("../10_modeling/", stack_model_folder_locn, "initial_preds_from_stack.csv")) %>% 
        data.frame(stringsAsFactors = FALSE) %>% 
        mutate(API = as.character(API))
      
      ### get quantile preds from Bolta models
      initial_quantile_preds_list =
        map(1:length(model_folder_locns),
            function(i){
              fread(paste0("../10_modeling/",
                           model_folder_locns[i],
                           "/data_with_predictions_3b_fewer_columns.csv")) %>% 
                data.frame(stringsAsFactors = FALSE) %>% 
                mutate(API = as.character(API),
                       RUN = model_names[i]) %>% 
                select(API,
                       RUN,
                       SENSITIVITY_CASE,
                       DAYS,
                       P10_PREDICTIONS_LIQUID,
                       P90_PREDICTIONS_LIQUID) %>% 
                na.omit() %>% 
                filter(DAYS %in% stacked_predictions$DAYS) %>% 
                filter(API != "a")
            })
      
      quantile_preds_df =
        initial_quantile_preds_list %>% 
        bind_rows() %>% 
        group_by(API, SENSITIVITY_CASE, DAYS) %>% 
        summarise(P10_PREDICTIONS_LIQUID = mean(P10_PREDICTIONS_LIQUID),
                  P90_PREDICTIONS_LIQUID = mean(P90_PREDICTIONS_LIQUID),
                  .groups = "drop") %>% 
      ### add base stack predictions
      left_join(stacked_predictions,
                by = c("API", "SENSITIVITY_CASE", "DAYS"))
      
      ### download MG data
      MG_data =
        fread(Mariano_data_locn) %>% 
        data.frame(stringsAsFactors = FALSE) %>% 
        mutate(API = as.character(API))
      
      MG_data =
        MG_data %>% 
        select(API, starts_with("Prediction_"))
      
      ### prep MG data
      pivot_and_fix_days =
        function(df, new_col = "PREDICTIONS_LIQUID"){
          df %>% 
            pivot_longer(cols = starts_with("Prediction_"),
                         names_to = "DAYS", values_to = new_col) %>% 
            mutate(DAYS = str_sub(DAYS, start = 20L),
                   DAYS = as.numeric(DAYS))    
        }
      
      MG_mid_preds =
        MG_data %>% 
        select(API, contains("_P50_")) %>% 
        pivot_and_fix_days()
      
      MG_low_preds =
        MG_data %>% 
        select(API, contains("_P90_")) %>% 
        pivot_and_fix_days(new_col = "P10_PREDICTIONS_LIQUID")
      
      MG_hi_preds =
        MG_data %>% 
        select(API, contains("_P10_")) %>% 
        pivot_and_fix_days(new_col = "P90_PREDICTIONS_LIQUID")
      
      MG_preds_combined =
        MG_mid_preds %>% 
        left_join(MG_low_preds,
                  by = c("API", "DAYS")) %>% 
        left_join(MG_hi_preds,
                  by = c("API", "DAYS")) %>% 
        ### fix blank p10s and p90s
        mutate(P10_PREDICTIONS_LIQUID = ifelse(is.na(P10_PREDICTIONS_LIQUID), PREDICTIONS_LIQUID * 0.7, P10_PREDICTIONS_LIQUID),
               P90_PREDICTIONS_LIQUID = ifelse(is.na(P90_PREDICTIONS_LIQUID), PREDICTIONS_LIQUID * 1.3, P90_PREDICTIONS_LIQUID))
      
      if (nrow(MG_preds_combined) != nrow(na.omit(MG_preds_combined))) stop("NA's found in MG_preds_combined")
      
      
      
      quantile_preds_combined =
        quantile_preds_df %>% 
        rename_with(~ paste0("DB_", .x), .cols = contains("PREDICTIONS")) %>% 
        left_join(MG_preds_combined %>% 
                    mutate(SENSITIVITY_CASE = "BASE_PLAN") %>% 
                    rename_with(~ paste0("MG_", .x), .cols = contains("PREDICTIONS")), 
                  by = c("API", "SENSITIVITY_CASE", "DAYS")) %>% 
        ### merge 2 prediction sets
        mutate(PREDICTIONS_LIQUID = ifelse(is.na(MG_PREDICTIONS_LIQUID), 
                                           DB_PREDICTIONS_LIQUID,
                                           (1 - MG_weight_of_whole) * DB_PREDICTIONS_LIQUID +
                                             MG_weight_of_whole * MG_PREDICTIONS_LIQUID),
               P10_PREDICTIONS_LIQUID = ifelse(is.na(MG_P10_PREDICTIONS_LIQUID), 
                                               DB_P10_PREDICTIONS_LIQUID,
                                               (1 - MG_weight_of_whole) * DB_P10_PREDICTIONS_LIQUID +
                                                 MG_weight_of_whole * MG_P10_PREDICTIONS_LIQUID),
               P90_PREDICTIONS_LIQUID = ifelse(is.na(MG_P90_PREDICTIONS_LIQUID), 
                                               DB_P90_PREDICTIONS_LIQUID,
                                               (1 - MG_weight_of_whole) * DB_P90_PREDICTIONS_LIQUID +
                                                 MG_weight_of_whole * MG_P90_PREDICTIONS_LIQUID)) %>% 
        select(-starts_with("DB_"), -starts_with("MG_"))
      
      
      
      
      i = 1L
      
      data_with_predictions_3b_fewer_columns =
        fread(paste0("../10_modeling/",
                     model_folder_locns[i],
                     "/data_with_predictions_3b_fewer_columns.csv")) %>% 
        data.frame(stringsAsFactors = FALSE) %>% 
        mutate(API = as.character(API))
      
      data_with_predictions_3c_fewer_columns_combined =
        data_with_predictions_3b_fewer_columns %>% 
        select(-PREDICTIONS_LIQUID,
               -P50_PREDICTIONS_LIQUID,
               -P10_PREDICTIONS_LIQUID,
               -P90_PREDICTIONS_LIQUID) %>% 
        left_join(quantile_preds_combined,
                  by = c("API", "SENSITIVITY_CASE", "DAYS")) %>% 
        select(API, SENSITIVITY_CASE, DAYS, LIQUID,
               LIQUID_FORECAST, LIQUID_ES_POD, contains("PRED"),
               everything()) %>% 
        mutate(PREDICTIONS_LIQUID = if_else(API == "a", 10.01, PREDICTIONS_LIQUID),
               P10_PREDICTIONS_LIQUID = if_else(API == "a", 10.01, P10_PREDICTIONS_LIQUID),
               P90_PREDICTIONS_LIQUID = if_else(API == "a", 10.01, P90_PREDICTIONS_LIQUID)) %>% 
        ### bandaid quantiles to 30% bands
        mutate(P90_PREDICTIONS_LIQUID = if_else(P90_PREDICTIONS_LIQUID < 1.3 * PREDICTIONS_LIQUID,
                                                PREDICTIONS_LIQUID * 1.3, P90_PREDICTIONS_LIQUID),
               P10_PREDICTIONS_LIQUID = if_else(P10_PREDICTIONS_LIQUID > 0.7 * PREDICTIONS_LIQUID,
                                                PREDICTIONS_LIQUID * 0.7, P10_PREDICTIONS_LIQUID))
      
      data_with_predictions_3c_fewer_columns_combined %>% 
        fwrite(paste0("../10_modeling/", stack_model_folder_locn, "data_with_predictions_3c_fewer_columns_combined.csv"))

      print("Successfully built combined predictions")
    }
  }