

stacked_model_train =
  function(
    model_folder_locns,
    target_variables,
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
      
      ### get df's for each model
      initial_oob_preds_list =
        map(1:length(model_folder_locns),
            function(i){
              oob_preds = fread(paste0("../10_modeling/",
                                       model_folder_locns[i],
                                       "/out_of_train_predictions.csv")) %>% 
                data.frame(stringsAsFactors = FALSE) %>% 
                mutate(API = as.character(API)) %>% 
                select(API, one_of(input_variables))
              
              oob_preds %>% 
                rename_with(~ paste0(.x, "_", model_names[i]), .cols = starts_with("LIQUID_"))
            })
      
      ### get just the API to start
      ### some models might not work on all wells
      oob_preds_df =
        map_dfr(1:length(initial_oob_preds_list),
                function(i){
                  initial_oob_preds_list[[i]] %>% 
                    select(API)
                }) %>% 
        distinct()
      
      # join production back onto the APIs
      for (i in 1:length(model_folder_locns)) {
        oob_preds_df =
          oob_preds_df %>% 
          left_join(initial_oob_preds_list[[i]],
                    by = "API")
      }
      
      ### get prod data
      prod_data =
        fread("../6_data_for_modeling/header_and_production_combined.csv") %>% 
        data.frame(stringsAsFactors = FALSE) %>% 
        select(API, starts_with("LIQUID_"))
      
      if (class(prod_data$API) != "character") {
        prod_data =
          prod_data %>% 
          mutate(API = as.character(API))
      }
      
      
      ### full data prod and oob combined
      full_data =
        prod_data %>% 
        left_join(oob_preds_df, by = "API") %>% 
        filter(API %in% oob_preds_df$API)
      
      full_data_complete_rows =
        full_data %>% 
        select(-API) %>% 
        na.omit()
      
      ### build formula
      target_variables_string = paste0(target_variables, collapse = ",")
      target_variables_string = paste0("Multivar(", target_variables_string, ")~")
      
      working_list_of_inputs = 
        full_data_complete_rows %>% 
        select(contains("run")) %>% 
        names()
      
      full_data %>% 
        na.omit() %>% 
        fwrite(paste0("../10_modeling/", stack_model_folder_locn, "full_data_complete_rows.csv"))
      
      #return("tmp stop point")
      
      rfsrc_formula = paste0(target_variables_string,
                             paste0(working_list_of_inputs, collapse = "+"))
      
      rfsrc_formula = as.formula(rfsrc_formula)
      
      ### train model
      final_stacked_model = 
        rfsrc(rfsrc_formula,
              data = full_data_complete_rows,
              samptype = "swor",
              ntree = 500,
              nsplit = 0, 
              seed = 42)
      
      final_stacked_model %>% 
        saveRDS(paste0("../10_modeling/", stack_model_folder_locn, "final_stacked_model.rds"))  

      print("Stack model successfully trained")
      
    }
  }