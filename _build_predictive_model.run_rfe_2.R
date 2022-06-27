

run_rfe =
  function(
    columns_for_training,
    train_data,
    test_data,
    output_folder_locn,
    target_variables = c("LIQUID_360", "LIQUID_720"),
    min_node_setting = 30,
    samptype = "swor",
    ntree_rfe_setting = 250,
    nsplit_setting = 800
  ){
    
    # v2 add training NRMSE
    
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    source("general_purpose_support_functions/calc_NRMSE_rfsrc.R", local = TRUE)
    source("anonymous_support_functions/_build_predictive_model.get_rfsrc_formula_as_string.R", local = TRUE)
    
    install_load_libraries_strings(c("ggplot2",
                                     "dplyr",
                                     "tidyr",
                                     "readr",
                                     "purrr",
                                     "tibble",
                                     "stringr",
                                     "forcats", "data.table", "randomForestSRC"))
    if (!("caret" %in% rownames(installed.packages()))) install.packages("caret")
    
    
    output_folder_locn = add_path_slash_if_needed(output_folder_locn)
    
    nsplit_setting = min(nrow(train_data), nsplit_setting)
    
    working_list_of_inputs = columns_for_training
    
    rfsrc_formula =
      get_rfsrc_formula_as_string(target_variables,
                                  working_list_of_inputs)
    rfsrc_formula = as.formula(rfsrc_formula)
    
    rfe_step_model = 
      rfsrc(rfsrc_formula,
            data = train_data,
            samptype = "swor",
            ntree = ntree_rfe_setting,
            nodesize = min_node_setting,
            nsplit = nsplit_setting,
            seed = 42)
    
    tmp_preds = 
      predict(rfe_step_model, 
              test_data)
   
    NRMSE_iter =
      calc_NRMSE_rfsrc(
        target_variables = target_variables,
        data_set_df = test_data,
        rfsrc_predict_object = tmp_preds
      )
    
    tmp_preds_in_train = 
      predict(rfe_step_model, 
              train_data)
    
    NRMSE_train_iter =
      calc_NRMSE_rfsrc(
        target_variables = target_variables,
        data_set_df = train_data,
        rfsrc_predict_object = tmp_preds_in_train
      )

    result_statistics =
      data.frame(VARIABLE_REMOVED = c("None"),
                 NRMSE = NRMSE_iter,
                 NRMSE_TRAIN = NRMSE_train_iter,
                 NUMBER_OF_INPUT_VARIABLES = length(working_list_of_inputs),
                 stringsAsFactors = FALSE)
      
    # return(result_statistics)
    
    pb_value = 0
    pb <- txtProgressBar(min = pb_value,      # Minimum value of the progress bar
                         max = length(working_list_of_inputs), # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         width = 50,   # Progress bar width. Defaults to getOption("width")
                         char = "=")   # Character used to create the bar
    
    while (length(working_list_of_inputs) > 1) {
      for (i in 1:length(working_list_of_inputs)) {
        rfsrc_formula =
          get_rfsrc_formula_as_string(target_variables,
                                      working_list_of_inputs[-i])
        rfsrc_formula = as.formula(rfsrc_formula)
        
        rfe_step_model = 
          rfsrc(rfsrc_formula,
                data = train_data,
                samptype = "swor",
                ntree = ntree_rfe_setting,
                nodesize = min_node_setting,
                nsplit = nsplit_setting,
                seed = 42)
        
        tmp_preds = 
          predict(rfe_step_model, 
                  test_data)
        
        NRMSE_iter =
          calc_NRMSE_rfsrc(
            target_variables = target_variables,
            data_set_df = test_data,
            rfsrc_predict_object = tmp_preds
          )
        
        tmp_preds_in_train = 
          predict(rfe_step_model, 
                  train_data)
        
        NRMSE_train_iter =
          calc_NRMSE_rfsrc(
            target_variables = target_variables,
            data_set_df = train_data,
            rfsrc_predict_object = tmp_preds_in_train
          )
        
        result_statistics_iter =
          data.frame(VARIABLE_REMOVED = working_list_of_inputs[i],
                     NRMSE = NRMSE_iter,
                     NRMSE_TRAIN = NRMSE_train_iter,
                     NUMBER_OF_INPUT_VARIABLES = length(working_list_of_inputs),
                     stringsAsFactors = FALSE)
        
        result_statistics = 
          result_statistics %>% 
          bind_rows(result_statistics_iter)

      }
      
      variable_to_remove =
        result_statistics %>% 
        filter(NUMBER_OF_INPUT_VARIABLES == min(NUMBER_OF_INPUT_VARIABLES)) %>% 
        filter(NRMSE == min(NRMSE))
      
      variable_to_remove =
        variable_to_remove$VARIABLE_REMOVED[1]
      
      working_list_of_inputs = working_list_of_inputs[-which(working_list_of_inputs == variable_to_remove)]
      print(length(working_list_of_inputs))
      
      fwrite(result_statistics, 
             paste0(output_folder_locn,
                    "RFE_statistics.csv"))
      
      pb_value = pb_value + 1
      setTxtProgressBar(pb, pb_value)
      
      }
    close(pb)
}