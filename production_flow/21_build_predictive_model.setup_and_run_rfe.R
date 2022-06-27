
build_predictive_model.setup_and_run_rfe =
  function(
    train_data_locn,
    test_data_locn,
    output_folder_locn,
    config_file_locn,
    model_iteration, # an integer matching a value in config column
    target_variables = c("LIQUID_360", "LIQUID_720"),
    run_if = FALSE
  ){
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_build_predictive_model.get_initial_columns_for_training_from_config.R", local = TRUE)
      source("anonymous_support_functions/_build_predictive_model.run_rfe_2.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats", "data.table"))
      
      ### load data
      train_data = load_data_that_has_api(train_data_locn)
      test_data = load_data_that_has_api(test_data_locn)
      
      ### rfe run setup details
      model_iteration = as.character(model_iteration)
      initial_columns_for_training =
        get_initial_columns_for_training_from_config(
          config_file_locn,
          training_data = train_data,
          model_iteration = model_iteration
        )
      
      ### rfsrc only handles factors
      ### set character input columns as factors with correct levels 
      ### based on globally available values from flat file
      columns_to_set_as_factors =
        train_data %>% 
        select(one_of(initial_columns_for_training)) %>% 
        select_if(is.character) %>% 
        names()
      
      train_data =
        train_data %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      test_data =
        test_data %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      
      
      ### can turn on to see initial columns to be used
      # return(initial_columns_for_training)
      
      run_rfe(
        columns_for_training = initial_columns_for_training,
        train_data = train_data,
        test_data = test_data,
        target_variables = target_variables,
        output_folder_locn = output_folder_locn,
        min_node_setting = 30,
        samptype = "swor",
        ntree_rfe_setting = 250,
        nsplit_setting = 800
      )
   
      return("RFE run successful.")   
    }
  }