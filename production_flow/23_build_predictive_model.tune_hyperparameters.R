
build_predictive_model.tune_hyperparameters =
  function(
    train_data_locn,
    test_data_locn,
    model_folder_locn, # the rfe folder location
    mtry_try_in = c(3, 5, 7, 9), #length(best_feature_model)),
    min.node.size_try_in = c(1, 5, 10, 20, 30, 50, 100),
    num_trees_try_in = c(100, 300, 500, 1000),
    sample.fraction_try_in = c(0.5, 0.6, 0.7, 1),
    target_variables = c("LIQUID_360", "LIQUID_720"),
    run_if = FALSE
  ){
    if (run_if) {
      
      SAMPTYPE = "swor"
      NSPLIT = 0 
      "Non-negative integer value for number of random splits to consider for each candidate splitting variable. 
      This significantly increases speed. When zero or NULL, uses much slower deterministic splitting 
      where all possible splits considered."
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/calc_NRMSE_rfsrc.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_build_predictive_model.get_rfsrc_formula_as_string.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      if (!("caret" %in% rownames(installed.packages()))) install.packages("caret")
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats", "data.table", "randomForestSRC"))
      
      train_data = load_data_that_has_api(train_data_locn)
      test_data = load_data_that_has_api(test_data_locn)
      
      model_folder_locn = add_path_slash_if_needed(model_folder_locn)
      
      ### get best features
      best_features =
        fread(paste0(model_folder_locn,
                     "best_features.csv"))
      check_column_names_as_expected(
        data_frame_to_check = best_features,
        expected_column_names = "BEST_FEATURES",
        data_frame_description = "best_features df from rfe")
      
      ### rfsrc only handles factors
      ### set character input columns as factors with correct levels 
      ### based on globally available values from flat file
      columns_to_set_as_factors =
        train_data %>% 
        select(one_of(best_features$BEST_FEATURES)) %>% 
        select_if(is.character) %>% 
        names()
      
      train_data =
        train_data %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      test_data =
        test_data %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      
      ### get formula for model
      rfsrc_formula = get_rfsrc_formula_as_string(target_variables = target_variables,
                                                  working_list_of_inputs = best_features$BEST_FEATURES)
      rfsrc_formula = as.formula(rfsrc_formula)
      
      
      HP_grid = expand.grid(mtry_try = mtry_try_in,
                            min.node.size_try = min.node.size_try_in,
                            num_trees_try = num_trees_try_in,
                            sample.fraction_try = sample.fraction_try_in,
                            VAL_NRMSE = 0,
                            TRAIN_NRMSE = 0) %>% 
        mutate(RUN_ID = 1:n()) %>% 
        select(RUN_ID, everything())
      
      pb_value = 0
      pb = txtProgressBar(min = pb_value,      # Minimum value of the progress bar
                           max = nrow(HP_grid), # Maximum value of the progress bar
                           style = 3,    # Progress bar style (also available style = 1 and style = 2)
                           width = 50,   # Progress bar width. Defaults to getOption("width")
                           char = "-")   # Character used to create the bar
      
      for (HP_grid_row_i in 1:nrow(HP_grid)) {
        ranger_rfe_step_model = 
          rfsrc(rfsrc_formula,
                data = train_data,
                samptype = SAMPTYPE,
                mtry = HP_grid$mtry_try[HP_grid_row_i],
                ntree = HP_grid$num_trees_try[HP_grid_row_i],
                nodesize = HP_grid$min.node.size_try[HP_grid_row_i],
                sampsize = round(HP_grid$sample.fraction_try[HP_grid_row_i] * nrow(train_data)),
                nsplit = NSPLIT, 
                seed = 42)
        
        preds_test = predict(ranger_rfe_step_model, test_data)
        preds_train = predict(ranger_rfe_step_model, train_data)
        ### copied from _build_predictive_model.run_rfe_2
        
        NRMSE_iter = calc_NRMSE_rfsrc(target_variables = target_variables,
                                      data_set_df = test_data,
                                      rfsrc_predict_object = preds_test)
        
        NRMSE_train_iter = calc_NRMSE_rfsrc(target_variables = target_variables,
                                            data_set_df = train_data,
                                            rfsrc_predict_object = preds_train)
        
        HP_grid$VAL_NRMSE[HP_grid_row_i] = NRMSE_iter
        HP_grid$TRAIN_NRMSE[HP_grid_row_i] = NRMSE_train_iter
        
        setTxtProgressBar(pb, HP_grid_row_i)
        
      }
      
      close(pb)
      
      HP_grid %>% 
        fwrite(paste0(model_folder_locn,
                      "HP_grid.csv"))
      
      return("Hyperparameters successfully tuned.")
      
    }
  }