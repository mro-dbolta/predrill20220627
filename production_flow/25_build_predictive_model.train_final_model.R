
build_predictive_model.train_final_model =
  function(
    train_data_locn,
    model_folder_locn,
    training_features_override = NULL, # vector of column names
    hyperparameters_override = NULL, # list of hyperparameters
    target_variables = c("LIQUID_90", "LIQUID_180", "LIQUID_360", "LIQUID_720", 
                         "LIQUID_1080", "LIQUID_1800",	"LIQUID_14400"),
    SAMPTYPE = "swor",
    retrain_main_model = FALSE,
    train_quantile_regression_model = FALSE,
    build_out_of_train_predictions = FALSE, # for stack
    build_shapley_object = FALSE,
    run_shap_on_train = FALSE,
    number_shaps_to_calc = 1000L,
    run_if = FALSE
  ){
    if (run_if) {
      "
    Train the final model with all rows, best rfe columns, and hp's tuned.
    Train the quantile regression model set.
    "
      NSPLIT = 0 
      NSPLIT_FOR_QUANTILE_REGRESSION = 0
      QUANTILE_TOTAL_NUMBER = 50L
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/set_factor_column_levels.R", local = TRUE)
      source("anonymous_support_functions/_build_predictive_model.get_rfsrc_formula_as_string.R", local = TRUE)
      source("anonymous_support_functions/_build_predictive_model.build_shapley_on_train.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats", "data.table", "randomForestSRC"))
      
      train_data = load_data_that_has_api(train_data_locn)
      model_folder_locn = add_path_slash_if_needed(model_folder_locn)
      
      ### get formula for model
      if (is.null(training_features_override)) {
        best_features =
          fread(paste0(model_folder_locn,
                       "best_features.csv"))
        check_column_names_as_expected(
          data_frame_to_check = best_features,
          expected_column_names = "BEST_FEATURES",
          data_frame_description = "best_features df from rfe")  
        
        best_features = best_features$BEST_FEATURES
      } else {
        best_features = training_features_override
      }
      
      ### rfsrc only handles factors
      ### set character input columns as factors with correct levels 
      ### based on globally available values from flat file
      columns_to_set_as_factors =
        train_data %>% 
        select(one_of(best_features)) %>% 
        select_if(is.character) %>% 
        names()
      
      train_data =
        train_data %>% 
        set_factor_column_levels(factor_column_names = columns_to_set_as_factors)
      
      ### get formulas
      rfsrc_formula = get_rfsrc_formula_as_string(target_variables = target_variables,
                                                  working_list_of_inputs = best_features)
      rfsrc_formula = as.formula(rfsrc_formula)
      
      ### get hyperparameters
      if (is.null(hyperparameters_override)) {
        HP_grid_results =
          fread(paste0(model_folder_locn,
                       "HP_grid_results.csv"))
        
        if ("manually picked on VAL rank" %in% HP_grid_results$RESULT) {
          HP_grid_results = 
            HP_grid_results %>% 
            filter(RESULT == "manually picked on VAL rank") %>%
            filter(VAL_RANK == min(VAL_RANK)) %>% 
            head(1)
        } else {
          HP_grid_results = 
            HP_grid_results %>% 
            filter(RESULT == "best VAL_NRMSE") %>%
            filter(VAL_RANK == min(VAL_RANK)) %>% 
            head(1)
        }
        best_hyperparameters =
          list(
            mtry = HP_grid_results$mtry_try[1],
            min.node.size = HP_grid_results$min.node.size_try[1],
            num_trees = HP_grid_results$num_trees_try[1],
            sample.fraction = HP_grid_results$sample.fraction_try[1]
          )
      } else {
        best_hyperparameters = hyperparameters_override
      }
      
      if (!("mtry" %in% names(best_hyperparameters))) stop("mtry missing from best_hyperparameters list")
      if (!("min.node.size" %in% names(best_hyperparameters))) stop("min.node.size missing from best_hyperparameters list")
      if (!("num_trees" %in% names(best_hyperparameters))) stop("num_trees missing from best_hyperparameters list")
      if (!("sample.fraction" %in% names(best_hyperparameters))) stop("sample.fraction missing from best_hyperparameters list")
      
      if (retrain_main_model) {
        final_model = 
          rfsrc(rfsrc_formula,
                data = train_data,
                samptype = SAMPTYPE,
                mtry = best_hyperparameters$mtry,
                ntree = best_hyperparameters$num_trees,
                nodesize = best_hyperparameters$min.node.size,
                sampsize = round(best_hyperparameters$sample.fraction * nrow(train_data)),
                nsplit = NSPLIT, 
                seed = 42)
        
        final_model %>% 
          saveRDS(paste0(model_folder_locn,
                         "final_model.rds"))  
      }
      
      ### train quantile regression model
      if (train_quantile_regression_model) {
        ### folders handling
        quantile_regression_models_locn = 
          paste0(model_folder_locn, "quantile_regression_models")
        if (!dir.exists(quantile_regression_models_locn)) dir.create(quantile_regression_models_locn)
        
        
        pb <- txtProgressBar(min = 0, max = QUANTILE_TOTAL_NUMBER, style = 3)
        for (tree_i in 1:QUANTILE_TOTAL_NUMBER) {
          quantile_model_iter =
            rfsrc(rfsrc_formula,
                  data = train_data,
                  samptype = SAMPTYPE,
                  mtry = best_hyperparameters$mtry,
                  ntree = 1L,
                  nodesize = best_hyperparameters$min.node.size,
                  sampsize = round(best_hyperparameters$sample.fraction * nrow(train_data)),
                  nsplit = NSPLIT_FOR_QUANTILE_REGRESSION, 
                  importance = "none",
                  seed = tree_i)
          
          quantile_model_iter %>% 
            write_rds(paste0(quantile_regression_models_locn, "/", 
                             "quantile_model_", tree_i, ".rds"),
                      "xz", compression = 9L)
          
          setTxtProgressBar(pb, tree_i)
        }
        close(pb)
      }
      
      ### data for stacked model
      if (build_out_of_train_predictions) {
        
        train_data_cleaned = 
          train_data %>% 
          select(one_of("API", "DT_FIRST_PRODUCTION_YEAR",
                        target_variables,
                        best_features)) %>% 
          na.omit()
        
        "stratified sampling by year"
        years_in_data =
          unique(train_data_cleaned$DT_FIRST_PRODUCTION_YEAR)
        
        ### set prediction df as starter row to remove later
        out_of_train_predictions =
          train_data_cleaned %>% 
          select(one_of("API", 
                        target_variables)) %>% 
          head(1) %>% 
          mutate(API = "a")
        
        pb <- txtProgressBar(min = 0, max = length(years_in_data), style = 3)
        for (year_this_fold in years_in_data) {
          
          model_this_fold = 
            rfsrc(rfsrc_formula,
                  data = train_data_cleaned %>% 
                    filter(DT_FIRST_PRODUCTION_YEAR != year_this_fold),
                  samptype = SAMPTYPE,
                  mtry = best_hyperparameters$mtry,
                  ntree = best_hyperparameters$num_trees,
                  nodesize = best_hyperparameters$min.node.size,
                  sampsize = round(best_hyperparameters$sample.fraction * nrow(train_data)),
                  nsplit = NSPLIT, 
                  seed = 42)
          
          out_of_train_predictions_this_fold_init =
            train_data_cleaned %>% 
            filter(DT_FIRST_PRODUCTION_YEAR == year_this_fold)
          
          rfsrc_prediction_this_fold = predict(model_this_fold, 
                                               out_of_train_predictions_this_fold_init)
          
          ### remove input feature columns
          out_of_train_predictions_this_fold =
            out_of_train_predictions_this_fold_init %>% 
            select(one_of("API",
                          target_variables))
          
          length_target_variables = length(target_variables)
          for (target_var_i in 1:length_target_variables) {
            
            if (length_target_variables == 1) {
              vector_of_predictions =
                rfsrc_prediction_this_fold$predicted
            } else {
              vector_of_predictions =
                rfsrc_prediction_this_fold$regrOutput[[target_variables[target_var_i]]]$predicted
            }
            out_of_train_predictions_this_fold =
              out_of_train_predictions_this_fold %>% 
              mutate(!! target_variables[target_var_i] := 
                       vector_of_predictions)
            
          }
          setTxtProgressBar(pb, which(year_this_fold == years_in_data))
          
          out_of_train_predictions =
            out_of_train_predictions %>% 
            bind_rows(out_of_train_predictions_this_fold)
        }
        
        out_of_train_predictions =
          out_of_train_predictions %>% 
          filter(API != "a")
        
        close(pb)
        
        out_of_train_predictions %>% 
          fwrite(paste0(model_folder_locn,
                        "out_of_train_predictions.csv"))
      }  
      
      if (build_shapley_object) {
        
        if (!("LIQUID_360" %in% target_variables)) {
          ranger_target = "LIQUID_360"
        } else {
          ranger_target = target_variables[1]
        }
        
        
        build_shapley_on_train(model_folder_locn = model_folder_locn,
                               ranger_target = ranger_target,
                               input_features = best_features,
                               initial_train_data = train_data,
                               samp_type = SAMPTYPE,
                               num_trees = 100L, # keep model small by default
                               m_try = best_hyperparameters$mtry,
                               min_node_size = best_hyperparameters$min.node.size,
                               sample_fraction = best_hyperparameters$sample.fraction,
                               run_shap_on_train = run_shap_on_train,
                               number_shaps_to_calc = number_shaps_to_calc)
        
        }
      
      return(paste0("Final model successfully trained for ",
                    model_folder_locn))
      
    }}


