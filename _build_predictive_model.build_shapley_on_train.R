
build_shapley_on_train =
  function(
    model_folder_locn,
    ranger_target,
    input_features,
    initial_train_data,
    samp_type,
    num_trees = 100L, # keep model small by default
    m_try,
    min_node_size,
    sample_fraction,
    run_shap_on_train,
    number_shaps_to_calc
  ){
    
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    source("general_purpose_support_functions/get_shaps_values.R", local = TRUE)
    source("anonymous_support_functions/_build_predictive_model.get_rfsrc_formula_as_string.R", local = TRUE)
    
    if (samp_type == "swor") {
      ranger_replace = FALSE
    } else if (samp_type == "swr") {
      ranger_replace = TRUE
    } else {
      stop("RF replacement parameter set wrong.")
    }
    
    install_load_libraries_strings(c("ggplot2",
                                     "dplyr",
                                     "tidyr",
                                     "readr",
                                     "purrr",
                                     "tibble",
                                     "stringr",
                                     "forcats", "data.table", "randomForestSRC",
                                     ### for shap
                                     "ranger",
                                     "treeshap",
                                     "caret"))
    
    ### continuous vars only for treeshap
    train_data_one_hot =
      initial_train_data %>% 
      select(one_of(c(ranger_target, input_features))) %>% 
      na.omit() ### missing's not yet handled
    
    ### this will not center and scale
    caret_dummy_for_shap = dummyVars(" ~ .", data = train_data_one_hot, sep = "_", fullRank = TRUE)
    train_data_one_hot = data.frame(predict(caret_dummy_for_shap, newdata = train_data_one_hot))
    
    input_vars_for_shap_model =
      train_data_one_hot %>% 
      select(-one_of(ranger_target)) %>% 
      names()
    
    ### this function still works for ranger formulas as long as only 1 target var
    ranger_formula = get_rfsrc_formula_as_string(target_variables = ranger_target,
                                                 working_list_of_inputs = input_vars_for_shap_model)
    ranger_formula = as.formula(ranger_formula)
    
    
    ### add unique id back
    if ("API" %in% names(initial_train_data)) {
      train_data_one_hot_with_API =
        initial_train_data %>% 
        select(one_of(c("API", ranger_target, input_features))) %>% 
        na.omit() %>% 
        select(API) %>% 
        bind_cols(train_data_one_hot)
    }
    
    
    ranger_model =
      ranger(ranger_formula, 
             data = train_data_one_hot,
             num.trees = num_trees,
             mtry = m_try,
             min.node.size = min_node_size,
             replace = ranger_replace,
             sample.fraction = sample_fraction,
             save.memory = FALSE,
             verbose = TRUE,
             seed = 42)
    
    
    shap_tree_unified_to_model = ranger.unify(ranger_model, train_data_one_hot)
    
    caret_dummy_for_shap %>% 
      saveRDS(paste0(model_folder_locn,
                     "shap_caret_dummy.rds"))
    
    shap_tree_unified_to_model %>% 
      saveRDS(paste0(model_folder_locn,
                     "shap_tree_unified_to_model.rds"))
    
    
    if (run_shap_on_train) {
      
      shap_training_set =
        get_shaps_values(number_shaps_to_calc = number_shaps_to_calc,
                         train_data_one_hot = train_data_one_hot,
                         train_data_one_hot_with_header_columns = train_data_one_hot_with_API,
                         header_columns = "API",
                         shap_tree_unified_to_model = shap_tree_unified_to_model,
                         input_vars_for_shap_model = input_vars_for_shap_model)

      shap_training_set %>% 
        fwrite(paste0(model_folder_locn,
                      "shap_training_set.csv"))
    }
    
    print("Shap run successful.")
    
  }



