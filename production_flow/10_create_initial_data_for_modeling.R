### function to create initial data for modeling
### takes the 5_YEARS_PIVOT production data
### joins with the header data (after completion has been imputed)
### outputs 2 files: all rows, EF wells only


create_initial_data_for_modeling = 
  function(header_data_locn,
           prod_pivot_data_locn,
           days_actual_history_locn,
           new_folder_locn,
           columns_to_exclude_locn,
           impute_LOC_STATE_NAME = TRUE,
           run_if = FALSE,
           run_post_diagnostic_tests = FALSE) {
    if (run_if) {
      
      # source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("anonymous_support_functions/_create_init_data_for_modeling.fix_target_folder_prefix.R", local = TRUE)
      source("anonymous_support_functions/_create_init_data_for_modeling.do_impute_LOC_STATE_NAME.R", local = TRUE)
      source("general_purpose_support_functions/cleanup_prop_fluid_columns.R", local = TRUE)
      
      
      header_data = load_data_that_has_api(header_data_locn)
      prod_pivot_data = load_data_that_has_api(prod_pivot_data_locn)
      days_actual_history_data = load_data_that_has_api(days_actual_history_locn)
      columns_to_exclude = fread(columns_to_exclude_locn) %>% 
        data.frame(stringsAsFactors = FALSE)
      
      
      days_actual_history_cleaned =
        days_actual_history_data %>% 
        select(-PROD_DATE) %>% 
        rename(DAYS_ACTUAL_HISTORY = CUM_PRODUCING_DAYS)
      
      
      header_and_production_combined =
        header_data %>% 
        select(-LIQUID_180, -PDAYS_LIQUID_180, # raw historic actuals
               -ES_LIQUID_180, -ES_PDAYS_LIQUID_180, # ES_pdp file, so could match historic actuals
               -ES_POD_LIQUID_180, -ES_POD_PDAYS_LIQUID_180 # ES_POD file
        ) %>% 
        cleanup_prop_fluid_columns() %>% 
        select(-one_of(columns_to_exclude$useless_columns_to_exclude_from_initial_modeling_data)) %>% 
        left_join(days_actual_history_cleaned, by = "API") %>% 
        ### fix future wells with blank values
        mutate(DAYS_ACTUAL_HISTORY = ifelse(is.na(DAYS_ACTUAL_HISTORY), 0, DAYS_ACTUAL_HISTORY)) %>% 
        left_join(prod_pivot_data, by = "API")
      
      if (nrow(header_and_production_combined) > nrow(header_data)) stop("Joining data created more rows, likely nonunique API's")
      
      ### impute LOC_STATE_NAME
      if (impute_LOC_STATE_NAME) {
        header_and_production_combined =
          do_impute_LOC_STATE_NAME(
            header_and_production_combined
          )
      }
      
      'This will remove rows with LOC_MP_X AND LOC_MP_Y missing'
      header_and_production_EF_in_Texas_only =
        header_and_production_combined %>% 
        filter(GEO_PROD_ZONE_NAME == "EAGLE FORD") %>% 
        filter(LOC_STATE_NAME == "TEXAS")
      
      ### add folder prefix and create new folder in shell
      dir.create(new_folder_locn)
      
      ### write data tables out
      new_folder_locn = add_path_slash_if_needed(new_folder_locn)
      
      header_and_production_combined %>% 
        fwrite(paste0(new_folder_locn, "header_and_production_combined.csv"))
      
      header_and_production_EF_in_Texas_only %>% 
        fwrite(paste0(new_folder_locn, "header_and_production_EF_in_Texas_only.csv"))
    
      ### Error check
      if (run_post_diagnostic_tests | run_if) {
        source("tests/test_check_14400_production_completeness.R", local = TRUE)
        source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
        
        test_check_14400_production_completeness(
          production_file_location = paste0(
            add_path_slash_if_needed(new_folder_locn),
            "header_and_production_combined.csv"
          ),
          run_if = TRUE)
        
        test_check_14400_production_completeness(
          production_file_location = paste0(
            add_path_slash_if_needed(new_folder_locn),
            "header_and_production_EF_in_Texas_only.csv"
          ),
          run_if = TRUE)
        
      }  
    }
  }