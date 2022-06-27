
marathon_spacing_pilots_in_test =
  function(
    input_whole_data_locn,
    output_filename_stub,
    spacing_analogs_config_locn,
    MRO_only = FALSE,
    run_if = FALSE
  ){
    if (run_if) {
      
      TARGET_VARIABLES = c("LIQUID_360", "LIQUID_720")
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/clip_string_after_last_forward_slash.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/str_clip_csv_from_end.R", local = TRUE)
      source("general_purpose_support_functions/read_xlsx.R", local = TRUE)
      source("general_purpose_support_functions/cleanup_prop_fluid_columns.R", local = TRUE)
      source("general_purpose_support_functions/cleanup_DP_TVD_SS.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      
      output_folder = clip_string_after_last_forward_slash(string_in = input_whole_data_locn)
      
      # output_filename = str_clip_csv_from_end(output_filename)
      # output_filename = paste0(output_filename, ".csv")
      if (str_sub(output_filename_stub, start = -1L) != "_") output_filename_stub = paste0(output_filename_stub, "_")
      
      
      
      input_data = load_data_that_has_api(input_whole_data_locn) %>% 
        mutate(API10 = str_sub(API, end = 10L)) %>% 
        cleanup_prop_fluid_columns() %>%
        cleanup_DP_TVD_SS() 
      
      for (target_variable in TARGET_VARIABLES) {
        input_data =
          input_data[!is.na(
            input_data[, target_variable]
            ),]
      }
      
        
      
      spacing_analogs_config_locn = 
        read_xlsx(spacing_analogs_config_locn) %>% 
        mutate(API = as.character(API)) %>% 
        mutate(API10 = str_sub(API, end = 10L))
      
      train_data = 
        input_data %>% 
        filter(!(API10 %in% spacing_analogs_config_locn$API10)) %>% 
        select(-API10)
      
      if (MRO_only) {
        train_data =
          train_data %>% 
          filter(OTH_OPERATOR_NAME == "MARATHON OIL COMPANY")
      } else {
        "
        Only write test set when MRO_only == FALSE.
        Don't need to write out the same test set mutliple times.
        "
        test_data = 
          input_data %>% 
          filter(API10 %in% spacing_analogs_config_locn$API10) %>% 
          select(-API10)
        
        test_data %>% 
          fwrite(paste0(output_folder, output_filename_stub, "test.csv"))  
      }
      
      train_data %>% 
        fwrite(paste0(output_folder, output_filename_stub, "train.csv"))
      
      return(paste(output_filename_stub, "spacing analog train and test csv's created successfully"))
      
    }
  }