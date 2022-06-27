
### final cleanup and build train & test sets

build_train_test_splits =
  function(
    train_test_splits_config_file_locn,
    input_whole_data_for_modeling_locn,
    output_folder_locn,
    run_if = FALSE
  ){
    if (run_if) {
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      
      ### load config file
      train_test_splits_config_file = 
        fread(train_test_splits_config_file_locn) %>% 
        data.frame(stringsAsFactors = FALSE) %>% 
        mutate(case = suppressWarnings(as.numeric(case))) %>% 
        filter(!is.na(case))
      
      ### defensive checks
      check_column_names_as_expected(
        data_frame_to_check = train_test_splits_config_file,
        expected_column_names = c("case",	"rule_type",	"rule", "notes"),
        data_frame_description = "train_test_splits_config_file"
      )
      
      ### load modeling data
      input_whole_data_for_modeling = 
        load_data_that_has_api(input_whole_data_for_modeling_locn)
      
      
      for (i_case in 1:max(train_test_splits_config_file$case)) {
        
        train_test_splits_config_file_iter =
          train_test_splits_config_file %>% 
          filter(case == i_case)
        
        whole_data_for_modeling_iter =
          input_whole_data_for_modeling
        
        ### base row filters first - data quality check
        for (i in 1:nrow(train_test_splits_config_file_iter)) {
          if (train_test_splits_config_file_iter$rule_type[i] == "filter") {
            whole_data_for_modeling_iter =
              eval(parse(text = 
                           paste0(
                             "filter(whole_data_for_modeling_iter, ",
                             train_test_splits_config_file_iter$rule[i],
                             ")"
                           )))
          }
        }
        
        
        ### clean up column names
        for (i in 1:nrow(train_test_splits_config_file_iter)) {
          if (train_test_splits_config_file_iter$rule_type[i] == "columns to exclude") {
            whole_data_for_modeling_iter =
              eval(parse(text = 
                           paste0(
                             "select(whole_data_for_modeling_iter, -",
                             train_test_splits_config_file_iter$rule[i],
                             ")"
                           )))
          }
        }
        
        ### run na.omit later at modeling and prediction
        # whole_data_for_modeling_iter = 
        #   whole_data_for_modeling_iter %>% 
        #   na.omit()
        
        ### build training df
        training_set_iter = whole_data_for_modeling_iter
        for (i in 1:nrow(train_test_splits_config_file_iter)) {
          if (train_test_splits_config_file_iter$rule_type[i] == "training filter") {
            training_set_iter =
              eval(parse(text = 
                           paste0(
                             "filter(training_set_iter, ",
                             train_test_splits_config_file_iter$rule[i],
                             ")"
                           )))
          }
        }
        
        ### build test df
        test_set_iter = whole_data_for_modeling_iter
        for (i in 1:nrow(train_test_splits_config_file_iter)) {
          if (train_test_splits_config_file_iter$rule_type[i] == "test filter") {
            test_set_iter =
              eval(parse(text = 
                           paste0(
                             "filter(test_set_iter, ",
                             train_test_splits_config_file_iter$rule[i],
                             ")"
                           )))
          }
        }
        
        ### get file name string to use
        output_filename_prepend_mask =
          which(train_test_splits_config_file_iter$rule_type == "output_file_prepend")
        if (length(output_filename_prepend_mask) > 1L) {
          stop(paste0("Output filename config for table ",
                      i_case,
                      " has more than one input.")) 
        }
        output_filename_prepend =
          train_test_splits_config_file_iter$rule[output_filename_prepend_mask]
        
        ### create new folder for train & test data
        dir.create(output_folder_locn, showWarnings = F)
        
        ### write data to folder
        files_to_write = list(whole_data_for_modeling_iter,
                              training_set_iter,
                              test_set_iter)
        postfixes = c("whole", "train", "test")
        
        for (i in 1:3) {
          files_to_write[[i]] %>% 
            fwrite(
              paste0(
                add_path_slash_if_needed(output_folder_locn),
                output_filename_prepend,
                postfixes[i],
                ".csv"
              ))  
        }
        
      }
      
      print(
        paste0("Successful run: build_train_test_splits write to ",
               output_folder_locn)
        )
    }
  }