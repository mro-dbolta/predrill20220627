
get_initial_columns_for_training_from_config =
  function(
    config_file_locn,
    training_data,
    model_iteration
  ){
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    source("general_purpose_support_functions/read_xlsx.R", local = TRUE)
    source("tests/check_column_names_as_expected.R", local = TRUE)
    
    install_load_libraries_strings(c("ggplot2",
                                     "dplyr",
                                     "tidyr",
                                     "readr",
                                     "purrr",
                                     "tibble",
                                     "stringr",
                                     "forcats", "data.table", "dplyr"))
    
    config_data =
      read_xlsx(config_file_locn)
    ### defensive checks
    check_column_names_as_expected(
      data_frame_to_check = config_data,
      expected_column_names = c("MODELING_ITERATION",	"TYPE",	"ATTRIBUTE"),
      data_frame_description = "columns_for_training config file"
    )
    
    ### defensive for var type: string
    config_data =
      config_data %>% 
      mutate(MODELING_ITERATION = as.character(MODELING_ITERATION))

    config_data_this_iter =
      config_data %>% 
      filter(MODELING_ITERATION == model_iteration)
    
    columns_for_training = training_data
    for (i in 1:nrow(config_data_this_iter)) {
      if (config_data_this_iter$TYPE[i] == "columns to exclude") {
          
          columns_for_training =
            eval(parse(text = 
                         paste0(
                           "select(columns_for_training, -",
                           config_data_this_iter$ATTRIBUTE[i],
                           ")"
                         )))
        }
    }
    
    columns_for_training = names(columns_for_training)
    
    return(columns_for_training)
  }