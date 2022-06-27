
join_phases_for_depletion_map =
  function(
    csv_locn_Liquid,
    csv_locn_Gas,
    output_locn
  ){
    
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    source("tests/check_column_names_as_expected.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    
    install_load_libraries_strings(c("data.table", 
                                     "ggplot2",
                                     "dplyr",
                                     "tidyr",
                                     "readr",
                                     "purrr",
                                     "tibble",
                                     "stringr",
                                     "forcats"))
    
    data_L = fread(csv_locn_Liquid) %>% 
      data.frame(stringsAsFactors = FALSE) 
    
    check_column_names_as_expected(data_frame_to_check = data_L,
                                   expected_column_names = c("grid_x", "grid_y", "DEPLETION_VOLUME"),
                                   data_frame_description = "Liquid depletion data")
    
    data_L =
      data_L %>% 
      rename(DEPLETION_VOLUME_LIQUID = DEPLETION_VOLUME)
    
    
    
    data_G = fread(csv_locn_Gas) %>% 
      data.frame(stringsAsFactors = FALSE) 
    
    check_column_names_as_expected(data_frame_to_check = data_G,
                                   expected_column_names = c("grid_x", "grid_y", "DEPLETION_VOLUME"),
                                   data_frame_description = "Gas depletion data")
    
    data_G =
      data_G %>% 
      rename(DEPLETION_VOLUME_GAS = DEPLETION_VOLUME)
    
    
    
    
    data_combined = 
      data_L %>% 
      left_join(data_G,
                by = c("grid_x", "grid_y"))
    
    if (nrow(data_L) != nrow(data_combined)) stop("Join of oil and gas depletion data changed number of rows.")
    
    
    
    data_combined %>% 
      fwrite(paste0(add_path_slash_if_needed(output_locn),
                    "final_depletion_grid.csv"))
    
    return(0)
    
    
  }