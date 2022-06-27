
fix_OTH_VOLUMES_SOURCE =
  function(
    flat_file_csv_path,
    run_if = F
  ){
    if (run_if) {
      "
      The column OTH_VOLUME_SOURCE uses values that only appear in train or plan wells.
      Use rule to unify so internal vs external data reads same.
      "
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "data.table"))
      
      flat_file = load_data_that_has_api(flat_file_csv_path) %>% 
        mutate(OTH_VOLUME_SOURCE = if_else(OTH_VOLUME_SOURCE == "I",
                                           "External",
                                           if_else(OTH_VOLUME_SOURCE == "P",
                                                   "Internal",
                                                   if_else(OTH_VOLUME_SOURCE == "PE",
                                                           "Internal",
                                                           if_else(OTH_VOLUME_SOURCE == "E",
                                                                   "Internal",
                                                                   OTH_VOLUME_SOURCE)))))
      
      # return(flat_file)
      flat_file %>% 
        fwrite(flat_file_csv_path)
      
      print(paste0("File successfully updated at ", flat_file_csv_path))
      
    }
  }

