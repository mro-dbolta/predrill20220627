
maximize_output_data =
  function(
    input_data_locn,
    output_filename,
    run_if = FALSE
  ){
    if (run_if) {
      "
      Take all output values in test set and replace with HIGH_PLACEHOLDER_VALUE
      Mariano's model may be more optimistic, so build a test set to tune for optimism.
      "
      HIGH_PLACEHOLDER_VALUE = 100000000
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/clip_string_after_last_forward_slash.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/str_clip_csv_from_end.R", local = TRUE)
      
      
      install_load_libraries_strings(c("data.table",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      
      output_folder = clip_string_after_last_forward_slash(string_in = input_data_locn)
      
      output_filename = str_clip_csv_from_end(output_filename)
      output_filename = paste0(output_filename, ".csv")
      
      input_data = load_data_that_has_api(input_data_locn)
      
      output_data =
        input_data %>% 
        mutate(across(starts_with("LIQUID_"), 
                      .fns = ~ (.x * 0 + HIGH_PLACEHOLDER_VALUE) ))
      
      output_data %>% 
        fwrite(paste0(output_folder,
                      output_filename))
      
      return("Dataset with maxed LIQUID_ columns created.")
    }
  }