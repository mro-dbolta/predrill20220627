
create_inference_data =
  function(
    initial_data_locn,
    inference_data_folder_locn = "../11_inference_data_sets/",
    output_filenames,
    inference_data_prep_function_locn =
      c("anonymous_support_functions/_create_inference_data_set.base_plan_1.R",
        "anonymous_support_functions/_create_inference_data_set.ppf_sensitivity_1.R",
        "anonymous_support_functions/_create_inference_data_set.BPF_sensitivity_1.R"),
    run_if = FALSE
  ){
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/str_clip_csv_from_end.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats", "data.table"))
      
      if (length(output_filenames) != length(inference_data_prep_function_locn)) stop("One filename required for each prep function.")
      
      initial_data = load_data_that_has_api(initial_data_locn)
      inference_data_folder_locn = add_path_slash_if_needed(inference_data_folder_locn)
      output_filenames = map_chr(output_filenames, str_clip_csv_from_end)
      
      dir.create(inference_data_folder_locn, showWarnings = FALSE)
      
      
      for (i in 1:length(inference_data_prep_function_locn)) {
        source(inference_data_prep_function_locn[i], local = TRUE)
      
        inference_data =
          inference_data_prep_function(initial_data)
        
        inference_data %>% 
          fwrite(paste0(
            inference_data_folder_locn, output_filenames[i], ".csv"
          ))
          
      }
      
    return("Successful: Inference data sets created")
    }
  }