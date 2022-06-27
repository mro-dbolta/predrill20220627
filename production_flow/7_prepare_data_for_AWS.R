
### this function finds wells from the first pass of dca that don't have full well histories
### the wells and production and fcasts will then be passed to AWS for arps fitting
### the source of these wells is enersite and the asset loads

prepare_data_for_AWS = function(path_to_initial_decline_data,
                                target_number_days_on = 40 * 12 * 30,
                                run_if = TRUE){
  
  if (run_if) {
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
    
    target_directory = "5_AWS_enersite_declines"
    
    input_data = load_data_that_has_api(target_path = path_to_initial_decline_data)
    
    wells_without_full_duration =
      input_data %>% 
      group_by(API) %>% 
      mutate(max_CUM_PRODUCING_DAYS = max(CUM_PRODUCING_DAYS)) %>% 
      ungroup() %>% 
      filter(max_CUM_PRODUCING_DAYS < 14400) %>% 
      select(-max_CUM_PRODUCING_DAYS)
    
    
    # for (folder in c("", "\\sagemaker_input", "\\sagemaker_results",
    #                  "\\cleanup_sagemaker_results_in_R")) {
    #   suppressWarnings(
    #     shell(paste("mkdir", 
    #                 paste0("..\\", target_directory, folder)
    #     ), 
    #     intern = TRUE)
    #   )  
    # }
    
    for (folder in c("", "\\sagemaker_input", "\\sagemaker_results",
                     "\\cleanup_sagemaker_results_in_R")) {
       
      # create_folder_dos(paste0("..\\", target_directory, folder))
      dir.create(paste0("../", target_directory, folder), showWarnings = FALSE)
      
    }
    
    
    wells_without_full_duration %>% 
      fwrite(
        paste0("../",
               target_directory,
               add_path_slash_if_needed("/sagemaker_input"),
               "wells_without_full_duration.csv")
      )
    
    return(wells_without_full_duration)
    
  }
}