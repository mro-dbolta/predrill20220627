
write_days_actual_history_to_disk = 
  function(fn_days_actual_history,
           fn_dca_output_target_directory){
    
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
    
    
    #DOS_target_directory = paste0("..\\", fn_dca_output_target_directory)
    R_target_directory = paste0("../", fn_dca_output_target_directory)
    
    # suppressWarnings(
    #   shell(paste("mkdir", DOS_target_directory), intern = TRUE)
    # )
    
    dir.create(R_target_directory, showWarnings = FALSE)
    #create_folder_dos(DOS_target_directory)
    
    fn_days_actual_history %>% 
      fwrite(
        paste0(add_path_slash_if_needed(R_target_directory),
               "days_actual_history.csv")
      )  
  }