
### create these folders for first AWS arps declines results
### /3_AWS_arps_declines 
### /3_AWS_arps_declines/sagemaker_results

create_AWS_arps_folders = 
  function(target_main_directory,
           run_if = FALSE){
    if (run_if) {
     
      #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
      
      dir.create(target_main_directory, showWarnings = FALSE)
      dir.create(paste0(target_main_directory, "/sagemaker_results"), showWarnings = FALSE)
      # create_folder_dos(target_main_directory)
      # create_folder_dos(paste0(target_main_directory, "\\sagemaker_results"))
       
    }
  }