
### copy using dos commands
create_sujan_data_files = 
  function(fn_sujan_data_files, 
           target_directory,
           sujan_files_source_path = "O:\\permanent\\PTIM\\DataScience\\0_Reservoir\\IHS-Data\\Current\\",
           run_if
  ){
    
    if (run_if) {
      
      source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
      
      create_folder_dos(target_directory)
      
      for (sujan_data_file in fn_sujan_data_files) {
        ### get just the file name
        sujan_data_file_reduced = 
          str_sub(sujan_data_file,
                  start = regexpr("([\\])", sujan_data_file) + 1)
        
        ### DOS copy file over to new location
        shell(paste0("copy ", 
                     sujan_files_source_path, sujan_data_file, " ",
                     target_directory, "\\", sujan_data_file_reduced), intern = TRUE)
        
      }  
    }
  }
