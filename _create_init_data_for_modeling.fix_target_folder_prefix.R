
fix_target_folder_prefix = 
  function(new_folder_locn){
  
  if (str_starts(new_folder_locn, "../")) {
    new_folder_locn_dos = paste0("..\\",
                                 str_sub(new_folder_locn, start = 4L)
    )
  } else if (str_starts(new_folder_locn, "..\\")) {
    new_folder_locn_dos = new_folder_locn
  } else {
    new_folder_locn_dos = paste0("..\\",
                                 str_sub(new_folder_locn, start = 4L)
    )
  }
  
  return(new_folder_locn_dos)
  
}