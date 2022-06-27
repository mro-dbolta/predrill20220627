
extract_model_names =
  function(model_folder_locns){
    model_names = 
      map_chr(1:length(model_folder_locns),
              function(i){
                second_underscore_locn =
                  str_locate_all(model_folder_locns[i], "_")[[1]][2,1]
                
                str_sub(model_folder_locns[i], end = second_underscore_locn - 1L)
              })
    
    return(model_names)
    
  }