
get_rfsrc_formula_as_string =
  function(
    target_variables,
    working_list_of_inputs
  ){
    if (length(target_variables) < 1) {
      stop("No target variables specified for rfe call")
    }
    
    if (length(target_variables) > 1) {
      target_variables_string = paste0(target_variables, collapse = ",")
      target_variables_string = paste0("Multivar(", target_variables_string, ")~")  
    } else {
      target_variables_string = paste0(target_variables, collapse = ",")
      target_variables_string = paste0(target_variables_string, "~")
    }
    
    rfsrc_formula = paste0(target_variables_string,
                           paste0(working_list_of_inputs, collapse = "+"))
    
    return(rfsrc_formula)
    
  }