
get_input_vars_from_rfsrc_call =
  function(
    rfsrc_model
  ){
    ### convert rfsrc call to string, keep only 2nd part which has formula
    model_formula_as_string = as.character(rfsrc_model$call)[2]
    
    ### find "~" position in formula string
    tilde_locn =
      str_locate(string = model_formula_as_string, 
                 pattern = "~")
    
    tilde_locn = tilde_locn[,"start"]
    tilde_locn = tilde_locn[!is.na(tilde_locn)]
    tilde_locn = tilde_locn[1]
    
    ### extract portion of formula before " ~"
    model_call_after_tilde = 
      str_sub(model_formula_as_string, start = tilde_locn + 1L)
    
    input_variable_names = 
      model_call_after_tilde %>% 
      str_replace(pattern = "\n", replacement = "") %>% 
      str_replace(pattern = " ", replacement = "") %>% 
      str_split(pattern = "\\+") %>% 
      .[[1]] %>%  
      str_trim()
    
return(input_variable_names)
  }

