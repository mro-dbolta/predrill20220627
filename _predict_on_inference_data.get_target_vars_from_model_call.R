
get_target_vars_from_model_call =
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
    model_call_before_tilde = 
      str_sub(model_formula_as_string, end = tilde_locn - 2L)
    
    ### if parenthesis at end, clip off
    if (str_ends(model_call_before_tilde, "\\)")) {
      model_call_before_tilde = 
        str_sub(model_call_before_tilde, end = -2L)
      }
    
    ### if "(" found, usually with Multivar(, clip
    if (str_detect(model_call_before_tilde, "\\(")) {
      open_parenthesis_locn =
        str_locate(model_call_before_tilde, "\\(")[1,"start"]
      
      model_call_target_vars = 
        str_sub(model_call_before_tilde,
                start = open_parenthesis_locn + 1L)
    } else {
      model_call_target_vars = model_call_before_tilde
    }
    
    ### if ", " detected as spacing in target vars, split the multiple target vars
    if (str_detect(model_call_target_vars, ", ")) {
      model_call_target_vars = str_split(model_call_target_vars, ", ")
      model_call_target_vars = unlist(model_call_target_vars)
    }
    
    return(model_call_target_vars)
    
  }