
# create_inference_data_set.base_plan_1 =
inference_data_prep_function =
  function(
    initial_data
  ){
    inference_data =
      initial_data %>% 
      filter(OTH_OPERATOR_NAME == "MARATHON OIL COMPANY") %>% 
      mutate(SENSITIVITY_CASE = "BASE_PLAN")
    
    return(inference_data)
  }
