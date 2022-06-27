

inference_data_prep_function =
  function(
    initial_data
  ){
    inference_data =
      initial_data %>% 
      filter(API %in% c("G7289",
                        "G2083",
                        "G6758",
                        "G6420",
                        "42177323520000")) %>% 
      mutate(SENSITIVITY_CASE = "IDEAL_AZIMUTH",
             DS_SIN_AZIMUTH = -0.99)
    
    return(inference_data)
  }
