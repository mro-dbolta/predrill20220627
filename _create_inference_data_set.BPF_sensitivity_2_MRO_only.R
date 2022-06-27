

# create_inference_data_set.BPF_sensitivity_1 =
inference_data_prep_function =
  function(
    initial_data
  ){
    inference_data =
      map(c(1200, 1500, 1800, 2000, 2200), 
          function(i){
            initial_data %>%
              filter(OTH_OPERATOR_NAME == "MARATHON OIL COMPANY") %>% 
              filter(DT_FIRST_PRODUCTION_YEAR >= 2021) %>%
              ### initial feature
              mutate(CP_TOTALPROPPERFOOT = 2500,
                     CP_TOTALFLUIDPERFOOT = i) %>%
              ### fix related features
              mutate(CP_TOTAL_PROPPANT = CP_TOTALPROPPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL,
                     CP_TOTAL_FLUID = CP_TOTALFLUIDPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL) %>%
              mutate(SENSITIVITY_CASE = paste0("FLUID_BPF_", CP_TOTALFLUIDPERFOOT/42))
          }) %>%
      bind_rows()
    
    return(inference_data)    
  }
