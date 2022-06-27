

#create_inference_data_set.ppf_sensitivity_1 =
inference_data_prep_function = 
  function(
    initial_data
  ){
    inference_data =
      map(c(1500, 2000, 2500, 3000), function(i){
        initial_data %>%
          filter(OTH_OPERATOR_NAME == "MARATHON OIL COMPANY") %>% 
          filter(DT_FIRST_PRODUCTION_YEAR >= 2021) %>%
          ### initial feature
          mutate(CP_TOTALPROPPERFOOT = i) %>%
          mutate(CP_TOTALFLUIDPERFOOT = CP_TOTALPROPPERFOOT / CP_TOT_PROPPANT_BY_TOT_FLUID) %>%
          ### fix related features
          mutate(CP_TOTAL_PROPPANT = CP_TOTALPROPPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL,
                 CP_TOTAL_FLUID = CP_TOTALFLUIDPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL) %>%
          mutate(SENSITIVITY_CASE = paste0("PPF_", CP_TOTALPROPPERFOOT))
      }) %>%
      bind_rows()
    
    return(inference_data)
  }


