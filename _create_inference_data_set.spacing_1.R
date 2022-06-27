
inference_data_prep_function =
  function(
    initial_data
  ){
    
    source("general_purpose_support_functions/spacing_conversions.R", local = TRUE)
    
    inference_data =
      map(c(20, 40, 60, 80, 160), 
          function(i){
            initial_data %>%
              filter(DT_FIRST_PRODUCTION_YEAR >= 2021) %>%
              # filter(OTH_OPERATOR_NAME == "MARATHON OIL COMPANY") %>% 
              ### initial feature
              mutate(
                WS_AVG_180POST_AF = acres2feet(acres = i),
                WS_MIN_180POST_AF = acres2feet(acres = i),
                WS_QQ_AVG_180POST_AF = acres2feet(acres = i),
                WS_QQ_MIN_180POST_AF = acres2feet(acres = i),
                WS_AVG_180POST_SF = acres2feet(acres = i),
                WS_MIN_180POST_SF = acres2feet(acres = i),
                WS_QQ_AVG_180POST_SF = acres2feet(acres = i),
                WS_QQ_MIN_180POST_SF = acres2feet(acres = i),
                WS_1MILE_WD_360_SF = acres2WPS(acres = i),
                WS_1MILE_WD_360_AF = acres2WPS(acres = i),
                WS_1MILE_WD_180_SF = acres2WPS(acres = i),
                WS_1MILE_WD_180_AF = acres2WPS(acres = i),
                WS_HALFMILE_WD_180_SF = acres2WPS(acres = i),
                WS_HALFMILE_WD_180_AF = acres2WPS(acres = i),
                WS_HALFMILE_WD_360_SF = acres2WPS(acres = i),
                WS_HALFMILE_WD_360_AF = acres2WPS(acres = i)) %>% 
              mutate(SENSITIVITY_CASE = paste0("SPACING_", i, "_acres"))
          }) %>%
      bind_rows()

    return(inference_data)    
  }