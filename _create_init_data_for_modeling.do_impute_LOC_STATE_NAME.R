

do_impute_LOC_STATE_NAME =
  function(
    data_frame_to_impute
  ){
    source("general_purpose_support_functions/install_load_libraries.R", local = TRUE)
    install_load_libraries(dplyr)
    install_load_libraries(class) # library for classification
    
    row_check =
      data_frame_to_impute %>% 
      filter(LOC_STATE_NAME != "TEXAS",
             LOC_STATE_NAME != "LOUISIANA",
             LOC_STATE_NAME != "MISSISSIPPI",
             LOC_STATE_NAME != "",
             !is.na(LOC_STATE_NAME)) %>%
      nrow()
    if (row_check > 0) stop("data_frame_to_impute LOC_STATE_NAME has additional states than expected.")
    
    ### setup train & prediction for knn model
    training_data =
      data_frame_to_impute %>% 
      filter(LOC_STATE_NAME %in% c("TEXAS", "LOUISIANA", "MISSISSIPPI")) %>% 
      mutate(LOC_STATE_NAME = factor(LOC_STATE_NAME,
                                     levels = c("TEXAS", "LOUISIANA", "MISSISSIPPI"))) %>% 
      select(API, LOC_MP_X, LOC_MP_Y, LOC_STATE_NAME) %>% 
      na.omit()
    
    prediction_data = 
      data_frame_to_impute %>% 
      filter(LOC_STATE_NAME != "TEXAS",
             LOC_STATE_NAME != "LOUISIANA",
             LOC_STATE_NAME != "MISSISSIPPI") %>% 
      select(API, LOC_MP_X, LOC_MP_Y) %>% 
      na.omit()
    
    prediction_data$LOC_STATE_NAME =
      knn(
        train = training_data %>% 
          select(LOC_MP_X, LOC_MP_Y),
        test = prediction_data %>% 
          select(LOC_MP_X, LOC_MP_Y),
        k = 1,
        cl = training_data$LOC_STATE_NAME
      )
    
    final_LOC_STATE_NAME =
      training_data %>% 
      bind_rows(prediction_data) %>% 
      select(API, LOC_STATE_NAME) %>% 
      mutate(LOC_STATE_NAME = as.character(LOC_STATE_NAME))
    
    final_output_df =
      data_frame_to_impute %>% 
      select(-LOC_STATE_NAME) %>% 
      left_join(final_LOC_STATE_NAME,
                by = "API")
    
    ## defensive check after row count
    if (nrow(final_output_df) != nrow(data_frame_to_impute)) {
      stop("Joining LOC_STATE_NAME impute results changed number of rows in original df.")
    }
    
    return(final_output_df) 
    
  }