### function to create depletion map data set
### take grid with wells within threshold and distance to those wells

create_phase_depletion_map = 
  function(
    input_actual_production_locn,
    grid_with_wells_locn,
    output_locn,
    Phase = "L"){
    
    
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("anonymous_support_functions/_create_liquid_depletion_map.R", local = TRUE)      
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    source("tests/check_column_names_as_expected.R", local = TRUE)
    
    
    # _create_liquid_depletion_map.R
    load_libraries_for_depln_map()
    
    ### load data
    input_actual_production_data = load_data_that_has_api(input_actual_production_locn)
    grid_with_wells_data = load_data_that_has_api(grid_with_wells_locn)
    
    ### error checking
    # names_check =
    #   identical(names(input_actual_production_data),
    #           c("API", "PRODUCT", "PROD_DATE", "VOLUME", "CUM_VOLUME", "CUM_PRODUCING_DAYS",
    #             "SOURCE", "ASSET_NAME"))
    # if (!names_check) stop("Column names of production data don't match expected")
    
    check_column_names_as_expected(
      data_frame_to_check = input_actual_production_data,
      expected_column_names = c("API", "PRODUCT", "PROD_DATE", "VOLUME", "CUM_VOLUME", "CUM_PRODUCING_DAYS",
                                "SOURCE", "ASSET_NAME"),
      data_frame_description = "production data"
    )
    
    check_column_names_as_expected(
      data_frame_to_check = grid_with_wells_data,
      expected_column_names = c("grid_x", "grid_y", "API", "MIN_DISTANCE_TO_WELL"),
      data_frame_description = "initial grid data"
    )
    
    ### prep data
    actual_production_latest_cum_oil =
      input_actual_production_data %>% 
      filter(PRODUCT == Phase,
             ASSET_NAME == "EAGLEFORD") %>% 
      group_by(API) %>% 
      summarise(CUM_VOLUME_TO_DATE = max(CUM_VOLUME),
                .groups = "drop")
    
    grid_with_wells_production =
      grid_with_wells_data %>% 
      left_join(actual_production_latest_cum_oil,
                by = "API") %>% 
      na.omit() %>% 
      mutate(CUM_VOLUME_TO_APPLY =
               if_else(MIN_DISTANCE_TO_WELL < 350,
                       CUM_VOLUME_TO_DATE,
                       if_else(MIN_DISTANCE_TO_WELL < 700,
                               CUM_VOLUME_TO_DATE * 0.5,
                               if_else(MIN_DISTANCE_TO_WELL < 1400,
                                       CUM_VOLUME_TO_DATE * 0.25,
                                       0.0)))) %>% 
      group_by(grid_x, grid_y) %>% 
      summarise(DEPLETION_VOLUME = sum(CUM_VOLUME_TO_APPLY),
                .groups = "drop")
    
    
    output_locn = add_path_slash_if_needed(output_locn)
    
    grid_with_wells_production %>% 
      fwrite(paste0(output_locn,
                    "final_depletion_grid_",
                    Phase,
                    ".csv"))
    
    
  }