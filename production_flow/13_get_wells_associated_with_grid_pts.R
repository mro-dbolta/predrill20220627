
### filter grid to those near wells, 
### get all wells associated with a given grid point
get_wells_associated_with_grid_pts =
  function(
    input_actual_production_locn,
    main_depletion_directory,
    output_filename,
    run_if = FALSE
  ){
    if (run_if) {
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/str_locate_start.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("anonymous_support_functions/_create_liquid_depletion_map.R", local = TRUE)
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      
      
      MAX_DISTANCE_TO_WELLS = 1400
      
      libraries_to_load = 
        c("dplyr",
          "tidyr",
          "readr",
          "purrr",
          "tibble",
          "stringr",
          "forcats",
          "data.table",
          "rlang",
          "furrr",
          "future")
      
      install_load_libraries_strings(libraries_to_load)
      
      ### load data
      initial_rectangular_grid = 
        fread(paste0(add_path_slash_if_needed(main_depletion_directory),
                     "data_to_send_to_AWS/initial_rectangular_grid.csv")) %>% 
        data.frame(stringsAsFactors = FALSE)
      well_locations_1_row_per_spot_on_lateral = 
        load_data_that_has_api(paste0(add_path_slash_if_needed(main_depletion_directory),
                                      "data_to_send_to_AWS/well_location_data.csv"))
      distances_from_grid_to_nearest_well = fread(paste0(add_path_slash_if_needed(main_depletion_directory),
                                                         "data received back from AWS merged.csv")) %>% 
        data.frame(stringsAsFactors = FALSE)
      
      
      ### trim down grid
      grid_within_range_of_wells =
        initial_rectangular_grid %>% 
        bind_cols(distances_from_grid_to_nearest_well) %>% 
        filter(min_distance_to_wells <= MAX_DISTANCE_TO_WELLS) %>% 
        select(-min_distance_to_wells)
      
      
      future::plan(multisession)
      
      wells_associated_with_grid_pts =
        future_map_dfr(1:nrow(grid_within_range_of_wells),
                       .progress = TRUE,
                       function(i){
                         # _create_liquid_depletion_map.R
                         get_wells_within_distance_of_single_grid_point(grid_x = grid_within_range_of_wells$X[i],
                                                                        grid_y = grid_within_range_of_wells$Y[i],
                                                                        well_df = well_locations_1_row_per_spot_on_lateral,
                                                                        distance_threshold = MAX_DISTANCE_TO_WELLS)
                       }) %>% 
        select(grid_x, grid_y, everything())
      
      wells_associated_with_grid_pts %>% 
        fwrite(
          paste0(add_path_slash_if_needed(main_depletion_directory),
                 output_filename)
        )
      
      print("Run successful: get_wells_associated_with_grid_pts")
    }
  }