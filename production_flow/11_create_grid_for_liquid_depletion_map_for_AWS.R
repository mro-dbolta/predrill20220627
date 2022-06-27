### creates the large grid
### calculates min distance from grid point to well locations
create_grid_for_liquid_depletion_map_for_AWS = 
  function(input_header_locn,
           output_directory = "7_depletion_maps",
           feet_between_points_on_well = 100L,
           grid_spacing,
           run_if = FALSE){
    if (run_if) {
      
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
      source("general_purpose_support_functions/str_locate_start.R", local = TRUE)
      source("general_purpose_support_functions/create_rectangular_grid.R", local = TRUE)
      source("general_purpose_support_functions/min_distance_between_points_and_a_grid.R", local = TRUE)
      source("anonymous_support_functions/_create_liquid_depletion_map.R", local = TRUE)
      
      MAX_DISTANCE_TO_WELLS = 1400
      DISTANCE_BETWEEN_GRID_POINTS = 500
      DISTANCE_BETWEEN_POINTS_ON_WELL_LATERAL = 100
      
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      
      libraries_to_load = 
        c("dplyr",
          "tidyr",
          "readr",
          "purrr",
          "tibble",
          "stringr",
          "forcats",
          "data.table",
          "rlang")
      
      install_load_libraries_strings(libraries_to_load)
      
      ### load data
      input_header_data = load_data_that_has_api(input_header_locn)
      
      ### folders setup
      dir.create(paste0("../", output_directory), showWarnings = FALSE)
      dir.create(paste0("../", output_directory,
                        "/data_to_send_to_AWS"), showWarnings = FALSE)
      dir.create(paste0("../", output_directory,
                        "/data_received_back_from_AWS"), showWarnings = FALSE)
      
      # create_folder_dos(paste0("..\\", output_directory))
      # create_folder_dos(paste0("..\\", output_directory,
      #                          "\\data_to_send_to_AWS"))
      # create_folder_dos(paste0("..\\", output_directory,
      #                          "\\data_received_back_from_AWS"))
      
      output_directory_R_AWS = paste0("../", output_directory, "/data_to_send_to_AWS/")
      
      ### header data locations columns only
      data_1_locations = 
        ### location data from header
        input_header_data %>% 
        mutate(NUMBER_POINTS_TO_MAP = ceiling(CP_LAT_LEN_HORIZ_DISPLACEMENT / DISTANCE_BETWEEN_POINTS_ON_WELL_LATERAL)) %>% 
        select(API, NUMBER_POINTS_TO_MAP,
               LOC_HEEL_X, LOC_HEEL_Y, 
               LOC_BH_X, LOC_BH_Y
        ) %>% 
        na.omit() %>% 
        ### correct for wells that are straight north/south
        ### will need to calculate as slope of linear equation later
        mutate(LOC_BH_X = if_else(LOC_HEEL_X == LOC_BH_X,
                                  LOC_BH_X + 0.1,
                                  LOC_BH_X)) %>% 
        na.omit() %>% 
        mutate(SLOPE = (LOC_BH_Y - LOC_HEEL_Y) / (LOC_BH_X - LOC_HEEL_X))
      
      ### each well gets several rows, 1 row per position along lateral
      data_2_row_per_posn_on_lateral =
        calc_1_row_per_posn_on_lateral(data_1_locations)
      
      data_2_row_per_posn_on_lateral %>% 
        fwrite(paste0(output_directory_R_AWS,
                      "well_location_data.csv"))
      
      ### initial large rectangular grid
      initial_rectangular_grid =
        create_rectangular_grid(x_pts = data_2_row_per_posn_on_lateral$X, 
                                y_pts = data_2_row_per_posn_on_lateral$Y, 
                                max_distance = MAX_DISTANCE_TO_WELLS, 
                                distance_between_grid_points = DISTANCE_BETWEEN_GRID_POINTS)
      
      initial_rectangular_grid %>% 
        fwrite(paste0(output_directory_R_AWS,
                      "initial_rectangular_grid.csv"))
    }}