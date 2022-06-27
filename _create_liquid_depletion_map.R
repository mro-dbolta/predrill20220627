
load_libraries_for_depln_map =
  function(){
    
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
  }

calc_1_row_per_posn_on_lateral = 
  function(df_in){
    map_dfr(1:nrow(df_in),
            function(i){
              data.frame(API = df_in$API[i],
                         X = seq(from = df_in$LOC_HEEL_X[i], 
                                 to = df_in$LOC_BH_X[i], 
                                 length.out = df_in$NUMBER_POINTS_TO_MAP[i]),
                         stringsAsFactors = FALSE) %>% 
                mutate(Y = df_in$SLOPE[i] * 
                         (X - df_in$LOC_BH_X[i]) + 
                         df_in$LOC_BH_Y[i])
            })
  }

### assumes col_names of well_df = [API, X, Y]
### returns df [API, grid_x, grid_y, MIN_DISTANCE_TO_WELL]
get_wells_within_distance_of_single_grid_point =
  function(grid_x,
           grid_y,
           well_df,
           distance_threshold){
    
    # options(dplyr.summarise.inform = FALSE)
    
    well_df %>% 
      mutate(grid_x = grid_x,
             grid_y = grid_y,
             distance_between_points = sqrt(abs(X - grid_x) * abs(X - grid_x) + 
                                              abs(Y - grid_y) * abs(Y - grid_y))) %>%
      filter(distance_between_points <= distance_threshold) %>% 
      group_by(API, grid_x, grid_y) %>% 
      summarise(MIN_DISTANCE_TO_WELL = min(distance_between_points)
                , .groups = "drop")
      
  }

