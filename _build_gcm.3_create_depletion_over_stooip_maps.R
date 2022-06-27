

create_depletion_over_stooip_maps =
  function(
    depletion_data_locn = "../../7_depletion_maps/final_depletion_grid.csv",
    flat_file_locn = "../../1_base_Sujan_data/EAGLEFORD_IHS_Prod_FlatTable.csv",
    output_locn_and_filename = "../../7_depletion_maps/final_depletion_grid_with_STOOIP.csv"
  ){
    "
    This uses all disk locations as inputs and outputs because it was built as a one-off at first.
    Inputs: the oil & gas depletion maps, the flat file with In-Place maps for each well
    Output: map with depletion grids PLUS depletion amount / in-place volume
    By phase- separate oil & gas maps
    "
    
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    install_load_libraries_strings(c("ggplot2",
                                     "dplyr",
                                     "tidyr",
                                     "readr",
                                     "purrr",
                                     "tibble",
                                     "stringr",
                                     "forcats", "data.table"))
    
    source("general_purpose_support_functions/linear_interpolate_data.R", local = TRUE)
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    
    depletion_data =
      fread(depletion_data_locn) %>% 
      data.frame(stringsAsFactors = FALSE)
    
    flat_file = load_data_that_has_api(flat_file_locn)
    
    flat_file_prepped =
      flat_file %>% 
      select(LOC_MP_X,
             LOC_MP_Y,
             MGEO_EF_STOOIP,
             MGEO_EF_OGIP) %>% 
      filter(!is.na(LOC_MP_X)) 
    
    flat_file_prepped_oil = 
      flat_file_prepped %>% 
      ### impute oil in gas area
      filter(!is.na(MGEO_EF_STOOIP) ) %>% 
      select(LOC_MP_X,
             LOC_MP_Y,
             MGEO_EF_STOOIP) %>% 
      rename(grid_x = LOC_MP_X,
             grid_y = LOC_MP_Y) %>% 
      na.omit()
    
    flat_file_prepped_gas = 
      flat_file_prepped %>% 
      ### impute oil in gas area
      filter(!is.na(MGEO_EF_OGIP) ) %>% 
      select(LOC_MP_X,
             LOC_MP_Y,
             MGEO_EF_OGIP) %>% 
      rename(grid_x = LOC_MP_X,
             grid_y = LOC_MP_Y) %>% 
      na.omit()
    
    ##################################################
    
    interpolated_data_oil =
      linear_interpolate_data(
        source_data = flat_file_prepped_oil,
        grid_data_to_interp_over = depletion_data,
        x_column_name = "grid_x",
        y_column_name = "grid_y",
        map_feature_name = "MGEO_EF_STOOIP",
        EXTRAP = TRUE,
        run_if = TRUE 
      )
    
    interpolated_data_gas =
      linear_interpolate_data(
        source_data = flat_file_prepped_gas,
        grid_data_to_interp_over = depletion_data,
        x_column_name = "grid_x",
        y_column_name = "grid_y",
        map_feature_name = "MGEO_EF_OGIP",
        EXTRAP = TRUE,
        run_if = TRUE 
      )
    
    #####################################################
    
    depletion_data =
      depletion_data %>% 
      mutate(MGEO_EF_STOOIP = interpolated_data_oil,
             MGEO_EF_OGIP = interpolated_data_gas,
             DEPLETION_VOLUME_OVER_MGEO_EF_STOOIP = DEPLETION_VOLUME_LIQUID / MGEO_EF_STOOIP,
             DEPLETION_VOLUME_OVER_MGEO_EF_OGIP = DEPLETION_VOLUME_GAS / MGEO_EF_OGIP)
    
    depletion_data %>% 
      fwrite(output_locn_and_filename)
    
    return(1L)
    
  }
