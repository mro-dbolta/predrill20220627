
recalculate_best_arps_per_well = function(full_grid_results_location,
                                          best_arps_per_well_output_location,
                                          run_if){
  
  if (run_if) {
    
    
    all_wells_arps_grid_results_oil = load_data_that_has_api(full_grid_results_location)
    
    all_wells_best_arps_fit_oil = 
      all_wells_arps_grid_results_oil %>% 
      group_by(API) %>% 
      filter(MEAN_ABS_ERROR_LOG10_VOLUME == min(MEAN_ABS_ERROR_LOG10_VOLUME)) %>% 
      ### keep only first in case of ties
      mutate(row_counter = 1L,
             row_counter = cumsum(row_counter)) %>%
      filter(row_counter == 1) %>% 
      select(-row_counter) %>% 
      ungroup()
    
    all_wells_best_arps_fit_oil %>% 
      fwrite(best_arps_per_well_output_location)
  }
}