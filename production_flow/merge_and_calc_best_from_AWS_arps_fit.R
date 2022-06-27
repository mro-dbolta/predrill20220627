

merge_and_calc_best_from_AWS_arps_fit =
  function(multiple_csvs_location = "",
           multiple_csvs_output_location,
           best_arps_per_well_output_location,
           merge_csvs = F,
           recalc_best_arps_per_well){
    
    source("general_purpose_support_functions/merge_csvs_in_folder.R", local = TRUE)
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("anonymous_support_functions/_well_declines.R", local = TRUE)
    
    ### merge production files
    if (merge_csvs) {
      merge_csvs_in_folder(input_files_location = multiple_csvs_location,
                           output_location_and_filename = multiple_csvs_output_location,
                           run_if = T)
    }
    ### get best arps result per well, write result to disk
    recalculate_best_arps_per_well(full_grid_results_location = multiple_csvs_output_location,
                                   best_arps_per_well_output_location = best_arps_per_well_output_location,
                                   run_if = recalc_best_arps_per_well)
    
    
  }