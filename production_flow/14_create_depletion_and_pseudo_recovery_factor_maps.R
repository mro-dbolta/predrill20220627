
create_depletion_and_pseudo_recovery_factor_maps =
  function(
    depletion_maps_folder = "../7_depletion_maps/",
    run_if = FALSE
  ){
    if (run_if) {
      source("anonymous_support_functions/_build_gcm.1_create_phase_depletion_map.R", local = TRUE)
      source("anonymous_support_functions/_build_gcm.2_join_phases_for_depletion_map.R", local = TRUE)
      source("anonymous_support_functions/_build_gcm.3_create_depletion_over_stooip_maps.R", local = TRUE)

      
      ### create final_depletion_grids for each phase
      map(c("L", "G"),
          function(phase){
            create_phase_depletion_map(
              input_actual_production_locn = "../1_base_Sujan_data/EAGLEFORD_INTEGRATED_VOLUMES_PDAYS.csv",
              grid_with_wells_locn = paste0(depletion_maps_folder,
                                            "wells_associated_with_grid_pts.csv"),
              output_locn = depletion_maps_folder,
              Phase = phase
            )      
          })
      
      join_phases_for_depletion_map(
        csv_locn_Liquid = paste0(depletion_maps_folder,
                                 "final_depletion_grid_L.csv"),
        csv_locn_Gas = paste0(depletion_maps_folder,
                              "final_depletion_grid_G.csv"),
        output_locn = depletion_maps_folder
      )
      
      create_depletion_over_stooip_maps(
        depletion_data_locn = paste0(depletion_maps_folder,
                                     "final_depletion_grid.csv"),
        flat_file_locn = "../1_base_Sujan_data/EAGLEFORD_IHS_Prod_FlatTable.csv",
        output_locn_and_filename = paste0(depletion_maps_folder,
                                          "final_depletion_grid_with_STOOIP.csv")
      )
      
      print("Run successful: create_depletion_and_pseudo_recovery_factor_maps")
    }
  }