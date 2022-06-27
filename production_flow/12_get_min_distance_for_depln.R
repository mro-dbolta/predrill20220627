
get_min_distance_for_depln =
  function(
    initial_rectangular_grid_locn,
    well_location_data_locn,
    output_folder,
    run_if = FALSE
  ){
    if (run_if) {
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/split_array_into_chunks.R", local = TRUE)
      
      
      vector_of_libraries_to_load =
        c("ggplot2",
          "dplyr",
          "tidyr",
          "readr",
          "purrr",
          "tibble",
          "stringr",
          "forcats", "data.table",
          "lubridate",
          #"furrr", "future"
          "foreach",
          "doParallel")
      
      install_load_libraries_strings(vector_of_libraries_to_load)
      
      output_folder = add_path_slash_if_needed(output_folder)
      
      initial_rectangular_grid = fread(initial_rectangular_grid_locn) %>% 
        data.frame(stringsAsFactors = F)
      
      well_location_data = load_data_that_has_api(well_location_data_locn)
      
      number_wells = length(unique(well_location_data$API))
      number_grid_points = nrow(initial_rectangular_grid)
      
      x_ref = well_location_data$X
      y_ref = well_location_data$Y
      
      x_grid = initial_rectangular_grid$X
      y_grid = initial_rectangular_grid$Y
      
      
      num_cores = detectCores()[1] - 1L
      cl = makeCluster(num_cores) #not to overload your computer
      registerDoParallel(cl)
      
      ### get indices to split the grid points into chunks
      number_of_chunks = 100L # 1000000L
      grid_index_chunks =
        split_array_into_chunks_n_equal_chunks(array_in = 1:length(x_grid),
                                               n_chunks = number_of_chunks)
      
      pb = txtProgressBar(min = 0, max = number_of_chunks, style = 3)
      
      grid_results = c()
      for (grid_tranche in 1:number_of_chunks) {
        grid_results_tranche = 
          foreach(i = grid_index_chunks[[grid_tranche]],
                  .packages = vector_of_libraries_to_load,
                  .combine = "c",
                  .multicombine = TRUE) %dopar% {
                    sqrt(
                      min(
                        (x_grid[i] - x_ref)*(x_grid[i] - x_ref) + 
                          (y_grid[i] - y_ref)*(y_grid[i] - y_ref)
                      )
                    )
                  }
        
        grid_results =
          grid_results %>% 
          append(values = grid_results_tranche)
        
        setTxtProgressBar(pb, grid_results_tranche)
      }
      
      close(pb)
      #stop cluster
      stopCluster(cl)
      
      min_distance_to_wells = 
        data.frame(min_distance_to_wells = grid_results)
      
      min_distance_to_wells %>% 
        fwrite(paste0(output_folder,
                      "min_distance_to_wells.csv"))
      
      print("Successful calc: Min distance from each grid point to nearest well")
    }
  }