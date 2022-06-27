

run_Arps_declines =
  function(
    prod_data_locn = "../1_base_Sujan_data/EAGLEFORD_INTEGRATED_VOLUMES_PDAYS.csv",
    output_folder = "../3_AWS_arps_declines",
    run_if = FALSE
  ){
    if (run_if) {
      MINIMUM_NUMBER_RECORDS_TO_FIT = 12
      DAYS_PER_TIMESTEP = 30.0
      
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/arps_equations.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/convert_PROD_DATE_string_to_date.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/split_array_into_chunks.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.initial_cleanup.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.setup_for_oil_fits.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.get_avg_rate_over_last_n_records.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.get_start_dates_of_oil_fit_data.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.complete_setup_prod_data_for_oil_fits.R", local = TRUE)
      source("anonymous_support_functions/_run_Arps_IHS_declines.fit_oil_on_individual_api.R", local = TRUE)
      
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
      
      if (DAYS_PER_TIMESTEP == 30.0) {
        number_of_records_at_end_to_anchor_on = 3.0
      } else if (DAYS_PER_TIMESTEP == 1.0) {
        number_of_records_at_end_to_anchor_on = 30.0
      }
      
      procount_prod_data_1 = load_data_that_has_api(prod_data_locn)
      
      ### initial cleanup
      procount_prod_data_2 = initial_cleanup(procount_prod_data_1)
      
      # ### get list of asset & api
      # ASSET_and_API =
      #   procount_prod_data_2 %>%
      #   select(ASSET_NAME, API) %>%
      #   distinct()
      
      ### setup for oil fits
      procount_prod_data_2a_LIQUID =
        complete_setup_prod_data_for_oil_fits(procount_prod_data_2, MINIMUM_NUMBER_RECORDS_TO_FIT)
      
      ### set up arps grid
      arps_grid = 
        expand.grid(IP_tries = c(seq(from = 0.1, to = 1.0, by = 0.3), 
                                 0.8,
                                 seq(from = 0.9, to = 1.15, by = 0.05),
                                 1.2, 2.0, 5.0, 10.0),
                    Di_Ae_tries = c(seq(from = 0.01, to = 0.81, by = 0.02),
                                    seq(from = 0.82, to = 0.97, by = 0.03),
                                    0.999), # keep this below 1 to avoid errors in Di_daily_nom_tries
                    b_tries = c(seq(from = 0.1, to = 1.1, by = 0.2),
                                0.99, 1.2)) 
      
      arps_grid =
        arps_grid %>% 
        mutate(Di_daily_nom_tries =
                 di_annual_eff_to_daily_nom(di_ann_eff = Di_Ae_tries, 
                                            b_factor = b_tries))
      
      ### get list of well apis to fit
      arps_best_fits_prep_OIL = 
        procount_prod_data_2a_LIQUID %>% 
        select(API) %>% 
        distinct()
      
      ### sample run and loop initialization
      all_wells_arps_grid_results_oil = fit_oil_on_individual_api(api_to_fit_i = 1L)
      
      
      
      num_cores = detectCores()[1] - 1L
      cl = makeCluster(num_cores) #not to overload your computer
      registerDoParallel(cl)
      
      number_of_chunks = 100L
      arps_fits_chunks =
        split_array_into_chunks_n_equal_chunks(array_in = 2:nrow(arps_best_fits_prep_OIL),
                                               n_chunks = number_of_chunks)
      
      pb = txtProgressBar(min = 0, max = number_of_chunks, style = 3)
      
      for (arps_fit_tranche in 1:number_of_chunks) {
        all_wells_arps_grid_results_oil_tranche = 
          foreach(i = arps_fits_chunks[[arps_fit_tranche]],
                  .packages = vector_of_libraries_to_load,
                  .combine = "bind_rows",
                  .multicombine = TRUE) %dopar% {
                    fit_oil_on_individual_api(api_to_fit_i = i)
                  }
        
        all_wells_arps_grid_results_oil =
          all_wells_arps_grid_results_oil %>% 
          bind_rows(all_wells_arps_grid_results_oil_tranche)
        
        setTxtProgressBar(pb, arps_fit_tranche)
      }
      
      close(pb)
      #stop cluster
      stopCluster(cl)
      
      # future::plan(multisession)
      # 
      # all_wells_arps_grid_results_oil =
      #   future_map_dfr(1:nrow(arps_best_fits_prep_OIL),
      #                  .progress = TRUE,
      #                  function(i){
      #                    fit_oil_on_individual_api(api_to_fit_i = i)
      #                  })
      
      all_wells_arps_grid_results_oil %>% 
        fwrite(paste0(output_folder,
                      "all_wells_arps_grid_results_oil.csv"))
      
      return("Arps run of autodeclines successful.")
      
    }
  }
