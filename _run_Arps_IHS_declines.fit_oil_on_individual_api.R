
fit_oil_on_individual_api =
  function(
    api_to_fit_i
  ){
    
    this_api = arps_best_fits_prep_OIL$API[api_to_fit_i]
    this_well_arps_grid = arps_grid
    this_well_arps_grid$API = this_api
    this_well_arps_grid$MEAN_ABS_ERROR_LOG10_VOLUME = -100.0
    
    this_well_production = 
      procount_prod_data_2a_LIQUID %>% 
      filter(API == this_api) %>% 
      select(-ASSET_NAME, -CUM_VOLUME, -API10, -PROD_DATE, -MAX_VOLUME, -PROD_DATE_OIL_DAY_1) %>% 
      mutate(VOLUME_MEAN = mean(VOLUME)) %>% 
      ### get IP correction factor so that end rates match properly
      ### get arps parms for this iteration
      mutate(IP_tries = arps_grid$IP_tries[1],
             Di_Ae_tries = arps_grid$Di_Ae_tries[1],
             b_tries = arps_grid$b_tries[1],
             Di_daily_nom_tries = arps_grid$Di_daily_nom_tries[1])
    
    this_well_production =
      this_well_production %>% 
      mutate(ARPS_INITIAL_VOLUME_PRED = 
               arps_cum(days = OIL_CUM_PRODUCING_DAYS, q_i = this_well_production$IP_tries[1], 
                        di_nom_daily = this_well_production$Di_daily_nom_tries[1], b_factor = this_well_production$b_tries[1]) -
               arps_cum(days = OIL_CUM_PRODUCING_DAYS - DAYS_PER_TIMESTEP, q_i = this_well_production$IP_tries[1], 
                        di_nom_daily = this_well_production$Di_daily_nom_tries[1], b_factor = this_well_production$b_tries[1])) %>% 
      ### fix IP so that rate ends at IP_tries * last_30_days_rate
      mutate(LAST_DAY_VOLUME_PRED = min(ARPS_INITIAL_VOLUME_PRED))
    this_well_arps_grid$IP_tries = 
      (this_well_production$LAST_30_DAYS_MEAN_VOLUME[1] / this_well_production$LAST_DAY_VOLUME_PRED[1]) *
      this_well_arps_grid$IP_tries
    
    this_well_production = 
      this_well_production %>% 
      select(-ARPS_INITIAL_VOLUME_PRED, -LAST_DAY_VOLUME_PRED)
    
    for (row_of_arps_grid_i in 1:nrow(arps_grid)) {
      this_well_production$IP_tries = this_well_arps_grid$IP_tries[row_of_arps_grid_i]
      this_well_production$Di_Ae_tries = this_well_arps_grid$Di_Ae_tries[row_of_arps_grid_i]
      this_well_production$b_tries = this_well_arps_grid$b_tries[row_of_arps_grid_i]
      this_well_production$Di_daily_nom_tries = this_well_arps_grid$Di_daily_nom_tries[row_of_arps_grid_i]
      ### calculate arps volumes with IP updated since original arps_grid
      this_well_production =
        this_well_production %>% 
        mutate(ARPS_VOLUME_PRED =
                 arps_cum(OIL_CUM_PRODUCING_DAYS, this_well_production$IP_tries[1], 
                          this_well_production$Di_daily_nom_tries[1], this_well_production$b_tries[1]) -
                 arps_cum(OIL_CUM_PRODUCING_DAYS - DAYS_PER_TIMESTEP, this_well_production$IP_tries[1], 
                          this_well_production$Di_daily_nom_tries[1], this_well_production$b_tries[1])) %>% 
        ### get error of log rate
        mutate(ABS_ERROR_LOG10_VOLUME = abs(log10(VOLUME) - log10(ARPS_VOLUME_PRED)))
      
      #this_well_production
      ### post this iteration arps result back to table of arps results for this well
      this_well_arps_grid[row_of_arps_grid_i, "IP_tries"] = this_well_production$IP_tries[1]
      this_well_arps_grid[row_of_arps_grid_i, "MEAN_ABS_ERROR_LOG10_VOLUME"] = mean(this_well_production$ABS_ERROR_LOG10_VOLUME)
      
    }
    
    
    return(this_well_arps_grid)
    
  }