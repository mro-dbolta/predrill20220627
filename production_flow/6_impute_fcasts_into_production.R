

impute_fcasts_into_production = function(
  production_data_locn = "../1_base_Sujan_data/EAGLEFORD_INTEGRATED_VOLUMES_PDAYS.csv",
  pdp_data_locn = "../1_base_Sujan_data/ES_PDP_VOLUMES_PDAYS.csv",
  pod_data_locn = "../1_base_Sujan_data/ES_POD_VOLUMES_PDAYS.csv",
  arps_oil_fcasts_locn = "../3_AWS_arps_declines/best_arps_per_well_results_oil.csv",
  dca_output_target_directory,
  run_if = TRUE){
  if (run_if) {
    
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("general_purpose_support_functions/arps_equations.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    source("general_purpose_support_functions/str_locate_start.R", local = TRUE)
    source("general_purpose_support_functions/install_load_libraries.R", local = TRUE)
    source("general_purpose_support_functions/convert_PROD_DATE_string_to_date.R", local = TRUE)
    source("anonymous_support_functions/_add_fcasts_to_prod.make_40_year_template_df.R", local = TRUE)
    source("anonymous_support_functions/_add_fcasts_to_prod.write_days_actual_history_to_disk.R", local = TRUE)
    source("anonymous_support_functions/_add_fcasts_to_prod.get_days_actual_history.R", local = TRUE)
    
    
    install_load_libraries("lubridate")
    
    ### load data
    production_data_in = load_data_that_has_api(target_path = production_data_locn)
    pdp_data_in = load_data_that_has_api(target_path = pdp_data_locn)
    pod_data_in = load_data_that_has_api(target_path = pod_data_locn)
    
    arps_oil_fcasts_in = load_data_that_has_api(target_path = arps_oil_fcasts_locn)
    
    ### raw production actuals
    oil_production_1 =
      production_data_in %>% 
      select(-ASSET_NAME, -CUM_VOLUME, -SOURCE) %>% 
      filter(PRODUCT == "L") %>% 
      select(-PRODUCT) %>% 
      convert_PROD_DATE_string_to_date(.)
    
    ### ES dca
    ### ES models typically start beginning of calendar year, even if ES run mid-year
    ### planning will overwrite fcast with actuals as available at ES run
    ### Sujan appends additional actuals (from procount) back to start date of well
    ### SOURCE column can be used to expose only what is pulled from ES
    oil_pdp_1 =
      pdp_data_in %>% 
      filter(ASSET_NAME == "EAGLEFORD",
             PRODUCT == "L") %>% 
      select(-PRODUCT, -ASSET_NAME, -CUM_VOLUME, -SOURCE) %>% 
      convert_PROD_DATE_string_to_date(.) %>% 
      filter(CUM_PRODUCING_DAYS <= 12 * 40 * 30)
    
    ### type curve foreasts
    ### includes last type cure entry for wells in pdp file 
    oil_pod_1 =
      pod_data_in %>% 
      filter(ASSET_NAME == "EAGLEFORD",
             PRODUCT == "L") %>% 
      select(-PRODUCT, -ASSET_NAME, -CUM_VOLUME, -SOURCE) %>% 
      convert_PROD_DATE_string_to_date(.) %>% 
      filter(CUM_PRODUCING_DAYS <= 12 * 40 * 30) %>% 
      ### remove wells from POD that are in pdp
      filter(!(API %in% unique(oil_pdp_1$API)))
    
    ### get days of history for actual prodction for each well
    days_actual_history = get_days_actual_history(fn_oil_production_1 = oil_production_1)
    
    write_days_actual_history_to_disk(days_actual_history,
                                      fn_dca_output_target_directory = dca_output_target_directory)
    
    
    COOP_oil_1 = 
      oil_pdp_1 %>% 
      bind_rows(oil_pod_1)           
    
    oil_production_2_non_COOP =
      oil_production_1 %>% 
      filter(!(API %in% unique(COOP_oil_1$API)))
    
    oil_production_3_non_COOP_40_years = 
      make_40_year_template_df(APIs = unique(oil_production_2_non_COOP$API))
    
    arps_oil_fcasts_1 =
      arps_oil_fcasts_in %>% 
      select(-MEAN_ABS_ERROR_LOG10_VOLUME) %>% 
      select(API, everything())
    
    oil_production_4_join_arps =
      oil_production_3_non_COOP_40_years %>% 
      left_join(arps_oil_fcasts_1,
                by = "API")
    
    oil_production_5_with_arps_fcasts =
      oil_production_4_join_arps %>% 
      mutate(ARPS_VOLUME = 
               map_dbl(1:nrow(.),
                       function(i){
                         arps_cum(days = oil_production_4_join_arps$CUM_PRODUCING_DAYS[i], 
                                  q_i = oil_production_4_join_arps$IP_tries[i], 
                                  di_nom_daily = oil_production_4_join_arps$Di_daily_nom_tries[i], 
                                  b_factor = oil_production_4_join_arps$b_tries[i]) -
                           arps_cum(days = oil_production_4_join_arps$CUM_PRODUCING_DAYS[i] - 30, 
                                    q_i = oil_production_4_join_arps$IP_tries[i], 
                                    di_nom_daily = oil_production_4_join_arps$Di_daily_nom_tries[i], 
                                    b_factor = oil_production_4_join_arps$b_tries[i])
                       }))
    
    oil_production_5b_cleanup =
      oil_production_5_with_arps_fcasts %>% 
      select(API, CUM_PRODUCING_DAYS, ARPS_VOLUME)
    
    oil_production_6_non_COOP_sources_integrated =
      oil_production_5b_cleanup %>% 
      left_join(oil_production_2_non_COOP %>% 
                  select(-PROD_DATE),
                by = c("API", "CUM_PRODUCING_DAYS")) %>% 
      mutate(VOLUME = ifelse(is.na(VOLUME), ARPS_VOLUME, VOLUME)) %>% 
      select(-ARPS_VOLUME) %>% 
      ### add reference dates
      left_join(days_actual_history %>% 
                  rename(reference_PROD_DATE = PROD_DATE,
                         reference_CUM_PRODUCING_DAYS = CUM_PRODUCING_DAYS),
                by = "API") %>% 
      mutate(DAYS_SHIFT = CUM_PRODUCING_DAYS - reference_CUM_PRODUCING_DAYS,
             PROD_DATE = reference_PROD_DATE + days(DAYS_SHIFT)) %>% 
      select(-starts_with("reference_"),
             -DAYS_SHIFT)
    
    oil_production_7_COOP_non_COOP_merged =
      COOP_oil_1 %>% 
      bind_rows(oil_production_6_non_COOP_sources_integrated) %>% 
      ### add days online
      left_join(days_actual_history %>% 
                  select(-PROD_DATE) %>% 
                  rename(DAYS_OF_ACTUAL_PRODUCTION = CUM_PRODUCING_DAYS),
                by = "API") %>% 
      mutate(DAYS_OF_ACTUAL_PRODUCTION = ifelse(is.na(DAYS_OF_ACTUAL_PRODUCTION),
                                                0,
                                                DAYS_OF_ACTUAL_PRODUCTION))
    
    oil_production_7_COOP_non_COOP_merged %>% 
      fwrite(
        paste0("../",
               add_path_slash_if_needed(dca_output_target_directory),
               "all_wells_oil_actuals_with_fcasts.csv")
      )
  
    print("impute_fcasts_into_production successful")  
  }
}










