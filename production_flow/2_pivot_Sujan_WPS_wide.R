
### take sujan's tall WPS table and pivot wide
### 202102-01 public data automation1\5_models_1\v6kc_all_mg_features_no_gas_prediction.Rmd

pivot_Sujan_WPS_wide =
  function(
    tall_WPS_filename,
    tall_WPS_file_locn,
    well_density_distance,
    run_if = TRUE
  ){
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "data.table"))
      
      ### defensive path corrections
      tall_WPS_file_locn = add_path_slash_if_needed(tall_WPS_file_locn)
      if (!str_ends(tall_WPS_filename, ".csv")) tall_WPS_filename = paste0(tall_WPS_filename, ".csv")
      
      tall_WPS = fread(
        paste0(tall_WPS_file_locn, tall_WPS_filename)
      ) %>% 
        data.frame(stringsAsFactors = FALSE)
      
      check_column_names_as_expected(
        data_frame_to_check = tall_WPS,
        expected_column_names = c(
          "TS_INDEX", "FORMATION_TYPE", "WS_WPS_LABEL", "CP_LAT_LEN_GROSS_PERF_INTVL",
          "WS_LATERAL_LENGTH", "WS_WPS_INTERSECT_LENGTH", "WS_BOX_AREA", "WS_WPS", "WS_WPS_XY"
        ),
        data_frame_description = "Initial tall WPS table"
      )
      
      tall_WPS_working =
        tall_WPS %>% 
        rename(ID_aAPI = WS_WPS_LABEL) %>% 
        select(ID_aAPI, FORMATION_TYPE, TS_INDEX, 
               WS_WPS) %>%
        mutate(TS_INDEX = as.character(TS_INDEX),
               ### fix negative sign for when they become column names on pivot_wide 
               TS_INDEX = if_else(str_starts(TS_INDEX, pattern = "-"),
                                  paste0("NEG",
                                         str_sub(TS_INDEX, start = 2)),
                                  TS_INDEX)) %>% 
        pivot_wider(names_from = TS_INDEX,
                    values_from = WS_WPS,
                    names_prefix = paste0("WS_", well_density_distance,"_WD_")) %>% 
        pivot_wider(names_from = FORMATION_TYPE,
                    values_from = starts_with(paste0("WS_", well_density_distance,"_WD_"))) %>% 
        ### convert ID_aAPI to API
        mutate(API = str_sub(ID_aAPI, start = 2L)) %>% 
        select(-ID_aAPI) %>% 
        select(API, everything())
      
      tall_WPS_working %>% 
        fwrite(
          paste0(tall_WPS_file_locn,
                 "EAGLEFORD_WPS_TIMESERIES_PIVOT_WIDE_", 
                 well_density_distance,
                 ".csv"
          ))
    }
  }



