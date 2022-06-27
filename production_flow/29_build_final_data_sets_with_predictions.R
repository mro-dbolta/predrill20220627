
build_final_data_sets_with_predictions =
  function(
    base_predictions_folder, 
    quantile_predictions_locn = NULL,
    TC_family_bins_locn = "config_files/TC family bins.csv",
    flat_file_locn = "../1_base_Sujan_data/EAGLEFORD_IHS_Prod_FlatTable.csv",
    pod_data_locn = "../1_base_Sujan_data/ES_POD_VOLUMES_PDAYS.csv",
    bandaid_in_procount_prod = FALSE,
    BASE_PREDICTIONS_FILE = "data_with_predictions_1.csv",
    procount_monthly_data_locn = "../1_base_Sujan_data/PROCOUNT_MONTHLY_VOLUMES_PDAYS.csv",
    bandaid_infinite_fluid_columns = TRUE,
    run_if = FALSE
  ){
    if (run_if) {
      "
      Take the inference data files (input columns with base predictions)
      combine with quantile predictions (if available)
      combine with TC_family_bins
      
      Post combined predictions, and combined predictions fewer columns
      back to directory where original predictions reside.
      "
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/str_locate_start_of_last_instance.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.get_intro_row_to_establish_column_types.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.prune_columns.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.pivot_liquid_predictions_tall.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.get_pod_liquid_data.R", local = TRUE)
      source("anonymous_support_functions/_predict_on_inference_data.bandaid_in_proct_prod_data.R", local = TRUE)
      
      install_load_libraries_strings(c("ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats",
                                       "data.table",
                                       "rlang"))
      
      base_predictions_folder = add_path_slash_if_needed(base_predictions_folder)
      
      data_with_predictions =
        load_data_that_has_api(paste0(base_predictions_folder,
                                      BASE_PREDICTIONS_FILE))
      
      ### bandaid in actual production updates
      "
      Currently, sujan only enters real prod when it appears in IHS
      this leaves some wells that have come online in procount 
      still look like POD wells in his 'integrated' production table
      "
      if (bandaid_in_procount_prod) {
        data_with_predictions_tmp =
          bandaid_in_proct_prod_data(data_with_predictions,
                                     procount_monthly_data_locn)
        ### Error check
        if (nrow(data_with_predictions) != nrow(data_with_predictions_tmp)) {
          stop("bandaid_in_proct_prod_data changed row count of data_with_predictions")
        } else {
          data_with_predictions = data_with_predictions_tmp
          rm(data_with_predictions_tmp)
        }
      }
      #return(data_with_predictions)
      
      ### join in quantilepredictions
      if (!is.null(quantile_predictions_locn)) {
        quantile_predictions_2_final =
          load_data_that_has_api(quantile_predictions_locn)
        
        data_with_predictions =
          data_with_predictions %>% 
          left_join(quantile_predictions_2_final,
                    by = c("API", "SENSITIVITY_CASE"))
        
        if (nrow(data_with_predictions) != nrow(quantile_predictions_2_final)) stop("Left_joining quantile predictions changed row count")
      }
      
      ### add flat file columns
      original_flat_file =
        load_data_that_has_api(flat_file_locn)
      columns_from_flat_to_add =
        setdiff(names(original_flat_file), names(data_with_predictions))
      columns_from_flat_to_add = c("API", columns_from_flat_to_add)
      
      flat_file_data_only_columns_to_add =
        original_flat_file %>% 
        select(one_of(columns_from_flat_to_add))
      
      nrow_pre_join = nrow(data_with_predictions)
      
      data_with_predictions =
        data_with_predictions %>% 
        left_join(flat_file_data_only_columns_to_add, by = "API")
      
      ### Error check
      if (nrow(data_with_predictions) != nrow_pre_join) stop("Joining in flat file changed row count of prediction df.")
      rm(nrow_pre_join)
      
      ### fix fluid columns showing as Inf
      if (bandaid_infinite_fluid_columns) {
        data_with_predictions =
          data_with_predictions %>% 
          mutate(CP_TOTAL_FLUID_2 = ifelse(is.infinite(CP_TOTAL_FLUID), 0, CP_TOTAL_FLUID),
                 CP_TOTALFLUIDPERFOOT_2 = ifelse(is.infinite(CP_TOTALFLUIDPERFOOT), 0, CP_TOTALFLUIDPERFOOT)) %>% 
          mutate(CP_TOTAL_FLUID = ifelse(is.infinite(CP_TOTAL_FLUID), max(CP_TOTAL_FLUID_2) + 10L, CP_TOTAL_FLUID),
                 CP_TOTALFLUIDPERFOOT = ifelse(is.infinite(CP_TOTALFLUIDPERFOOT), max(CP_TOTALFLUIDPERFOOT_2) + 10L, CP_TOTALFLUIDPERFOOT)) %>%
          select(-CP_TOTAL_FLUID_2, -CP_TOTALFLUIDPERFOOT_2)
      }
      
      ### join in TC_family_bins and Phase
      TC_family_bins = fread(TC_family_bins_locn) %>% 
        data.frame(stringsAsFactors = FALSE)
      
      data_with_predictions = 
        data_with_predictions %>% 
        left_join(TC_family_bins, by = "ES_TYPE_CURVE_FAMILY")
      
      ### pivot production and predictions to tall format
      tall_predictions_list =
        map(c("", "PREDICTIONS_", "P10_PREDICTIONS_", "P50_PREDICTIONS_", "P90_PREDICTIONS_"),
            function(starter_string) { 
              data_with_predictions %>% 
                pivot_liquid_predictions_tall(starter_string = starter_string) %>% 
                as.data.frame()
            })
      
      tall_predictions =
        tall_predictions_list[[1]]
      
      for (i in 2:length(tall_predictions_list)) {
        tall_predictions =
          tall_predictions %>% 
          left_join(tall_predictions_list[[i]],
                    by = c("API", "SENSITIVITY_CASE", "DAYS"))
      }
      
      ### Error check
      if (nrow(tall_predictions) != nrow(tall_predictions_list[[1]])) stop("Joining tall tables changed row counts.")
      rm(tall_predictions_list)
      
      ### add in header data
      tall_predictions =
        tall_predictions %>% 
        left_join(data_with_predictions %>% 
                    select(-starts_with("LIQUID_"),
                           -contains("PREDICTIONS_")),
                  by = c("API", "SENSITIVITY_CASE")
        ) %>% 
        mutate(ACTUAL_OR_DECLINE = if_else(DAYS <= DAYS_ACTUAL_HISTORY, "Actual", "Forecast"),
               LIQUID_FORECAST = if_else(DAYS <= DAYS_ACTUAL_HISTORY , na_dbl, LIQUID),
               ### wipe the fcast for future wells
               LIQUID_FORECAST = if_else(DAYS_ACTUAL_HISTORY < 1, na_dbl, LIQUID_FORECAST),
               LIQUID = if_else(DAYS <= DAYS_ACTUAL_HISTORY, LIQUID, na_dbl))
      
      ### add in type curve ES data
      oil_pod_1 = get_pod_liquid_data(pod_data_locn)
      
      tall_predictions = 
        tall_predictions %>% 
        left_join(oil_pod_1,
                  by = c("API", "DAYS"))
      
      ### fix column order
      tall_predictions = 
        tall_predictions %>% 
        select(API, SENSITIVITY_CASE, DAYS, LIQUID,
               LIQUID_FORECAST,
               LIQUID_ES_POD,
               PREDICTIONS_LIQUID,
               contains("PREDICTIONS_"),
               ACTUAL_OR_DECLINE,
               ES_TYPE_CURVE_FAMILY_BIN,
               PHASE,
               everything())
      
      
      ### prep and write wide table
      intro_row_to_establish_column_types =
        get_intro_row_to_establish_column_types(df = data_with_predictions)
      
      data_with_predictions_2_with_intro_row =
        intro_row_to_establish_column_types %>% 
        bind_rows(data_with_predictions)
      
      
      data_with_predictions_2b_fewer_columns =
        suppressWarnings(
          prune_columns(data_with_predictions_2_with_intro_row)
        )
      
      data_with_predictions_2_with_intro_row %>% 
        fwrite(paste0(base_predictions_folder,
                      "data_with_predictions_2_with_intro_row.csv"))
      
      data_with_predictions_2b_fewer_columns %>% 
        fwrite(paste0(base_predictions_folder,
                      "data_with_predictions_2b_fewer_columns.csv"))
      
      ### prep and write tall table
      intro_row_to_establish_column_types =
        get_intro_row_to_establish_column_types(df = tall_predictions)
      
      data_with_predictions_3_tall_with_intro_row =
        intro_row_to_establish_column_types %>% 
        bind_rows(tall_predictions)
      
      
      data_with_predictions_3b_fewer_columns =
        suppressWarnings(
          prune_columns(data_with_predictions_3_tall_with_intro_row)
        )
      
      
      data_with_predictions_3_tall_with_intro_row %>% 
        fwrite(paste0(base_predictions_folder,
                      "data_with_predictions_3_tall_with_intro_row.csv"))
      
      data_with_predictions_3b_fewer_columns %>% 
        fwrite(paste0(base_predictions_folder,
                      "data_with_predictions_3b_fewer_columns.csv"))
      
      return(paste0("Predictions 2,2b,3,3b successful for ",
                    base_predictions_folder))  
    }
  }