
### takes flat table and fills out missing geological cells with a value lower than any preexisting entry

create_imputed_geology = function(flat_file_name, 
                                  specific_columns_config_locn,
                                  target_directory = "2_geo_impute",
                                  well_density_data_locns = c(),
                                  cleanup_PP_0_wellcount = FALSE,
                                  impute_PP = FALSE,
                                  impute_DP_TVD_SS = FALSE,
                                  run_if = FALSE){
  if (run_if) {
    source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
    source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
    source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
    #source("general_purpose_support_functions/create_folder_dos.R", local = TRUE)
    source("general_purpose_support_functions/read_xlsx.R", local = TRUE)
    
    source("anonymous_support_functions/_create_imputed_geology.geo_data_cleanup_1.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.impute_primary_geo_columns.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.impute_secondary_geo_columns.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.impute_geo_with_low_value.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.impute_PP_with_low_value.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.cleanup_PP_CMP_with_0_wellcount.R", local = TRUE)
    source("anonymous_support_functions/_create_imputed_geology.impute_DP_TVD_SS.R", local = TRUE)
    
    
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
    
    specific_columns_config_file = 
      read_xlsx(xlsx_file = specific_columns_config_locn)
    
    
    check_column_names_as_expected(
        data_frame_to_check = specific_columns_config_file,
        expected_column_names = c("type", "column_to_action"),
        data_frame_description = "specific_columns_config_file")
    
    geo_feats_remove_mask = which(specific_columns_config_file$type == "geo_feats_remove")
    if (length(geo_feats_remove_mask) < 1L) stop("No geo features to remove found in config file.")
    geo_feats_remove = specific_columns_config_file$column_to_action[geo_feats_remove_mask] 
    
    
    mgeo_header_columns_not_needed_mask = which(specific_columns_config_file$type == "mgeo_header_columns_not_needed")
    if (length(mgeo_header_columns_not_needed_mask) < 1L) stop("No mgeo prod zone features to remove found in config file.")
    mgeo_header_columns_not_needed = specific_columns_config_file$column_to_action[mgeo_header_columns_not_needed_mask]

    
    #create_folder_dos(paste0("..\\", target_directory))
    dir.create(paste0("../", target_directory), showWarnings = FALSE)
    
    data_1 = load_data_that_has_api(paste0("../1_base_Sujan_data/", flat_file_name))
    
    ### reverse SUjan's map-imputation for PP_CMP columns
    if (cleanup_PP_0_wellcount) data_1 = cleanup_PP_CMP_with_0_wellcount(data_1)
    
    ### impute prod potential variables
    if (impute_PP) {
      data_1 = impute_PP_with_low_value(data_1)
    }
    
    ### impute prod potential variables
    if (impute_DP_TVD_SS) {
      data_1 = impute_DP_TVD_SS_from_DP_MAX_TVD(data_1)
    }
    
    
    ### initial cleanup and only keep EF wells
    data_1b_EF_TX = geo_data_cleanup_1(data_1, geo_feats_remove, mgeo_header_columns_not_needed)
    
    data_2_geo = data_1b_EF_TX %>% 
        select(API,
               starts_with("MGEO_"),
               starts_with("DIGEO_"))
      
      geo_column_names = data_2_geo %>% 
        select(starts_with("MGEO_"),
               starts_with("DIGEO_")) %>% 
        names()  
    
    
    data_2b_geo_filled = impute_geo_with_low_value(fn_geo_df = data_2_geo)
    
    EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed =
      data_1 %>% 
      select(-one_of(geo_column_names)) %>% 
      left_join(data_2b_geo_filled,
                by = "API")
    
    ### handle well density data
    if (length(well_density_data_locns) > 0) {
      pre_WPS_nrows = nrow(EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed)
      for (well_density_data_locn in well_density_data_locns) {
        wps_data = load_data_that_has_api(well_density_data_locn)
        EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed =
          EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed %>% 
          left_join(wps_data, by = "API")
        
        post_WPS_nrows = nrow(EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed)
        if (post_WPS_nrows != pre_WPS_nrows) {
          stop(paste0("Joining ", well_density_data_locn, " has created more rows.")) 
        }
      }
    }
    
    ### write updated data to disk
    EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed %>% 
      fwrite(
        paste0(
          "../",
          add_path_slash_if_needed(target_directory),
          "EAGLEFORD_IHS_Prod_FlatTable_2_geo_imputed.csv"
        )
      )
    
  }
  
}






