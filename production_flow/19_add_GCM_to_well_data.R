
add_GCM_to_well_data =
  function(
    GCM_locn_and_filename,
    GCM_source_column_names, #"LIQUID_720" or c() of column names
    well_data_locn_and_filename,
    append_output_filename = "",
    prepend_column_names = "GCM_", # string to append to front of GCM columns in new data
    linear_or_kknn = "linear", #"kknn"
    run_if = FALSE
  ){
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/fread_data_frame.R", local = TRUE)
      source("general_purpose_support_functions/str_clip_csv_from_end.R", local = TRUE)
      source("general_purpose_support_functions/linear_interpolate_data.R", local = TRUE)
      source("general_purpose_support_functions/kknn_interpolate_data.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      
      well_data_locn_and_filename_no_csv = str_clip_csv_from_end(well_data_locn_and_filename)
      well_data_locn_and_filename = paste0(well_data_locn_and_filename_no_csv, ".csv")
      
      GCM_locn_and_filename = 
        paste0(str_clip_csv_from_end(GCM_locn_and_filename), 
               ".csv")
      
      well_data_with_GCM =
        well_data =
        load_data_that_has_api(well_data_locn_and_filename)
      
      GCM_data = 
        fread_data_frame(GCM_locn_and_filename)
      
      if (linear_or_kknn == "linear") {
        for (GCM_source_column_name in GCM_source_column_names) {
          well_data_with_GCM[, paste0(prepend_column_names, 
                                      GCM_source_column_name)] =
            linear_interpolate_data(
              source_data = GCM_data,
              grid_data_to_interp_over = well_data,
              x_column_name = "LOC_MP_X",
              y_column_name = "LOC_MP_Y",
              map_feature_name = GCM_source_column_name,
              EXTRAP = TRUE,
              run_if = TRUE
            )
        }
      } else if (linear_or_kknn == "kknn") {
        for (GCM_source_column_name in GCM_source_column_names) {
          well_data_with_GCM[, paste0(prepend_column_names, 
                                      GCM_source_column_name)] =
            kknn_interpolate_data(
              train_data = GCM_data,
              test_data = well_data,
              model_formula = paste0(GCM_source_column_name,
                                     "~LOC_MP_X+LOC_MP_Y"),
              k_neighbors = 4,
              apply_scaling = FALSE
            )
        }
      } else {
        stop("Invalid selection for linear_or_kknn")
      }
      
      well_data_with_GCM %>% 
        fwrite(paste0(
          well_data_locn_and_filename_no_csv,
          append_output_filename,
          ".csv"
        ))
      
      # return(well_data_with_GCM)
      
    }
  }