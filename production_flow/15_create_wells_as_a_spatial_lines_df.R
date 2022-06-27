

create_wells_as_a_spatial_lines_df =
  function(
    data_for_shape_file_locn,
    output_locn,
    run_if
  ){
    if (run_if) {
      
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("tests/check_column_names_as_expected.R", local = TRUE)
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      
      libraries_for_spatial = c("sp", "rgeos", "data.table", "rgdal",
                                "ggplot2",
                                "dplyr",
                                "tidyr",
                                "readr",
                                "purrr",
                                "tibble",
                                "stringr",
                                "forcats")
      install_load_libraries_strings(libraries_for_spatial)
      
      ### prep data, could add production or prediction (eg- LIQUID_720) if desired
      data_1_for_shape_file = 
        load_data_that_has_api(data_for_shape_file_locn) %>% 
        select(API, WELL_NAME, OTH_OPERATOR_NAME, GEO_PROD_ZONE_NAME, OTH_WELL_TYPE,
               DT_FIRST_PRODUCTION_YEAR, DT_FIRST_PRODUCTION,
               LOC_BH_X, LOC_HEEL_X, LOC_BH_Y, LOC_HEEL_Y) %>% 
        mutate(DT_FIRST_PRODUCTION = as.character(DT_FIRST_PRODUCTION)) %>% 
        na.omit() %>% 
        distinct() %>% 
        ### shorten column names
        rename(NAME = WELL_NAME,
               OPER = OTH_OPERATOR_NAME,
               FORM = GEO_PROD_ZONE_NAME, 
               TYPE = OTH_WELL_TYPE,
               YEAR = DT_FIRST_PRODUCTION_YEAR, 
               DATE = DT_FIRST_PRODUCTION)
      
      
      spatial_lines_list = map(1:nrow(data_1_for_shape_file),
                               function(i){
                                 l1 = cbind(c(data_1_for_shape_file$LOC_HEEL_X[i], data_1_for_shape_file$LOC_BH_X[i]),
                                            c(data_1_for_shape_file$LOC_HEEL_Y[i], data_1_for_shape_file$LOC_BH_Y[i]))
                                 
                                 Sl1 <- Line(l1)
                                 
                                 Lines(list(Sl1), ID = data_1_for_shape_file$API[i])
                                 
                               }) %>%
        SpatialLines()
      
      
      data_2_for_shape_file = data_1_for_shape_file %>% 
        select(-API, -starts_with("LOC_"))
      
      rownames(data_2_for_shape_file) = data_1_for_shape_file$API
      
      
      well_sticks_spatial = SpatialLinesDataFrame(spatial_lines_list, 
                                   data = data_2_for_shape_file)
      
      
      ### write out results
      output_locn = add_path_slash_if_needed(output_locn)
        
      rgdal::writeOGR(well_sticks_spatial, 
                      dsn = paste0(output_locn, "well_sticks_spatial.shp"),
                      "well_sticks_spatial",
                      driver = 'ESRI Shapefile', 
                      overwrite = TRUE) 
      
      print("Run successful: create_wells_as_a_spatial_lines_df")
    }
  }

