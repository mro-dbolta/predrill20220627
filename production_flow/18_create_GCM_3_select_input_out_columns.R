

create_GCM =
  function(
    input_data_locn,
    output_folder_locn,
    standard_ppf = 2500,
    standard_bpf = 34,
    DISTANCE_BETWEEN_GRID_POINTS = 2000,
    output_file_descriptor = "",
    include_depletion = FALSE,
    input_variables = NULL,
    output_variables = NULL,
    run_if = FALSE
  ){
    '
    Create a GCM map at several output time steps
    Depletion @ parent
    Spacing @ parent
    Completion at current standard
    '
    
    MTRY = 7
    MIN_NODE_SIZE = 20
    NTREE = 1000
    SAMPLE_FRACTION = 0.5
    
    NUM_PREDICTION_CHUNKS = 100
    
    base_input_variables =
      c(#"LOC_MP_X", "LOC_MP_Y", 
        "MGEO_BUDA_TVDSS", "MGEO_LEF_ISO", 
        "MGEO_EGFDL_TVDSS", "MGEO_LEF_TVD_SEISMIC", "MGEO_AVG_SW_EGFDL_74RANCH", 
        "MGEO_ISO_ASTNL_74RANCH", "MGEO_ISO_EGFDL_74RANCH", "MGEO_EGFDL_SOPHIH_74RANCH", 
        "MGEO_ASTNU_TVDSS_74RANCH", "MGEO_EGFDU_TVDSS_74RANCH", "MGEO_ASTNL_TVDSS_74RANCH", 
        "MGEO_ISO_ASTNU_74RANCH", "MGEO_EGFDL_TVDSS_74RANCH", "MGEO_AVG_PHI_EGFDL_74RANCH", 
        "MGEO_BUDA_TVDSS_74RANCH", "MGEO_TEMP_EGFD_BEG", "MGEO_SATURATION_PRESSURE", 
        "MGEO_EF_GOR", "MGEO_RES_PRESSURE", "MGEO_EF_SWT", "MGEO_EF_STOOIP", 
        "MGEO_EF_TOC", "MGEO_HEADROOM_PRESSURE", "MGEO_EF_PHIT", "MGEO_EF_BO", 
        "MGEO_EF_BG", "MGEO_BUDA_TVD", "MGEO_EF_OGIP", "MGEO_EF_VCLAY", 
        "MGEO_EF_ISO", "MGEO_EF_SOPHIH", "MGEO_REX_EF_TOTAL_ISO", "MGEO_REX_EF_RES_PRESSURE", 
        "MGEO_REX_EF_TOTAL_OOIP_OCIP_1280AC", "MGEO_REX_EF_GOR", "MGEO_REX_EF_UPP_ISO", 
        "MGEO_REX_EF_LWR_SOPHIH", "MGEO_REX_EF_UPP_SOPHIH", "MGEO_REX_EF_API_GRAVITY", 
        "MGEO_REX_EF_LWR_ISO", "MGEO_REX_EF_HEADROOM_PRESSURE", "MGEO_REX_EF_LWR_OOIP_OCIP_1280AC", 
        "MGEO_REX_EF_UPP_OOIP_OCIP_1280AC", "MGEO_LEF_MATURITY", "MGEO_LEF_GRADIENT", 
        "MGEO_LEF_GOR", "MGEO_LEF_API", "MGEO_LEF_PRESSURE_GRADIENT", 
        "MGEO_LEF_EFFECTIVE_STRESS", "MGEO_LEF_POROSITY", "MGEO_LEF_ERODED_THICKNESS", 
        "MGEO_LEF_GAS_RETAINED", "MGEO_LEF_OIL_RETAINED", "MGEO_LEF_TOTAL_RETAINED", 
        "MGEO_LECO_TOC_WT_PERCENT_MAX", "MGEO_LECO_TOC_WT_PERCENT_MEDIAN", 
        "MGEO_TMAX_DEGC", "MGEO_HI", "MGEO_OI", "MGEO_RO_MEASURED_PERCENT", 
        "DIGEO_DIMENSIONLESS_AUSTIN_CHALK_LOG10_RESD_AVG", "DIGEO_DIMENSIONLESS_BUDA_LOG10_RESD_AVG", 
        "DIGEO_DIMENSIONLESS_EAGLE_FORD_LOG10_RESD_AVG", "DIGEO_DIMENSIONLESS_EAGLE_FORD_LOWER_LOG10_RESD_AVG", 
        "DIGEO_DIMENSIONLESS_EAGLE_FORD_UPPER_LOG10_RESD_AVG", "DIGEO_GAMMARAY_AUSTIN_CHALK_GR_AVG", 
        "DIGEO_GAMMARAY_BUDA_GR_AVG", "DIGEO_GAMMARAY_EAGLE_FORD_GR_AVG", 
        "DIGEO_GAMMARAY_EAGLE_FORD_LOWER_GR_AVG", "DIGEO_GAMMARAY_EAGLE_FORD_UPPER_GR_AVG", 
        "DIGEO_NEUTRONPOROSITY_AUSTIN_CHALK_NPHI_AVG", "DIGEO_NEUTRONPOROSITY_BUDA_NPHI_AVG", 
        "DIGEO_NEUTRONPOROSITY_EAGLE_FORD_LOWER_NPHI_AVG", "DIGEO_NEUTRONPOROSITY_EAGLE_FORD_NPHI_AVG", 
        "DIGEO_NEUTRONPOROSITY_EAGLE_FORD_UPPER_NPHI_AVG", "DIGEO_TEMPERATURE_AUSTIN_CHALK_TEMP", 
        "DIGEO_TEMPERATURE_EAGLE_FORD_LOWER_TEMP", "DIGEO_TEMPERATURE_EAGLE_FORD_UPPER_TEMP", 
        "DIGEO_THICKNESS_AUSTIN_CHALK_ISOPACH", "DIGEO_THICKNESS_EAGLE_FORD_ISO", 
        "DIGEO_THICKNESS_EAGLE_FORD_LOWER_PHIH_AVG", "DIGEO_THICKNESS_EAGLE_FORD_UPPER_ISO", 
        "DIGEO_THICKNESS_THICKNESS_BUDA_ISO", "DIGEO_THICKNESS_THICKNESS_EAGLEFORD_LOWER_ISO",
        "DIGEO_TOC_EAGLE_FORD_LOWER_CALCTOC_SCHMOKER_AVG", "DIGEO_TVDSS_AUSTIN_CHALK", 
        "DIGEO_TVDSS_BUDA", "DIGEO_TVDSS_EAGLE_FORD", "DIGEO_TVDSS_EAGLE_FORD_LOWER", 
        "DIGEO_TVDSS_EAGLE_FORD_UPPER", "DIGEO_VSHALE_EAGLE_FORD_LOWER_VSHALE_AVG", 
        "DIGEO_VSHALE_EAGLE_FORD_UPPER_VSHALE_AVG", "WS_2MILES_WD_NEG360_SF", 
        "WS_2MILES_WD_NEG360_AF", "WS_1MILE_WD_NEG360_SF", "WS_1MILE_WD_NEG360_AF", 
        "WS_2MILES_WD_360_SF", "WS_2MILES_WD_360_AF", "WS_2MILES_WD_0_SF", 
        "WS_2MILES_WD_0_AF", "WS_2MILES_WD_180_SF", "WS_2MILES_WD_180_AF",
        "WS_1MILE_WD_0_SF", "WS_1MILE_WD_0_AF", "WS_1MILE_WD_180_SF", 
        "WS_1MILE_WD_180_AF", "WS_1MILE_WD_360_SF", "WS_1MILE_WD_360_AF", 
        "WS_AVG_180POST_AF", "WS_MIN_180POST_AF", "WS_QQ_AVG_180POST_AF", 
        "WS_QQ_MIN_180POST_AF", "WS_AVG_180POST_SF", "WS_MIN_180POST_SF", 
        "WS_QQ_AVG_180POST_SF", "WS_QQ_MIN_180POST_SF", "WS_AVG_31PRE_AF", 
        "WS_MIN_31PRE_AF", "WS_QQ_AVG_31PRE_AF", "WS_QQ_MIN_31PRE_AF", 
        "WS_AVG_31PRE_SF", "WS_MIN_31PRE_SF", "WS_QQ_AVG_31PRE_SF", 
        "WS_QQ_MIN_31PRE_SF", "CP_LAT_LEN_GROSS_PERF_INTVL", 
        "CP_TOTALPROPPERFOOT", "CP_TOTALFLUIDPERFOOT", "CP_TOTAL_PROPPANT", 
        "CP_TOTAL_FLUID", "CP_TOT_PROPPANT_BY_TOT_FLUID")
    
    if (include_depletion) {
      dpln_variables =
        c("DPL_AVG_DIST_1M_AF", "DPL_MAX_DIST_DIFF_1M_AF", 
          "DPL_MAX_VOL_DIST_RATIO_1M_AF", "DPL_AVG_VOL_DIST_RATIO_1M_AF", 
          "DPL_SUM_VOL_DIST_RATIO_1M_AF", "DPL_QQ_AVG_DIST_1M_AF", "DPL_QQ_MAX_DIST_DIFF_1M_AF", 
          "DPL_QQ_MAX_VOL_DIST_RATIO_1M_AF", "DPL_QQ_AVG_VOL_DIST_RATIO_1M_AF", 
          "DPL_QQ_SUM_VOL_DIST_RATIO_1M_AF", "DPL_AVG_DIST_1M_SF", "DPL_MAX_DIST_DIFF_1M_SF", 
          "DPL_MAX_VOL_DIST_RATIO_1M_SF", "DPL_AVG_VOL_DIST_RATIO_1M_SF", 
          "DPL_SUM_VOL_DIST_RATIO_1M_SF", "DPL_QQ_AVG_DIST_1M_SF", "DPL_QQ_MAX_DIST_DIFF_1M_SF", 
          "DPL_QQ_MAX_VOL_DIST_RATIO_1M_SF", "DPL_QQ_AVG_VOL_DIST_RATIO_1M_SF", 
          "DPL_QQ_SUM_VOL_DIST_RATIO_1M_SF", "DPL_WELL_COUNT_1M_AF", 
          "DPL_AVG_DAYSON_1M_AF", "DPL_TOT_LIQUID_1M_AF", "DPL_WEIGHTED_HM_LIQUID_1M_AF", 
          "DPL_QQ_WELL_COUNT_1M_AF", "DPL_QQ_AVG_DAYSON_1M_AF", "DPL_QQ_TOT_LIQUID_1M_AF", 
          "DPL_QQ_WEIGHTED_HM_LIQUID_1M_AF", "DPL_WELL_COUNT_1M_SF", 
          "DPL_AVG_DAYSON_1M_SF", "DPL_TOT_LIQUID_1M_SF", "DPL_WEIGHTED_HM_LIQUID_1M_SF", 
          "DPL_QQ_WELL_COUNT_1M_SF", "DPL_QQ_AVG_DAYSON_1M_SF", "DPL_QQ_TOT_LIQUID_1M_SF", 
          "DPL_QQ_WEIGHTED_HM_LIQUID_1M_SF")
      base_input_variables =
        base_input_variables %>% 
        append(dpln_variables)
    }
    
    base_output_variables =
      c("LIQUID_90", "LIQUID_180", "LIQUID_360", "LIQUID_540", "LIQUID_720")
    
    if (is.null(input_variables)) input_variables = base_input_variables
    if (is.null(output_variables)) output_variables = base_output_variables
    
    if (run_if) {
      source("general_purpose_support_functions/install_load_libraries_strings.R", local = TRUE)
      source("general_purpose_support_functions/load_data_that_has_api.R", local = TRUE)
      source("anonymous_support_functions/_create_GCM_1.create_grid_in_convex_hull_from_points.R", local = TRUE)
      source("general_purpose_support_functions/linear_interpolate_data.R", local = TRUE)
      source("general_purpose_support_functions/add_path_slash_if_needed.R", local = TRUE)
      source("general_purpose_support_functions/get_split_vector.R", local = TRUE)
      
      install_load_libraries_strings(c("data.table", "randomForestSRC",
                                       "ggplot2",
                                       "dplyr",
                                       "tidyr",
                                       "readr",
                                       "purrr",
                                       "tibble",
                                       "stringr",
                                       "forcats"))
      
      ### create new folder for train & test data
      dir.create(output_folder_locn, showWarnings = F)
      
      ### read in
      input_data = load_data_that_has_api(input_data_locn)
      
      data_2_drop_columns =
        input_data %>% 
        filter(DAYS_ACTUAL_HISTORY > 720) %>% 
        select(-API, -OTH_OPERATOR_NAME, -starts_with("DT_"),
               -LOC_COUNTY_NAME, -starts_with("DS_"), -starts_with("ES_"),
               -starts_with("PP_"), -starts_with("BT_COUNT"),
               -LIQUID_30, -LIQUID_60, -contains("PERSQFOOT_"), -WS_PARENT_CHILD_IND)
      
      # return(data_2_drop_columns)
      
      well_locations_for_convex_hull_grid =
        data_2_drop_columns %>% 
        select(LOC_MP_X, LOC_MP_Y) %>% 
        rename(x = LOC_MP_X, 
               y = LOC_MP_Y)
      
      convex_hull_grid =
        create_grid_in_convex_hull_from_points(
          input_points = well_locations_for_convex_hull_grid,
          distance_between_grid_points = DISTANCE_BETWEEN_GRID_POINTS,
          epsilon = 5000,
          run_if = TRUE
        )
      
      data_for_predictions =
        convex_hull_grid %>% 
        rename(LOC_MP_X = x, 
               LOC_MP_Y = y)
      
      ### create mappable columns
      geo_columns = 
        data_2_drop_columns %>% 
        select(starts_with("MGEO_"),
               starts_with("DIGEO_")) %>% 
        names()
      
      pb = txtProgressBar(min = 0, max = length(geo_columns), style = 3)
      for (geo_column in geo_columns) {
        data_for_predictions[,geo_column] =
          linear_interpolate_data(
            source_data = data_2_drop_columns,
            grid_data_to_interp_over = data_for_predictions,
            x_column_name = "LOC_MP_X",
            y_column_name = "LOC_MP_Y",
            map_feature_name = geo_column,
            EXTRAP = FALSE,
            run_if = TRUE
          )
        setTxtProgressBar(pb, which(geo_columns == geo_column))
      }
      close(pb)
      
      data_for_predictions =
        data_for_predictions %>% 
        na.omit()
      
      ### create WPS columns
      ### negative time steps to 0
      negative_WPS_columns =
        data_2_drop_columns %>% 
        select(starts_with("WS_2MILES_"),
               starts_with("WS_1MILE_")) %>% 
        select(contains("_NEG")) %>% 
        names()
      
      for (WPS_column in negative_WPS_columns) {
        data_for_predictions[, WPS_column] = 0
      }
      
      ### positive time steps to 1
      positive_WPS_columns =
        data_2_drop_columns %>% 
        select(starts_with("WS_2MILES_"),
               starts_with("WS_1MILE_")) %>% 
        select(-contains("_NEG")) %>% 
        names()
      
      for (WPS_column in positive_WPS_columns) {
        data_for_predictions[, WPS_column] = 1
      }
      
      ### populate remaining well spacing columns
      remaining_well_spacing_columns =
        data_2_drop_columns %>% 
        select(starts_with("WS_")) %>% 
        select(-one_of(negative_WPS_columns)) %>% 
        select(-one_of(positive_WPS_columns)) %>% 
        names()
      
      for (WS_column in remaining_well_spacing_columns) {
        data_for_predictions[, WS_column] = 
          max(data_2_drop_columns[, WS_column],
              na.rm = TRUE)
      }
      
      ### populate depletion variables
      if (include_depletion) {
        ### variables to max
        DPL_columns_to_max = 
          data_2_drop_columns %>% 
          select(starts_with("DPL_")) %>% 
          select(contains("_DIST_")) %>% 
          names()
        
        for (DPL_column in DPL_columns_to_max) {
          data_for_predictions[, DPL_column] = 
            max(data_2_drop_columns[, DPL_column],
                na.rm = TRUE)
        }
        
        ### variables to min
        DPL_columns_to_min = 
          data_2_drop_columns %>% 
          select(starts_with("DPL_")) %>% 
          select(-contains("_DIST_")) %>% 
          names()
        
        for (DPL_column in DPL_columns_to_min) {
          data_for_predictions[, DPL_column] = 0
        }
      }
      
      ### populate standard modern completion
      completions_columns =
        c("CP_LAT_LEN_GROSS_PERF_INTVL",
          "CP_TOTAL_PROPPANT",                   
          "CP_TOTAL_FLUID",                                
          "CP_TOT_PROPPANT_BY_TOT_FLUID",                       
          "CP_TOTALPROPPERFOOT",
          "CP_TOTALFLUIDPERFOOT")
      
      check_for_additional_cmp_columns =
        data_2_drop_columns %>% 
        select(starts_with("CP_")) %>% 
        select(-completions_columns) %>% 
        names()
      if (length(check_for_additional_cmp_columns) > 0) {
        stop(paste0("Additional completions columns than expected: ",
                    check_for_additional_cmp_columns))
      }
      
      data_for_predictions =
        data_for_predictions %>% 
        mutate(CP_LAT_LEN_GROSS_PERF_INTVL = 8000,
               CP_TOTALPROPPERFOOT = standard_ppf,
               CP_TOTALFLUIDPERFOOT = standard_bpf * 42,
               CP_TOTAL_PROPPANT = CP_TOTALPROPPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL,
               CP_TOTAL_FLUID = CP_TOTALFLUIDPERFOOT * CP_LAT_LEN_GROSS_PERF_INTVL,
               CP_TOT_PROPPANT_BY_TOT_FLUID = CP_TOTAL_PROPPANT / CP_TOTAL_FLUID)
      
      ### train GCM model
      print("Training GCM model")
      
      rfsrc_formula = paste0("Multivar(",
                             paste0(output_variables, collapse = ", "),
                             ") ~ ",
                             paste0(input_variables, collapse = "+"))
      rfsrc_formula = as.formula(rfsrc_formula)
      
      GCM_model =
        rfsrc(rfsrc_formula,
              data = data_2_drop_columns %>% 
                filter(DAYS_ACTUAL_HISTORY >= 720) %>% 
                select(LIQUID_90, LIQUID_180, LIQUID_360, LIQUID_540, LIQUID_720,
                       one_of(names(data_for_predictions))) %>% 
                na.omit(),
              samptype = "swor",
              mtry = MTRY,
              ntree = NTREE,
              nodesize = MIN_NODE_SIZE,
              sampsize = round(SAMPLE_FRACTION * nrow(data_2_drop_columns)),
              nsplit = 0, 
              seed = 42,
              do.trace = TRUE)
      
      GCM_model %>% 
        saveRDS(paste0(
          add_path_slash_if_needed(output_folder_locn),
          "GCM_model.rds"))
      
      # return("GCM saved")
      
      ### do predictions to create GCM
      
      ### returns a list of vectors
      split_vector = 
        get_split_vector(
          full_vector_length = nrow(data_for_predictions), 
          individual_chunk_length = round(nrow(data_for_predictions) / NUM_PREDICTION_CHUNKS)
        )
      
      number_of_chunks = length(split_vector)
      
      ### initialize empty list
      # tmp_preds_list = vector(mode = "list", length = number_of_chunks)
      LIQUID_90_preds = c()
      LIQUID_180_preds = c() 
      LIQUID_360_preds = c() 
      LIQUID_540_preds = c()
      LIQUID_720_preds = c()
      
      pb = txtProgressBar(min = 0, max = number_of_chunks, style = 3)
      
      for (i in 1:number_of_chunks) {
        tmp_preds =
          predict(GCM_model, 
                  data_for_predictions[split_vector[[i]],])
        
        LIQUID_90_preds = c(LIQUID_90_preds, tmp_preds$regrOutput$LIQUID_90$predicted) 
        LIQUID_180_preds = c(LIQUID_180_preds, tmp_preds$regrOutput$LIQUID_180$predicted) 
        LIQUID_360_preds = c(LIQUID_360_preds, tmp_preds$regrOutput$LIQUID_360$predicted) 
        LIQUID_540_preds = c(LIQUID_540_preds, tmp_preds$regrOutput$LIQUID_540$predicted) 
        LIQUID_720_preds = c(LIQUID_720_preds, tmp_preds$regrOutput$LIQUID_720$predicted)
        
        setTxtProgressBar(pb, i)  
      }
      close(pb)
      
      GCM_result =
        data_for_predictions %>% 
        mutate(
          LIQUID_90 = LIQUID_90_preds, 
          LIQUID_180 = LIQUID_180_preds, 
          LIQUID_360 = LIQUID_360_preds, 
          LIQUID_540 = LIQUID_540_preds, 
          LIQUID_720 = LIQUID_720_preds
        )
      
      ### get output filename
      if (str_length(output_file_descriptor) > 0) {
        if (str_sub(output_file_descriptor, end = 1) == "_") {
          output_file_prefix = ""
        } else {
          output_file_prefix = "_"
        }
      }
      
      ### write to disk
      GCM_result %>% 
        fwrite(
          paste0(
            add_path_slash_if_needed(output_folder_locn),
            "GCM", output_file_prefix, output_file_descriptor, ".csv"))
      
      # return(GCM_result)
      
    }
  }