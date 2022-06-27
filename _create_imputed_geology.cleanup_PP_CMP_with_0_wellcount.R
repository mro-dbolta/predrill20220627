
### _create_imputed_geology.cleanup_PP_CMP_with_0_wellcount.R

cleanup_PP_CMP_with_0_wellcount =
  function(data_in) {
    
    "
    Sujan's base PP function uses map interpolation to impute all PP columns,
    including the PP_completions columns
    
    These are not map interpolatable
    
    Overwrite with NA, update the PP_DELTA columns
    "
    data_out = data_in
    for (timing in c("ALL", "180PRE")) {
      for (distance in c("10K", "2K")) {
        for (summary_calc in c("MED", "MAX"))
          for (completions_var in c(
            "CP_LAT_LEN_GROSS_PERF_INTVL",
            "CP_TOTALFLUIDPERFOOT",
            "CP_TOTALPROPPERFOOT",
            "CP_TOTAL_FLUID",
            "CP_STAGES",
            "CP_TOTAL_PROPPANT",
            "CP_TOT_PROPPANT_BY_TOT_FLUID"
          )) {
            pp_wellct_var = paste0("PP_", timing, "_", distance, "_WELLCOUNT_SF")
            
            pp_var = paste0("PP_", timing, "_", distance, "_",
                            summary_calc, "_", completions_var, "_SF")
            
            pp_delta_var = paste0("PP_", timing, "_", distance, "_",
                                  summary_calc, "_DELTA_", completions_var, "_SF")
            
            ### if wellcount 0, replace PP with NA
            data_out[pp_var] =
              map_dbl(1:nrow(data_out),
                      function(i){
                        ifelse(data_out[i, pp_wellct_var] < 1, NA, data_out[i,pp_var])        
                      })
            
            data_out[pp_delta_var] = data_out[completions_var] - data_out[pp_var]
            
          }
      }
    }
    
    return(data_out)
  }
