
### _create_imputed_geology.impute_geo_with_low_value.R

impute_geo_with_low_value = 
  function(fn_geo_df){
    ### which columns start with MGEO_ or DIGEO_
    MGEO_mask = which(str_starts(names(fn_geo_df), "MGEO_"))
    DIGEO_mask = which(str_starts(names(fn_geo_df), "DIGEO_"))
    
    masks_combined = c(MGEO_mask, DIGEO_mask)
    
    for (col_i in masks_combined) {
      
      ### find minimum value in column
      ### set impute value for -100 or minimum of column less 100
      impute_value = min(min(fn_geo_df[,col_i], na.rm = TRUE) - 100,
                         -100)
      ### replace NA values in missing column
      fn_geo_df[,col_i] = 
        map_dbl(1:nrow(fn_geo_df),
                function(row_j){
                  ifelse(is.na(fn_geo_df[row_j,col_i]), impute_value, fn_geo_df[row_j,col_i])          
                })
    }
    
    return(fn_geo_df)
  }