
### _create_imputed_geology.impute_PP_with_low_value.R

impute_PP_with_low_value =
  function(df_in){
    
    "
    Fill missing values with -100 
    or 
    lowest_value_in_column - 100
    whichever is less
    "  
    PP_mask = which(str_starts(names(df_in), "PP_"))
    
    for (col_i in PP_mask) {
      
      ### find minimum value in column
      ### set impute value for -100 or minimum of column less 100
      impute_value = 
        suppressWarnings(
          min(min(df_in[,col_i], na.rm = TRUE) - 100,
              -100)
        )
      ### replace NA values in missing column
      df_in[,col_i] = 
        map_dbl(1:nrow(df_in),
                function(row_j){
                  ifelse(is.na(df_in[row_j,col_i]), impute_value, df_in[row_j,col_i])          
                })
    }
    
    return(df_in)
  }