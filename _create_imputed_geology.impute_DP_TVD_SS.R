

impute_DP_TVD_SS_from_DP_MAX_TVD =
  function(df){
    "
    Impute the DP_TVD_SS using correlation with DP_MAX_TVD.
    This is needed cause enersight doesn't populate DP_TVD_SS
    "
    
    train_data =
      df %>% 
      select(DP_MAX_TVD, DP_TVD_SS) %>% 
      na.omit()
    
    lin_model = lm(DP_TVD_SS ~ DP_MAX_TVD,
                   data = train_data)
    
    df =
      df %>% 
      mutate(DP_TVD_SS =
               ifelse(is.na(DP_TVD_SS),
                      lin_model$coefficients["(Intercept)"] +
                        DP_MAX_TVD * lin_model$coefficients["DP_MAX_TVD"],
                      DP_TVD_SS))
    
    return(df)
    
  }