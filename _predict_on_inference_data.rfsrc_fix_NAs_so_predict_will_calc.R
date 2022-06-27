
rfsrc_fix_NAs_so_predict_will_calc =
  function(df){
    for (col_i in 1:ncol(df)) {
      col_type = class(df[,col_i])[1]
      
      if (col_type == "character") {
        df[,col_i] = ifelse(is.na(df[,col_i]),
                            "this_entry_blank",
                            df[,col_i])
      } else if (col_type == "integer") {
        df[,col_i] = ifelse(is.na(df[,col_i]),
                            -100L,
                            df[,col_i])
      } else if (col_type == "numeric") {
        df[,col_i] = ifelse(is.na(df[,col_i]),
                            -100.0,
                            df[,col_i])
      } else if (col_type == "POSIXct") {
        df[,col_i] = ifelse(is.na(df[,col_i]),
                            as.POSIXct("1990/1/1 12:01:01"),
                            df[,col_i])
      } else  if (col_type == "factor") {
        df[,"placeholder"] = as.character(df[,col_i])
        
        df[,"placeholder"] = ifelse(is.na(df[,"placeholder"]),
                                    levels(df[,col_i])[1],
                                    df[,"placeholder"])
        
        df[,"placeholder"] = factor(df[,"placeholder"]
                                    , levels = levels(df[,col_i])
        )
        
        df[,col_i] = df[,"placeholder"]
        
        df = df %>% 
          select(-placeholder)
        
        
      } else {
        stop(paste0("Column type not handled: ",
                    col_type))
      }
    }
    return(df)
  }
