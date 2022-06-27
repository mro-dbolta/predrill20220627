
get_intro_row_to_establish_column_types =
  function(df){
    intro_row =
      df %>% 
      head(1)
    
    for (col_i in 1:ncol(df)) {
      col_type = class(df[,col_i])[1]
      
      if (names(df)[col_i] == "DT_FIRST_PRODUCTION_YEAR") {
        intro_row[1,col_i] = 2018 
      } else if (names(df)[col_i] == "DT_FIRST_PRODUCTION") {
        intro_row[1,col_i] = na.omit(df$DT_FIRST_PRODUCTION)[1]
      } else if (col_type == "character") {
        intro_row[1,col_i] = "a" 
      } else if (col_type == "integer") {
        intro_row[1,col_i] = 1L
      } else if (col_type == "numeric") {
        intro_row[1,col_i] = 10.01
      } else if (col_type == "logical") {
        intro_row[1,col_i] = FALSE
      } else if (col_type == "POSIXct") {
        intro_row[1,col_i] = na.omit(df[,col_i])[1]
      } else {
         
        stop(paste0("Column type not handled: ",
                    names(df)[col_i], ": ",
                    col_type))
      }
    }
    return(intro_row)
  }
