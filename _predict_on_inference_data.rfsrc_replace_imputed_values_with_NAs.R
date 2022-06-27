
rfsrc_replace_imputed_values_with_NAs =
  function(
    short_vector,
    long_vector,
    digits_of_precision = 1L
  ){
    "
    rfsrc.predict does not currently return a NA for a row of newdata which fails to predict.
    rfsrc.predict simply returns a shortened vector.
    Making it difficult to determine which rows are causing failures.
    rfsrc.predict does have a 'impute' mode
    By comparing the regular (short) and impute mode (long) vectors,
    Can determine which values in impute vector were imputed and replace with NA.
    
    This will return a vector of length matching long vector, 
    with NAs in positions matching rows of failed predictions.
    "
    
    short_vector = round(short_vector, digits = digits_of_precision)
    long_vector = round(long_vector, digits = digits_of_precision)
    
    length_long_vector = length(long_vector)
    length_short_vector = length(short_vector)

    ### initialize for loop
    cleaned_vector = rep(na_int, times = length_long_vector)
    short_vector_index = 1L
    i = 1L
    
    ### End if 
    ### loop finishes checking values in long vector
    ### or runs out of values in short vector to check against 
    ### (leaving the rest of cleaned vector as NA)
    while ((i <= length_long_vector) & (short_vector_index <= length_short_vector)) {
      if (long_vector[i] == short_vector[short_vector_index]) {
        cleaned_vector[i] = short_vector_index
        short_vector_index = short_vector_index + 1L
      }
      i = i + 1L
    }
    
    ### error check: if not all short vector values were matched
    max_matched_value = max(cleaned_vector, na.rm = TRUE)
    
    if (max_matched_value != length_short_vector) {
      stop(paste0("Only matched ", 
                  max_matched_value, " of ", 
                  length_short_vector, " values."))
    }
    
    return(cleaned_vector)
  }