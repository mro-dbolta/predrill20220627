
tmp_quant_10 = 
  function(x){
    quantile(x, probs = 0.1, na.rm = TRUE)
  }

tmp_quant_50 = 
  function(x){
    quantile(x, probs = 0.5, na.rm = TRUE)
  }

tmp_quant_90 = 
  function(x){
    quantile(x, probs = 0.9, na.rm = TRUE)
  }