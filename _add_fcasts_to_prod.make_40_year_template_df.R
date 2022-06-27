
make_40_year_template_df = function(APIs){
  
  template_40_year_prod =
    data.frame(CUM_PRODUCING_DAYS = 1:(40 * 12)) %>% 
    mutate(CUM_PRODUCING_DAYS = CUM_PRODUCING_DAYS * 30)
  
  template_40_year_prod =
    expand.grid(API = APIs,
                CUM_PRODUCING_DAYS = template_40_year_prod$CUM_PRODUCING_DAYS,
                stringsAsFactors = FALSE)
  
  return(template_40_year_prod)
}




