
output_pars_shortfall <- function(pars_sf, data_norm, vset = "uk") {
  require(jsonlite)
  
  pars_demo <- data_norm %>% 
    filter(Year == 2023) %>% 
    ungroup() %>% 
    select(Age, r_death = mortality, norm = norm_leoss, N = N) 
  
  js <- list(
    ValueSet = vset,
    pars_demo = pars_demo,
    pars_shortfall = pars_sf
  )
  
  jsonlite::write_json(js, here::here("pars", paste0("pars_qol_", vset, ".json")), auto_unbox = T)

  return(js)
}
