
boot_pars <- function(file_pars_tte, file_pars_qol, n_sim = 1000) {
  boot_qol <- read_csv(file_pars_qol) %>%
    filter(Agp == "All") %>% 
    crossing(Key = 1:n_sim) %>% 
    mutate(
      std = ifelse(is.na(std), 0, std),
      Q = rnorm(n(), mu, std),
      Q = pmin(Q, 1)
    ) %>% 
    relocate(Key) %>% 
    select(Key, Q, Prop, Cluster) %>% 
    pivot_wider(names_from = Cluster, values_from = c(Q, Prop))
  
  boot_tte <- read_csv(file_pars_tte) %>% mutate(ID = 1:n())
  
  boot_tte <- tibble(Key = 1:n_sim, ID = sample.int(nrow(boot_tte), n_sim, replace = n_sim > nrow(boot_tte))) %>% 
    left_join(boot_tte, by = "ID") %>% 
    select(- ID)
  
  pars <- merge(boot_qol, boot_tte)
  return(pars)
}
