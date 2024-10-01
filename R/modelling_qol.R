fit_qol <- function(dat) {
  require(dplyr)
  
  dat_cluster <- dat %>% 
    left_join(
      dat %>% 
        filter(EQ5D != 1) %>% 
        mutate(
          Cluster = kmeans(EQ5D, 2)$cluster
        )
    ) %>% 
    mutate(
      Cluster = ifelse(is.na(Cluster), 0, Cluster),
      Cluster = as.character(Cluster)
    )
  
  stats_cluster_Agp <- dat_cluster %>% 
    group_by(Agp, Cluster) %>% 
    summarise(
      mu = mean(EQ5D),
      std = sd(EQ5D),
      N = n()
    ) %>% 
    group_by(Agp) %>% 
    mutate(
      Prop = N / sum(N)
    ) 
  
  
  stats_cluster_All <- dat_cluster %>% 
    group_by(Cluster) %>% 
    summarise(
      mu = mean(EQ5D),
      std = sd(EQ5D),
      N = n()
    ) %>% 
    mutate(
      Prop = N / sum(N)
    ) 
  
  stats_cluster <- bind_rows(
    stats_cluster_Agp,
    stats_cluster_All %>% 
      mutate(Agp = "All")
    
  )
  
  return(list(
    src = dat_cluster,
    stats = stats_cluster
  ))
}


summarise_qol <- function(fit, vset) {
  write_csv(fit$stats, file = here::here("docs", "tabs", paste0("fit_qol_", vset, ".csv")))
  write_csv(fit$stats, file = here::here("posteriors", paste0("fit_qol_", vset, ".csv")))
  return(fit$stats)
}


vis_qol <- function(dat_qol, fit, vset) {
  theme_set(theme_bw())
  
  pars <- fit$stats %>%
    ungroup() %>% 
    filter(Agp == "All")
  
  dat_cluster <- fit$src
  
  n_sim <- nrow(dat_cluster)

  
  sims <- tibble(Key = 1:n_sim) %>% 
    mutate(
      Cluster = sample(pars$Cluster, n(), prob = pars$Prop, rep = T)
    ) %>% 
    left_join(pars %>% select(Cluster, mu, std), by = "Cluster") %>% 
    mutate(
      Q = rnorm(n(), mu, std),
      Q = pmin(Q, 1)
    ) %>% 
    select(Key, Cluster, Q)
  
  sims_data <- bind_rows(
    sims %>% select(Key, Cluster, Q) %>% mutate(Source = "Simulated"),
    dat_cluster %>% select(Key, Cluster, Q = EQ5D) %>% mutate(Source = "Data")
  ) 

  
  g_sim_q <- sims_data %>% 
    ggplot() +
    geom_histogram(aes(x = Q, fill = Cluster), alpha = 0.8) +
    scale_x_continuous("EQ5D") +
    facet_grid(.~Source)
  
  ggsave(g_sim_q, filename = here::here("docs", "figs", paste0("g_qol_sim_", vset, ".png")), width = 7, height = 4)
  
  gs <- list(
    g_sim_q = g_sim_q
  )
  
  return(gs)
}



