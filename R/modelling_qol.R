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


summarise_qol <- function(fit, file) {
  write_csv(fit$stats, file)
  return(fit$stats)
}


simulate_qol <- function(fit, n_sim = 2000) {
  boot_cluster <- fit$stats_cluster %>%
    filter(Agp == "All") %>% 
    crossing(Key = 1:n_sim) %>% 
    mutate(
      Q = rnorm(n(), mu, std),
      Q = pmin(Q, 1)
    ) %>% 
    relocate(Key) %>% 
    select(Key, Q, Prop, Cluster)
  
  return(boot_cluster)
}

