fit_qol_kmeans <- function(dat) {
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


summarise_qol_kmeans <- function(fit) {
  
}


fit_qol <- function(model, dat_qol, n_iter = 2e4, n_collect = 500, n_chains = 4) {
  n_warmup <- floor(n_iter - n_collect)
  
  ds <- list(
    N = nrow(dat_qol),
    K = 2,
    Ys = dat_qol$EQ5D
  )
  
  km <- fit_qol_kmeans(dat_qol)$stats %>% 
    filter(Agp == "All" & Cluster != "0") %>% 
    arrange(mu)
  
  post <- sampling(model, data = ds, pars = c("theta", "mu", "sigma"), 
                   init = \() { list(mu = km$mu, sigma = km$std)},
                   chains = n_chains, iter = n_iter, warmup = n_warmup)
  
  res <- restructure_stan(post)
  return(res)
  
}


summarise_qol <- function(fit, vset) {
  write_csv(fit$Summary, file = here::here("docs", "tabs", paste0("fit_qol_", vset, ".csv")))
  write_csv(fit$Ext, file = here::here("posteriors", paste0("post_qol_", vset, ".csv")))
  return(here::here("posteriors", paste0("post_qol_", vset, ".csv")))
}


summarise_qol_kmeans <- function(fit, vset) {
  write_csv(fit$stats, file = here::here("posteriors", paste0("post_qol_k_", vset, ".csv")))
  return(here::here("posteriors", paste0("post_qol_k_", vset, ".csv")))
}


vis_qol <- function(dat_qol, fit, vset) {
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



