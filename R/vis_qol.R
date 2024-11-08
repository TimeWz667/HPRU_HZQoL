
visualise_qol <- function(dat_qol, pars_qol_k, vset) {
  require(tidyverse)
  
  #pars_qol_k <- tar_read(pars_qol_k, 1)

  
  pars <- pars_qol_k$stats %>%
    ungroup() %>% 
    filter(Agp == "All")
  
  dat_cluster <- pars_qol_k$src
  
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
    left_join(dat_cluster %>% select(ti, Key)) %>% 
    select(Key, ti, Cluster, Q)
  
  
  tags <- c("0" = "temporal well-off", "2" = "mild discomfort", "1" = "severe discomfort")
  
  
  sims_data <- bind_rows(
    sims %>% select(Key, ti, Cluster, Q) %>% mutate(Source = "Simulated"),
    dat_cluster %>% select(Key, ti, Cluster, Q = EQ5D) %>% mutate(Source = "Data")
  ) %>% 
    filter(ti > 0) %>% 
    mutate(Cluster = factor(Cluster, names(tags)))
  
  

  g_sim_q <- sims_data %>% 
    ggplot() +
    geom_histogram(aes(x = Q, fill = Cluster), alpha = 0.8) +
    scale_fill_discrete("Cluster", labels = tags) +
    scale_x_continuous("EQ5D") +
    facet_grid(.~Source)
  
  
  g_sim_qt <- sims_data %>% 
    ggplot() +
    geom_point(aes(x = ti, y = Q, colour = Cluster), alpha = 0.2) +
    scale_y_continuous("EQ-5D score", breaks = seq(-0.5, 1, 0.5)) +
    scale_x_continuous("Time since the onset of rash", breaks = seq(0, 1.5, 0.25), 
                       labels = scales::number_format(scale = 12, suffix = " mo."),
                       limits = c(0, 1.5)) +
    scale_color_discrete("Cluster", labels = tags) +
    expand_limits(y = c(-0.5, 1)) +
    facet_grid(.~Source) +
    theme(legend.position = "bottom")
  
  
  
  gs <- list(
    g_sim_q = g_sim_q,
    g_sim_qt = g_sim_qt
  )
  
  ggsave(g_sim_q, filename = here::here("docs", "figs", paste0("g_qol_sim_", vset, ".png")), width = 7, height = 4)
  
  ggsave(g_sim_qt, filename = here::here("docs", "figs", paste0("g_qol_t_sim_", vset, ".png")), width = 7, height = 4)
  
  
  return(gs)
}

