
visualise_qol_k <- function(dat_qol, pars_qol_k, vset) {
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
  
  
  tags <- c("0" = "temporally well", "1" = "mild discomfort", "2" = "severe discomfort")
  
  
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
  
  g_sim_qp <- sims_data %>% 
    filter(ti < 1) %>% 
    mutate(
      tig = cut(ti, c(0, 0.25, 0.5, 0.75, 1), right = F)
    ) %>% 
    group_by(Source, tig, Cluster) %>% 
    summarise(
      pr = n()
    ) %>% 
    group_by(Source, tig) %>% 
    mutate(
      pr = pr / sum(pr)
    ) %>% 
    extract(tig, c("ti0", "ti1"), "\\[(\\S+),(\\S+)\\)", remove = F, convert = T) %>% 
    ungroup() %>% 
    ggplot() +
    geom_rect(aes(xmin = ti0, xmax = ti1, 
                  ymin = 4 - as.numeric(Cluster) - 0.5, ymax = 4 - as.numeric(Cluster) + 0.5, fill = pr), alpha = 0.33)  +
    geom_text(aes(x = (ti0 + ti1) /2, y = Cluster, label = scales::percent(pr, accuracy = 2)))  +
    scale_y_discrete("Cluster", labels = tags, limits = rev) +
    scale_x_continuous("Time since the onset of rash", breaks = seq(0, 1.5, 0.25), 
                       labels = scales::number_format(scale = 12, suffix = " mo."),
                       limits = c(0, 1.5)) +
    scale_fill_gradient("Proportion in cluster", low = "#132B4385", high = "#56B1F785", limits = 0:1, labels = scales::percent) +
    facet_grid(.~Source) +
    theme(legend.position = "bottom", axis.text.y = element_text(angle = 70, hjust = 0.5))
  
  g_sim_qbind = ggpubr::ggarrange(g_sim_qt + labs(subtitle = "(A)"), 
                                  g_sim_qp + labs(subtitle = "(B)"), 
                                  nrow = 2, align = "v")  
  
  gs <- list(
    g_sim_q = g_sim_q,
    g_sim_qt = g_sim_qt,
    g_sim_qp = g_sim_qp,
    g_sim_qbind = g_sim_qbind
  )
  
  ggsave(g_sim_q, filename = here::here("docs", "figs", paste0("g_qol_sim_", vset, ".png")), width = 7, height = 4)
  
  ggsave(g_sim_qt, filename = here::here("docs", "figs", paste0("g_qol_t_sim_", vset, ".png")), width = 7, height = 4)
  
  ggsave(g_sim_qp, filename = here::here("docs", "figs", paste0("g_qol_p_sim_", vset, ".png")), width = 7, height = 4)
  ggsave(g_sim_qbind, filename = here::here("docs", "figs", paste0("g_qol_b_sim_", vset, ".png")), width = 7, height = 7)
  
  
  return(gs)
}

