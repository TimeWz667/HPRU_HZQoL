
visualise_qol_freq <- function(dat_qol, pars_qol_f, vset, drf = FALSE) {
  #pars_qol_f <- tar_read(pars_qol_f, 1)
  #dat_qol <- tar_read(data_qol)
  
  require(tidyverse)
  
  tags <- c("0" = "temporally well", "1" = "mild discomfort", "2" = "severe discomfort")

  
  dat <- pars_qol_f[[2]] %>% 
    select(Key, SID, ti, Age, Agp, Cluster = cluster, Q = EQ5D)
  
  sim_lme <- \(d, model) {
    pred <- predict(model, d, re.form = NA, se = T)
    unlist(pred$fit + rnorm(nrow(d), 0, pred$se.fit))
  }
  ms = pars_qol_f$Models
  
  d2s <- dat %>% 
    select(-Cluster, -Q) %>% 
    mutate(
      d15 = (ti < 15 / 365.25),
      d30 = (ti < 30 / 365.25)
    )  
  
  sim_fixed <- d2s %>% 
    mutate(
      p_z = predict(ms$pz, ., re.form = NA, type = "response"),
      p_c1 = predict(ms$pc1, ., re.form = NA, type = "response") * (1 - p_z),
      p_c2 = 1 - p_z - p_c1,
      q_c1 = sim_lme(., ms$c1),
      q_c2 = sim_lme(., ms$c2),
      q_c1 = pmin(q_c1, 1),
      q_c2 = pmin(q_c2, 1)
    ) %>% 
    mutate(
      rd = runif(n()),
      Cluster = case_when(
        rd < p_z ~ "0",
        rd < p_z + p_c1 ~ "1",
        T ~ "2"
      ),
      Q = case_when(
        Cluster == "0" ~ 1,
        Cluster == "1" ~ q_c1,
        T ~ q_c2
      )
    ) %>% 
    select(Key, ti, Age, Agp, Cluster, Q)

  if (drf) { # including random effects
    sim <- d2s %>% 
      mutate(
        is_z = simulate(ms$pz, 1, newdata = ., type = "response")[[1]],
        is_c1 = simulate(ms$pc1, 1, newdata = ., type = "response")[[1]],
        q_c1 = simulate(ms$c1, 1, newdata = .)[[1]],
        q_c2 = simulate(ms$c2, 1, newdata = .)[[1]],
        q_c1 = pmin(q_c1, 1),
        q_c2 = pmin(q_c2, 1)
      ) %>% 
      mutate(
        rd = runif(n()),
        Cluster = case_when(
          is_z == 1 ~ "0",
          is_c1 == 1 ~ "1",
          T ~ "2"
        ),
        Q = case_when(
          Cluster == "0" ~ 1,
          Cluster == "1" ~ q_c1,
          T ~ q_c2
        )
      ) %>% 
      select(Key, ti, Age, Agp, Cluster, Q)
    
    sims_data <- bind_rows(
      sim %>% mutate(Source = "Simulated"),
      sim_fixed %>% mutate(Source = "Simulated (fixed effects)"),
      dat %>% mutate(Cluster = as.character(Cluster), Source = "Data")
    ) %>% 
      filter(ti > 0) %>% 
      mutate(Cluster = factor(Cluster, names(tags)))
  } else {
    sims_data <- bind_rows(
      sim_fixed %>% mutate(Source = "Simulated"),
      dat %>% mutate(Cluster = as.character(Cluster), Source = "Data")
    ) %>% 
      filter(ti > 0) %>% 
      mutate(Cluster = factor(Cluster, names(tags)))
  }
  

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
    scale_x_continuous("Time since the onset of rash", breaks = seq(0, 1.25, 0.25), 
                       labels = scales::number_format(scale = 12, suffix = " mo."),
                       limits = c(0, 1.25)) +
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
    scale_x_continuous("Time since the onset of rash", breaks = seq(0, 1.25, 0.25), 
                       labels = scales::number_format(scale = 12, suffix = " mo."),
                       limits = c(0, 1.25)) +
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
  
  if (drf) {
    ggsave(g_sim_q, filename = here::here("docs", "figs", paste0("g_qol_sim_drf_", vset, ".png")), width = 10, height = 4)
    ggsave(g_sim_qt, filename = here::here("docs", "figs", paste0("g_qol_t_sim_drf_", vset, ".png")), width = 10, height = 4)
    ggsave(g_sim_qp, filename = here::here("docs", "figs", paste0("g_qol_p_sim_drf_", vset, ".png")), width = 10, height = 4)
    ggsave(g_sim_qbind, filename = here::here("docs", "figs", paste0("g_qol_b_sim_drf_", vset, ".png")), width = 10, height = 7)
  } else {
    ggsave(g_sim_q, filename = here::here("docs", "figs", paste0("g_qol_sim_", vset, ".png")), width = 7, height = 4)
    ggsave(g_sim_qt, filename = here::here("docs", "figs", paste0("g_qol_t_sim_", vset, ".png")), width = 7, height = 4)
    ggsave(g_sim_qp, filename = here::here("docs", "figs", paste0("g_qol_p_sim_", vset, ".png")), width = 7, height = 4)
    ggsave(g_sim_qbind, filename = here::here("docs", "figs", paste0("g_qol_b_sim_", vset, ".png")), width = 7, height = 7)
  }

  
  
  return(gs)
}

