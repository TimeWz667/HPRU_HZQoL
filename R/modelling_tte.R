fit_tte <- function(model, dat_tte, n_iter = 2e4, n_collect = 500, n_chains = 4) {
  n_warmup <- floor(n_iter - n_collect)
  

  ds <- local({
    #dat_tte <- tar_read(data_tte) #%>% head(200)
    dat_tte <- dat_tte %>% filter(T_evt > 0)
    
    dat_int <- dat_tte %>% filter(Recovered)
    dat_cen <- dat_tte %>% filter(!Recovered)
    
    list(
      N = nrow(dat_int),
      Ts0 = dat_int$T_last,
      Ts1 = dat_int$T_end,
      As = dat_int$Age,
      N_Cen = nrow(dat_cen),
      Ts_Cen = dat_cen$T_end,
      As_Cen = dat_cen$Age
    )
  })
  
  post <- sampling(model, data = ds, pars = c("r0", "ba1"), init = \() { list(r0 = exp(-2.35), ba1 = 0.01)},
                   chains = n_chains, iter = n_iter, warmup = n_warmup)
  
  res <- restructure_stan(post)
  return(res)
  
}


summarise_tte <- function(fit) {
  write_csv(fit$Summary, file = here::here("docs", "tabs", "fit_time2zero.csv"))
  write_csv(fit$Ext, file = here::here("posteriors", "post_time2zero.csv"))
  return(here::here("posteriors", "post_time2zero.csv"))
}


vis_tte <- function(dat_tte, fit) {
  theme_set(theme_bw())
  
  g_tte_raw <- dat_tte %>% 
    filter(Recovered) %>% 
    arrange(-T_evt2) %>% 
    mutate(pr = 1:n() / n(), Censored = !Recovered) %>% 
    ggplot() +
    geom_point(aes(T_evt2, pr, colour = Censored), alpha = 0.2) +
    #geom_line(data = haz, aes(x = ti, y = Surv)) +
    scale_y_continuous("Survival", labels = scales::percent) +
    scale_x_continuous("Years since rash onset") +
    scale_colour_discrete("Censored") +
    theme(legend.position = "bottom")
  
  
  g_tte_imputated <- dat_tte %>% 
    arrange(-T_evt2) %>% 
    mutate(pr = 1:n() / n(), Censored = !Recovered) %>% 
    ggplot() +
    geom_point(aes(T_evt2, pr, colour = Censored), alpha = 0.2) +
    #geom_line(data = haz, aes(x = ti, y = Surv)) +
    scale_y_continuous("Survival", labels = scales::percent) +
    scale_x_continuous("Years since rash onset") +
    scale_colour_discrete("Censored") +
    theme(legend.position = "bottom")
  
  pars_tte <- fit$Ext %>% mutate(Key = 1:n())
  
  sims <- dat_tte %>% 
    mutate(
      Key = sample(pars_tte$Key, n(), rep = T)
    ) %>% 
    left_join(pars_tte) %>% 
    mutate(
      rate = (r0 * exp(Age * ba1)),
      T_Sim = rexp(n(), rate)
    )
  
  exp_sims <- pars_tte %>%
    mutate(
      rd = runif(n()),
      rd = -log(1 - runif(n()))
    ) %>% 
    crossing(Age = min(sims$Age):100) %>% 
    mutate(
      rate = (r0 * exp(Age * ba1)),
      T_Sim = rd / rate
    )
  
  
  g_tte_sim <- sims %>% 
    group_by(Age) %>% 
    summarise(
      Data = mean(T_evt2),
      Simulated = mean(T_Sim)
    ) %>% 
    pivot_longer(-Age, names_to = "Source") %>% 
    ggplot() +
    stat_lineribbon(data = exp_sims, aes(x = Age, y = T_Sim), alpha = 0.5) +
    geom_point(aes(x = Age, y = value, colour = Source)) +
    scale_fill_brewer() +
    scale_y_continuous("Time to recovery, year") +
    facet_grid(.~Source)
  
  
  gs <- list(
    g_tte_raw = g_tte_raw,
    g_tte_imputated = g_tte_imputated,
    g_tte_sim = g_tte_sim
  )
  
  ggsave(g_tte_raw, file = here::here("docs", "figs", "g_tte_raw.png"), width = 6, height = 4)
  ggsave(g_tte_imputated, file = here::here("docs", "figs", "g_tte_imputated.png"), width = 6, height = 4)
  ggsave(g_tte_sim, filename = here::here("docs", "figs", "g_tte_sim.png"), width = 7, height = 4)
  
}

