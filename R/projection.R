
boot_pars <- function(pars_tte, pars_qol, n_sim = 1000) {
  boot_qol <- pars_qol$stats %>%
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
  
  boot_tte <- pars_tte$Ext %>% mutate(ID = 1:n())
  
  boot_tte <- tibble(Key = 1:n_sim, ID = sample.int(nrow(boot_tte), n_sim, replace = n_sim > nrow(boot_tte))) %>% 
    left_join(pars_tte$Ext %>% mutate(ID = 1:n()), by = "ID") %>% 
    select(- ID)
  
  pars <- merge(boot_qol, boot_tte)
  return(pars)
}


sim_ql <- \(age0 = 50, pars, pars_demo, dt = 0.01) {
  n_sim <- max(pars$Key)
  ti <- seq(0, 100 - age0, dt)
  crossing(Key = 1:n_sim, Age = age0, ti = ti) %>% 
    mutate(
      AgeT = floor(Age + ti)
    ) %>% 
    left_join(pars, by = "Key") %>% 
    left_join(pars_demo %>% rename(AgeT = Age), by = "AgeT") %>% 
    mutate(
      rate = r0 * exp(Age * ba1),
      p_health = pexp(ti, rate),
      p_hz = 1 - p_health,
      ql_ph = 1 - (Q_0 * Prop_0 + Q_1 * Prop_1 + Q_2 * Prop_2),
      ql_pn = norm - (pmin(Q_0, norm) * Prop_0 + pmin(Q_1, norm) * Prop_1 + pmin(Q_2, norm) * Prop_2),
      ql_ph = ql_ph * p_hz,
      ql_pn = ql_pn * p_hz
    ) %>% 
    group_by(Key, Age, AgeT) %>% 
    summarise(
      across(c(ql_ph, ql_pn, mr, norm), mean)
    ) %>% 
    mutate(
      surv = c(1, cumprod(1 - mr)[-n()]),
      d15 = (1 + 0.015) ^ -(AgeT - Age),
      d35 = (1 + 0.035) ^ -(AgeT - Age)
    ) %>% 
    summarise(
      LE = sum(surv),
      QALE1 = sum((norm - ql_pn) * surv),
      QALE0 = sum(norm * surv),
      QL00 = sum(ql_pn * surv),
      QL15 = sum(ql_pn * surv * d15),
      QL35 = sum(ql_pn * surv * d35),
      QLH00 = sum(ql_ph * surv),
      QLH15 = sum(ql_ph * surv * d15),
      QLH35 = sum(ql_ph * surv * d35)
    ) %>% 
    ungroup()
}


simulate_shortfall <- function(pars, data_norm, vset = "uk", age0 = 50, age1 = 99) {
  pars_demo <- data_norm %>% 
    filter(Year == 2023) %>% 
    ungroup() %>% 
    select(Age, mr = mortality, norm = norm_leoss) 
  
  sim <- bind_rows(lapply(age0:age1, \(a0) sim_ql(age0 = a0, pars = pars, pars_demo = pars_demo)))
  
  write_csv(sim, here::here("posteriors", paste0("QALY_loss_sims_", vset, ".csv")))
  
  return(sim)
}


summarise_shortfall <- function(sim, vset) {
  stats <- sim %>% 
    select(-Key) %>% 
    group_by(Age) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-Age, names_to = c("Index", "name"), names_sep = "_") %>% 
    pivot_wider()
  

  write_csv(stats, here::here("docs", "tabs", paste0("QALY_loss_stats_", vset, ".csv")))
  
  return(stats)
}

vis_shortfall <- function(sim, stats, vset) {
  gs <- list()
  gs$g_ql <- stats %>% 
    filter(startsWith(Index, "QL")) %>% 
    filter(endsWith(Index, "35")) %>% 
    filter(Age < 98) %>% 
    ggplot(aes(x = Age)) +
    geom_ribbon(aes(ymin = L, ymax = U), alpha = 0.2) +
    geom_line(aes(y = M)) +
    scale_y_continuous("QALY loss") +
    scale_x_continuous("Age at onset of HZ rush") +
    facet_wrap(.~Index, labeller = labeller(Index = c(QL35 = "From population norm", QLH35 = "From perfect health"))) +
    expand_limits(y = 0) +
    labs(caption = "discounting: 3.5%") +
    theme_bw()
  
  
  gs$g_ql_grad <- sim %>% 
    select(Age, QL35, QLH35) %>% 
    filter(Age < 99) %>%  
    pivot_longer(-Age, names_to = "Index") %>% 
    ggplot(aes(x = Age, y = value)) +
    stat_lineribbon() +
    scale_fill_brewer() +
    scale_y_continuous("QALY loss") +
    scale_x_continuous("Age at onset of HZ rush") +
    facet_wrap(.~Index, labeller = labeller(Index = c(QL35 = "From population norm", QLH35 = "From perfect health"))) +
    expand_limits(y = 0) +
    labs(caption = "discounting: 3.5%") +
    theme_bw()
  
  
  ggsave(gs$g_ql, filename = here::here("docs", "figs", paste0("g_ql_", vset, ".png")), width = 7, height = 4)
  ggsave(gs$g_ql_grad, filename = here::here("docs", "figs", paste0("g_ql_grad_", vset, ".png")), width = 7.3, height = 4)
  
  return(gs)

}

