library(tidyverse)

theme_set(theme_bw())


# Function for calucating QALY loss
sim_ql <- \(age0 = 50, pars, pars_demo, n_sim = 2000, dt = 0.01) {
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


amlu <- list(
  A = mean,
  M = median,
  L = \(x) quantile(x, 0.025),
  U = \(x) quantile(x, 0.975)
)


# Load population norm

load(here::here("data", "sup_demo.rdata"))
pars_demo <- sup_demo %>% 
  filter(Year == 2023) %>% 
  ungroup() %>% 
  select(Age, mr = mortality, norm = norm_leoss) 


# Load parameters

pars_tte <- read_csv(here::here("posteriors", "post_time2zero_age.csv")) %>% 
  mutate(Key = 1:n())


for(vs in c("orig", "uk")) {
  
  vs <- glue::as_glue(vs)
  
  pars_qol <- read_csv(here::here("posteriors", "boot_cluster_" + vs + ".csv")) %>% 
    pivot_wider(names_from = Cluster, values_from = c(Q, Prop))
  
  
  pars <- pars_tte %>% 
    left_join(pars_qol)
  
  
  sims <- bind_rows(lapply(50:99, \(a0) sim_ql(age0 = a0, pars = pars, pars_demo = pars_demo)))
  
  
  stats <- sims %>% 
    select(-Key) %>% 
    group_by(Age) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-Age, names_to = c("Index", "name"), names_sep = "_") %>% 
    pivot_wider() %>% 
    mutate(
      ValueSet = vs
    )
  
  
  write_csv(sims, here::here("posteriors", "QALY_loss_sims_" + vs + ".csv"))
  write_csv(stats, here::here("docs", "tabs", "QALY_loss_stats_" + vs + ".csv"))
  
  
  g_ql <- stats %>% 
    filter(startsWith(Index, "QL")) %>% 
    filter(endsWith(Index, "35")) %>% 
    filter(Age < 98) %>% 
    ggplot(aes(x = Age)) +
    geom_ribbon(aes(ymin = L, ymax = U), alpha = 0.2) +
    geom_line(aes(y = M)) +
    scale_y_continuous("QALY loss") +
    scale_x_continuous("Age at rash onset") +
    facet_wrap(.~Index, labeller = labeller(Index = c(QL35 = "From population norm", QLH35 = "From perfect health"))) +
    expand_limits(y = 0)
  
  
  ggsave(g_ql, filename = here::here("docs", "figs", "g_ql_" + vs + ".png"), width = 7, height = 4)
  
}
