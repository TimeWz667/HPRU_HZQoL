library(tidyverse)

theme_set(theme_bw())


source(here::here("scripts/fn_sim.R"))
pars_lmer_at <- read_csv(here::here("posteriors", "boot_lmer_at.csv"))


## Load data
load(here::here("data", "qol_reformed.rdata"))
min_qol <- min(reformed$EQ5D)

reformed


## Load population norm
load(here::here("data", "sup_demo.rdata"))
pars_demo <- sup_demo %>% 
  filter(Year == 2023) %>% 
  ungroup() %>% 
  select(Age, mr = mortality, norm = norm_leoss) 


norm <- pars_demo %>% 
  mutate(
    Norm_rescaled = (norm - min_qol) / (1 - min_qol)
  ) %>% 
  select(Age, Norm_rescaled)


## 
n_sim <- 1000

targets <- tibble(
  Key = 1:n_sim
) %>% 
  crossing(Age = 50:99, Ti = seq(0, 3, 0.02))


## T2Z + const qol

sim_lmer_at <- \(df, pars, q0) {
  n_df <- length(unique(df$Key))
  n_pars <- nrow(pars)
  
  pars <- pars %>% 
    mutate(Key = sample.int(n())) %>% 
    arrange(Key) %>% 
    filter(Key <= n_df) 
  
  sim <- df %>% 
    left_join(pars, by = "Key") %>% 
    mutate(
      logit_q = b0 + b_t1 * Ti + b_t2 * Ti ^ 2 + b_a1 * Age + b_a2 * Age ^ 2,
      qol = 1 / (1 + exp(- logit_q)),
      qol = pmin(pmax(qol, 0.3), 0.7),
      qol = qol * (1 - min_qol) + min_qol,
      p_z = 0
    ) %>% 
    select(Key, Age, Ti, qol, p_z)
  
  return(df %>% left_join(sim, by = c("Key", "Age", "Ti")))
}

proj_lmer <- targets %>% 
  sim_lmer_at(pars_lmer_at, min_qol) %>% 
  calc_shortfall(pars_demo)


stats_lmer <- proj_lmer %>% 
  group_by(Age) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = \(x) quantile(x, 0.025),
      U = \(x) quantile(x, 0.975)
    ))
  ) 

stats_lmer %>% 
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = QL35_L, ymax = QL35_U), alpha = 0.2) +
  geom_line(aes(y = QL35_M)) +
  scale_y_continuous("Quality of life loss due to HZ against population norm in the UK") +
  scale_x_continuous("Age of rush onset") +
  expand_limits(y = 0.2)


stats_lmer %>% 
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = QLH35_L, ymax = QLH35_U), alpha = 0.2) +
  geom_line(aes(y = QLH35_M)) +
  scale_y_continuous("Quality of life loss due to HZ against perfect health") +
  scale_x_continuous("Age of rush onset") +
  expand_limits(y = 0.2)


