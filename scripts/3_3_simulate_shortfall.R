library(tidyverse)

theme_set(theme_bw())


source(here::here("scripts/fn_sim.R"))
pars_time2zero <- read_csv(here::here("posteriors", "post_time2zero_age.csv"))
post_logit_qol_a <- read_csv(here::here("posteriors", "post_logit_qol_a.csv"))
post_logit_qol_at <- read_csv(here::here("posteriors", "post_logit_qol_at.csv"))
post_logit_cap_qol_at <- read_csv(here::here("posteriors", "post_logit_cap_qol_at.csv"))


## Load data
load(here::here("data", "qol_reformed.rdata"))
min_qol <- min(reformed$EQ5D)


reformed %>% 
  ggplot() +
  geom_point(aes(x = ti, y = EQ5D))


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


## Simulation
n_sim <- 500

targets <- tibble(
  Key = 1:n_sim
) %>% 
  crossing(Age = 50:99, Ti = seq(0, 3, 0.02))


### T2Z + const qol
proj_m1 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_a(post_logit_qol_a, min_qol) %>% 
  calc_shortfall(pars_demo)


### T2Z + qol(t)
proj_m2 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_at(post_logit_qol_at, min_qol) %>% 
  calc_shortfall(pars_demo)


### Capped logit
proj_m3 <- targets %>% 
  sim_cap_qol_at(post_logit_cap_qol_at, min_qol, norm) %>% 
  calc_shortfall(pars_demo)


### Outputs
write_csv(proj_m1, here::here("posteriors", "pars_model1.csv"))
write_csv(proj_m2, here::here("posteriors", "pars_model2.csv"))
write_csv(proj_m3, here::here("posteriors", "pars_model3.csv"))


## Statistics
stats <- bind_rows(
  proj_m1 %>% mutate(Model = "M1"),
  proj_m2 %>% mutate(Model = "M2"),
  proj_m3 %>% mutate(Model = "M3")
) %>% 
  group_by(Age, Model) %>% 
  summarise(
    across(everything(), list(
      M = median,
      L = \(x) quantile(x, 0.025),
      U = \(x) quantile(x, 0.975)
    ))
  )

write_csv(stats, here::here("docs", "tabs", "stats_shortfall.csv"))


g_ql <- stats %>% 
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = QL35_L, ymax = QL35_U), alpha = 0.2) +
  geom_line(aes(y = QL35_M)) +
  facet_wrap(.~Model) +
  scale_y_continuous("Quality of life loss due to HZ against population norm in the UK") +
  scale_x_continuous("Age of rush onset") +
  expand_limits(y = c(0, 0.2))



g_qlh <- stats %>% 
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = QLH35_L, ymax = QLH35_U), alpha = 0.2) +
  geom_line(aes(y = QLH35_M)) +
  facet_wrap(.~Model) +
  scale_y_continuous("Quality of life loss due to HZ against perfect health") +
  scale_x_continuous("Age of rush onset") +
  expand_limits(y = c(0, 0.2))


g_ql
g_qlh

ggsave(g_ql, filename = here::here("docs", "figs", "g_proj_ql_loss_norm.png"), width = 7, height = 5)
ggsave(g_qlh, filename = here::here("docs", "figs", "g_proj_ql_loss_perfect.png"), width = 7, height = 5)



